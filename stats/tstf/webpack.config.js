const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const CopyPlugin = require('copy-webpack-plugin');
const GitRevisionPlugin = require('git-revision-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

const TerserJSPlugin = require('terser-webpack-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');


module.exports = (env) => {
  const tag = (env && env.TAG_NAME) || (process && process.env && process.env.TAG_NAME);
  const hasTag = typeof tag !== 'undefined' && tag !== '';
  const gitRevisionPlugin = new GitRevisionPlugin();

  return {
    optimization: {
      minimizer: [new TerserJSPlugin({}), new OptimizeCSSAssetsPlugin({})],
    },
    plugins: [
      new webpack.ProvidePlugin({
        $: 'zepto-webpack',
      }),
      new CleanWebpackPlugin(),
      new HtmlWebpackPlugin({
        template: './index.html',
        filename: 'index.html',
        inject: false,
      }),
      new MiniCssExtractPlugin({
        filename: hasTag ? `[name].${tag}.css` : '[name].[contenthash].css',
        chunkFilename: '[id].[contenthash].css',
      }),
      new CopyPlugin([{ from: 'static/*', flatten: true }], {
        // Always copy (for --watch / webpack-dev-server). Needed
        // because CleanWebpackPlugin wipes everything out.
        copyUnmodified: true,
      }),
      new webpack.DefinePlugin({
        NODE_ENV: JSON.stringify((env && env.NODE_ENV) || 'dev'),
        __VERSION: JSON.stringify(gitRevisionPlugin.version()),
        __COMMITHASH: JSON.stringify(gitRevisionPlugin.commithash()),
        __BRANCH: JSON.stringify(gitRevisionPlugin.branch()),
      }),
    ],
    devtool: (env && env.NODE_ENV === 'production') ? 'hidden-source-map' : false,
    entry: {
      index: './src/index.ts',
    },
    output: {
      filename: hasTag ? `[name].${tag}.js` : '[name].[contenthash].js',
      path: path.resolve(__dirname, 'dist'),
    },
    resolve: { extensions: ['.ts', '.tsx', '.js', '.json'] },
    module: {
      rules: [
        { test: /\.[jt]sx?$/, exclude: /node_modules/, use: [{ loader: 'babel-loader' }, { loader: 'eslint-loader', options: { fix: true } }] },
        { test: /\.css$/, use: [MiniCssExtractPlugin.loader, 'css-loader', 'postcss-loader'] },
        { test: /\.hbs$/, loader: 'handlebars-loader' },
        { test: /\.svg$/, loader: 'svg-inline-loader' },
      ],
    },
  };
};
