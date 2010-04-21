// Mandelbrot Set Generator
// Mohit Muthanna Cheppudira - 2009
// http://muthanna.com
//
// See mandelbrot.html for sample usage.

function rgb(r, g, b) {
  return "rgb(" + r + "," + g + "," + b + ")";
}

function logBase(x, base) {
    return (Math.log(x))/(Math.log(base));
}

function Mandelbrot(canvasid) {
  this.canvas = document.getElementById(canvasid);
  this.ctx = this.canvas.getContext("2d");
  this.width = this.canvas.width;
  this.height = this.canvas.height;
  this.Clear(rgb(80,10,10));
}

Mandelbrot.prototype.Plot = function(x, y, fillStyle) {
  this.ctx.fillStyle = fillStyle;
  this.ctx.fillRect(x, y, 1, 1);
}

Mandelbrot.prototype.Clear = function(fillStyle) {
  this.ctx.fillStyle = fillStyle;
  this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
}

Mandelbrot.prototype.PlotCount = function(x, y, count) {
  if (count < 10) {
    this.Plot(x, y, rgb(150 - (count * 10), 0, 0));
  } else if (count < 40) {
    this.Plot(x, y, rgb(count * 5, 0, 0));
  } else {
    this.Plot(x, y, rgb(count * 2, count, 0));
  }
}

Mandelbrot.prototype.Draw = function(xcorner, ycorner, zoom) {
  this.ProgressiveDraw(xcorner, ycorner, zoom, 1, this.width);
}

Mandelbrot.prototype.ProgressiveDraw = function(xcorner, ycorner, zoom,
                                                xstart, xend) {
  for (var x = xstart; x < xend; x++) {
    for (var y = 1; y < this.height; y++) {
      var count = 0;
      var size = 0.0;
      var cx = xcorner + ((x * zoom) / this.width);
      var cy = ycorner + ((y * zoom) / this.height);

      var zx = 0;
      var zy = 0;

      while (count < 100 && size <= 4) {
        count += 1;
        temp = (zx * zx) - (zy * zy);
        zy = (2 * zx * zy) + cy;
        zx = temp + cx;
        size = (zx * zx) + (zy * zy);
      }

      this.PlotCount(x, y, count);
    }
  }
}
