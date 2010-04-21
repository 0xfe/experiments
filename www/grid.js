function Canvas(tag) {
  this.canvas = document.getElementById(tag);
  this.ctx = this.canvas.getContext("2d");
}

Canvas.prototype.Width = function () {
  return this.canvas.width;
}

Canvas.prototype.Height = function () {
  return this.canvas.height;
}

Canvas.prototype.SaveState = function () {
  this.ctx.save();
}

Canvas.prototype.RestoreState = function () {
  this.ctx.restore();
}

Canvas.prototype.Box = function (x1, y1, w, h, style) {
  this.ctx.fillStyle = style;
  this.ctx.fillRect(x1, y1, w, h);
}

Canvas.prototype.Move = function (x) {
  this.canvas.style.position = "absolute";
  this.canvas.style.left = x + "px";
}

Canvas.prototype.Graph = function (array) {
  this.ctx.Box;
}

function Grid(canvas, grid_width, grid_height, default_style) {
  this.canvas = canvas;
  this.grid_height = grid_height;
  this.grid_width = grid_width;
}

Grid.prototype.BlockHeight = function () {
  return this.canvas.Height() / this.grid_height;
}

Grid.prototype.BlockWidth = function () {
  return this.canvas.Width() / this.grid_width;
}

Grid.prototype.Plot = function (x, y, style) {
  this.canvas.Box(x * this.BlockWidth(),
                  y * this.BlockHeight(),
                  this.BlockWidth(),
                  this.BlockHeight(),
                  style);
}

Grid.prototype.Clear = function (style) {
  this.canvas.Box(0, 0, this.canvas.Width(), this.canvas.Height(), style);
}
