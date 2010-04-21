/* 
JSGoL - Game of Death in Javascript
2007 - Mohit Muthanna <mohit AT muthanna DOT com>

How to embed this game into your page:

<html>
<head>
<script src="js/GoL.js"></script>
<script language="JavaScript">
var gol;

function Initialize() {
  var grid = new Grid("life", 50, 50);

  // Canvas colors
  grid.ctx.fillStyle = "rgba(128,128,10,0.2)";
  grid.ctx.lineWidth = 1;
  grid.ctx.strokeStyle = "rgba(0,0,0,0.2)"
 
  // Setup the game and draw
  gol = new GoL(grid);
  gol.Draw();
}
</script>
</head>

<body>
  <canvas id="life" width="400" height="400"></canvas>  
  <p/>  

  <input type="button" value="Start" onClick='gol.StartGame()'/>  
  <input type="button" value="Stop" onClick='gol.StopGame()'/>
</body>
</html>


*/

// The Grid object. This is basically a wrapper around a 2D Canvas object.
function Grid(tag, grid_width, grid_height) {
  this.canvas = document.getElementById(tag);
  this.ctx = this.canvas.getContext("2d");

  this.grid_height = grid_height;
  this.grid_width = grid_width;
}

Grid.prototype.BlockHeight = function () {
  return this.canvas.height / this.grid_height;
}

Grid.prototype.BlockWidth = function () {
  return this.canvas.width / this.grid_width;
}

Grid.prototype.SetFillStyle = function (style) {
  this.ctx.fillStyle = style;
}

Grid.prototype.Plot = function (x, y) {
  this.ctx.fillRect(x * this.BlockWidth(), 
                    y * this.BlockHeight(),
                    this.BlockWidth(),
                    this.BlockHeight());

  this.Stroke(x, y);
}

Grid.prototype.UnPlot = function (x, y) {
  this.ctx.clearRect(x * this.BlockWidth(), 
                     y * this.BlockHeight(),
                     this.BlockWidth(),
                     this.BlockHeight());
  
  this.Stroke(x, y);
}

Grid.prototype.Stroke = function (x, y) {
  this.ctx.strokeRect(x * this.BlockWidth(), 
                      y * this.BlockHeight(),
                      this.BlockWidth(),
                      this.BlockHeight());
}

Grid.prototype.GetCellX = function (x) {
  return Math.floor((x - this.canvas.offsetLeft) / this.BlockWidth());
}

Grid.prototype.GetCellY = function (y) {
  return Math.floor((y - this.canvas.offsetTop) / this.BlockHeight());
}

// CellMap: A representaion of the board using multidimensional arrays.

function CellMap(width, height) {
  this.width = width;
  this.height = height;

  // Empty the Map
  this.Clear()
}

CellMap.prototype.Width = function () {
  return this.width;
}

CellMap.prototype.Height = function () {
  return this.height;
}

CellMap.prototype.Clear = function () {
  this.cells = new Array(this.width);

  for (w = 0; w < this.width; w++) {
    this.cells[w] = new Array(this.height);
    for (h = 0; h < this.height; h++) {
      this.cells[w][h] = false;
    }
  }
}

CellMap.prototype.ValidateCoordinates = function (x, y) {
  if (x >= this.width || x < 0) return false;
  if (y >= this.height || y < 0) return false;

  return true;
}
  
CellMap.prototype.Set = function (x, y, value) {
  if (!this.ValidateCoordinates(x, y)) 
    throw new Error("Coordinates out of range.");

  this.cells[x][y] = value;
}

CellMap.prototype.Get = function (x, y) {
  if (this.ValidateCoordinates(x, y) == false)
    return false;
  else return this.cells[x][y];
}

// GoL: The Game of Death. Business Logic (really).
  
function GoL(grid) {
  this.grid = grid;
  this.grid.canvas.gol = this;
  this.grid.canvas.onmousedown = this.MouseDown;
  this.grid.canvas.onmouseup = this.MouseUp;
  this.grid.canvas.onmousemove = this.MouseMove;

  this.width = this.grid.grid_width;
  this.height = this.grid.grid_height;

  this.cellmap = new CellMap(this.width, this.height);
  this.cellmap.Clear();

  this.dragging = false;
  this.running = false;
}

GoL.prototype.GiveBirth = function (x, y) {
  this.cellmap.Set(x, y, true);
}

GoL.prototype.Die = function (x, y) {
  this.cellmap.Set(x, y, false);
}

GoL.prototype.Occupied = function (x, y) {
  return this.cellmap.Get(x, y);
}

GoL.prototype.Draw = function () {
  for (x = 0; x < this.cellmap.Width(); x++) {
    for (y = 0; y < this.cellmap.Height(); y++) {
      if (this.cellmap.Get(x, y)) {
        this.grid.Plot(x, y);
      } else {
        this.grid.UnPlot(x, y);
      }
    }
  }
}

// Return the number of live neighbours, given a
// cell.
GoL.prototype.NeighbourCount = function (x, y) {
  count = 0;

  // Get top left to top right, and bottom left
  // to bottom right.
  for (w = -1; w < 2; w++) {
    if (this.cellmap.Get(x + w, y - 1))
      count++;
    if (this.cellmap.Get(x + w, y + 1))
      count++;
  }

  // Get left and right cells
  if (this.cellmap.Get(x - 1, y)) count++;
  if (this.cellmap.Get(x + 1, y)) count++;

  return count;
}
      
GoL.prototype.NextGeneration = function () {
  for (x = 0; x < this.width; x++) {
    for (y = 0; y < this.height; y++) {
      // Get number of neighbours
      neighbours = this.NeighbourCount(x, y);

      // If the cell is occupied
      if (this.Occupied(x, y)) {
        // Die of overcrowding
        if (neighbours >= 4 && neighbours <= 8)
          this.Die(x, y);
        // Die of loneliness
        if (neighbours == 0 || neighbours == 1)
          this.Die(x, y);
      } else {
        // Be born
        if (neighbours == 3) {
          this.GiveBirth(x, y, true);
        }
      }
    }
  }
}

GoL.prototype.Clicked = function (e) {
  x = this.grid.GetCellX(e.clientX);
  y = this.grid.GetCellY(e.clientY);

  this.GiveBirth(x, y);
  this.grid.Plot(x, y);
}

GoL.prototype.MouseDown = function (e) {
  this.gol.dragging = true;
  this.gol.Clicked(e);
}

GoL.prototype.MouseUp = function (e) {
  this.gol.dragging = false;
}

GoL.prototype.MouseMove = function (e) {
  if (this.gol.dragging) this.gol.Clicked(e);
}

GoL.prototype.StartGame = function () {
  this.running = true;
  this.Run(this);
}

GoL.prototype.ClearGame = function () {
  this.cellmap.Clear();
  this.Draw();
}

GoL.prototype.StopGame = function () {
  this.running = false;
}

GoL.prototype.Run = function (a_gol) {
  a_gol.Draw();
  if (a_gol.running) 
    setTimeout (function () {a_gol.Run(a_gol)}, 1000);

  a_gol.NextGeneration();
}
