Raphael.prototype.vexLine = function(x, y, new_x, new_y) {
  return this.path("M" + x + " " + y + "L" + new_x + " " + new_y);
}

ChordBox = function(paper, x, y, width, height) {
  this.paper = paper;
  this.x = x;
  this.y = y;
  this.width = width;
  this.height = height;
  this.tuning = ["E", "A", "D", "G", "B", "E"];

  this.metrics = {
    circle_radius: this.width / 20,
    num_strings: 6,
    num_frets: 5,
    font_size: this.width / 8
  };

  this.spacing = this.width / this.metrics.num_strings;
  this.fret_spacing = this.height / (this.metrics.num_frets + 1);
  this.chord = [];
}

ChordBox.prototype.setChord = function(chord) {
  this.chord = chord;
}

ChordBox.prototype.draw = function() {
  var spacing = this.spacing;
  var fret_spacing = this.fret_spacing;

  for (var i = 0; i < this.metrics.num_strings; ++i) {
    this.paper.vexLine(this.x + (spacing * i), this.y,
      this.x + (spacing * i),
      this.y + (fret_spacing * (this.metrics.num_frets)));
  }

  for (var i = 0; i < this.metrics.num_frets + 1; ++i) {
    this.paper.vexLine(this.x, this.y + (fret_spacing * i),
      this.x + (spacing * (this.metrics.num_strings - 1)),
      this.y + (fret_spacing * i));
  }

  var tuning = this.tuning;
  for (var i = 0; i < tuning.length; ++i) {
    var t = this.paper.text(
      this.x + (this.spacing * i),
      this.y - (this.fret_spacing / 2) +
      ((this.metrics.num_frets + 1) * this.fret_spacing),
      tuning[i]);
    t.attr("font-size", this.metrics.font_size);
  }

  for (var i = 0; i < this.chord.length; ++i) {
    this.lightUp(this.chord[i][0], this.chord[i][1]);
  }
}

ChordBox.prototype.lightUp = function(string_num, fret_num) {
  string_num = this.metrics.num_strings - string_num;
  var x = this.x + (this.spacing * string_num);
  var y = this.y + (this.fret_spacing * (fret_num - 1)) +
    (this.fret_spacing / 2);

  var c = this.paper.circle(x, y, this.metrics.circle_radius)
  if (fret_num > 0) c.attr("fill", "#000");
}
