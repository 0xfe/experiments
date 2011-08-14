/*
   SpectrumBox - A JavaScript spectral analyzer.
   Mohit Cheppudira - 0xfe.blogspot.com
*/

/**
  @constructor
  Create an n-point FFT based spectral analyzer.

  @param num_points - Number of points for transform.
  @param num_bins - Number of bins to show on canvas.
  @param canvas_id - Canvas element ID.
  @param audio_context - An AudioContext instance.
*/
SpectrumBox = function(num_points, num_bins, canvas_id, audio_context) {
  this.num_bins = num_bins;
  this.num_points = num_points;
  this.canvas_id = canvas_id;
  this.update_rate_ms = 50;
  this.smoothing = 0.75;
  this.enabled = false;

  // Number of points we actually want to display. If zero, display all points.
  this.valid_points = 0;

  // Determine the boundaries of the canvas.
  this.canvas = document.getElementById(canvas_id);
  this.width = this.canvas.width - 12;
  this.height = this.canvas.height - 12;
  this.bar_spacing = 3;

  this.ctx = this.canvas.getContext('2d');
  this.actx = audio_context;

  // Create the spectral analyzer
  this.fft = this.actx.createAnalyser();
  this.fft.fftSize = this.num_points;
  this.data = new Uint8Array(this.fft.frequencyBinCount);
}

/* Returns the AudioNode of the FFT. You can route signals into this. */
SpectrumBox.prototype.getAudioNode = function() {
  return this.fft;
}

/* Returns the canvas' 2D context. Use this to configure the look
   of the display. */
SpectrumBox.prototype.getCanvasContext = function() {
  return this.ctx;
}

/* Set the number of points to work with. */
SpectrumBox.prototype.setValidPoints = function(points) {
  this.valid_points = points;
}

/* Enable the analyzer. Starts drawing stuff on the canvas. */
SpectrumBox.prototype.enable = function() {
  var that = this;
  window.setTimeout(function() { that.update(); }, this.update_rate_ms);
  this.enabled = true;
}

/* Disable the analyzer. Stops drawing stuff on the canvas. */
SpectrumBox.prototype.disable = function() {
  this.enabled = false;
}

/* Updates the canvas display. */
SpectrumBox.prototype.update = function() {
  // Get the frequency samples
  data = this.data;
  this.fft.smoothingTimeConstant = this.smoothing;
  this.fft.getByteFrequencyData(data);

  var length = data.length;
  if (this.valid_points > 0) length = this.valid_points;

  // Break the samples up into bins
  var bin_size = Math.floor(length / this.num_bins);
  for (var i=0; i < this.num_bins; ++i) {
    var sum = 0;
    for (var j=0; j < bin_size; ++j) {
      sum += data[(i * bin_size) + j];
    }

    // Calculate the average frequency of the samples in the bin
    var average = sum / bin_size;

    // Draw the bars on the canvas
    var bar_size = this.width / this.num_bins;
    var scaled_average = (average / 256) * this.height;

    this.ctx.clearRect(
      i * bar_size - 1,
      this.height + 1,
      (bar_size - this.bar_spacing) + 2,
      -this.height - 2);
    this.ctx.fillRect(
      i * bar_size, this.height, bar_size - this.bar_spacing, -scaled_average);
    this.ctx.strokeRect(
      i * bar_size, this.height, bar_size - this.bar_spacing, -scaled_average);
  }

  // Reschedule update
  if (this.enabled) {
    that = this;
    window.setTimeout(function() { that.update(); }, this.update_rate_ms);
  }
}
