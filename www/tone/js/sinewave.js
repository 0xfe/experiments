/*
   Sine Wave Generator for Web Audio API.
   Currently works on Chrome.

   Mohit Cheppudira - http://0xfe.blogspot.com
*/

/* Create a generator for the given AudioContext. */
SineWave = function(context) {
  this.x = 0;
  this.context = context;
  this.sampleRate = this.context.sampleRate;
  this.frequency = 440;
  this.next_frequency = this.frequency;
  this.amplitude = 0.5;
  this.playing = false;
  this.nr = true; // noise reduction

  // Create an audio node for the tone generator
  this.node = context.createJavaScriptNode(128, 1, 1);

  // Setup audio data callback for this node. The callback is called
  // when the node is connected and expects a buffer full of audio data
  // in return.
  var that = this;
  this.node.onaudioprocess = function(e) { that.process(e) };
}

SineWave.prototype.setAmplitude = function(amplitude) {
  this.amplitude = amplitude;
}

// Enable/Disable Noise Reduction
SineWave.prototype.setNR = function(nr) {
  this.nr = nr;
}

SineWave.prototype.setFrequency = function(freq) {
  this.next_frequency = freq;

  // Only change the frequency if not currently playing. This
  // is to minimize noise.
  if (!this.playing) this.frequency = freq;
}

SineWave.prototype.process = function(e) {
  // Get a reference to the output buffer and fill it up.
  var right = e.outputBuffer.getChannelData(0),
      left = e.outputBuffer.getChannelData(1);

  // We need to be careful about filling up the entire buffer and not
  // overflowing.
  for (var i = 0; i < right.length; ++i) {
    right[i] = left[i] = this.amplitude * Math.sin(
        this.x++ / (this.sampleRate / (this.frequency * 2 * Math.PI)));

    // A vile low-pass-filter approximation begins here.
    //
    // This reduces high-frequency blips while switching frequencies. It works
    // by waiting for the sine wave to hit 0 (on it's way to positive territory)
    // before switching frequencies.
    if (this.next_frequency != this.frequency) {
      if (this.nr) {
        // Figure out what the next point is.
        next_data = this.amplitude * Math.sin(
          this.x / (this.sampleRate / (this.frequency * 2 * Math.PI)));

        // If the current point approximates 0, and the direction is positive,
        // switch frequencies.
        if (right[i] < 0.001 && right[i] > -0.001 && right[i] < next_data) {
          this.frequency = this.next_frequency;
          this.x = 0;
        }
      } else {
        this.frequency = this.next_frequency;
        this.x = 0;
      }
    }
  }
}

SineWave.prototype.play = function() {
  // Plug the node into the output.
  this.node.connect(this.context.destination);
  this.playing = true;
}

SineWave.prototype.pause = function() {
  // Unplug the node.
  this.node.disconnect();
  this.playing = false;
}
