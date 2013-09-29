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
  this.amplitude = 0.75;

  // Create an audio node for the tone generator
  this.node = context.createJavaScriptNode(128, 3, 3);

  // Setup audio data callback for this node. The callback is called
  // when the node is connected and expects a buffer full of audio data
  // in return.
  var that = this;
  this.node.onaudioprocess = function(e) { that.process(e) };
}

SineWave.prototype.setAmplitude = function(amplitude) {
  this.amplitude = amplitude;
}

SineWave.prototype.setFrequency = function(freq) {
  this.next_frequency = freq;
}

SineWave.prototype.getAudioNode = function() {
  return this.node;
}

SineWave.prototype.process = function(e) {
  // Get a reference to the output buffer and fill it up.
  var data = e.outputBuffer.getChannelData(0);

  // We need to be careful about filling up the entire buffer and not
  // overflowing.
  var amp = this.amplitude,
      freq = this.frequency,
      sr = this.sampleRate,
      x = this.x,
      len = data.length;
  for (var i = 0; i < len; i++) {
    // Add phase derivative
    x = x + ((2 * Math.PI * freq) / sr);
    // Prevent overflow (not really needed...)
    if (x > 2 * Math.PI) x = x - 2 * Math.PI;
    data[i] = amp * Math.sin(x);
  }
  this.x = x;
}
