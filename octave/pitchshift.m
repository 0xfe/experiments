function [output] = pitchshift(audio, pitch_shift, sample_rate)
%
% Shift pitch of given audio data while preserving speed/tempo.
%
% Author: Mohit Muthanna Cheppudira <mohit@muthanna.com>
%
% Arguments:
%   audio: Audio data matrix (one channel per column)
%   pitch_shift: Pitch shift factor. 2 = one octave up, 0.5 = one octave down.
%   sample_rate: Sample rate. (Default 44100)

if (nargin < 2)
  pitch_shift = 2;
end

if (nargin < 3)
  sample_rate = 44100;
end

% Frame size for fourier transform. Powers of two work best.
chunk_size = 2048;

% The center frequency of each bin of the frequency-domain data.
freq_per_bin = sample_rate / chunk_size;

% Get number of samples and channels from provided audio.
[num_samples, num_channels] = size(audio);
total_samples = num_samples;
padding = 0;

% Pad audio data so that last chunk has 'chunk_size' samples.
if mod(num_samples, chunk_size) > 0
  padding = chunk_size - mod(num_samples, chunk_size);
  total_samples = num_samples + padding;
  audio = [audio; zeros(padding, num_channels)];
end

% Precalculate operations matrices.
% The windowing matrix is used to apply hanning window to the audio frame to
% reduce the spectral smear caused by frequencies with non-integral cycles.
windowing_matrix = repmat(hanning(chunk_size), 1, num_channels);

% These index matrices assist in writing vectorized code.
index_matrix = repmat([0:(chunk_size-1)]', 1, num_channels);
half_index_matrix = index_matrix(1:chunk_size/2, :);

% Expected phase shift in each frame. We use an overlapping factor of 4 (75%.
% This allows us to slightly more accurately estimate phase deltas for
% frequencies with non-integral cycles.
osamp = 4;
step_size = chunk_size / osamp;
expct = 2 * pi * step_size / chunk_size;
expected_phase_shift = half_index_matrix * expct;

% We need to keep track of the phase of the last frame to calculate the
% phase deltas, and estimate true frequencies.
last_phase = zeros(chunk_size/2, num_channels);

% Phase is accumulated here for the final output.
sum_phase = zeros(chunk_size/2, num_channels);
output_accum = zeros(chunk_size * 2, num_channels);

for i = 0:step_size:(total_samples - chunk_size - 1)
  startx = i + 1;
  endx = (startx - 1) + chunk_size;

  fprintf('Pitch shifting samples: %d through %d\n', startx, endx);
  fflush(1);

  %%%%%%%%%% ANALYSIS %%%%%%%%%%%%
  % Extract data and apply hanning window (removed)
  data = audio(startx:endx,:);

  % TODO(0xfe): Something's broken when de-windowing the samples, so this is
  % currently commented out.
  % data .*= windowing_matrix;

  % Perform FFT and strip out second half (overtones)
  ft = fft(data)(1:chunk_size/2, :);

  % calculate magnitude and phase
  phase = angle(ft);

  % Calculate phase shift between current and last frame
  phase_shift = phase .- last_phase;
  last_phase = phase;

  % Subtract expected phase difference
  phase_shift .-= expected_phase_shift;

  % Wrap phase_shift to between -pi and +pi centered around 0
  phase_shift .-= pi * (fix(phase_shift / pi));

  % Get deviation from bin frequency
  phase_shift = (osamp * phase_shift) ./ (2 * pi);

  % Calculate the k-th partial's true frequency and associate magnitde
  true_freq = (freq_per_bin .* half_index_matrix) .+ (phase_shift .* freq_per_bin);
  true_mag = abs(ft); % magnitude

  %%%%%%%%%% PITCH SHIFT %%%%%%%%%%%%

  shifted_freq = zeros(chunk_size/2, num_channels);
  shifted_mag = zeros(chunk_size/2, num_channels);

  % This part of the code is tricky to vectorize, so we use straight up loops.
  for chan = 1:num_channels
    for bin = 0 : ((chunk_size / 2) - 1)
      index = round(bin * pitch_shift) + 1;
      if (index <= chunk_size / 2)
        shifted_mag(index, chan) += true_mag(bin+1, chan);
        shifted_freq(index, chan) = true_freq(bin+1, chan) * pitch_shift;
      end
    end
  end

  %%%%%%%%%% SYNTHESIS %%%%%%%%%%%%

  % Subtract bin mid-frequency
  shifted_freq .-= (half_index_matrix .* freq_per_bin);

  % Get bin deviation from frequency deviation
  shifted_freq ./= freq_per_bin;
  shifted_freq = 2 * pi * (shifted_freq ./ osamp);

  % Add overlap phase advance back in
  shifted_freq .+= expected_phase_shift;

  % Accumulate delta phase to get bin phase
  sum_phase .+= shifted_freq;

  % Get real/imaginary parts for ifft
  output_data = shifted_mag .* cos(sum_phase);
  output_data .+= j * (shifted_mag .* sin(sum_phase));

  % Specify chunk_size here because we need to pad with chunk_size/2 zeros
  data = real(ifft(output_data, chunk_size));

  % Inverse hanning window (removed)
  output_accum(1:chunk_size,:) = data;
  output(startx:startx+step_size - 1,:) = output_accum(1:step_size,:);
end

end
