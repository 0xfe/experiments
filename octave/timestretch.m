function [output] = timestretch(audio, factor)
%
% Stretch audio track by 'factor'.
%
% Author: Mohit Muthanna Cheppudira <mohit@muthanna.com>
%
% Arguments:
%   audio: Audio data matrix (one channel per column)
%   factor: Stretching factor

[samples, channels] = size(audio);
output = [];

for each_c = 1:channels
  output = [output interp1([1:samples]', audio(:,each_c), [1:1/factor:samples]', "nearest")];
end

end
