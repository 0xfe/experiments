Build MIDI file:

$ gem install midilib
$ ./midigen

Convert to WAV:
$ brew install fluidsynth
$ fluidsynth -a coreaudio ~/w/audio/octave/FluidR3_GM.sf2 from_scratch.mid -F test.ra

$ brew install sox
$ sox -t raw -r 44100 -e signed -b 16 -c 2 test.raw ~/Downloads/test1.wav


