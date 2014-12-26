#! /usr/bin/env ruby
#
# usage: from_scratch.rb
#
# This script shows you how to create a new sequence from scratch and save it
# to a MIDI file. It creates a file called 'from_scratch.mid'.

# Start looking for MIDI module classes in the directory above this one.
# This forces us to use the local copy, even if there is a previously
# installed version out there somewhere.
$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'midilib/sequence'
require 'midilib/consts'
include MIDI

seq = Sequence.new()

# Create a first track for the sequence. This holds tempo events and stuff
# like that.
track = Track.new(seq)
seq.tracks << track
track.events << Tempo.new(Tempo.bpm_to_mpq(120))
track.events << MetaEvent.new(META_SEQ_NAME, 'Sequence Name')

# Create a track to hold the notes. Add it to the sequence.
track = Track.new(seq)
seq.tracks << track

# Give the track a name and an instrument name (optional).
track.name = 'My New Track'
track.instrument = GM_PATCH_NAMES[0]

# Add a volume controller event (optional).
track.events << Controller.new(0, CC_VOLUME, 127)

# Add events to the track: a major scale. Arguments for note on and note off
# constructors are channel, note, velocity, and delta_time. Channel numbers
# start at zero. We use the new Sequence#note_to_delta method to get the
# delta time length of a single quarter note.
track.events << ProgramChange.new(0, 1, 0)
quarter_note_length = seq.note_to_delta('quarter')

# Intervals
module I
  IU = 0
  Im2 = 1
  IM2 = 2
  Im3 = 3
  IM3 = 4
  Ip4 = 5
  Id5 = 6
  Ip5 = 7
  Ia5 = 8
  Im6 = 8
  IM6 = 9
  Im7 = 10
  IM7 = 11
  IO = 12
end

# Chords
module C
  include I

  Major_i0 = [IM3, Ip5]
  Major_i1 = [Im3, Ip4]
  Major_i2 = [Ip4, Im3]

  def gen(intervals, bottom)
    return [bottom] + intervals.map {|i| bottom + i}
  end

  module_function :gen
end

include C

def add_chord(events, notes, duration, velocity=127)
  notes.each do |i|
    events << NoteOn.new(0, i, velocity, 0)
  end
  notes.each do |i|
    events << NoteOff.new(0, i, velocity, duration)
  end
end

[0, 2, 4, 5, 7, 9, 11, 12].each do |offset|
  add_chord(track.events, C.gen(Major_i0, 64 + offset), quarter_note_length)
end

# Calling recalc_times is not necessary, because that only sets the events'
# start times, which are not written out to the MIDI file. The delta times are
# what get written out.

# track.recalc_times
puts "Generating MIDI for #{track.instrument}"
File.open('from_scratch.mid', 'wb') { |file| seq.write(file) }