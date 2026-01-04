'use strict';

const { noteNameToPc } = require('./notes');

const TUNINGS = [
  { id: 'standard', label: 'Standard (EADGBE)', notes: ['E', 'A', 'D', 'G', 'B', 'E'] },
  { id: 'drop-d', label: 'Drop D (DADGBE)', notes: ['D', 'A', 'D', 'G', 'B', 'E'] },
  { id: 'dadgad', label: 'DADGAD', notes: ['D', 'A', 'D', 'G', 'A', 'D'] },
  { id: 'open-g', label: 'Open G (DGDGBD)', notes: ['D', 'G', 'D', 'G', 'B', 'D'] },
  { id: 'open-d', label: 'Open D (DADF#AD)', notes: ['D', 'A', 'D', 'F#', 'A', 'D'] },
  { id: 'open-c', label: 'Open C (CGCGCE)', notes: ['C', 'G', 'C', 'G', 'C', 'E'] },
  { id: 'open-e', label: 'Open E (EBEG#BE)', notes: ['E', 'B', 'E', 'G#', 'B', 'E'] },
  { id: 'eb-standard', label: 'Eb Standard (Eb Ab Db Gb Bb Eb)', notes: ['Eb', 'Ab', 'Db', 'Gb', 'Bb', 'Eb'] },
  { id: 'd-standard', label: 'D Standard (DGCFAD)', notes: ['D', 'G', 'C', 'F', 'A', 'D'] },
  { id: 'all-fourths', label: 'All Fourths (EADGCF)', notes: ['E', 'A', 'D', 'G', 'C', 'F'] }
];

function tuningToPcs(tuning) {
  return tuning.notes.map(noteNameToPc);
}

module.exports = {
  TUNINGS,
  tuningToPcs
};
