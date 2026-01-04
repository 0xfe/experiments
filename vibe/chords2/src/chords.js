'use strict';

const { transposePc } = require('./notes');

const MAJOR_DEGREE_TO_SEMITONE = {
  1: 0,
  2: 2,
  3: 4,
  4: 5,
  5: 7,
  6: 9,
  7: 11
};

const CHORD_TYPE_STRUCTURES = {
  major: '1;3;5',
  minor: '1;b3;5',
  dim: '1;b3;b5',
  dim7: '1;b3;b5;bb7',
  sus2: '1;2;5',
  sus4: '1;4;5',
  '7sus4': '1;4;5;b7',
  aug: '1;3;#5',
  '6': '1;3;5;6',
  '69': '1;3;5;6;9',
  '7': '1;3;5;b7',
  '7b5': '1;3;b5;b7',
  aug7: '1;3;#5;b7',
  '9': '1;3;5;b7;9',
  '9b5': '1;3;b5;b7;9',
  aug9: '1;3;#5;b7;9',
  '7b9': '1;3;5;b7;b9',
  '7#9': '1;3;5;b7;#9',
  '11': '1;3;5;b7;9;11',
  '9#11': '1;3;5;b7;9;#11',
  '13': '1;3;5;b7;9;11;13',
  maj7: '1;3;5;7',
  maj7b5: '1;3;b5;7',
  'maj7#5': '1;3;#5;7',
  maj9: '1;3;5;7;9',
  maj11: '1;3;5;7;9;11',
  maj13: '1;3;5;7;9;11;13',
  m6: '1;b3;5;6',
  m69: '1;b3;5;6;9',
  m7: '1;b3;5;b7',
  m7b5: '1;b3;b5;b7',
  m9: '1;b3;5;b7;9',
  m11: '1;b3;5;b7;9;11',
  mmaj7: '1;b3;5;7',
  mmaj7b5: '1;b3;b5;7',
  mmaj9: '1;b3;5;7;9',
  mmaj11: '1;b3;5;7;9;11',
  add9: '1;3;5;9',
  madd9: '1;b3;5;9'
};

function degreeTokenToInterval(token) {
  const match = token.match(/^([#b]*)(\d+)$/);
  if (!match) return null;
  const accidentals = match[1] || '';
  const degree = parseInt(match[2], 10);
  if (!degree || degree < 1) return null;

  const baseDegree = ((degree - 1) % 7) + 1;
  const octave = Math.floor((degree - 1) / 7);
  const base = MAJOR_DEGREE_TO_SEMITONE[baseDegree] + octave * 12;

  let offset = 0;
  for (const ch of accidentals) {
    if (ch === '#') offset += 1;
    if (ch === 'b') offset -= 1;
  }

  return base + offset;
}

function parseChordStructure(structure) {
  if (!structure) return { tokens: [], intervals: [], formula: '' };
  const tokens = structure.split(';').map(token => token.trim()).filter(Boolean);
  const intervals = tokens
    .map(degreeTokenToInterval)
    .filter(interval => interval !== null);
  const formula = tokens.join('-');
  return { tokens, intervals, formula };
}

function labelFromId(id) {
  const map = {
    major: 'Major',
    minor: 'Minor',
    dim: 'Diminished',
    dim7: 'Diminished 7',
    sus2: 'Sus2',
    sus4: 'Sus4',
    '7sus4': '7sus4',
    aug: 'Augmented',
    '6': 'Sixth',
    '69': 'Sixth / Ninth',
    '7': 'Dominant 7',
    '7b5': '7b5',
    aug7: 'Augmented 7',
    '9': 'Dominant 9',
    '9b5': '9b5',
    aug9: 'Augmented 9',
    '7b9': '7b9',
    '7#9': '7#9',
    '11': 'Dominant 11',
    '9#11': '9#11',
    '13': 'Dominant 13',
    maj7: 'Major 7',
    maj7b5: 'maj7b5',
  'maj7#5': 'maj7#5',
    maj9: 'Major 9',
    maj11: 'Major 11',
    maj13: 'Major 13',
    m6: 'Minor 6',
    m69: 'Minor 6/9',
    m7: 'Minor 7',
    m7b5: 'Half-diminished',
    m9: 'Minor 9',
    m11: 'Minor 11',
    mmaj7: 'Minor Major 7',
    mmaj7b5: 'mMaj7b5',
    mmaj9: 'Minor Major 9',
    mmaj11: 'Minor Major 11',
    add9: 'Add9',
    madd9: 'mAdd9'
  };
  return map[id] || id;
}

function shortFromId(id) {
  const map = {
    major: '',
    minor: 'm',
    dim: 'dim',
    dim7: 'dim7',
    sus2: 'sus2',
    sus4: 'sus4',
    '7sus4': '7sus4',
    aug: 'aug',
    '6': '6',
    '69': '6/9',
    '7': '7',
    '7b5': '7b5',
    aug7: 'aug7',
    '9': '9',
    '9b5': '9b5',
    aug9: 'aug9',
    '7b9': '7b9',
    '7#9': '7#9',
    '11': '11',
    '9#11': '9#11',
    '13': '13',
    maj7: 'maj7',
    maj7b5: 'maj7b5',
    'maj7#5': 'maj7#5',
    maj9: 'maj9',
    maj11: 'maj11',
    maj13: 'maj13',
    m6: 'm6',
    m69: 'm6/9',
    m7: 'm7',
    m7b5: 'm7b5',
    m9: 'm9',
    m11: 'm11',
    mmaj7: 'mMaj7',
    mmaj7b5: 'mMaj7b5',
    mmaj9: 'mMaj9',
    mmaj11: 'mMaj11',
    add9: 'add9',
    madd9: 'madd9'
  };
  return map[id] ?? id;
}

function categoryFromId(id) {
  if (/sus/.test(id)) return 'Suspended';
  if (/13|11|9/.test(id)) return 'Extended';
  if (/b|#|aug|dim/.test(id)) return 'Altered';
  if (/7/.test(id)) return 'Sevenths';
  if (/6/.test(id)) return 'Sixths';
  return 'Basic';
}

const CHORD_TYPES = Object.entries(CHORD_TYPE_STRUCTURES).map(([id, structure]) => {
  const { intervals, formula } = parseChordStructure(structure);
  return {
    id,
    label: labelFromId(id),
    short: shortFromId(id),
    category: categoryFromId(id),
    intervals,
    formula
  };
}).sort((a, b) => {
  if (a.category !== b.category) return a.category.localeCompare(b.category);
  return a.label.localeCompare(b.label);
});

const CHORD_TYPE_MAP = new Map(CHORD_TYPES.map(type => [type.id, type]));

function chordPcs(rootPc, chordType) {
  return chordType.intervals.map(interval => transposePc(rootPc, interval));
}

module.exports = {
  parseChordStructure,
  CHORD_TYPES,
  CHORD_TYPE_MAP,
  chordPcs
};
