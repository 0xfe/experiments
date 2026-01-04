'use strict';

const NOTE_NAME_TO_PC = new Map([
  ['C', 0], ['B#', 0],
  ['C#', 1], ['Db', 1],
  ['D', 2],
  ['D#', 3], ['Eb', 3],
  ['E', 4], ['Fb', 4],
  ['F', 5], ['E#', 5],
  ['F#', 6], ['Gb', 6],
  ['G', 7],
  ['G#', 8], ['Ab', 8],
  ['A', 9],
  ['A#', 10], ['Bb', 10],
  ['B', 11], ['Cb', 11]
]);

const PC_TO_SHARP = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
const PC_TO_FLAT  = ['C', 'Db', 'D', 'Eb', 'E', 'F', 'Gb', 'G', 'Ab', 'A', 'Bb', 'B'];

function normalizeNoteName(name) {
  if (!name || typeof name !== 'string') return null;
  const trimmed = name.trim();
  if (!trimmed) return null;
  const upper = trimmed[0].toUpperCase() + trimmed.slice(1);
  return upper.replace(/[^A-Ga-g#b]/g, '');
}

function noteNameToPc(name) {
  const norm = normalizeNoteName(name);
  if (!norm) return null;
  return NOTE_NAME_TO_PC.has(norm) ? NOTE_NAME_TO_PC.get(norm) : null;
}

function pcToNoteName(pc, preferFlats = false) {
  const idx = ((pc % 12) + 12) % 12;
  return preferFlats ? PC_TO_FLAT[idx] : PC_TO_SHARP[idx];
}

function transposePc(pc, semitones) {
  return ((pc + semitones) % 12 + 12) % 12;
}

function isFlatName(name) {
  return /b/.test(name);
}

const ROOT_CHOICES = [
  { label: 'C', name: 'C' },
  { label: 'C#/Db', name: 'C#' },
  { label: 'D', name: 'D' },
  { label: 'D#/Eb', name: 'D#' },
  { label: 'E', name: 'E' },
  { label: 'F', name: 'F' },
  { label: 'F#/Gb', name: 'F#' },
  { label: 'G', name: 'G' },
  { label: 'G#/Ab', name: 'G#' },
  { label: 'A', name: 'A' },
  { label: 'A#/Bb', name: 'A#' },
  { label: 'B', name: 'B' }
];

module.exports = {
  NOTE_NAME_TO_PC,
  PC_TO_SHARP,
  PC_TO_FLAT,
  ROOT_CHOICES,
  normalizeNoteName,
  noteNameToPc,
  pcToNoteName,
  transposePc,
  isFlatName
};
