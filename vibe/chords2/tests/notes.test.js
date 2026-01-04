'use strict';

const assert = require('assert');
const { noteNameToPc, pcToNoteName, transposePc, isFlatName } = require('../src/notes');

assert.strictEqual(noteNameToPc('C'), 0);
assert.strictEqual(noteNameToPc('C#'), 1);
assert.strictEqual(noteNameToPc('Db'), 1);
assert.strictEqual(noteNameToPc('F#'), 6);
assert.strictEqual(noteNameToPc('Gb'), 6);
assert.strictEqual(noteNameToPc('Bb'), 10);

assert.strictEqual(pcToNoteName(10, true), 'Bb');
assert.strictEqual(pcToNoteName(10, false), 'A#');

assert.strictEqual(transposePc(0, 7), 7);
assert.strictEqual(transposePc(11, 1), 0);

assert.strictEqual(isFlatName('Bb'), true);
assert.strictEqual(isFlatName('F#'), false);

console.log('notes.test.js passed');
