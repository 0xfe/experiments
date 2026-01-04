'use strict';

const assert = require('assert');
const { parseChordStructure, CHORD_TYPE_MAP, chordPcs } = require('../src/chords');

const parsed = parseChordStructure('1;3;5;b7;9');
assert.deepStrictEqual(parsed.intervals, [0, 4, 7, 10, 14]);
assert.strictEqual(parsed.formula, '1-3-5-b7-9');

const maj7 = CHORD_TYPE_MAP.get('maj7');
assert.ok(maj7);

const pcs = chordPcs(0, maj7);
assert.deepStrictEqual(pcs.sort((a, b) => a - b), [0, 4, 7, 11]);

console.log('chords.test.js passed');
