'use strict';

const assert = require('assert');
const { assignFingers, generateVoicings } = require('../src/voicings');
const { chordPcs, CHORD_TYPE_MAP } = require('../src/chords');
const { tuningToPcs, TUNINGS } = require('../src/tuning');

function assertFingerConsistency(frets, fingers) {
  const fingerToFret = new Map();
  frets.forEach((fret, idx) => {
    const finger = fingers[idx];
    if (fret <= 0 || finger <= 0) return;
    if (!fingerToFret.has(finger)) {
      fingerToFret.set(finger, fret);
      return;
    }
    const prevFret = fingerToFret.get(finger);
    assert.strictEqual(prevFret, fret, `Finger ${finger} used on different frets ${prevFret} and ${fret}`);
  });
}

const frets = [1, 3, 3, 2, 1, 1];
const fingers = assignFingers(frets);
assert.strictEqual(fingers[0], 1);
assert.strictEqual(fingers[3], 2);
assert.strictEqual(fingers[1], 3);
assertFingerConsistency(frets, fingers);

const standard = TUNINGS.find(t => t.id === 'standard');
const tuningPcs = tuningToPcs(standard);
const maj7 = CHORD_TYPE_MAP.get('maj7');
const pcs = chordPcs(0, maj7);

const voicings = generateVoicings({ tuningPcs, chordPcs: pcs, rootPc: 0 });
assert.ok(voicings.length > 0);

for (const v of voicings) {
  assert.strictEqual(v.frets.length, 6);
  assertFingerConsistency(v.frets, v.fingers);
}

console.log('voicings.test.js passed');
