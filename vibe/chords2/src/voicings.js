'use strict';

const { transposePc } = require('./notes');

function fretToPc(openPc, fret) {
  return transposePc(openPc, fret);
}

function generateCandidatesForString(openPc, chordSet, options) {
  const frets = [];
  for (let fret = options.minFret; fret <= options.maxFret; fret += 1) {
    const pc = fretToPc(openPc, fret);
    if (chordSet.has(pc)) frets.push(fret);
  }
  frets.sort((a, b) => a - b);
  return frets.slice(0, options.candidateLimit);
}

function computeSpan(frets) {
  const fretted = frets.filter(f => f > 0);
  if (fretted.length === 0) return 0;
  return Math.max(...fretted) - Math.min(...fretted);
}

function countSoundingStrings(frets) {
  return frets.filter(f => f >= 0).length;
}

function hasRoot(frets, tuningPcs, rootPc) {
  for (let i = 0; i < frets.length; i += 1) {
    if (frets[i] < 0) continue;
    const pc = fretToPc(tuningPcs[i], frets[i]);
    if (pc === rootPc) return true;
  }
  return false;
}

function scoreVoicing({ frets, tuningPcs, rootPc }) {
  const fretted = frets.filter(f => f > 0);
  const minFret = fretted.length ? Math.min(...fretted) : 0;
  const maxFret = fretted.length ? Math.max(...fretted) : 0;
  const span = computeSpan(frets);
  const openCount = frets.filter(f => f === 0).length;
  const bassIndex = frets.findIndex(f => f >= 0);
  const bassPc = bassIndex >= 0 ? fretToPc(tuningPcs[bassIndex], frets[bassIndex]) : null;
  return (minFret || 0) * 4 + span * 3 + (5 - openCount) + (bassPc === rootPc ? -3 : 2) + (maxFret > 7 ? 2 : 0);
}

function assignFingers(frets) {
  const fingers = frets.map(() => 0);
  const fretted = frets.map((fret, idx) => ({ fret, idx })).filter(item => item.fret > 0);
  if (!fretted.length) return fingers;

  const fretsUsed = [...new Set(fretted.map(item => item.fret))].sort((a, b) => a - b);
  const fretGroups = new Map();
  for (const item of fretted) {
    if (!fretGroups.has(item.fret)) fretGroups.set(item.fret, []);
    fretGroups.get(item.fret).push(item.idx);
  }

  const barreFrets = new Set();
  for (const [fret, indices] of fretGroups.entries()) {
    if (indices.length >= 2) barreFrets.add(fret);
  }

  const fingerMap = new Map();
  let finger = 1;
  const minFret = fretsUsed[0];
  if (barreFrets.has(minFret)) {
    fingerMap.set(minFret, 1);
    finger = 2;
  }

  for (const fret of fretsUsed) {
    if (fingerMap.has(fret)) continue;
    if (finger > 4) {
      fingerMap.set(fret, 0);
      continue;
    }
    fingerMap.set(fret, finger);
    finger += 1;
  }

  fretted.forEach(item => {
    const assigned = fingerMap.get(item.fret) || 0;
    fingers[item.idx] = assigned;
  });

  return fingers;
}

function generateVoicings({
  tuningPcs,
  chordPcs,
  rootPc,
  options = {}
}) {
  const opts = {
    minFret: 0,
    maxFret: 12,
    maxSpan: 4,
    maxMutes: 2,
    minStrings: 4,
    maxVoicings: 10,
    candidateLimit: 6,
    ...options
  };

  const chordSet = new Set(chordPcs);
  const stringCandidates = tuningPcs.map(openPc => {
    const frets = generateCandidatesForString(openPc, chordSet, opts);
    if (opts.maxMutes > 0) frets.push(-1);
    return frets;
  });

  const results = [];

  function dfs(stringIndex, frets, toneSet, minFret, maxFret, soundingStrings, muteCount) {
    if (stringIndex === tuningPcs.length) {
      if (soundingStrings < opts.minStrings) return;
      if (muteCount > opts.maxMutes) return;

      const uniqueTones = toneSet.size;
      const minTonesNeeded = chordPcs.length <= 2 ? 2 : 3;
      if (uniqueTones < minTonesNeeded) return;
      if (!toneSet.has(rootPc)) return;

      const span = computeSpan(frets);
      if (span > opts.maxSpan) return;

      results.push({
        frets: [...frets],
        score: scoreVoicing({ frets, tuningPcs, rootPc })
      });
      return;
    }

    for (const fret of stringCandidates[stringIndex]) {
      const nextFrets = frets.concat(fret);
      const nextMuteCount = muteCount + (fret === -1 ? 1 : 0);
      if (nextMuteCount > opts.maxMutes) continue;

      let nextToneSet = toneSet;
      let nextMinFret = minFret;
      let nextMaxFret = maxFret;
      let nextSounding = soundingStrings;

      if (fret >= 0) {
        nextSounding += 1;
        const pc = fretToPc(tuningPcs[stringIndex], fret);
        if (!toneSet.has(pc)) {
          nextToneSet = new Set(toneSet);
          nextToneSet.add(pc);
        }

        if (fret > 0) {
          if (nextMinFret === null || fret < nextMinFret) nextMinFret = fret;
          if (nextMaxFret === null || fret > nextMaxFret) nextMaxFret = fret;

          const span = (nextMaxFret ?? 0) - (nextMinFret ?? 0);
          if (span > opts.maxSpan) continue;
        }
      }

      dfs(stringIndex + 1, nextFrets, nextToneSet, nextMinFret, nextMaxFret, nextSounding, nextMuteCount);
      if (results.length > opts.maxVoicings * 12) return;
    }
  }

  dfs(0, [], new Set(), null, null, 0, 0);

  results.sort((a, b) => a.score - b.score);
  const unique = [];
  const seen = new Set();
  for (const voicing of results) {
    const key = voicing.frets.join(',');
    if (seen.has(key)) continue;
    seen.add(key);
    unique.push({
      frets: voicing.frets,
      fingers: assignFingers(voicing.frets),
      score: voicing.score
    });
    if (unique.length >= opts.maxVoicings) break;
  }

  return unique;
}

function getDatasetVoicings({
  voicingData,
  tuningPcs,
  rootPc,
  typeId,
  options = {}
}) {
  const opts = {
    maxFret: 12,
    maxSpan: 4,
    minStrings: 4,
    maxVoicings: 10,
    ...options
  };
  const key = `${rootPc}|${typeId}`;
  const shapes = voicingData?.voicings?.[key] || [];
  if (!shapes.length) return [];

  const scored = [];
  for (const shape of shapes) {
    const frets = shape.frets;
    if (!Array.isArray(frets) || frets.length !== 6) continue;
    const fretted = frets.filter(f => f > 0);
    const maxFret = fretted.length ? Math.max(...fretted) : 0;
    if (maxFret > opts.maxFret) continue;
    const span = computeSpan(frets);
    if (span > opts.maxSpan) continue;
    if (countSoundingStrings(frets) < opts.minStrings) continue;
    if (!hasRoot(frets, tuningPcs, rootPc)) continue;

    scored.push({
      frets,
      fingers: Array.isArray(shape.fingers) && shape.fingers.length === 6
        ? shape.fingers
        : assignFingers(frets),
      barres: Array.isArray(shape.barres) ? shape.barres : [],
      score: scoreVoicing({ frets, tuningPcs, rootPc })
    });
  }

  scored.sort((a, b) => a.score - b.score);
  const unique = [];
  const seen = new Set();
  for (const voicing of scored) {
    const keySig = voicing.frets.join(',');
    if (seen.has(keySig)) continue;
    seen.add(keySig);
    unique.push(voicing);
    if (unique.length >= opts.maxVoicings) break;
  }

  return unique;
}

module.exports = {
  assignFingers,
  generateVoicings,
  getDatasetVoicings
};
