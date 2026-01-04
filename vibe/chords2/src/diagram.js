'use strict';

const { transposePc } = require('./notes');

function renderChordSVG({ frets, fingers, tuningPcs, rootPc, barres = [], width = 360, height = 360 }) {
  const stringCount = 6;
  const fretCount = 5;
  const padding = { left: 36, right: 18, top: 36, bottom: 28 };
  const gridWidth = width - padding.left - padding.right;
  const gridHeight = height - padding.top - padding.bottom;
  const stringGap = gridWidth / (stringCount - 1);
  const fretGap = gridHeight / fretCount;

  const fretted = frets.filter(f => f > 0);
  const minFretted = fretted.length ? Math.min(...fretted) : 1;
  const maxFretted = fretted.length ? Math.max(...fretted) : 1;
  const startFret = maxFretted <= 4 ? 1 : minFretted;

  const lines = [];

  for (let i = 0; i < stringCount; i += 1) {
    const x = padding.left + i * stringGap;
    lines.push(`<line class="string" x1="${x}" y1="${padding.top}" x2="${x}" y2="${padding.top + gridHeight}" />`);
  }

  for (let i = 0; i <= fretCount; i += 1) {
    const y = padding.top + i * fretGap;
    const cls = (i === 0 && startFret === 1) ? 'nut' : 'fret';
    lines.push(`<line class="${cls}" x1="${padding.left}" y1="${y}" x2="${padding.left + gridWidth}" y2="${y}" />`);
  }

  const markers = [];
  const dots = [];
  const barLines = [];
  const fretGroups = new Map();

  frets.forEach((fret, stringIndex) => {
    const x = padding.left + stringIndex * stringGap;
    if (fret === -1) {
      markers.push(`<text class="mute" x="${x}" y="${padding.top - 10}">×</text>`);
      return;
    }
    if (fret === 0) {
      markers.push(`<circle class="open" cx="${x}" cy="${padding.top - 12}" r="6" />`);
      return;
    }
    const fretIndex = fret - startFret;
    if (fretIndex < 0 || fretIndex >= fretCount) return;
    const y = padding.top + (fretIndex + 0.5) * fretGap;
    const pc = transposePc(tuningPcs[stringIndex], fret);
    const isRoot = pc === rootPc;
    const finger = fingers[stringIndex] || 0;
    dots.push(`<circle class="dot ${isRoot ? 'root' : ''}" cx="${x}" cy="${y}" r="12" />`);
    if (finger > 0) {
      dots.push(`<text class="finger" x="${x}" y="${y + 4}">${finger}</text>`);
    }

    if (!fretGroups.has(fret)) fretGroups.set(fret, []);
    fretGroups.get(fret).push({ stringIndex, finger });
  });

  const barreFrets = Array.isArray(barres) && barres.length ? barres : [];

  if (barreFrets.length) {
    for (const barreFret of barreFrets) {
      const entries = (fretGroups.get(barreFret) || []).map(e => e.stringIndex);
      if (entries.length < 2) continue;
      const x1 = padding.left + Math.min(...entries) * stringGap;
      const x2 = padding.left + Math.max(...entries) * stringGap;
      const fretIndex = barreFret - startFret;
      const y = padding.top + (fretIndex + 0.5) * fretGap;
      barLines.push(`<line class="barre" x1="${x1}" y1="${y}" x2="${x2}" y2="${y}" />`);
    }
  } else {
    for (const [fret, entries] of fretGroups.entries()) {
      if (entries.length < 2) continue;
      const fingerNums = entries.map(e => e.finger).filter(n => n > 0);
      const allSame = fingerNums.length && fingerNums.every(n => n === fingerNums[0]);
      if (!allSame) continue;
      const strings = entries.map(e => e.stringIndex);
      const x1 = padding.left + Math.min(...strings) * stringGap;
      const x2 = padding.left + Math.max(...strings) * stringGap;
      const fretIndex = fret - startFret;
      const y = padding.top + (fretIndex + 0.5) * fretGap;
      barLines.push(`<line class="barre" x1="${x1}" y1="${y}" x2="${x2}" y2="${y}" />`);
    }
  }

  const fretLabel = startFret > 1
    ? `<text class="start-fret" x="${padding.left - 22}" y="${padding.top + 14}">${startFret}fr</text>`
    : '';

  return `
<svg viewBox="0 0 ${width} ${height}" role="img" aria-label="Chord diagram">
  <g class="grid">
    ${lines.join('\n    ')}
  </g>
  ${fretLabel}
  <g class="markers">
    ${markers.join('\n    ')}
  </g>
  <g class="barres">
    ${barLines.join('\n    ')}
  </g>
  <g class="dots">
    ${dots.join('\n    ')}
  </g>
</svg>
  `.trim();
}

module.exports = {
  renderChordSVG
};
