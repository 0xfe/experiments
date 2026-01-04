'use strict';

const fs = require('fs');
const path = require('path');
const { noteNameToPc } = require('../src/notes');

const ROOT = path.resolve(__dirname, '..');
const CHORDS_DB_JSON = path.join(ROOT, 'data', 'chords-db', 'package', 'lib', 'guitar.json');
const TEMPLATE = path.join(ROOT, 'src', 'chords.template.html');
const CSS_PATH = path.join(ROOT, 'src', 'app.css');
const ENTRY = path.join(ROOT, 'src', 'app.js');
const VOICING_DATA_PATH = path.join(ROOT, 'src', 'voicing-data.js');
const OUTPUT = path.join(ROOT, 'chords.html');

function normalizeRootName(name) {
  if (!name) return null;
  return name.replace('sharp', '#');
}

function buildVoicingData() {
  if (!fs.existsSync(CHORDS_DB_JSON)) {
    throw new Error(`Missing chords-db dataset: ${CHORDS_DB_JSON}\nSee README.md for download instructions.`);
  }

  const data = JSON.parse(fs.readFileSync(CHORDS_DB_JSON, 'utf8'));
  const voicings = {};
  const suffixes = Array.isArray(data.suffixes) ? data.suffixes : [];

  for (const [rootKey, chordEntries] of Object.entries(data.chords || {})) {
    const rootName = normalizeRootName(rootKey);
    const rootPc = noteNameToPc(rootName);
    if (rootPc === null || rootPc === undefined) continue;

    for (const chord of chordEntries) {
      const suffix = chord.suffix;
      const positions = chord.positions || [];

      for (const pos of positions) {
        const baseFret = pos.baseFret || 1;
        const frets = (pos.frets || []).map(fret => {
          if (fret <= 0) return fret;
          return fret + baseFret - 1;
        });
        if (frets.length !== 6) continue;

        const fingers = Array.isArray(pos.fingers) ? pos.fingers : [];
        const barresRaw = Array.isArray(pos.barres) ? pos.barres : (pos.barres ? [pos.barres] : []);
        const barres = barresRaw.map(fret => fret + baseFret - 1);

        const key = `${rootPc}|${suffix}`;
        if (!voicings[key]) voicings[key] = [];
        voicings[key].push({
          frets,
          fingers,
          barres,
          baseFret,
          capo: Boolean(pos.capo)
        });
      }
    }
  }

  const payload = `"use strict";\n\nmodule.exports = ${JSON.stringify({ suffixes, voicings })};\n`;
  fs.writeFileSync(VOICING_DATA_PATH, payload, 'utf8');
}

function bundle(entryPath) {
  const modules = new Map();

  function resolveModule(request, parent) {
    if (request.startsWith('.')) {
      const base = path.resolve(path.dirname(parent), request);
      if (fs.existsSync(base)) return base;
      if (fs.existsSync(`${base}.js`)) return `${base}.js`;
    }
    return null;
  }

  function addModule(filePath) {
    const absPath = path.resolve(filePath);
    if (modules.has(absPath)) return;
    let code = fs.readFileSync(absPath, 'utf8');

    const requires = [];
    code = code.replace(/require\(['"](.+?)['"]\)/g, (match, reqPath) => {
      const resolved = resolveModule(reqPath, absPath);
      if (resolved) {
        requires.push(resolved);
        return `__require__("${resolved}")`;
      }
      return match;
    });

    modules.set(absPath, code);
    requires.forEach(addModule);
  }

  addModule(entryPath);

  const moduleEntries = [];
  for (const [id, code] of modules.entries()) {
    moduleEntries.push(`"${id}": function(module, exports, __require__){\n${code}\n}`);
  }

  return `(() => {\n` +
    `  const __modules__ = {\n${moduleEntries.join(',\n')}\n  };\n` +
    `  const __cache__ = {};\n` +
    `  function __require__(id) {\n` +
    `    if (__cache__[id]) return __cache__[id].exports;\n` +
    `    const module = { exports: {} };\n` +
    `    __cache__[id] = module;\n` +
    `    __modules__[id](module, module.exports, __require__);\n` +
    `    return module.exports;\n` +
    `  }\n` +
    `  __require__("${path.resolve(entryPath)}");\n` +
    `})();\n`;
}

function buildHtml() {
  const template = fs.readFileSync(TEMPLATE, 'utf8');
  const css = fs.readFileSync(CSS_PATH, 'utf8');
  const js = bundle(ENTRY);

  const html = template
    .replace('/*__APP_CSS__*/', css)
    .replace('/*__APP_JS__*/', js);

  fs.writeFileSync(OUTPUT, html, 'utf8');
}

function run() {
  buildVoicingData();
  buildHtml();
  console.log(`Built ${OUTPUT}`);
}

run();
