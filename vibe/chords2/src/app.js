'use strict';

const { ROOT_CHOICES, noteNameToPc, pcToNoteName } = require('./notes');
const { CHORD_TYPES, CHORD_TYPE_MAP, chordPcs } = require('./chords');
const { TUNINGS, tuningToPcs } = require('./tuning');
const { generateVoicings, getDatasetVoicings } = require('./voicings');
const { renderChordSVG } = require('./diagram');
const VOICING_DATA = require('./voicing-data');

const QUICK_TYPES = ['major', 'minor', '7', 'maj7', 'm7', 'm7b5', '9', '13', 'sus4'];
const STANDARD_TUNING_ID = 'standard';

const state = {
  rootPc: noteNameToPc('D'),
  chordTypeId: 'maj7',
  tuningId: STANDARD_TUNING_ID,
  preferFlats: false,
  voicingIndex: 0,
  voicings: [],
  voicingSource: 'Dataset'
};

const elements = {
  rootGrid: document.getElementById('root-grid'),
  typeSelect: document.getElementById('type-select'),
  typeSearch: document.getElementById('type-search'),
  quickRow: document.getElementById('quick-row'),
  tuningSelect: document.getElementById('tuning-select'),
  formula: document.getElementById('formula'),
  notes: document.getElementById('notes'),
  tuning: document.getElementById('tuning'),
  voicingCount: document.getElementById('voicing-count'),
  voicingSource: document.getElementById('voicing-source'),
  chordName: document.getElementById('chord-name'),
  diagram: document.getElementById('diagram'),
  voicingIndex: document.getElementById('voicing-index'),
  prevVoicing: document.getElementById('prev-voicing'),
  nextVoicing: document.getElementById('next-voicing'),
  shuffle: document.getElementById('shuffle-voicing'),
  flatToggle: document.getElementById('flat-toggle')
};

function displayRootName() {
  return pcToNoteName(state.rootPc, state.preferFlats);
}

function getChordType(id) {
  return CHORD_TYPE_MAP.get(id) || CHORD_TYPES[0];
}

function buildRootButtons() {
  elements.rootGrid.innerHTML = '';
  ROOT_CHOICES.forEach(choice => {
    const button = document.createElement('button');
    button.className = 'root-btn';
    button.textContent = choice.label;
    button.dataset.pc = String(noteNameToPc(choice.name));
    button.addEventListener('click', () => {
      state.rootPc = Number(button.dataset.pc);
      state.voicingIndex = 0;
      updateAll();
    });
    elements.rootGrid.appendChild(button);
  });
}

function buildChordOptions(filterText = '') {
  const term = filterText.trim().toLowerCase();
  const groups = new Map();
  CHORD_TYPES.forEach(type => {
    const shortDisplay = type.short || (type.id === 'major' ? 'maj' : type.id);
    const hay = `${type.label} ${shortDisplay} ${type.id}`.toLowerCase();
    if (term && !hay.includes(term)) return;
    if (!groups.has(type.category)) groups.set(type.category, []);
    groups.get(type.category).push(type);
  });

  elements.typeSelect.innerHTML = '';
  let firstId = null;
  for (const [category, types] of groups.entries()) {
    const optgroup = document.createElement('optgroup');
    optgroup.label = category;
    types.forEach(type => {
      if (!firstId) firstId = type.id;
      const option = document.createElement('option');
      const shortDisplay = type.short || (type.id === 'major' ? 'maj' : type.id);
      option.value = type.id;
      option.textContent = `${type.label} (${shortDisplay})`;
      optgroup.appendChild(option);
    });
    elements.typeSelect.appendChild(optgroup);
  }

  if (!CHORD_TYPE_MAP.has(state.chordTypeId) || !elements.typeSelect.querySelector(`option[value="${state.chordTypeId}"]`)) {
    state.chordTypeId = firstId || state.chordTypeId;
  }
  elements.typeSelect.value = state.chordTypeId;
}

function buildQuickRow() {
  elements.quickRow.innerHTML = '';
  QUICK_TYPES.forEach(id => {
    const type = CHORD_TYPE_MAP.get(id);
    if (!type) return;
    const chip = document.createElement('button');
    chip.className = 'chip';
    chip.textContent = type.short || (type.id === 'major' ? 'maj' : type.id);
    chip.dataset.id = id;
    chip.addEventListener('click', () => {
      state.chordTypeId = id;
      state.voicingIndex = 0;
      updateAll();
    });
    elements.quickRow.appendChild(chip);
  });
}

function buildTuningOptions() {
  TUNINGS.forEach(tuning => {
    const option = document.createElement('option');
    option.value = tuning.id;
    option.textContent = tuning.label;
    elements.tuningSelect.appendChild(option);
  });
  elements.tuningSelect.value = state.tuningId;
}

function updateActiveButtons() {
  document.querySelectorAll('.root-btn').forEach(btn => {
    btn.classList.toggle('active', Number(btn.dataset.pc) === state.rootPc);
  });
  document.querySelectorAll('.chip').forEach(chip => {
    chip.classList.toggle('active', chip.dataset.id === state.chordTypeId);
  });
  elements.flatToggle.classList.toggle('active', state.preferFlats);
}

function updateAll() {
  const chordType = getChordType(state.chordTypeId);
  const tuning = TUNINGS.find(t => t.id === state.tuningId);
  const tuningPcs = tuningToPcs(tuning);
  const pcs = chordPcs(state.rootPc, chordType);

  const noteNames = [...new Set(pcs.map(pc => pcToNoteName(pc, state.preferFlats)))];
  const displayName = `${displayRootName()}${chordType.short}`;

  let voicings = [];
  let source = 'Generated';

  if (state.tuningId === STANDARD_TUNING_ID) {
    voicings = getDatasetVoicings({
      voicingData: VOICING_DATA,
      tuningPcs,
      rootPc: state.rootPc,
      typeId: chordType.id,
      options: { maxFret: 12, maxSpan: 4, maxVoicings: 10 }
    });
    if (voicings.length) {
      source = 'Chords DB';
    }
  }

  if (!voicings.length) {
    voicings = generateVoicings({
      tuningPcs,
      chordPcs: pcs,
      rootPc: state.rootPc,
      options: { maxFret: 12, maxSpan: 4, maxVoicings: 8 }
    });
  }

  state.voicings = voicings;
  state.voicingSource = source;

  if (state.voicingIndex >= state.voicings.length) {
    state.voicingIndex = 0;
  }

  elements.chordName.textContent = displayName;
  elements.formula.textContent = chordType.formula || '—';
  elements.notes.textContent = noteNames.join(' · ');
  elements.tuning.textContent = tuning.notes.join(' ');
  elements.voicingCount.textContent = `${state.voicings.length} shapes`;
  elements.voicingSource.textContent = state.voicingSource;
  elements.typeSelect.value = state.chordTypeId;

  updateActiveButtons();

  if (state.voicings.length === 0) {
    elements.diagram.innerHTML = '<div style="color: var(--muted); font-family: Space Mono, monospace;">No compact voicings found. Try a different tuning or chord.</div>';
    elements.voicingIndex.textContent = '0 / 0';
    return;
  }

  const voicing = state.voicings[state.voicingIndex];
  elements.diagram.innerHTML = renderChordSVG({
    frets: voicing.frets,
    fingers: voicing.fingers,
    barres: voicing.barres || [],
    tuningPcs,
    rootPc: state.rootPc
  });
  elements.voicingIndex.textContent = `${state.voicingIndex + 1} / ${state.voicings.length}`;
}

function registerEvents() {
  elements.typeSelect.addEventListener('change', event => {
    state.chordTypeId = event.target.value;
    state.voicingIndex = 0;
    updateAll();
  });

  elements.tuningSelect.addEventListener('change', event => {
    state.tuningId = event.target.value;
    state.voicingIndex = 0;
    updateAll();
  });

  elements.typeSearch.addEventListener('input', event => {
    buildChordOptions(event.target.value);
  });

  elements.prevVoicing.addEventListener('click', () => {
    if (!state.voicings.length) return;
    state.voicingIndex = (state.voicingIndex - 1 + state.voicings.length) % state.voicings.length;
    updateAll();
  });

  elements.nextVoicing.addEventListener('click', () => {
    if (!state.voicings.length) return;
    state.voicingIndex = (state.voicingIndex + 1) % state.voicings.length;
    updateAll();
  });

  elements.shuffle.addEventListener('click', () => {
    if (!state.voicings.length) return;
    state.voicingIndex = Math.floor(Math.random() * state.voicings.length);
    updateAll();
  });

  elements.flatToggle.addEventListener('click', () => {
    state.preferFlats = !state.preferFlats;
    updateAll();
  });
}

buildRootButtons();
buildChordOptions();
buildQuickRow();
buildTuningOptions();
registerEvents();
updateAll();
