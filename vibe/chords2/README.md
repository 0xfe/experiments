# Chordonaut

Single‑page guitar chord generator with SVG diagrams. It uses a comprehensive voicing + fingering dataset (Chords DB) for standard tuning and falls back to a smart generator for alternate tunings.

## What’s inside

- `chords.html` — the bundled, single‑page app (open in any modern browser)
- `src/` — source files
  - `app.js` / `app.css` — UI + logic
  - `chords.template.html` — HTML template used by the bundler
  - `notes.js`, `chords.js`, `tuning.js`, `voicings.js`, `diagram.js` — core modules
  - `voicing-data.js` — generated dataset payload (do not edit by hand)
- `data/chords-db/` — Chords DB dataset (source of voicings + fingerings; not checked in)
- `scripts/build.js` / `scripts/build.sh` — bundler and dataset generator
- `tests/` — Node tests for chord parsing + voicing logic

## Run

Just open `chords.html` in your browser.

## Build (generate `chords.html`)

```bash
./scripts/build.sh
```

This will:
1) regenerate `src/voicing-data.js` from `data/chords-db/package/lib/guitar.json`
2) bundle the HTML/CSS/JS into `chords.html`

## Download voicing data (Chords DB)

The dataset is not checked in. Download it once before building:

```bash
mkdir -p data
curl -L -o data/chords-db.tgz https://registry.npmjs.org/@tombatossals/chords-db/-/chords-db-0.5.1.tgz
mkdir -p data/chords-db
tar -xzf data/chords-db.tgz -C data/chords-db
```

## Modify

Common edits:
- UI/behavior: `src/app.js`
- Styles: `src/app.css`
- Layout: `src/chords.template.html`
- Chord logic: `src/chords.js`
- Voicing filtering/fallback: `src/voicings.js`

After edits, rebuild with:

```bash
./scripts/build.sh
```

## Tests

```bash
node tests/run-tests.js
```

## Notes on voicing data

- Standard tuning uses Chords DB voicings + fingerings (downloaded locally into `data/chords-db/`).
- Alternate tunings use a generator fallback.
- `src/voicing-data.js` is auto‑generated; edit the dataset or generator instead.
