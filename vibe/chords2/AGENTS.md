# AGENTS.md

Guidelines for agents working in this repo.

## Project goals
- Keep `chords.html` as a single, no‑scroll, modern UI.
- Prefer dataset voicings (Chords DB) for standard tuning; use generator fallback for alternates.
- Keep voicing diagrams accurate (fingerings + barres).

## Build pipeline
- Source lives in `src/`
- Run `./scripts/build.sh` to regenerate `src/voicing-data.js` and bundle `chords.html`
- Do not hand‑edit `src/voicing-data.js`

## Tests
- `node tests/run-tests.js`

## Data
- Chords DB dataset is stored in `data/chords-db/`
- If you update or replace the dataset, ensure `scripts/build.js` still parses it correctly.

## Style
- Keep HTML/CSS/JS vanilla (no framework dependencies).
- Avoid unnecessary dependencies; prefer small, explicit modules.
- Preserve the compact, terminal‑tinged aesthetic.
