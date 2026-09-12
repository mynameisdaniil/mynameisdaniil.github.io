# AGENTS.md

Instructions for coding agents working in this repository.

## What this is

Static personal site (`mynameisdaniil.dev`, GitHub Pages, custom domain via
`CNAME`). No server, no framework, no build at deploy: generated files are
committed and served as-is. Typography: Knuth-Plass justification, hyphenation,
hanging punctuation via the local `typography/` package.

## Commands

```sh
npm install          # dev dependencies (Node 22)
npm run serve        # static server on http://localhost:8000
npm test             # unit tests via node --test (model, content, protrusion, punctuation, pipeline, hyphenation, service worker)
npm run check        # rebuild generated artifacts and fail on stale committed copies
npm run build        # fonts -> favicon -> typeset -> app bundle -> service worker
npm run build:fonts  # subset + woff2 the fonts (fetches the GOST TTF from GitHub)
npm run build:favicon
npm run build:typeset # the four typography/dist bundles (for external consumers)
npm run build:app    # assets/app.min.js (site modules + typeset + js-yaml + marked)
npm run build:sw     # stamps sw.js from scripts/sw.template.js
```

Always run `npm test` and `npm run check` before committing changes that touch
anything under `js/`, `typography/src/`, `assets/site.css`, `index.html`, or the
build scripts.

## Invariants and traps

- **Generated files are committed.** `assets/app.min.js`, `assets/fonts/*.woff2`,
  `assets/favicon-32.png`, `assets/apple-touch-icon.png`, `typography/dist/*`, and
  `sw.js` are build outputs. After editing their sources, run the matching build
  (or `npm run build`) and commit the result. CI (`npm run check`) fails otherwise.
- **Never edit `sw.js` directly.** It is stamped from `scripts/sw.template.js`
  with a cache version derived from a hash of the shell assets. Edit the template.
- **Never set `font-kerning` or `font-variant-*` on text that is measured** (the
  paper's paragraphs). Chrome then returns an empty computed `font` shorthand and
  tex-linebreak's canvas measurement silently falls back to 10px sans-serif,
  breaking line breaks. `typography/src/pipeline.js` warns when this happens.
- **Content is runtime data.** `content.yml` is fetched at runtime (network-first
  in the service worker); editing it needs no build.
- **The site bundles `typography/src/index-en.js`** (en-only, via `build:app`).
  `typography/dist/*` exists for external consumers of the package; keep it
  building even if the site does not load it.
- **CSS is hand-written** in `assets/site.css` (small reset + styles). There is no
  Tailwind and no utility framework.
- **Keep `CNAME`.** Deployment is branch-based: merge to `master` and push.

## Layout

- `index.html` — shell; metadata/OG tags, font preloads, `<script type="module" src="assets/app.min.js">`.
- `js/main.js` — boot: load content, render masthead/sections, start typesetting, register the SW.
- `js/content.js` — fetch + parse (`js-yaml`) + `renderMarkdown` (`marked`).
- `js/model.js` — build/validate the content model.
- `js/punctuation.js` — smart punctuation over text nodes (skips `code`/`pre`).
- `typography/` — the typeset package: `src/` (pipeline, justify, protrusion, punctuation, hyphenate), `test/`, `dist/`, its own README.
- `content.yml` — all site content.
- `assets/site.css` — styles (reset, masthead, columns, print, skip link).
- `sw.js` / `scripts/sw.template.js` — service worker and its template.
- `scripts/` — `build-fonts.mjs` (subset-font), `build-favicon.mjs` (jimp), `build-sw.mjs`.
- `tests/` — `node:test` suites; `docs/superpowers/` — specs and plans history.

## Testing notes

`jsdom` cannot measure text (no canvas), so the justification path is covered by
browser verification instead of unit tests. For visual/behavioral checks a
headless Chromium is available (`chromium --headless=new`): screenshots, computed
styles, and service-worker/offline checks via the DevTools protocol. When a
change affects rendering (spacing, fonts, columns), verify at 1200px and 1600px
and mention it in the commit; a human still does the final visual pass.

## Commit style

Imperative subject with a scope prefix: `feat:`, `fix:`, `perf:`, `build:`,
`ci:`, `docs:`, `style:`, `refactor:`, `chore:`. Keep generated-artifact rebuilds
in the same commit as the source change that caused them.
