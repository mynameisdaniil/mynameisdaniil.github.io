# Typeset Package — Design

Date: 2026-09-11
Status: Approved (pending written-spec review)
Branch: `v2.1`

## Goal

Extract the non-stylistic typography machinery built for this site into a
self-contained, reusable package (`typography/`, global name `typeset`) that can be
dropped into other pages and sites, and adopt it on this site so there is a single
source of truth.

## Non-Goals

- No fonts, font sizes, line heights, spacing, colors, columns, or layout.
- No npm publishing (repo-local package), no React/Vue wrappers.
- No patching of tex-linebreak options (advanced line breaking was dropped).
- No line-break-heavy features such as runt/last-line control.

## Package Layout

```
typography/
  README.md            # usage, font recommendations, measurement caveat
  src/
    index.js           # public API (typeset + low-level exports)
    pipeline.js        # options handling, font readiness, resize wiring, styles
    justify.js         # tex-linebreak orchestration + empty-font guard
    protrusion.js      # protrusion tables + apply/clear
    punctuation.js     # smart punctuation
    hyphenate.js       # bundled en-us/ru patterns + createHyphenator
  test/
    protrusion.test.js
    punctuation.test.js
    pipeline.test.js   # jsdom: options toggles, idempotence, destroy
  demo.html            # standalone demo (system fonts)
  dist/
    typeset.min.js     # IIFE, window.typeset, all engines inlined
    typeset.esm.js     # ESM build, zero imports
```

`dist/` is built by `npm run build:typeset` (esbuild) and committed, matching the
repo's committed-artifacts convention. Engines (tex-linebreak, Hypher, patterns) are
bundled into the dist files; consumers need nothing else.

## Public API

```js
const handle = typeset(root, {
  selector: "p",        // elements to justify (within root)
  lang: "en-us",        // default language; el.getAttribute("lang") wins
  hyphenate: true,      // pass hyphenator to Knuth-Plass
  justify: true,        // run tex-linebreak
  protrude: true,       // hanging punctuation pass
  punctuation: true,    // smart quotes/dashes/ellipses in text nodes
  onResize: true,       // debounced 150 ms re-run
  awaitFonts: true,     // wait for document.fonts.ready before first run
  injectStyles: true,   // inject the one functional CSS rule
  protrusion: { ... },  // optional per-character amount overrides
});

handle.typeset(el);     // typeset one subtree on demand
handle.refresh();       // clear + re-run for the whole root
handle.destroy();       // remove resize listener (styles stay)
```

Low-level exports (exact signatures):

- `justify(elements, hyphenate?) → void` — wrapper over tex-linebreak's
  `justifyContent`; `elements` is an element or array.
- `unjustify(elements) → void` — wrapper over `unjustifyContent`.
- `applyProtrusion(root) → void`, `clearProtrusion(root) → void`,
  `protrusionFor(char, side) → number`.
- `smartPunctuation(text) → string`, `applySmartPunctuation(root) → void`.
- `hyphenateFor(lang) → (word) => string[]` — bundled language convenience
  (`en-us`, `ru`, fallback `en-us`).
- `createHyphenator(patternsObject)` — re-export of the upstream helper.
- `patterns → { "en-us": Patterns, ru: Patterns }`.

The IIFE build exposes the same API under `window.typeset`.

## Behavior

- Pipeline per subtree: clear protrusion → `justifyContent` (if enabled) → apply
  protrusion; smart punctuation runs first over text nodes.
- `punctuation` applies to all text nodes in `root` (and in `handle.typeset(el)`
  subtrees), independent of `selector`; `selector` only chooses justification targets.
- `injectStyles` adds the rule only once (guarded by `style[data-typeset]`).
- Idempotent: re-runs always clear prior protrusion and rely on
  `unjustifyContent` inside `justifyContent`; safe for resize and manual refresh.
- Language: for each paragraph, the nearest ancestor `[lang]` wins
  (`el.closest("[lang]")`), then `options.lang`, then `"en-us"`. Paragraphs are
  grouped by resolved language and justified per group with the matching hyphenator;
  unknown languages fall back to `en-us`.
- Smart punctuation skips `code` and `pre`; attributes are never touched.
- Empty computed font guard: before justifying, warn if
  `getComputedStyle(el).font` is empty, because tex-linebreak's canvas measurement
  then silently falls back to 10px sans-serif. The README documents the rule:
  never set `font-kerning`/`font-variant-*` on measured text.
- Injected CSS (only if `injectStyles`):
  `.protrude-left, .protrude-right { display: inline; }` in a `<style data-typeset>`.
- `awaitFonts` defaults to true; if `document.fonts` is unavailable, proceed.
- `destroy()` disconnects the resize listener; DOM effects remain (call
  `unjustify`/`clearProtrusion` for that).

## Build

`package.json`:

```json
"build:typeset": "esbuild typography/src/index.js --bundle --minify --format=iife --global-name=typeset --outfile=typography/dist/typeset.min.js && esbuild typography/src/index.js --bundle --minify --format=esm --outfile=typography/dist/typeset.esm.js"
```

The package's `src` imports `tex-linebreak`, `hypher`, `hyphenation.en-us`, and
`hyphenation.ru`; esbuild inlines them. The existing
`scripts/typography-entry.js` and `assets/vendor/typography.min.js` are removed.

## Site Migration (Dogfood)

- `js/main.js` imports `typeset` from `../typography/dist/typeset.esm.js`.
- Delete `js/typography.js`, `js/protrusion.js`, `js/punctuation.js`,
  `assets/vendor/typography.min.js`, `scripts/typography-entry.js`.
- `index.html` drops the `typography.min.js` script tag.
- Remove `.protrude-left/right` from `assets/site.css` (package injects it).
- Move `tests/protrusion.test.js` and `tests/punctuation.test.js` to
  `typography/test/` and retarget imports; `node --test` discovers them recursively.
- `assets/tailwind.src.css` still scans `js/` and `index.html`; the package source is
  not Tailwind-scanned (it emits no utility classes).

## Verification

1. `npm test` — moved protrusion/punctuation tests plus a new jsdom pipeline test
   (toggles, idempotence, `destroy`).
2. `npm run build:typeset` produces both dist files; `node --check` the ESM build;
   grep that the IIFE exposes `typeset`.
3. Browser (controller): fonts load, paragraphs justified, protrusion spans present,
   no horizontal overflow at 1200/1600; resize re-runs; smart apostrophes rendered.
4. Demo page (`typography/demo.html`) loads the IIFE build standalone and typesets
   sample text with system fonts.

## Definition of Done

- `typography/` is self-contained: a consumer adds `typeset.min.js` (+ one call) and
  gets Knuth-Plass justification, hyphenation, hanging punctuation, and smart
  punctuation — no other files required.
- This site uses the same package build; duplicated modules are deleted.
- All tests pass; browser checks confirm no visual regression on the site.
