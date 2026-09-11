# Typeset Package Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the site's typography machinery into a self-contained `typography/` package (global `typeset`, IIFE + ESM dist) and adopt it on this site.

**Architecture:** Package source lives in `typography/src/` (pipeline, justify, protrusion, punctuation, hyphenate); esbuild bundles tex-linebreak + Hypher + en-us/ru patterns into `typography/dist/typeset.{min.js,esm.js}`. The site imports the ESM build and the duplicated `js/` modules are deleted.

**Tech Stack:** ES modules, tex-linebreak, Hypher, hyphenation patterns, esbuild, `node:test` + jsdom.

**Spec:** `docs/superpowers/specs/2026-09-11-typeset-package-design.md`

## Global Constraints

- Package must be self-contained: no fonts, no external imports at runtime, engines inlined.
- Public API per spec: `typeset(root, options) → { typeset, refresh, destroy }` plus low-level exports (`justify`, `unjustify`, `applyProtrusion`, `clearProtrusion`, `protrusionFor`, `smartPunctuation`, `applySmartPunctuation`, `hyphenateFor`, `createHyphenator`, `patterns`).
- Language resolution per paragraph: `closest("[lang]")` → `options.lang` → `en-us`; group paragraphs by language.
- Idempotent re-runs; empty computed-font warning only when `document.fonts` exists.
- Built dist is committed; `node --test` discovers `typography/test/` recursively.
- Every task ends with a commit; no `node_modules` or `.superpowers` commits.

---

### Task 1: Package source, tests, README, demo

**Files:**
- Create: `typography/src/protrusion.js`, `typography/src/punctuation.js`, `typography/src/hyphenate.js`, `typography/src/justify.js`, `typography/src/pipeline.js`, `typography/src/index.js`
- Move: `tests/protrusion.test.js` → `typography/test/protrusion.test.js`; `tests/punctuation.test.js` → `typography/test/punctuation.test.js`
- Create: `typography/test/pipeline.test.js`, `typography/README.md`, `typography/demo.html`

**Interfaces:**
- Produces: the exports listed under Global Constraints; `typeset(root, options)` with the option set from the spec.

- [ ] **Step 1: Move the existing test files and retarget their imports**

```bash
mkdir -p typography/src typography/test
git mv tests/protrusion.test.js typography/test/protrusion.test.js
git mv tests/punctuation.test.js typography/test/punctuation.test.js
sed -i 's#"../js/protrusion.js"#"../src/protrusion.js"#' typography/test/protrusion.test.js
sed -i 's#"../js/punctuation.js"#"../src/punctuation.js"#' typography/test/punctuation.test.js
```

- [ ] **Step 2: Create `typography/src/protrusion.js`**

Copy the current `js/protrusion.js` verbatim:

```bash
cp js/protrusion.js typography/src/protrusion.js
```

- [ ] **Step 3: Create `typography/src/punctuation.js`**

```bash
cp js/punctuation.js typography/src/punctuation.js
```

- [ ] **Step 4: Create `typography/src/hyphenate.js`**

```js
import { createHyphenator } from "tex-linebreak";
import enUs from "hyphenation.en-us";
import ru from "hyphenation.ru";

export const patterns = { "en-us": enUs, ru };

export function hyphenateFor(lang) {
  return createHyphenator(patterns[lang] || patterns["en-us"]);
}

export { createHyphenator };
```

- [ ] **Step 5: Create `typography/src/justify.js`**

```js
import { justifyContent, unjustifyContent } from "tex-linebreak";

export function justify(elements, hyphenate) {
  justifyContent(elements, hyphenate);
}

export function unjustify(elements) {
  unjustifyContent(elements);
}
```

- [ ] **Step 6: Create `typography/src/pipeline.js`**

```js
import { justify } from "./justify.js";
import { hyphenateFor } from "./hyphenate.js";
import { applyProtrusion, clearProtrusion } from "./protrusion.js";
import { applySmartPunctuation } from "./punctuation.js";

const DEFAULTS = {
  selector: "p",
  lang: "en-us",
  hyphenate: true,
  justify: true,
  protrude: true,
  punctuation: true,
  onResize: true,
  awaitFonts: true,
  injectStyles: true,
  resizeDelay: 150,
};

function injectStylesOnce(doc) {
  if (doc.querySelector("style[data-typeset]")) return;
  const style = doc.createElement("style");
  style.setAttribute("data-typeset", "");
  style.textContent = ".protrude-left, .protrude-right { display: inline; }";
  doc.head.appendChild(style);
}

function resolveLang(el, fallback) {
  const tagged = el.closest ? el.closest("[lang]") : null;
  return (tagged && tagged.getAttribute("lang")) || fallback || "en-us";
}

export function typeset(root, options = {}) {
  const opts = { ...DEFAULTS, ...options };
  const doc = root.ownerDocument || document;

  if (opts.injectStyles) injectStylesOnce(doc);

  function run(el) {
    if (opts.punctuation) applySmartPunctuation(el);

    if (opts.justify) {
      const paragraphs = [...el.querySelectorAll(opts.selector)];
      if (paragraphs.length === 0) return;

      clearProtrusion(el);
      if (doc.fonts && !getComputedStyle(paragraphs[0]).font) {
        console.warn(
          "typeset: empty computed font shorthand; line measurement may be wrong (avoid font-kerning/font-variant-* on measured text)",
        );
      }

      const groups = new Map();
      for (const paragraph of paragraphs) {
        const lang = resolveLang(paragraph, opts.lang);
        if (!groups.has(lang)) groups.set(lang, []);
        groups.get(lang).push(paragraph);
      }

      try {
        for (const [lang, elements] of groups) {
          justify(elements, opts.hyphenate ? hyphenateFor(lang) : undefined);
        }
        if (opts.protrude) applyProtrusion(el);
      } catch (err) {
        console.error("typeset: justification failed", err);
      }
    } else if (opts.protrude) {
      clearProtrusion(el);
      applyProtrusion(el);
    }
  }

  const win = doc.defaultView || (typeof window !== "undefined" ? window : null);
  let resizeTimer = 0;

  function onResize() {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => run(root), opts.resizeDelay);
  }

  if (opts.onResize && win) win.addEventListener("resize", onResize);

  if (opts.awaitFonts && doc.fonts && doc.fonts.ready) {
    doc.fonts.ready.then(() => run(root)).catch(() => {});
  } else {
    run(root);
  }

  return {
    typeset: (el) => run(el || root),
    refresh: () => run(root),
    destroy() {
      if (win) win.removeEventListener("resize", onResize);
      clearTimeout(resizeTimer);
    },
  };
}
```

- [ ] **Step 7: Create `typography/src/index.js`**

```js
export { typeset } from "./pipeline.js";
export { justify, unjustify } from "./justify.js";
export { applyProtrusion, clearProtrusion, protrusionFor } from "./protrusion.js";
export { smartPunctuation, applySmartPunctuation } from "./punctuation.js";
export { createHyphenator, hyphenateFor, patterns } from "./hyphenate.js";
```

- [ ] **Step 8: Create `typography/test/pipeline.test.js`**

```js
import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { typeset } from "../src/index.js";

function setup() {
  const dom = new JSDOM(
    `<div id="root"><p>don't stop</p><p>"quoted" text...</p></div>`,
    { url: "http://localhost/" },
  );
  globalThis.document = dom.window.document;
  globalThis.window = dom.window;
  globalThis.NodeFilter = dom.window.NodeFilter;
  globalThis.getComputedStyle = dom.window.getComputedStyle.bind(dom.window);
  return dom;
}

test("typeset applies punctuation and protrusion when justification is off", () => {
  const dom = setup();
  const root = dom.window.document.getElementById("root");
  typeset(root, { justify: false, awaitFonts: false, onResize: false });
  assert.match(root.textContent, /don\u2019t/);
  assert.match(root.textContent, /\u201Cquoted\u201D/);
  assert.match(root.textContent, /\u2026/);
  assert.ok(root.querySelectorAll(".protrude-right").length > 0);
});

test("typeset is idempotent across refresh", () => {
  const dom = setup();
  const root = dom.window.document.getElementById("root");
  const handle = typeset(root, { justify: false, awaitFonts: false, onResize: false });
  const first = root.innerHTML;
  handle.refresh();
  assert.equal(root.innerHTML, first);
});

test("destroy removes the resize listener without throwing", () => {
  const dom = setup();
  const root = dom.window.document.getElementById("root");
  const handle = typeset(root, { justify: false, awaitFonts: false, onResize: true });
  handle.destroy();
  dom.window.dispatchEvent(new dom.window.Event("resize"));
  assert.ok(true);
});
```

- [ ] **Step 9: Create `typography/README.md`**

````markdown
# typeset

Self-contained browser typography for justified prose:

- Knuth-Plass line breaking (TeX algorithm) via `tex-linebreak`
- Hyphenation via Hypher, with `en-us` and `ru` patterns bundled
- Hanging punctuation (character protrusion) at line edges
- Smart punctuation (`'` → `’`, paired `"` → `“ ”`, `...` → `…`, `--` → `–`, `---` → `—`)

It does not ship fonts, sizes, spacing, or layout — bring your own styles.

## Quick start

```html
<link rel="stylesheet" href="your-styles.css" />
<script src="dist/typeset.min.js"></script>
<script>
  typeset.typeset(document.body);
</script>
```

Or as a module (bundlers or `<script type="module">`):

```js
import { typeset } from "./typography/dist/typeset.esm.js";

const handle = typeset(document.querySelector("main"));
```

The first run happens automatically after `document.fonts.ready`; resizing
re-runs after 150 ms.

## Options

| option | default | meaning |
|---|---|---|
| `selector` | `"p"` | elements to justify inside the root |
| `lang` | `"en-us"` | fallback language; nearest ancestor `[lang]` wins |
| `hyphenate` | `true` | pass a Hypher hyphenator to the line breaker |
| `justify` | `true` | run Knuth-Plass justification |
| `protrude` | `true` | hanging punctuation at line edges |
| `punctuation` | `true` | smart quotes/dashes/ellipses in text nodes |
| `onResize` | `true` | debounced re-run on window resize |
| `awaitFonts` | `true` | wait for `document.fonts.ready` |
| `injectStyles` | `true` | inject `.protrude-left/right { display: inline }` |
| `resizeDelay` | `150` | resize debounce in ms |

Returns `{ typeset(el), refresh(), destroy() }`.

## Low-level API

`justify(elements, hyphenate)`, `unjustify(elements)`, `applyProtrusion(root)`,
`clearProtrusion(root)`, `protrusionFor(char, side)`, `smartPunctuation(text)`,
`applySmartPunctuation(root)`, `hyphenateFor(lang)`, `createHyphenator(patterns)`,
`patterns`.

## Font requirements

Use a serif/sans face with real regular/italic/bold faces, loaded before the first
run (`awaitFonts` handles this). **Never set `font-kerning` or `font-variant-*` on
text that is measured** — Chrome then returns an empty computed `font` shorthand and
the canvas measurement silently falls back to 10px sans-serif, producing wrong line
breaks. The package warns when this happens.

## Rebuilding

```sh
npm run build:typeset
```

Bundled third-party licenses: see `../NOTICE`.
````

- [ ] **Step 10: Create `typography/demo.html`**

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>typeset demo</title>
    <style>
      body {
        margin: 0 auto;
        max-width: 900px;
        padding: 3rem 2rem;
        font-family: Georgia, "Times New Roman", serif;
        font-size: 18px;
        line-height: 1.6;
        color: #111;
        background: #fff;
      }
      #demo {
        columns: 2;
        column-gap: 2.5rem;
        column-rule: 1px solid #111;
      }
      h1 {
        font-family: "Courier New", monospace;
        font-size: 2rem;
      }
    </style>
  </head>
  <body>
    <h1>typeset</h1>
    <div id="demo">
      <p>
        Justified prose with hyphenation, hanging punctuation and smart quotes: "the
        quick brown fox" won't jump over the lazy dog...</p>
      <p>
        The TeX algorithm minimises the badness of the whole paragraph rather than
        filling lines greedily, which matters most in narrow columns like these. Try
        resizing the window -- the text re-breaks after a short delay.</p>
      <p>
        Add your own fonts and typographic scale; this package only handles line
        breaking, hyphenation, protrusion and punctuation.</p>
    </div>
    <script src="dist/typeset.min.js"></script>
    <script>
      typeset.typeset(document.getElementById("demo"));
    </script>
  </body>
</html>
```

- [ ] **Step 11: Run the suite**

```bash
npm test
```

Expected: all tests PASS, now including `typography/test/*` (protrusion, punctuation,
pipeline) and the existing model/content suites.

- [ ] **Step 12: Commit**

```bash
git add -A
git commit -m "feat: add self-contained typeset typography package"
```

---

### Task 2: Build the dist bundles

**Files:**
- Modify: `package.json` (add `build:typeset`)
- Create (generated): `typography/dist/typeset.min.js`, `typography/dist/typeset.esm.js`

**Interfaces:**
- Consumes: `typography/src/index.js` from Task 1.
- Produces: `window.typeset` (IIFE) and an importable ESM build with zero imports.

- [ ] **Step 1: Add the build script to `package.json`**

```json
"build:typeset": "esbuild typography/src/index.js --bundle --minify --format=iife --global-name=typeset --outfile=typography/dist/typeset.min.js && esbuild typography/src/index.js --bundle --minify --format=esm --outfile=typography/dist/typeset.esm.js"
```

- [ ] **Step 2: Build**

```bash
npm run build:typeset
ls -la typography/dist
```

Expected: both files exist; the IIFE is roughly 160–200 KB.

- [ ] **Step 3: Verify the artifacts**

```bash
node --check typography/dist/typeset.esm.js
node -e "
const fs = require('fs');
const esm = fs.readFileSync('typography/dist/typeset.esm.js', 'utf8');
const iife = fs.readFileSync('typography/dist/typeset.min.js', 'utf8');
if (!/export\s*\{/.test(esm)) throw new Error('ESM build has no exports');
if (!iife.includes('typeset')) throw new Error('IIFE build missing global');
for (const name of ['smartPunctuation', 'applyProtrusion', 'hyphenateFor', 'typeset']) {
  if (!esm.includes(name)) throw new Error('missing export ' + name);
}
console.log('dist looks good');
"
```

Expected: `dist looks good`; `node --check` prints nothing.

- [ ] **Step 4: Demo smoke check**

```bash
python3 -m http.server 8000 &
SERVER_PID=$!
sleep 1
curl -s -o /dev/null -w 'demo %{http_code}\n' http://localhost:8000/typography/demo.html
curl -s -o /dev/null -w 'dist %{http_code}\n' http://localhost:8000/typography/dist/typeset.min.js
kill "$SERVER_PID"
```

Expected: two `200` lines.

- [ ] **Step 5: Commit**

```bash
git add package.json typography/dist
git commit -m "build: add typeset dist bundles"
```

---

### Task 3: Migrate the site to the package

**Files:**
- Modify: `js/main.js`, `index.html`, `assets/site.css`, `package.json`, `NOTICE`
- Delete: `js/typography.js`, `js/protrusion.js`, `js/punctuation.js`, `scripts/typography-entry.js`, `assets/vendor/typography.min.js`

**Interfaces:**
- Consumes: `typeset` from `../typography/dist/typeset.esm.js`.
- Produces: site behavior unchanged (justification, protrusion, smart punctuation, resize re-run) via the package.

- [ ] **Step 1: Update `js/main.js`**

Replace the content module import lines:

```js
import { loadContent, renderMarkdown, ContentError } from "./content.js";
import { applySmartPunctuation } from "./punctuation.js";
import { createTypography } from "./typography.js";
```

with:

```js
import { loadContent, renderMarkdown, ContentError } from "./content.js";
import { typeset } from "../typography/dist/typeset.esm.js";
```

Remove the `applySmartPunctuation(body);` call inside `renderSection` (the package now
punctuates the whole root). Remove the `createTypography` setup block, the
`applyTypography` function, the `resizeTimer` resize listener, and the
`await document.fonts.ready; applyTypography();` lines. After
`paper.replaceChildren(...)`, call:

```js
typeset(paper);
```

The final `boot()` flow should be:

```js
async function boot() {
  try {
    model = await loadContent("content.yml");
    document.title = model.meta.title;
    titleEl.textContent = model.meta.title;
    subtitleEl.textContent = model.meta.subtitle;
    renderLinks(linksEl, model.meta.links);
    paper.replaceChildren(...model.sections.map(renderSection));
    typeset(paper);
  } catch (err) {
    console.error(err);
    showError(err instanceof ContentError ? err.details : [String(err)]);
  }
}

boot();
```

(`model` and `showError` stay as they are.)

- [ ] **Step 2: Remove the vendor script from `index.html`**

Delete the line:

```html
    <script src="assets/vendor/typography.min.js"></script>
```

- [ ] **Step 3: Remove the protrusion rule from `assets/site.css`**

Delete:

```css
.protrude-left,
.protrude-right {
  display: inline;
}
```

- [ ] **Step 4: Delete the superseded modules and bundles**

```bash
git rm -q js/typography.js js/protrusion.js js/punctuation.js scripts/typography-entry.js assets/vendor/typography.min.js
```

- [ ] **Step 5: Simplify `build:vendor` in `package.json`**

Change:

```json
"build:vendor": "bash scripts/fetch-vendor.sh && esbuild scripts/typography-entry.js --bundle --minify --format=iife --global-name=typography --outfile=assets/vendor/typography.min.js",
```

to:

```json
"build:vendor": "bash scripts/fetch-vendor.sh",
```

- [ ] **Step 6: Update `NOTICE`**

In the JavaScript section, replace the tex-linebreak and Hypher lines with:

```
- tex-linebreak — https://github.com/robertknight/tex-linebreak — MIT License
  (bundled inside typography/dist/typeset.min.js and typeset.esm.js)
- Hypher — https://github.com/bramstein/hypher — BSD-3-Clause License
  (bundled inside typography/dist/typeset.min.js and typeset.esm.js)
```

- [ ] **Step 7: Verify no stale references and run the suite**

```bash
grep -rn "typography-entry\|vendor/typography\|js/typography\|js/protrusion\|js/punctuation" js/ index.html package.json scripts/ assets/ typography/src typography/README.md 2>/dev/null | grep -v node_modules || echo "no stale references"
npm test
```

Expected: `no stale references`; all tests PASS.

- [ ] **Step 8: Headless smoke**

```bash
node --check js/main.js
python3 -m http.server 8000 &
SERVER_PID=$!
sleep 1
for f in index.html content.yml js/main.js typography/dist/typeset.esm.js assets/vendor/js-yaml.min.js assets/vendor/marked.min.js assets/site.css; do
  printf '%s ' "$f"; curl -s -o /dev/null -w '%{http_code}\n' "http://localhost:8000/$f"
done
kill "$SERVER_PID"
```

Expected: every line `200`; no `node --check` output.

- [ ] **Step 9: Commit**

```bash
git add -A
git commit -m "refactor: use the typeset package on the site"
```

---

### Task 4: Final verification

**Files:**
- Modify: `typography/README.md` only if verification reveals a documentation gap.

**Interfaces:**
- Consumes: everything above.
- Produces: the spec's Definition of Done, verified.

- [ ] **Step 1: Unit suite**

```bash
npm test
```

Expected: model, content, protrusion, punctuation, and pipeline tests all PASS.

- [ ] **Step 2: Site browser checks (controller/human)**

Serve with `npm run serve` and verify at 1200px and 1600px:

- Paragraphs justified with hyphenation; protrusion spans present at line edges.
- Smart apostrophes (`’`) rendered; no straight `'` in the paper.
- `document.fonts.status === "loaded"`; no 404s; no horizontal overflow.
- Resizing the window re-justifies after ~150 ms (lines stay inside columns).

- [ ] **Step 3: Demo browser check**

Open `typography/demo.html` (served): two columns, justified text with protrusion,
`window.typeset` defined.

- [ ] **Step 4: Self-containment check**

```bash
grep -o 'import[^;]*' typography/dist/typeset.esm.js | head
```

Expected: no external import statements (everything inlined).

---

## Verification (after all tasks)

- `npm test` passes (package + site suites).
- Dist bundles build reproducibly and are importable/loadable standalone.
- Site visually unchanged except no regressions; smart punctuation/protrusion/resize all
  still work through the package.
- `typography/` can be copied to another project with only `dist/typeset.min.js` plus
  the `typeset(root)` call.
