# v2 Force-Directed Card Graph — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebuild `mynameisdaniil.dev` as a static, no-deploy-build single-page site where content cards live on a live d3 force-directed graph; one card is focused at a time and the rest recede into a blurred background.

**Architecture:** Vanilla ES modules loaded directly by the browser. A vendored, dev-time esbuild IIFE (`typography.min.js`) exposes `tex-linebreak` + Hypher as `window.typography`. Pure logic lives in small modules (`model`, `router`, `theme`, `protrusion`) with `node:test` unit tests; DOM/graph modules are verified in a browser via a checklist.

**Tech Stack:** JavaScript ES modules, d3 v7 (force), js-yaml, marked, tex-linebreak, Hypher, Tailwind CSS v4 (CLI, output committed), self-hosted GOST Type A + Latin Modern Roman, `node:test` + jsdom for unit tests.

**Spec:** `docs/superpowers/specs/2026-09-10-v2-graph-cards-design.md`

## Global Constraints

- Deploy target is GitHub Pages serving the repo root of `master`; work happens on branch `v2`. **No build or CI runs at deploy time** — every asset is committed.
- **No runtime CDN or external requests.** All JS, fonts, and patterns are self-hosted under `assets/`.
- Old site is scrapped: everything tracked on `master` except `CNAME` is deleted on `v2`.
- JavaScript is native ES modules (`<script type="module">`); `package.json` has `"type": "module"`.
- Palette is grayscale only (no accent color). Exact tokens are in Task 1.
- Titles: `"GOST Type A"` (Metrolog `v0.7.6`, Apache-2.0). Body: `"Latin Modern Roman"` (OFL).
- Content markdown is trusted (author-owned); no sanitizer.
- Runtime globals provided by vendor scripts: `d3`, `jsyaml`, `marked`, `typography`.
- Every task ends with a commit. Do not skip hooks or amend commits.
- Do not commit `node_modules/`.

---

### Task 1: Reset and walking skeleton

**Files:**
- Delete (tracked): `Makefile`, `contents.yaml`, `elm.json`, `index.html`, `src/`, `slides/`, `icons/`, `images/`, `habr/`, `files/`, `ef_hack_xchnge/`
- Delete (untracked): `elm-stuff/`
- Create: `.gitignore`, `package.json`, `index.html`, `assets/tailwind.src.css`, `assets/fonts.css`, `assets/site.css`, `js/main.js`

**Interfaces:**
- Consumes: nothing.
- Produces: `package.json` scripts `test`, `build:css`, `build:vendor`, `serve`; DOM ids `#stage`, `#edges`, `#cards`, `#live`, `#theme-toggle`; CSS custom property tokens; `assets/site.css` classes `.card`, `.card--focused`, `.card--background`, `.card-title`, `.card-body`, `.card-links`, `.edge`, `.edge--active`, `.theme-toggle`, `.sr-only`, `.card--error`, `.protrude-left`, `.protrude-right`.

- [ ] **Step 1: Remove the old site**

```bash
git rm -r -q Makefile contents.yaml elm.json index.html src slides icons images habr files ef_hack_xchnge
rm -rf elm-stuff
git status --short | head
```

Expected: a long list of `D` (deleted) entries; `CNAME` untouched.

- [ ] **Step 2: Write `.gitignore`**

```
node_modules/
.DS_Store
._*
tags
*.tgz
```

- [ ] **Step 3: Write `package.json`**

```json
{
  "name": "mynameisdaniil-site",
  "private": true,
  "type": "module",
  "scripts": {
    "test": "node --test tests/",
    "build:css": "tailwindcss -i assets/tailwind.src.css -o assets/tailwind.css --minify",
    "build:vendor": "bash scripts/fetch-vendor.sh && esbuild scripts/typography-entry.js --bundle --minify --format=iife --global-name=typography --outfile=assets/vendor/typography.min.js",
    "serve": "python3 -m http.server 8000"
  },
  "devDependencies": {
    "@tailwindcss/cli": "^4.3.3",
    "@typopro/web-latin-modern": "3.7.5",
    "d3": "^7.9.0",
    "esbuild": "^0.28.2",
    "hyphenation.en-us": "^0.2.1",
    "hyphenation.ru": "^0.2.1",
    "hypher": "^0.2.5",
    "js-yaml": "^4.3.2",
    "jsdom": "^30.0.1",
    "marked": "^18.0.12",
    "tailwindcss": "^4.3.3",
    "tex-linebreak": "^0.9.0"
  }
}
```

- [ ] **Step 4: Install dev dependencies**

```bash
npm install --no-audit --no-fund
```

Expected: `node_modules/` created, no vulnerabilities message required.

- [ ] **Step 5: Write `assets/fonts.css`**

```css
@font-face {
  font-family: "GOST Type A";
  src: url("fonts/gost-type-a-regular.woff") format("woff");
  font-weight: 400;
  font-style: normal;
  font-display: block;
}

@font-face {
  font-family: "Latin Modern Roman";
  src: url("fonts/latin-modern-roman-regular.woff") format("woff");
  font-weight: 400;
  font-style: normal;
  font-display: block;
}

@font-face {
  font-family: "Latin Modern Roman";
  src: url("fonts/latin-modern-roman-italic.woff") format("woff");
  font-weight: 400;
  font-style: italic;
  font-display: block;
}

@font-face {
  font-family: "Latin Modern Roman";
  src: url("fonts/latin-modern-roman-bold.woff") format("woff");
  font-weight: 700;
  font-style: normal;
  font-display: block;
}

@font-face {
  font-family: "Latin Modern Roman";
  src: url("fonts/latin-modern-roman-bolditalic.woff") format("woff");
  font-weight: 700;
  font-style: italic;
  font-display: block;
}

@font-face {
  font-family: "Latin Modern Mono";
  src: url("fonts/latin-modern-mono-regular.woff") format("woff");
  font-weight: 400;
  font-style: normal;
  font-display: block;
}

@font-face {
  font-family: "Latin Modern Mono";
  src: url("fonts/latin-modern-mono-italic.woff") format("woff");
  font-weight: 400;
  font-style: italic;
  font-display: block;
}
```

- [ ] **Step 6: Write `assets/tailwind.src.css`**

```css
@import "tailwindcss";
@source "../index.html";
@source "../js";
@custom-variant dark (&:where(.dark, .dark *));
```

- [ ] **Step 7: Write `assets/site.css`**

```css
:root {
  --bg-center: #f7f5f2;
  --bg-edge: #e3e6eb;
  --card-bg: rgb(90 98 110 / 0.07);
  --card-bg-focus: rgb(90 98 110 / 0.1);
  --card-border: #1d1c1b;
  --text: #3b3835;
  --title: #22252b;
  --edge-line: rgb(138 146 160 / 0.25);
  --edge-line-active: rgb(138 146 160 / 0.55);
  --shadow: 0 18px 50px rgb(40 44 52 / 0.12);
  color-scheme: light;
}

.dark {
  --bg-center: #1c1d20;
  --bg-edge: #101216;
  --card-bg: rgb(200 206 216 / 0.05);
  --card-bg-focus: rgb(200 206 216 / 0.08);
  --card-border: rgb(217 213 206 / 0.55);
  --text: #b8b4ad;
  --title: #e6e8ec;
  --edge-line: rgb(154 149 141 / 0.2);
  --edge-line-active: rgb(154 149 141 / 0.5);
  --shadow: 0 18px 50px rgb(0 0 0 / 0.4);
  color-scheme: dark;
}

html,
body {
  height: 100%;
}

body {
  margin: 0;
  overflow: hidden;
  background: radial-gradient(circle at 50% 45%, var(--bg-center), var(--bg-edge));
  background-attachment: fixed;
  color: var(--text);
  font-family: "Latin Modern Roman", "Computer Modern", serif;
  font-size: 18px;
  line-height: 1.55;
}

h1,
h2,
h3 {
  color: var(--title);
  font-family: "GOST Type A", "Latin Modern Roman", serif;
  font-weight: 400;
  letter-spacing: 0.04em;
}

#stage {
  position: fixed;
  inset: 0;
  visibility: hidden;
}

#edges {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  overflow: visible;
}

#cards {
  position: absolute;
  inset: 0;
}

.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  margin: -1px;
  padding: 0;
  overflow: hidden;
  clip: rect(0 0 0 0);
  white-space: nowrap;
  border: 0;
}

@media (prefers-reduced-motion: reduce) {
  .card,
  .edge {
    transition: none;
  }
}
```

- [ ] **Step 8: Write `index.html`**

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>Daniil Sobol</title>
    <script>
      (() => {
        try {
          const stored = localStorage.getItem("theme");
          const dark = stored
            ? stored === "dark"
            : matchMedia("(prefers-color-scheme: dark)").matches;
          document.documentElement.classList.toggle("dark", dark);
          document.documentElement.style.colorScheme = dark ? "dark" : "light";
        } catch (err) {}
      })();
    </script>
    <link rel="stylesheet" href="assets/fonts.css" />
    <link rel="stylesheet" href="assets/tailwind.css" />
    <link rel="stylesheet" href="assets/site.css" />
  </head>
  <body>
    <button
      id="theme-toggle"
      class="theme-toggle"
      type="button"
      aria-label="Switch theme"
      aria-pressed="false"
    >
      <svg class="theme-toggle__sun" viewBox="0 0 24 24" aria-hidden="true">
        <circle cx="12" cy="12" r="4.5" />
        <path
          d="M12 1.5v3M12 19.5v3M1.5 12h3M19.5 12h3M4.6 4.6l2.1 2.1M17.3 17.3l2.1 2.1M19.4 4.6l-2.1 2.1M6.7 17.3l-2.1 2.1"
          stroke="currentColor"
          stroke-width="1.5"
          fill="none"
          stroke-linecap="round"
        />
      </svg>
      <svg class="theme-toggle__moon" viewBox="0 0 24 24" aria-hidden="true">
        <path d="M20 14.5A8.5 8.5 0 0 1 9.5 4a8.5 8.5 0 1 0 10.5 10.5Z" />
      </svg>
    </button>
    <main id="stage" aria-busy="true">
      <svg id="edges" aria-hidden="true"></svg>
      <div id="cards"></div>
    </main>
    <div id="live" class="sr-only" aria-live="polite"></div>
    <!-- Vendor scripts are added in Task 2; the module script is added in Task 10. -->
  </body>
</html>
```

- [ ] **Step 9: Write temporary `js/main.js`**

```js
document.documentElement.dataset.site = "v2";
```

- [ ] **Step 10: Build Tailwind CSS and verify the skeleton serves**

```bash
npm run build:css
python3 -m http.server 8000 &
SERVER_PID=$!
sleep 1
for f in index.html assets/tailwind.css assets/site.css assets/fonts.css; do
  printf '%s ' "$f"; curl -s -o /dev/null -w '%{http_code}\n' "http://localhost:8000/$f"
done
kill "$SERVER_PID"
```

Expected: four `200` lines, and `assets/tailwind.css` exists and is non-empty.

- [ ] **Step 11: Browser check**

Run `npm run serve`, open `http://localhost:8000`. Expected: radial warm-to-cool gray gradient fills the viewport; theme-toggle button is visible top-right; no visible content (stage hidden); console has no errors.

- [ ] **Step 12: Commit**

```bash
git add -A
git commit -m "chore: reset site to v2 skeleton with tokens, fonts, and tailwind"
```

---

### Task 2: Vendor JS, fonts, and the typography bundle

**Files:**
- Create: `scripts/fetch-vendor.sh`, `scripts/typography-entry.js`, `NOTICE`
- Create (generated): `assets/vendor/d3.v7.min.js`, `assets/vendor/js-yaml.min.js`, `assets/vendor/marked.min.js`, `assets/vendor/typography.min.js`, `assets/fonts/*.woff`
- Modify: `index.html` (add vendor `<script>` tags before `</body>`)

**Interfaces:**
- Consumes: `package.json` devDependencies from Task 1.
- Produces: browser globals `d3`, `jsyaml`, `marked`, `typography`; `window.typography.hyphenateFor(lang) → (word) => string[]`, `window.typography.justifyContent`, `window.typography.unjustifyContent`.

- [ ] **Step 1: Write `scripts/fetch-vendor.sh`**

```bash
#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

mkdir -p assets/vendor assets/fonts

cp node_modules/d3/dist/d3.min.js assets/vendor/d3.v7.min.js
cp node_modules/js-yaml/dist/js-yaml.min.js assets/vendor/js-yaml.min.js
cp node_modules/marked/lib/marked.umd.js assets/vendor/marked.min.js

LM=node_modules/@typopro/web-latin-modern
cp "$LM/TypoPRO-LatinModernRoman-Regular.woff" assets/fonts/latin-modern-roman-regular.woff
cp "$LM/TypoPRO-LatinModernRoman-Italic.woff" assets/fonts/latin-modern-roman-italic.woff
cp "$LM/TypoPRO-LatinModernRoman-Bold.woff" assets/fonts/latin-modern-roman-bold.woff
cp "$LM/TypoPRO-LatinModernRoman-BoldItalic.woff" assets/fonts/latin-modern-roman-bolditalic.woff
cp "$LM/TypoPRO-LatinModernMono-Regular.woff" assets/fonts/latin-modern-mono-regular.woff
cp "$LM/TypoPRO-LatinModernMono-Italic.woff" assets/fonts/latin-modern-mono-italic.woff

if [ ! -f assets/fonts/gost-type-a-regular.woff ]; then
  curl -sSL -o /tmp/gost-fonts.zip \
    https://github.com/Metrolog/Font.GOST2.304-81/releases/download/v0.7.6/fonts.zip
  unzip -j -o /tmp/gost-fonts.zip 'woff/GOST2.304-81TypeA-Regular.woff' -d assets/fonts/
  mv assets/fonts/GOST2.304-81TypeA-Regular.woff assets/fonts/gost-type-a-regular.woff
fi

echo "vendor assets written"
```

- [ ] **Step 2: Write `scripts/typography-entry.js`**

```js
import {
  createHyphenator,
  justifyContent,
  unjustifyContent,
} from "tex-linebreak";
import enUs from "hyphenation.en-us";
import ru from "hyphenation.ru";

const patterns = { "en-us": enUs, ru };

export function hyphenateFor(lang) {
  return createHyphenator(patterns[lang] || patterns["en-us"]);
}

export { patterns, justifyContent, unjustifyContent };
```

- [ ] **Step 3: Run the vendor build**

```bash
chmod +x scripts/fetch-vendor.sh
npm run build:vendor
ls -la assets/vendor assets/fonts
```

Expected: four vendor JS files and seven `.woff` files; `typography.min.js` ~150KB.

- [ ] **Step 4: Add vendor script tags to `index.html`**

Replace the comment line:

```html
    <!-- Vendor scripts are added in Task 2; the module script is added in Task 10. -->
```

with:

```html
    <script src="assets/vendor/d3.v7.min.js"></script>
    <script src="assets/vendor/js-yaml.min.js"></script>
    <script src="assets/vendor/marked.min.js"></script>
    <script src="assets/vendor/typography.min.js"></script>
    <!-- The module script is added in Task 10. -->
```

- [ ] **Step 5: Write `NOTICE`**

```
Third-party assets bundled with mynameisdaniil.dev

Fonts
- GOST 2.304-81 Type A Regular — Metrolog/Font.GOST2.304-81 v0.7.6
  https://github.com/Metrolog/Font.GOST2.304-81
  License: Apache-2.0

- Latin Modern Roman / Mono — GUST e-foundry, via @typopro/web-latin-modern 3.7.5
  License: SIL Open Font License 1.1
  https://www.gust.org.pl/projects/e-foundry/latin-modern

JavaScript
- d3 v7 — https://d3js.org — ISC License
- js-yaml v4 — https://github.com/nodeca/js-yaml — MIT License
- marked — https://github.com/markedjs/marked — MIT License
- tex-linebreak — https://github.com/robertknight/tex-linebreak — MIT License
- Hypher — https://github.com/bramstein/hypher — BSD-3-Clause License

Hyphenation patterns (via Hypher pattern packages, from hyph-utf8/TeX and
LibreOffice dictionaries; see upstream pattern metadata for per-language terms)
- hyphenation.en-us — https://github.com/bramstein/hypher
- hyphenation.ru — https://github.com/bramstein/hypher
```

- [ ] **Step 6: Browser check for globals**

Run `npm run serve`, open `http://localhost:8000`, then in the console:

```js
typeof d3 + " " + typeof jsyaml + " " + typeof marked + " " + typeof typography
```

Expected: `"object object object object"`. Then:

```js
typography.hyphenateFor("en-us")("hyphenation")
```

Expected: an array such as `["hy", "phen", "ation"]` (values may differ slightly). Then check the Network tab has no external requests, and Application → Frames shows no 404s.

- [ ] **Step 7: Commit**

```bash
git add -A
git commit -m "chore: vendor d3, js-yaml, marked, typography bundle, and fonts"
```

---

### Task 3: Content model and validation (TDD)

**Files:**
- Create: `js/model.js`, `tests/model.test.js`

**Interfaces:**
- Consumes: nothing (pure).
- Produces: `buildModel(raw) → { meta: { title, defaultCard }, cards: Map<string, { id, title, body, lang, links }>, edges: [{ source, target }], errors: string[] }` where each link is `{ label, url }` or `{ label, card }`; `isValid(model) → boolean`.

- [ ] **Step 1: Write the failing tests**

Create `tests/model.test.js`:

```js
import test from "node:test";
import assert from "node:assert/strict";
import { buildModel, isValid } from "../js/model.js";

const validRaw = {
  meta: { title: "Daniil Sobol", default_card: "about" },
  cards: [
    {
      id: "about",
      title: "Daniil Sobol",
      body: "Hello.",
      links: [
        { label: "GitHub", url: "https://github.com/mynameisdaniil" },
        { label: "Contact", card: "contact" },
      ],
    },
    {
      id: "contact",
      title: "Contact",
      links: [{ label: "Email", url: "mailto:sobol.daniil@gmail.com" }],
    },
  ],
};

test("builds a valid model and derives edges from internal links", () => {
  const model = buildModel(validRaw);
  assert.deepEqual(model.errors, []);
  assert.equal(model.meta.title, "Daniil Sobol");
  assert.equal(model.meta.defaultCard, "about");
  assert.equal(model.cards.size, 2);
  assert.deepEqual(model.edges, [{ source: "about", target: "contact" }]);
  assert.deepEqual(model.cards.get("about").links[1], { label: "Contact", card: "contact" });
  assert.equal(isValid(model), true);
});

test("flags duplicate ids and keeps the first card", () => {
  const model = buildModel({
    meta: { default_card: "a" },
    cards: [
      { id: "a", title: "A" },
      { id: "a", title: "B" },
    ],
  });
  assert.equal(model.errors.includes('duplicate card id "a"'), true);
  assert.equal(model.cards.get("a").title, "A");
});

test("requires meta.default_card", () => {
  const model = buildModel({ cards: [{ id: "a", title: "A" }] });
  assert.equal(model.errors.includes("meta.default_card is required"), true);
});

test("flags a default_card that does not exist", () => {
  const model = buildModel({ meta: { default_card: "nope" }, cards: [{ id: "a", title: "A" }] });
  assert.equal(model.errors.includes('meta.default_card "nope" does not exist'), true);
});

test("flags unknown internal link targets", () => {
  const model = buildModel({
    meta: { default_card: "a" },
    cards: [{ id: "a", title: "A", links: [{ label: "X", card: "ghost" }] }],
  });
  assert.equal(model.errors.includes('card "a" links to unknown card "ghost"'), true);
  assert.deepEqual(model.edges, []);
});

test("flags links with both url and card, or neither", () => {
  const model = buildModel({
    meta: { default_card: "a" },
    cards: [
      {
        id: "a",
        title: "A",
        links: [
          { label: "Both", url: "https://x", card: "a" },
          { label: "Neither" },
        ],
      },
    ],
  });
  assert.equal(model.errors.includes("cards[0].links[0] must have exactly one of url or card"), true);
  assert.equal(model.errors.includes("cards[0].links[1] must have exactly one of url or card"), true);
});

test("validates card ids and titles", () => {
  const model = buildModel({
    meta: { default_card: "Bad Id" },
    cards: [
      { id: "Bad Id", title: "A" },
      { id: "b" },
    ],
  });
  assert.equal(model.errors.includes("cards[0].id must match [a-z0-9-]+"), true);
  assert.equal(model.errors.includes("cards[1].title is required"), true);
});

test("defaults unknown link labels to the target", () => {
  const model = buildModel({
    meta: { default_card: "a" },
    cards: [{ id: "a", title: "A", links: [{ url: "https://x" }, { card: "a" }] }],
  });
  assert.equal(model.cards.get("a").links[0].label, "https://x");
  assert.equal(model.cards.get("a").links[1].label, "a");
});
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
npm test
```

Expected: FAIL — `Cannot find module .../js/model.js`.

- [ ] **Step 3: Implement `js/model.js`**

```js
const ID_RE = /^[a-z0-9-]+$/;

export function buildModel(raw) {
  const errors = [];
  const cards = new Map();
  const edges = [];

  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    return {
      meta: { title: "", defaultCard: "" },
      cards,
      edges,
      errors: ["content.yml: root must be a mapping"],
    };
  }

  const metaRaw = raw.meta && typeof raw.meta === "object" ? raw.meta : {};
  const title = typeof metaRaw.title === "string" ? metaRaw.title : "";
  const defaultCard = typeof metaRaw.default_card === "string" ? metaRaw.default_card : "";
  const list = Array.isArray(raw.cards) ? raw.cards : [];

  if (!defaultCard) errors.push("meta.default_card is required");
  if (!Array.isArray(raw.cards)) errors.push("cards must be a list");

  for (const [i, c] of list.entries()) {
    const where = `cards[${i}]`;
    if (!c || typeof c !== "object" || Array.isArray(c)) {
      errors.push(`${where} must be a mapping`);
      continue;
    }

    const id = typeof c.id === "string" ? c.id : "";
    const cardTitle = typeof c.title === "string" ? c.title : "";
    if (!id) errors.push(`${where}.id is required`);
    else if (!ID_RE.test(id)) errors.push(`${where}.id must match [a-z0-9-]+`);
    if (!cardTitle) errors.push(`${where}.title is required`);
    if (!id || cards.has(id)) continue;

    const links = [];
    const rawLinks = Array.isArray(c.links) ? c.links : [];
    if (c.links !== undefined && !Array.isArray(c.links)) {
      errors.push(`${where}.links must be a list`);
    }

    for (const [j, l] of rawLinks.entries()) {
      const lw = `${where}.links[${j}]`;
      if (!l || typeof l !== "object" || Array.isArray(l)) {
        errors.push(`${lw} must be a mapping`);
        continue;
      }
      const hasUrl = typeof l.url === "string" && l.url.length > 0;
      const hasCard = typeof l.card === "string" && l.card.length > 0;
      if (hasUrl === hasCard) {
        errors.push(`${lw} must have exactly one of url or card`);
        continue;
      }
      const fallback = hasUrl ? l.url : l.card;
      const label = typeof l.label === "string" && l.label ? l.label : fallback;
      links.push(hasUrl ? { label, url: l.url } : { label, card: l.card });
    }

    cards.set(id, {
      id,
      title: cardTitle,
      body: typeof c.body === "string" ? c.body : "",
      lang: typeof c.lang === "string" ? c.lang : "",
      links,
    });
  }

  for (const card of cards.values()) {
    for (const link of card.links) {
      if (!link.card) continue;
      if (!cards.has(link.card)) {
        errors.push(`card "${card.id}" links to unknown card "${link.card}"`);
      } else {
        edges.push({ source: card.id, target: link.card });
      }
    }
  }

  if (defaultCard && !cards.has(defaultCard)) {
    errors.push(`meta.default_card "${defaultCard}" does not exist`);
  }

  return { meta: { title, defaultCard }, cards, edges, errors };
}

export function isValid(model) {
  return model.errors.length === 0;
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
npm test
```

Expected: all `tests/model.test.js` tests PASS.

- [ ] **Step 5: Commit**

```bash
git add js/model.js tests/model.test.js
git commit -m "feat: add content model validation with tests"
```

---

### Task 4: Content loader and markdown rendering (TDD)

**Files:**
- Create: `js/content.js`, `tests/content.test.js`

**Interfaces:**
- Consumes: `buildModel` from `js/model.js` (Task 3).
- Produces: `loadContent(url, deps?) → Promise<model>`, `ContentError` (class with `.details: string[]`), `renderMarkdown(text, deps?) → string`. `deps` accepts `{ fetchImpl, yaml, marked }` for tests; defaults to `globalThis`.

- [ ] **Step 1: Write the failing tests**

Create `tests/content.test.js`:

```js
import test from "node:test";
import assert from "node:assert/strict";
import { loadContent, renderMarkdown, ContentError } from "../js/content.js";

const yaml = {
  load: () => ({
    meta: { title: "T", default_card: "a" },
    cards: [{ id: "a", title: "A" }],
  }),
};

test("loads and validates content", async () => {
  const fetchImpl = async () => ({ ok: true, status: 200, text: async () => "raw" });
  const model = await loadContent("content.yml", { fetchImpl, yaml });
  assert.equal(model.meta.title, "T");
  assert.equal(model.cards.has("a"), true);
});

test("rejects on non-ok HTTP responses", async () => {
  const fetchImpl = async () => ({ ok: false, status: 404, text: async () => "" });
  await assert.rejects(
    () => loadContent("content.yml", { fetchImpl, yaml }),
    /Failed to load content.yml: HTTP 404/,
  );
});

test("rejects with details on invalid content", async () => {
  const fetchImpl = async () => ({ ok: true, status: 200, text: async () => "raw" });
  const badYaml = { load: () => ({ cards: [] }) };
  await assert.rejects(
    () => loadContent("content.yml", { fetchImpl, yaml: badYaml }),
    (err) => {
      assert.equal(err instanceof ContentError, true);
      assert.equal(err.details.includes("meta.default_card is required"), true);
      return true;
    },
  );
});

test("delegates markdown rendering and handles empty text", () => {
  const marked = { parse: (text) => `<p>${text}</p>` };
  assert.equal(renderMarkdown("hi", { marked }), "<p>hi</p>");
  assert.equal(renderMarkdown("", { marked }), "");
});
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
npm test
```

Expected: FAIL — `Cannot find module .../js/content.js`.

- [ ] **Step 3: Implement `js/content.js`**

```js
import { buildModel } from "./model.js";

export class ContentError extends Error {
  constructor(message, details = []) {
    super(message);
    this.name = "ContentError";
    this.details = details;
  }
}

export async function loadContent(url, deps = {}) {
  const fetchImpl = deps.fetchImpl ?? globalThis.fetch;
  const yaml = deps.yaml ?? globalThis.jsyaml;

  const response = await fetchImpl(url);
  if (!response.ok) {
    throw new ContentError(`Failed to load ${url}: HTTP ${response.status}`);
  }

  const text = await response.text();
  const model = buildModel(yaml.load(text));
  if (model.errors.length > 0) {
    throw new ContentError(`${url} is invalid`, model.errors);
  }
  return model;
}

export function renderMarkdown(text, deps = {}) {
  if (!text) return "";
  const marked = deps.marked ?? globalThis.marked;
  return marked.parse(text);
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
npm test
```

Expected: all tests in `tests/model.test.js` and `tests/content.test.js` PASS.

- [ ] **Step 5: Commit**

```bash
git add js/content.js tests/content.test.js
git commit -m "feat: add content loader and markdown rendering with tests"
```

---

### Task 5: Hash router (TDD + jsdom wiring test)

**Files:**
- Create: `js/router.js`, `tests/router.test.js`

**Interfaces:**
- Consumes: nothing.
- Produces: `parseHash(hash, cardIds: Set<string>, defaultCard) → string`; `createRouter({ defaultCard, cardIds, onFocus, win? }) → { current, focus(id) }` where `current` is a getter.

- [ ] **Step 1: Write the failing tests**

Create `tests/router.test.js`:

```js
import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { parseHash, createRouter } from "../js/router.js";

const ids = new Set(["about", "contact"]);

test("parseHash returns valid ids, otherwise the default", () => {
  assert.equal(parseHash("#about", ids, "about"), "about");
  assert.equal(parseHash("about", ids, "about"), "about");
  assert.equal(parseHash("", ids, "about"), "about");
  assert.equal(parseHash("#nope", ids, "about"), "about");
  assert.equal(parseHash("#About", ids, "about"), "about");
});

test("router follows hash changes and reports focus", async () => {
  const dom = new JSDOM("<html></html>", { url: "http://localhost/#about" });
  const focused = [];
  const router = createRouter({
    defaultCard: "about",
    cardIds: ids,
    onFocus: (id) => focused.push(id),
    win: dom.window,
  });

  assert.equal(router.current, "about");
  dom.window.location.hash = "#contact";
  await new Promise((resolve) => dom.window.setTimeout(resolve, 10));
  assert.equal(router.current, "contact");
  assert.deepEqual(focused, ["contact"]);
});

test("focus sets the hash without double-reporting on the echo event", async () => {
  const dom = new JSDOM("<html></html>", { url: "http://localhost/#about" });
  const focused = [];
  const router = createRouter({
    defaultCard: "about",
    cardIds: ids,
    onFocus: (id) => focused.push(id),
    win: dom.window,
  });

  router.focus("contact");
  assert.equal(dom.window.location.hash, "#contact");
  assert.deepEqual(focused, ["contact"]);
  await new Promise((resolve) => dom.window.setTimeout(resolve, 10));
  assert.deepEqual(focused, ["contact"]);
});

test("focus falls back to the default for unknown ids", () => {
  const dom = new JSDOM("<html></html>", { url: "http://localhost/" });
  const focused = [];
  const router = createRouter({
    defaultCard: "about",
    cardIds: ids,
    onFocus: (id) => focused.push(id),
    win: dom.window,
  });

  router.focus("ghost");
  assert.equal(router.current, "about");
  assert.deepEqual(focused, ["about"]);
});

test("Escape focuses the default card", () => {
  const dom = new JSDOM("<html></html>", { url: "http://localhost/#contact" });
  const focused = [];
  const router = createRouter({
    defaultCard: "about",
    cardIds: ids,
    onFocus: (id) => focused.push(id),
    win: dom.window,
  });

  dom.window.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "Escape" }));
  assert.equal(router.current, "about");
  assert.deepEqual(focused, ["about"]);
});
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
npm test
```

Expected: FAIL — `Cannot find module .../js/router.js`.

- [ ] **Step 3: Implement `js/router.js`**

> **REVISED after review:** the original `lastSet` sentinel below had races
> (duplicate reports; a swallowed external `hashchange`). The approved final
> design reports synchronously from `focus()`, dedupes the echo when the
> parsed target equals `current`, and filters superseded queued events via
> `event.newURL`. The shipped code is:

```js
export function parseHash(hash, cardIds, defaultCard) {
  const id = String(hash ?? "").replace(/^#/, "");
  return cardIds.has(id) ? id : defaultCard;
}

export function createRouter({ defaultCard, cardIds, onFocus, win = globalThis }) {
  let current = parseHash(win.location.hash, cardIds, defaultCard);

  function focus(id) {
    const target = parseHash(`#${id}`, cardIds, defaultCard);
    if (target === current && win.location.hash === `#${target}`) return;
    current = target;
    if (win.location.hash !== `#${target}`) {
      win.location.hash = target;
    }
    onFocus(target);
  }

  function onHashChange(event) {
    if (event.newURL && event.newURL !== win.location.href) return;
    const target = parseHash(win.location.hash, cardIds, defaultCard);
    if (target === current) return;
    current = target;
    onFocus(target);
  }

  function onKeydown(event) {
    if (event.key === "Escape") focus(defaultCard);
  }

  win.addEventListener("hashchange", onHashChange);
  win.addEventListener("keydown", onKeydown);

  return {
    get current() {
      return current;
    },
    focus,
  };
}
```

The shipped `tests/router.test.js` keeps the five original tests (focus
tests made synchronous-first with a post-echo assertion) and adds three
regression tests: repeated same-target focus reports once, rapid
`focus("contact"); focus("about")` reports exactly `["contact","about"]`,
and an external hash change interleaved with a pending echo is not swallowed
(`["contact","about","contact"]`). Original sentinel code, for reference:

```js
export function createRouter({ defaultCard, cardIds, onFocus, win = globalThis }) {
  let current = parseHash(win.location.hash, cardIds, defaultCard);
  let lastSet = null;

  function focus(id) {
    const next = parseHash(`#${id}`, cardIds, defaultCard);
    current = next;
    if (win.location.hash !== `#${next}`) {
      lastSet = `#${next}`;
      win.location.hash = next;
    }
    onFocus(next);
  }

  function onHashChange() {
    if (win.location.hash === lastSet) {
      lastSet = null;
      return;
    }
    current = parseHash(win.location.hash, cardIds, defaultCard);
    onFocus(current);
  }

  function onKeydown(event) {
    if (event.key === "Escape") focus(defaultCard);
  }

  win.addEventListener("hashchange", onHashChange);
  win.addEventListener("keydown", onKeydown);

  return {
    get current() {
      return current;
    },
    focus,
  };
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
npm test
```

Expected: all tests PASS.

- [ ] **Step 5: Commit**

```bash
git add js/router.js tests/router.test.js
git commit -m "feat: add hash router with tests"
```

---

### Task 6: Theme resolution and toggle (TDD + styles)

**Files:**
- Create: `js/theme.js`, `tests/theme.test.js`
- Modify: `assets/site.css` (append toggle styles)

**Interfaces:**
- Consumes: DOM id `#theme-toggle` (Task 1).
- Produces: `resolveTheme(stored, systemDark) → "light" | "dark"`; `initTheme({ root?, button, storage?, mql? }) → { apply }`.

- [ ] **Step 1: Write the failing tests**

Create `tests/theme.test.js`:

```js
import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { resolveTheme, initTheme } from "../js/theme.js";

test("resolveTheme prefers stored override over system", () => {
  assert.equal(resolveTheme("dark", false), "dark");
  assert.equal(resolveTheme("light", true), "light");
  assert.equal(resolveTheme(null, true), "dark");
  assert.equal(resolveTheme(null, false), "light");
  assert.equal(resolveTheme("bogus", true), "dark");
});

test("toggle flips the theme, updates aria, and persists", () => {
  const dom = new JSDOM("<button id='t'></button>");
  const { document } = dom.window;
  const store = new Map();
  const storage = {
    getItem: (k) => (store.has(k) ? store.get(k) : null),
    setItem: (k, v) => store.set(k, v),
  };
  const mql = { matches: false, addEventListener() {} };
  const button = document.getElementById("t");

  initTheme({ root: document.documentElement, button, storage, mql });
  assert.equal(document.documentElement.classList.contains("dark"), false);
  assert.equal(button.getAttribute("aria-pressed"), "false");

  button.click();
  assert.equal(document.documentElement.classList.contains("dark"), true);
  assert.equal(button.getAttribute("aria-pressed"), "true");
  assert.equal(store.get("theme"), "dark");

  button.click();
  assert.equal(document.documentElement.classList.contains("dark"), false);
  assert.equal(store.get("theme"), "light");
});

test("system changes apply only when no override is stored", () => {
  const dom = new JSDOM("<button id='t'></button>");
  const { document } = dom.window;
  const store = new Map();
  const storage = {
    getItem: (k) => (store.has(k) ? store.get(k) : null),
    setItem: (k, v) => store.set(k, v),
  };
  const listeners = [];
  const mql = {
    matches: false,
    addEventListener: (_type, fn) => listeners.push(fn),
  };

  initTheme({ root: document.documentElement, button: document.getElementById("t"), storage, mql });
  mql.matches = true;
  listeners.forEach((fn) => fn());
  assert.equal(document.documentElement.classList.contains("dark"), true);

  store.set("theme", "light");
  mql.matches = false;
  listeners.forEach((fn) => fn());
  assert.equal(document.documentElement.classList.contains("dark"), true);
});
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
npm test
```

Expected: FAIL — `Cannot find module .../js/theme.js`.

- [ ] **Step 3: Implement `js/theme.js`**

```js
export function resolveTheme(stored, systemDark) {
  if (stored === "light" || stored === "dark") return stored;
  return systemDark ? "dark" : "light";
}

export function initTheme({
  root = document.documentElement,
  button = null,
  storage = globalThis.localStorage,
  mql = globalThis.matchMedia("(prefers-color-scheme: dark)"),
} = {}) {
  function storedValue() {
    try {
      return storage ? storage.getItem("theme") : null;
    } catch (err) {
      return null;
    }
  }

  function apply() {
    const mode = resolveTheme(storedValue(), mql.matches);
    root.classList.toggle("dark", mode === "dark");
    root.style.colorScheme = mode;
    if (button) {
      const isDark = mode === "dark";
      button.setAttribute("aria-pressed", String(isDark));
      button.setAttribute("aria-label", isDark ? "Switch to light theme" : "Switch to dark theme");
    }
  }

  function onToggle() {
    const next = root.classList.contains("dark") ? "light" : "dark";
    try {
      storage?.setItem("theme", next);
    } catch (err) {}
    apply();
  }

  button?.addEventListener("click", onToggle);
  mql.addEventListener?.("change", () => {
    if (!storedValue()) apply();
  });
  apply();

  return { apply };
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
npm test
```

Expected: all tests PASS.

- [ ] **Step 5: Append toggle styles to `assets/site.css`**

```css
.theme-toggle {
  position: fixed;
  top: 1rem;
  right: 1rem;
  z-index: 10;
  display: grid;
  place-items: center;
  width: 2.75rem;
  height: 2.75rem;
  border: 1px solid var(--card-border);
  border-radius: 999px;
  background: var(--card-bg);
  color: var(--title);
  cursor: pointer;
  transition: background-color 0.3s ease;
}

.theme-toggle:hover {
  background: var(--card-bg-focus);
}

.theme-toggle svg {
  width: 1.1rem;
  height: 1.1rem;
  fill: currentColor;
}

.theme-toggle .theme-toggle__moon {
  display: none;
}

.dark .theme-toggle .theme-toggle__sun {
  display: none;
}

.dark .theme-toggle .theme-toggle__moon {
  display: block;
}
```

- [ ] **Step 6: Browser check**

Run `npm run serve`. Expected: toggle switches the gradient/text between light and dark instantly; the icon swaps sun/moon; reloading keeps the choice; deleting `localStorage.theme` and reloading follows the OS setting; no flash of the wrong theme.

- [ ] **Step 7: Commit**

```bash
git add js/theme.js tests/theme.test.js assets/site.css
git commit -m "feat: add theme resolution, toggle, and styles with tests"
```

---

### Task 7: Card component (TDD + styles)

**Files:**
- Create: `js/card.js`, `tests/card.test.js`
- Modify: `assets/site.css` (append card styles)

**Interfaces:**
- Consumes: card objects from `js/model.js` (Task 3).
- Produces: `createCard(card, { onSelect }) → HTMLElement`; `setCardState(el, state)` where state is `"focused" | "background"`; `setCardBody(el, html)`; `markActiveLinks(el, activeId)`; `createErrorCard(message, details = []) → HTMLElement`.

- [ ] **Step 1: Write the failing tests**

Create `tests/card.test.js`:

```js
import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { createCard, setCardState, setCardBody, markActiveLinks, createErrorCard } from "../js/card.js";

function setup() {
  const dom = new JSDOM("<div id='root'></div>");
  globalThis.document = dom.window.document;
  globalThis.Node = dom.window.Node;
  return dom;
}

const card = {
  id: "about",
  title: "Daniil Sobol",
  body: "Hello.",
  links: [
    { label: "GitHub", url: "https://github.com/mynameisdaniil" },
    { label: "Contact", card: "contact" },
  ],
};

test("createCard builds the structure and links", () => {
  setup();
  const el = createCard(card, { onSelect: () => {} });

  assert.equal(el.tagName, "ARTICLE");
  assert.equal(el.dataset.cardId, "about");
  assert.equal(el.classList.contains("card--background"), true);
  assert.equal(el.querySelector(".card-title").textContent, "Daniil Sobol");
  assert.equal(el.querySelector(".card-title").tabIndex, -1);

  const [github, contact] = el.querySelectorAll(".card-links a");
  assert.equal(github.getAttribute("href"), "https://github.com/mynameisdaniil");
  assert.equal(github.getAttribute("target"), "_blank");
  assert.equal(github.getAttribute("rel"), "noopener noreferrer");
  assert.equal(contact.getAttribute("href"), "#contact");
  assert.equal(contact.dataset.internal, "true");
});

test("clicking a background card selects it, links are left alone", () => {
  const dom = setup();
  const selected = [];
  const el = createCard(card, { onSelect: (id) => selected.push(id) });
  dom.window.document.body.appendChild(el);

  el.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true }));
  assert.deepEqual(selected, ["about"]);

  const contact = el.querySelector("a[data-internal]");
  contact.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true, cancelable: true }));
  assert.deepEqual(selected, ["about"]);

  const github = el.querySelector('a[target="_blank"]');
  github.addEventListener("click", (event) => event.preventDefault());
  github.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true, cancelable: true }));
  assert.deepEqual(selected, ["about"]);
});

test("Enter and Space select a card", () => {
  const dom = setup();
  const selected = [];
  const el = createCard(card, { onSelect: (id) => selected.push(id) });
  el.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
  el.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: " ", bubbles: true }));
  assert.deepEqual(selected, ["about", "about"]);
});

test("setCardState toggles focused/background classes", () => {
  setup();
  const el = createCard(card, { onSelect: () => {} });
  setCardState(el, "focused");
  assert.equal(el.classList.contains("card--focused"), true);
  assert.equal(el.classList.contains("card--background"), false);
  setCardState(el, "background");
  assert.equal(el.classList.contains("card--focused"), false);
  assert.equal(el.classList.contains("card--background"), true);
});

test("setCardBody renders once and unhides", () => {
  setup();
  const el = createCard(card, { onSelect: () => {} });
  setCardBody(el, "<p>one</p>");
  setCardBody(el, "<p>two</p>");
  const body = el.querySelector(".card-body");
  assert.equal(body.hidden, false);
  assert.equal(body.innerHTML, "<p>one</p>");
});

test("markActiveLinks sets aria-current on the matching internal link", () => {
  setup();
  const el = createCard(card, { onSelect: () => {} });
  markActiveLinks(el, "contact");
  assert.equal(el.querySelector("a[data-internal]").getAttribute("aria-current"), "page");
  markActiveLinks(el, "about");
  assert.equal(el.querySelector("a[data-internal]").hasAttribute("aria-current"), false);
});

test("createErrorCard shows the message and details", () => {
  setup();
  const el = createErrorCard("Could not load content.", ["first problem", "second problem"]);
  assert.equal(el.classList.contains("card--error"), true);
  assert.match(el.textContent, /Could not load content\./);
  assert.match(el.textContent, /first problem/);
  assert.equal(el.querySelectorAll("li").length, 2);
});
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
npm test
```

Expected: FAIL — `Cannot find module .../js/card.js`.

- [ ] **Step 3: Implement `js/card.js`**

```js
export function createCard(card, { onSelect }) {
  const el = document.createElement("article");
  el.className = "card card--background";
  el.dataset.cardId = card.id;
  el.tabIndex = 0;
  el.setAttribute("aria-labelledby", `card-${card.id}-title`);

  const title = document.createElement("h1");
  title.className = "card-title";
  title.id = `card-${card.id}-title`;
  title.tabIndex = -1;
  title.textContent = card.title;

  const body = document.createElement("div");
  body.className = "card-body";
  body.hidden = true;

  const nav = document.createElement("nav");
  nav.className = "card-links";
  nav.setAttribute("aria-label", `Links for ${card.title}`);

  for (const link of card.links) {
    const a = document.createElement("a");
    a.textContent = link.label;
    if (link.url) {
      a.href = link.url;
      a.target = "_blank";
      a.rel = "noopener noreferrer";
    } else {
      a.href = `#${link.card}`;
      a.dataset.internal = "true";
    }
    nav.appendChild(a);
  }

  el.append(title, body, nav);

  el.addEventListener("click", (event) => {
    if (event.target.closest("a")) return;
    event.preventDefault();
    onSelect(card.id);
  });

  el.addEventListener("keydown", (event) => {
    if (event.key === "Enter" || event.key === " ") {
      event.preventDefault();
      onSelect(card.id);
    }
  });

  return el;
}

export function setCardState(el, state) {
  const focused = state === "focused";
  el.classList.toggle("card--focused", focused);
  el.classList.toggle("card--background", !focused);
}

export function setCardBody(el, html) {
  const body = el.querySelector(".card-body");
  if (!body || body.dataset.rendered === "true") return;
  body.innerHTML = html;
  body.hidden = false;
  body.dataset.rendered = "true";
}

export function markActiveLinks(el, activeId) {
  for (const a of el.querySelectorAll("a[data-internal]")) {
    if (a.getAttribute("href") === `#${activeId}`) {
      a.setAttribute("aria-current", "page");
    } else {
      a.removeAttribute("aria-current");
    }
  }
}

export function createErrorCard(message, details = []) {
  const el = document.createElement("article");
  el.className = "card card--error";

  const title = document.createElement("h1");
  title.className = "card-title";
  title.textContent = "Error";

  const body = document.createElement("div");
  body.className = "card-body";
  const p = document.createElement("p");
  p.textContent = message;
  body.appendChild(p);

  if (details.length > 0) {
    const list = document.createElement("ul");
    for (const detail of details) {
      const li = document.createElement("li");
      li.textContent = detail;
      list.appendChild(li);
    }
    body.appendChild(list);
  }

  el.append(title, body);
  return el;
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
npm test
```

Expected: all tests PASS.

- [ ] **Step 5: Append card styles to `assets/site.css`**

```css
.card {
  position: absolute;
  top: 0;
  left: 0;
  box-sizing: border-box;
  padding: 1.4rem 1.6rem;
  border: 1px solid var(--card-border);
  border-radius: 4px;
  background: var(--card-bg);
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  cursor: pointer;
  outline: none;
  will-change: transform;
  transition:
    scale 0.4s cubic-bezier(0.22, 0.61, 0.36, 1),
    filter 0.4s ease,
    opacity 0.4s ease,
    background-color 0.4s ease,
    box-shadow 0.4s ease;
}

.card--focused {
  width: min(42rem, 92vw);
  max-height: min(80vh, 720px);
  overflow: auto;
  background: var(--card-bg-focus);
  box-shadow: var(--shadow);
  cursor: default;
}

.card--background {
  width: 14rem;
  scale: 0.6;
  filter: blur(5px);
  opacity: 0.45;
}

.card--background:hover {
  opacity: 0.7;
  filter: blur(2px);
}

.card:focus-visible {
  outline: 2px solid var(--title);
  outline-offset: 3px;
}

.card-title {
  margin: 0 0 0.8rem;
  font-size: clamp(1.6rem, 4vw, 2.6rem);
  line-height: 1.1;
}

.card--background .card-title {
  margin: 0;
  font-size: 1.1rem;
}

.card-body {
  max-width: 65ch;
}

.card-body p {
  margin: 0;
}

.card-body p + p {
  text-indent: 1.5em;
}

.card-body a {
  color: inherit;
  text-decoration: underline;
  text-decoration-thickness: 1px;
  text-underline-offset: 0.15em;
}

.card-body a:hover {
  text-decoration-thickness: 2px;
}

.card-links {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem 1.25rem;
  margin-top: 1.2rem;
}

.card-links a {
  color: var(--title);
  text-decoration: underline;
  text-decoration-thickness: 1px;
  text-underline-offset: 0.15em;
}

.card-links a[aria-current="page"] {
  opacity: 0.55;
  text-decoration: none;
}

.card--background .card-body,
.card--background .card-links {
  display: none;
}

.card--error {
  position: static;
  width: min(36rem, 92vw);
  margin: 15vh auto;
  cursor: default;
}

.card--error ul {
  margin: 0.75rem 0 0;
  padding-left: 1.25rem;
  text-align: left;
}
```

- [ ] **Step 6: Commit**

```bash
git add js/card.js tests/card.test.js assets/site.css
git commit -m "feat: add card component with tests and styles"
```

---

### Task 8: Graph engine

> **REVISED after review:** the implementation code below is stale. The shipped
> `js/graph.js` additionally: seeds nodes in a deterministic ring and measures
> before the first tick; uses `Math.hypot(w, h) / 2` for the collision radius;
> re-invokes the d3 force accessors via `refreshForces()` after every
> measurement (d3-force v3 caches them at `initialize()`); and uses
> `settle(alpha)` (stop + set alpha + 150 manual ticks + render) for
> `prefers-reduced-motion` on creation (`settle(1)`), focus (`settle(0.6)`),
> and resize (`settle(0.2)`). See `js/graph.js` for the source of truth.

**Files:**
- Create: `js/graph.js`

**Interfaces:**
- Consumes: globals `d3` (Task 2 vendor) and `setCardState` from `js/card.js` (Task 7); DOM ids `#stage`, `#edges`, `#cards` (Task 1).
- Produces: `createGraph({ stage, svg, cardsById, edges, win? }) → { focusCard(id), resize(), destroy() }`. `cardsById` is a `Map<string, HTMLElement>`; `edges` is `[{ source, target }]`.

- [ ] **Step 1: Implement `js/graph.js`**

```js
import { setCardState } from "./card.js";

function forceBounds(padding) {
  let nodes = [];
  let width = 0;
  let height = 0;

  function force(alpha) {
    for (const node of nodes) {
      const r = node.radius + padding;
      if (node.x < r) node.vx += (r - node.x) * alpha * 0.4;
      if (node.x > width - r) node.vx -= (node.x - (width - r)) * alpha * 0.4;
      if (node.y < r) node.vy += (r - node.y) * alpha * 0.4;
      if (node.y > height - r) node.vy -= (node.y - (height - r)) * alpha * 0.4;
    }
  }

  force.initialize = (n) => {
    nodes = n;
  };

  force.size = ([w, h]) => {
    width = w;
    height = h;
    return force;
  };

  return force;
}

export function createGraph({ stage, svg, cardsById, edges, win = window }) {
  const nodeById = new Map();
  const nodes = [...cardsById.entries()].map(([id, el]) => {
    const node = { id, el, x: 0, y: 0, w: 0, h: 0, radius: 60, focused: false };
    nodeById.set(id, node);
    return node;
  });

  const linkData = edges.map((edge) => ({ source: edge.source, target: edge.target }));

  let width = stage.clientWidth || win.innerWidth;
  let height = stage.clientHeight || win.innerHeight;

  const svgNS = "http://www.w3.org/2000/svg";
  const lineById = new Map();
  for (const edge of linkData) {
    const line = document.createElementNS(svgNS, "line");
    line.setAttribute("class", "edge");
    line.dataset.source = edge.source;
    line.dataset.target = edge.target;
    svg.appendChild(line);
    lineById.set(`${edge.source}->${edge.target}`, line);
  }

  const reducedMotion = win.matchMedia?.("(prefers-reduced-motion: reduce)")?.matches ?? false;
  const boundary = forceBounds(48);

  function linkDistance(link) {
    return link.source.focused || link.target.focused ? 240 : 170;
  }

  const simulation = d3
    .forceSimulation(nodes)
    .force(
      "link",
      d3
        .forceLink(linkData)
        .id((d) => d.id)
        .distance(linkDistance)
        .strength(0.25),
    )
    .force("charge", d3.forceManyBody().strength(-420))
    .force(
      "collide",
      d3
        .forceCollide()
        .radius((d) => d.radius + 12)
        .strength(0.9),
    )
    .force("x", d3.forceX(() => width / 2).strength((d) => (d.focused ? 0.14 : 0.015)))
    .force("y", d3.forceY(() => height / 2).strength((d) => (d.focused ? 0.14 : 0.015)))
    .force("boundary", boundary.size([width, height]))
    .alphaDecay(0.05)
    .on("tick", tick);

  function measure(node) {
    node.w = node.el.offsetWidth || 200;
    node.h = node.el.offsetHeight || 80;
    const scale = node.focused ? 1 : 0.6;
    node.radius = (Math.max(node.w, node.h) / 2) * scale;
  }

  function tick() {
    for (const node of nodes) {
      node.el.style.transform = `translate(${node.x - node.w / 2}px, ${node.y - node.h / 2}px)`;
    }
    for (const link of linkData) {
      const line = lineById.get(`${link.source.id}->${link.target.id}`);
      if (!line) continue;
      line.setAttribute("x1", link.source.x);
      line.setAttribute("y1", link.source.y);
      line.setAttribute("x2", link.target.x);
      line.setAttribute("y2", link.target.y);
    }
  }

  function focusCard(id) {
    const target = nodeById.get(id);
    if (!target) return;

    for (const node of nodes) {
      node.focused = node.id === id;
      setCardState(node.el, node.focused ? "focused" : "background");
    }
    for (const node of nodes) measure(node);

    for (const line of lineById.values()) {
      const active = line.dataset.source === id || line.dataset.target === id;
      line.classList.toggle("edge--active", active);
    }

    if (reducedMotion) {
      for (let i = 0; i < 120; i += 1) simulation.tick();
      tick();
    } else {
      simulation.alpha(0.6).restart();
    }
  }

  function resize() {
    width = stage.clientWidth || win.innerWidth;
    height = stage.clientHeight || win.innerHeight;
    boundary.size([width, height]);
    simulation.force("x").x(() => width / 2);
    simulation.force("y").y(() => height / 2);
    for (const node of nodes) measure(node);
    if (reducedMotion) {
      tick();
    } else {
      simulation.alpha(0.2).restart();
    }
  }

  const observer = new ResizeObserver(() => resize());
  observer.observe(stage);

  return {
    focusCard,
    resize,
    destroy() {
      observer.disconnect();
      simulation.stop();
    },
  };
}
```

- [ ] **Step 2: Syntax check**

```bash
node --check js/graph.js
```

Expected: no output (exit 0).

- [ ] **Step 3: Browser scratch check**

Temporarily add this to the end of `js/main.js` (will be replaced in Task 10):

```js
const scratch = document.createElement("div");
scratch.innerHTML =
  '<div id="s" style="position:fixed;inset:0"></div><svg id="se"></svg>';
document.body.appendChild(scratch);
```

Then in the console after `npm run serve`:

```js
const { createGraph } = await import("./js/graph.js");
const stage = document.getElementById("s");
const svg = document.getElementById("se");
const el = document.createElement("article");
el.className = "card card--focused";
el.textContent = "Test";
stage.appendChild(el);
const g = createGraph({ stage, svg, cardsById: new Map([["a", el]]), edges: [], onSelect: () => {} });
g.focusCard("a");
```

Expected: the card animates to the viewport center and stays there; `el.style.transform` is a `translate(...)`; `g.resize()` keeps it centered after a window resize. Remove the scratch code afterwards.

- [ ] **Step 4: Commit**

```bash
git add js/graph.js
git commit -m "feat: add d3 force graph engine"
```

---

### Task 9: Protrusion pass and typography pipeline (TDD)

**Files:**
- Create: `js/protrusion.js`, `js/typography.js`, `tests/protrusion.test.js`

**Interfaces:**
- Consumes: global `typography` (Task 2 vendor).
- Produces: `protrusionFor(char, side: "left" | "right") → number` (em units); `applyProtrusion(root) → void`; `clearProtrusion(root) → void`; `createTypography({ getHyphenator, typography? }) → { applyTo(el, lang), schedule(el, lang) → Promise, flush() → Promise }`.

- [ ] **Step 1: Write the failing tests**

Create `tests/protrusion.test.js`:

```js
import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { protrusionFor, applyProtrusion, clearProtrusion } from "../js/protrusion.js";

test("protrusionFor knows left and right punctuation", () => {
  assert.equal(protrusionFor(".", "right"), 0.15);
  assert.equal(protrusionFor(",", "right"), 0.1);
  assert.equal(protrusionFor("-", "right"), 0.2);
  assert.equal(protrusionFor("-", "left"), 0.2);
  assert.equal(protrusionFor("«", "left"), 0.15);
  assert.equal(protrusionFor("»", "right"), 0.15);
  assert.equal(protrusionFor("a", "right"), 0);
  assert.equal(protrusionFor(".", "left"), 0);
});

function makeParagraph() {
  const dom = new JSDOM("<p>Hello, world.<br>Second line!</p>");
  globalThis.document = dom.window.document;
  globalThis.Node = dom.window.Node;
  globalThis.NodeFilter = dom.window.NodeFilter;
  return dom.window.document.querySelector("p");
}

test("applyProtrusion wraps line-edge punctuation with negative margins", () => {
  const p = makeParagraph();
  applyProtrusion(p);

  const left = p.querySelector(".protrude-left");
  const right = p.querySelectorAll(".protrude-right");
  assert.equal(left.textContent, "H");
  assert.equal(left.style.marginLeft, "-0.1em");

  const dots = [...right].filter((span) => span.textContent === ".");
  assert.equal(dots.length, 1);
  assert.equal(dots[0].style.marginRight, "-0.15em");

  const bangs = [...right].filter((span) => span.textContent === "!");
  assert.equal(bangs.length, 1);
  assert.equal(bangs[0].style.marginRight, "-0.1em");
});

test("clearProtrusion removes the wrappers and restores the text", () => {
  const p = makeParagraph();
  const original = p.textContent;
  applyProtrusion(p);
  clearProtrusion(p);
  assert.equal(p.querySelectorAll(".protrude-left, .protrude-right").length, 0);
  assert.equal(p.textContent, original);
});

test("applyProtrusion is idempotent when cleared first", () => {
  const p = makeParagraph();
  applyProtrusion(p);
  const first = p.innerHTML;
  clearProtrusion(p);
  applyProtrusion(p);
  assert.equal(p.innerHTML, first);
});
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
npm test
```

Expected: FAIL — `Cannot find module .../js/protrusion.js`.

- [ ] **Step 3: Implement `js/protrusion.js`**

```js
const LEFT = {
  '"': 0.1,
  "'": 0.1,
  "«": 0.15,
  "»": 0.15,
  "(": 0.1,
  "[": 0.1,
  "-": 0.2,
};

const RIGHT = {
  ",": 0.1,
  ".": 0.15,
  "!": 0.1,
  "?": 0.15,
  ":": 0.1,
  ";": 0.1,
  '"': 0.1,
  "'": 0.1,
  "»": 0.15,
  ")": 0.1,
  "]": 0.1,
  "-": 0.2,
};

export function protrusionFor(char, side) {
  const table = side === "left" ? LEFT : RIGHT;
  return table[char] ?? 0;
}

function wrapRange(textNode, start, length, className) {
  const node = textNode.splitText(start);
  node.splitText(length);
  const span = document.createElement("span");
  span.className = className;
  node.parentNode.insertBefore(span, node);
  span.appendChild(node);
  return span;
}

function wrapLeft(textNode, offset, char, amount) {
  const span = wrapRange(textNode, offset, char.length, "protrude-left");
  span.style.marginLeft = `-${amount}em`;
}

function wrapRight(textNode, offset, char, amount) {
  const span = wrapRange(textNode, offset, char.length, "protrude-right");
  span.style.marginRight = `-${amount}em`;
}

function collectLines(paragraph) {
  const lines = [];
  let current = null;
  const walker = document.createTreeWalker(paragraph, NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT);

  let node = walker.nextNode();
  while (node) {
    if (node.nodeType === Node.ELEMENT_NODE && node.tagName === "BR") {
      if (current) lines.push(current);
      current = null;
    } else if (node.nodeType === Node.TEXT_NODE && node.nodeValue.length > 0) {
      if (!current) current = { first: { node, offset: 0 }, last: null };
      current.last = { node, offset: node.nodeValue.length - 1 };
    }
    node = walker.nextNode();
  }

  if (current) lines.push(current);
  return lines;
}

export function applyProtrusion(root) {
  for (const paragraph of root.querySelectorAll("p")) {
    for (const line of collectLines(paragraph)) {
      const firstChar = line.first.node.nodeValue.charAt(line.first.offset);

      let last = line.last;
      let lastChar = last.node.nodeValue.charAt(last.offset);
      while (lastChar !== "" && /\s/.test(lastChar)) {
        if (last.offset > 0) {
          last = { node: last.node, offset: last.offset - 1 };
        } else {
          const previous = previousTextNode(last.node);
          if (!previous) break;
          last = { node: previous, offset: previous.nodeValue.length - 1 };
        }
        lastChar = last.node.nodeValue.charAt(last.offset);
      }

      const rightAmount = protrusionFor(lastChar, "right");
      if (rightAmount > 0) wrapRight(last.node, last.offset, lastChar, rightAmount);

      const firstStillThere =
        line.first.node.nodeValue.length > line.first.offset &&
        line.first.node.nodeValue.charAt(line.first.offset) === firstChar;
      const leftAmount = protrusionFor(firstChar, "left");
      if (leftAmount > 0 && firstStillThere) {
        wrapLeft(line.first.node, line.first.offset, firstChar, leftAmount);
      }
    }
  }
}

function previousTextNode(node) {
  let current = node.previousSibling;
  while (current) {
    if (current.nodeType === Node.TEXT_NODE) return current;
    if (current.lastChild) {
      let inner = current.lastChild;
      while (inner.lastChild) inner = inner.lastChild;
      if (inner.nodeType === Node.TEXT_NODE) return inner;
    }
    current = current.previousSibling;
  }
  return null;
}

export function clearProtrusion(root) {
  for (const span of root.querySelectorAll(".protrude-left, .protrude-right")) {
    const parent = span.parentNode;
    while (span.firstChild) parent.insertBefore(span.firstChild, span);
    parent.removeChild(span);
  }
  root.normalize();
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
npm test
```

Expected: all tests PASS.

- [ ] **Step 5: Implement `js/typography.js`**

```js
import { applyProtrusion, clearProtrusion } from "./protrusion.js";

export function createTypography({ getHyphenator, typography = globalThis.typography }) {
  let raf = 0;
  let pending = null;

  function applyTo(el, lang = "en-us") {
    const paragraphs = [...el.querySelectorAll(".card-body p")];
    if (paragraphs.length === 0) return;

    clearProtrusion(el);
    try {
      typography.justifyContent(paragraphs, getHyphenator(lang));
      applyProtrusion(el);
    } catch (err) {
      console.error("typography: justification failed", err);
    }
  }

  function schedule(el, lang = "en-us") {
    cancelAnimationFrame(raf);
    pending = new Promise((resolve) => {
      raf = requestAnimationFrame(() => {
        applyTo(el, lang);
        pending = null;
        resolve();
      });
    });
    return pending;
  }

  async function flush() {
    if (pending) await pending;
  }

  return { applyTo, schedule, flush };
}
```

- [ ] **Step 6: Syntax check**

```bash
node --check js/typography.js
```

Expected: no output (exit 0).

- [ ] **Step 7: Commit**

```bash
git add js/protrusion.js js/typography.js tests/protrusion.test.js
git commit -m "feat: add protrusion pass and typography pipeline with tests"
```

---

### Task 10: Wire everything together

**Files:**
- Modify: `js/main.js` (replace the scratch content entirely), `index.html` (add module script)

**Interfaces:**
- Consumes: all modules from Tasks 3–9 and globals from Task 2.
- Produces: the complete running app.

- [ ] **Step 1: Replace `js/main.js`**

```js
import { loadContent, renderMarkdown, ContentError } from "./content.js";
import {
  createCard,
  setCardBody,
  markActiveLinks,
  createErrorCard,
} from "./card.js";
import { createGraph } from "./graph.js";
import { createRouter } from "./router.js";
import { initTheme } from "./theme.js";
import { createTypography } from "./typography.js";

const stage = document.getElementById("stage");
const cardsLayer = document.getElementById("cards");
const svg = document.getElementById("edges");
const live = document.getElementById("live");
const toggle = document.getElementById("theme-toggle");

initTheme({ button: toggle });

const typography = createTypography({
  getHyphenator: (lang) => globalThis.typography.hyphenateFor(lang),
});

try {
  const model = await loadContent("content.yml");
  document.title = model.meta.title;

  const cardsById = new Map();
  let router = null;

  for (const card of model.cards.values()) {
    const el = createCard(card, { onSelect: (id) => router.focus(id) });
    cardsById.set(card.id, el);
    cardsLayer.appendChild(el);
  }

  const graph = createGraph({
    stage,
    svg,
    cardsById,
    edges: model.edges,
  });

  function currentLang() {
    const card = model.cards.get(router.current);
    return card?.lang || "en-us";
  }

  function render(id) {
    const card = model.cards.get(id);
    const el = cardsById.get(id);
    setCardBody(el, renderMarkdown(card.body));
    graph.focusCard(id);
    el.querySelector(".card-title")?.focus({ preventScroll: true });
    for (const cardEl of cardsById.values()) markActiveLinks(cardEl, id);
    live.textContent = `Showing ${card.title}`;
    document.title =
      id === model.meta.defaultCard ? model.meta.title : `${card.title} — ${model.meta.title}`;
    typography.schedule(el, card.lang || "en-us");
  }

  router = createRouter({
    defaultCard: model.meta.defaultCard,
    cardIds: new Set(model.cards.keys()),
    onFocus: render,
  });

  let resizeTimer = 0;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => {
      typography.schedule(cardsById.get(router.current), currentLang());
    }, 150);
  });

  await document.fonts.ready;
  render(router.current);
  await typography.flush();
  stage.style.visibility = "visible";
  stage.setAttribute("aria-busy", "false");
} catch (err) {
  console.error(err);
  const details = err instanceof ContentError ? err.details : [String(err)];
  stage.appendChild(createErrorCard("Could not load content.", details));
  stage.style.visibility = "visible";
  stage.setAttribute("aria-busy", "false");
}
```

- [ ] **Step 2: Add the module script to `index.html`**

Replace:

```html
    <!-- The module script is added in Task 10. -->
```

with:

```html
    <script type="module" src="js/main.js"></script>
```

- [ ] **Step 3: Create `content.yml`**

```yaml
meta:
  title: Daniil Sobol
  default_card: about
cards:
  - id: about
    title: Daniil Sobol
    body: |
      Software engineer working on distributed systems, Erlang, and Ethereum
      infrastructure. This site is a graph; right now it is a single vertex.
    links:
      - label: GitHub
        url: https://github.com/mynameisdaniil
      - label: Email
        url: mailto:sobol.daniil@gmail.com
```

- [ ] **Step 4: Browser check**

Run `npm run serve`, open `http://localhost:8000`. Expected:

- The card fades in centered, title in GOST Type A, body in Latin Modern Roman.
- Body paragraphs are justified with TeX line breaking and hyphenation; `\n` in the YAML body produces one paragraph (a blank line produces a second).
- GitHub/Email links open in a new tab.
- `document.title` is `Daniil Sobol`; `location.hash` becomes `#about`.
- No console errors, no network requests outside `localhost`.

- [ ] **Step 5: Error-path check**

```bash
mv content.yml content.yml.bak
```

Reload. Expected: a centered error card saying "Could not load content." with an HTTP 404 detail, `aria-busy` removed, no uncaught exception. Restore:

```bash
mv content.yml.bak content.yml
```

- [ ] **Step 6: Commit**

```bash
git add js/main.js index.html content.yml
git commit -m "feat: wire content, graph, router, theme, and typography together"
```

---

### Task 11: Final polish and end-to-end verification

**Files:**
- Modify: `assets/site.css` (protrusion helpers + responsive tweaks, if missing)

**Interfaces:**
- Consumes: everything above.
- Produces: the spec's Definition of Done, verified.

- [ ] **Step 1: Ensure graph/protrusion helper classes exist in `assets/site.css`**

The protrusion pass sets inline margins, but these classes keep the elements inline and are useful for debugging. The edge stroke rules were missing from Task 1's initial stylesheet (only the reduced-motion transition rule existed). Append if not present:

```css
.edge {
  stroke: var(--edge-line);
  stroke-width: 1;
  transition: stroke 0.35s ease;
}

.edge--active {
  stroke: var(--edge-line-active);
}

.protrude-left,
.protrude-right {
  display: inline;
}

@media (max-width: 640px) {
  body {
    font-size: 16px;
  }

  .card--focused {
    padding: 1rem 1.1rem;
  }
}
```

- [ ] **Step 2: Run the full unit suite**

```bash
npm test
```

Expected: all tests in `tests/` PASS.

- [ ] **Step 3: Verify the spec checklist**

Run `npm run serve` and confirm each item:

1. Single `about` card loads from `content.yml`; GOST titles; Latin Modern body.
2. Justified, hyphenated text; punctuation protrudes slightly; no horizontal overflow.
3. Light/dark: OS default honored on first visit (test by clearing `localStorage.theme` and toggling OS/browser preference); toggle flips and persists; no wrong-theme flash on reload.
4. `#about` deep link loads; `#bogus` falls back to about; back/forward after clicking internal links (tested in Step 4).
5. Resize re-justifies after ~150ms and the card stays centered.
6. Keyboard: Tab reaches the card, Enter selects, Esc returns to the default card.
7. With OS reduced motion enabled, focus changes are instant (no animation).
8. Error path verified in Task 10; verify again after polish.
9. DevTools Network filtered to "3rd-party" shows nothing; console is clean.
10. Second-card test in Step 4.

- [ ] **Step 4: Temporary second-card test**

Append to `content.yml`:

```yaml
  - id: contact
    title: Contact
    body: |
      The best way to reach me is email.
    links:
      - label: Email
        url: mailto:sobol.daniil@gmail.com
```

Add `{ label: Contact, card: contact }` to the `about` card's links, reload, then verify:

- Two cards appear; an edge line connects them.
- Clicking "Contact" (in the focused card or the background card) animates focus: Contact grows to center, About shrinks/blurs.
- `#contact` is in the URL; browser Back returns to `#about`.
- Contact's body is justified/hyphenated after it becomes focused.

Then remove the second card and internal link: `git checkout content.yml`.

- [ ] **Step 5: Check git state and commit any polish**

```bash
git status --short
git add assets/site.css
git commit -m "style: final responsive and typography polish"
```

If `assets/site.css` was already complete and nothing changed, skip the commit.

- [ ] **Step 6: Confirm the reset is complete**

```bash
git ls-files | grep -v '^docs/' | sort
```

Expected (exactly these, modulo `assets/vendor` contents):

```
.gitignore
CNAME
NOTICE
assets/fonts.css
assets/fonts/gost-type-a-regular.woff
assets/fonts/latin-modern-mono-italic.woff
assets/fonts/latin-modern-mono-regular.woff
assets/fonts/latin-modern-roman-bold.woff
assets/fonts/latin-modern-roman-bolditalic.woff
assets/fonts/latin-modern-roman-italic.woff
assets/fonts/latin-modern-roman-regular.woff
assets/site.css
assets/tailwind.css
assets/tailwind.src.css
assets/vendor/d3.v7.min.js
assets/vendor/js-yaml.min.js
assets/vendor/marked.min.js
assets/vendor/typography.min.js
content.yml
index.html
js/card.js
js/content.js
js/graph.js
js/main.js
js/model.js
js/protrusion.js
js/router.js
js/theme.js
js/typography.js
package.json
package-lock.json
scripts/fetch-vendor.sh
scripts/typography-entry.js
tests/card.test.js
tests/content.test.js
tests/model.test.js
tests/protrusion.test.js
tests/router.test.js
tests/theme.test.js
```

No old site files (`slides/`, `icons/`, `src/`, `elm.json`, `contents.yaml`) may remain.

---

## Final acceptance

All items in the spec's Definition of Done are covered:

- Reset: Tasks 1 and 11 (Step 6).
- `content.yml`-driven content with implicit edges: Tasks 3 and 10.
- Graph with focus animation and blurred background: Tasks 8, 10, 11.
- Typography pipeline (fonts, Knuth-Plass, hyphenation, protrusion): Tasks 2, 9, 11.
- Light/dark themes with no flash and persisted toggle: Tasks 1 and 6.
- Hash deep links, back/forward, Esc, error card: Tasks 5, 10, 11.
- Responsive graph and reduced motion: Tasks 8, 9 (CSS), 11.
- Unit tests via `npm test`: Tasks 3, 4, 5, 6, 7, 9.
