# PostHog Cookieless Analytics Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add cookieless PostHog analytics (EU) to the site: pageviews, referrers, outbound-link clicks, scroll depth, and click/scroll heatmaps — no cookies, no banner, no server.

**Architecture:** A single `js/analytics.js` module, bundled into `assets/app.min.js`, loads the PostHog SDK from the EU CDN after `window.load` and only on the production hostname. It initializes PostHog with `cookieless_mode: "always"` and wires delegated click and scroll listeners. Events are delivered through an injected loader so everything is testable in jsdom. `sw.js` is restamped because the app shell hash changes.

**Tech Stack:** ES modules bundled by esbuild, `node:test` + jsdom, PostHog `posthog-js` loaded from `https://eu-assets.i.posthog.com` (not an npm dependency).

**Spec:** `docs/superpowers/specs/2026-09-13-posthog-cookieless-analytics-design.md`

## Global Constraints

- No code comments unless explicitly requested.
- No new npm dependencies; `jsdom`, `esbuild`, and `node:test` are already available. PostHog is loaded at runtime, never bundled.
- Commit style: imperative subject with scope prefix (`feat:`, `fix:`, `build:`, …).
- Run `npm test` before every commit; run `npm run check` after any commit that rebuilds artifacts.
- Generated artifacts (`assets/app.min.js`, `sw.js`) are committed in the same commit as their source change. Never edit `sw.js` by hand — it is stamped by `npm run build:sw`.
- Analytics must no-op on any hostname other than `mynameisdaniil.dev` and while `POSTHOG_KEY` is empty.
- No cookies, no storage, no consent UI.

---

### Task 1: Analytics module — guards, loader, and init config

**Files:**
- Create: `js/analytics.js`
- Test: `tests/analytics.test.js`

**Interfaces:**
- Consumes: nothing.
- Produces: `initAnalytics(deps) → Promise<void>`; deps: `{ win, doc, key, load, schedule, whenReady, scriptUrl }`. `load(win, doc, url) → Promise<void>`; `schedule(fn) → void`; `whenReady(fn) → void` (calls `fn` immediately when `doc.readyState === "complete"`, else once on `load`). Also exports nothing else. Module constants: `POSTHOG_KEY = ""`, `POSTHOG_SCRIPT`, `POSTHOG_HOST`, `PRODUCTION_HOST`, `POSTHOG_CONFIG`.

- [ ] **Step 1: Write the failing tests**

Create `tests/analytics.test.js`:

```js
import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { initAnalytics } from "../js/analytics.js";

const HTML = `<!doctype html><html><body>
  <header class="masthead"><a href="https://t.me/mynameisdaniil">Telegram</a></header>
  <main><section id="talks"><a href="/slides/fscala25/">Deck</a></section></main>
</body></html>`;

function harness({ hostname = "mynameisdaniil.dev", html = HTML } = {}) {
  const dom = new JSDOM(html, { url: `https://${hostname}/` });
  const calls = { load: [], init: [], capture: [] };
  const load = (win, doc, url) => {
    calls.load.push(url);
    win.posthog = {
      init: (...args) => calls.init.push(args),
      capture: (...args) => calls.capture.push(args),
    };
    return Promise.resolve();
  };
  const deps = {
    win: dom.window,
    doc: dom.window.document,
    key: "phc_test",
    load,
    schedule: (fn) => fn(),
    whenReady: (fn) => fn(),
  };
  return { dom, calls, deps };
}

async function run(options) {
  const h = harness(options);
  await initAnalytics(h.deps);
  return h;
}

test("does nothing outside the production hostname", async () => {
  const h = await run({ hostname: "localhost" });
  assert.equal(h.calls.load.length, 0);
});

test("does nothing without a project key", async () => {
  const h = harness();
  h.deps.key = "";
  await initAnalytics(h.deps);
  assert.equal(h.calls.load.length, 0);
});

test("waits for window load before loading the SDK", async () => {
  const h = harness();
  Object.defineProperty(h.deps.doc, "readyState", { value: "loading", configurable: true });
  delete h.deps.whenReady;
  const pending = initAnalytics(h.deps);
  assert.equal(h.calls.load.length, 0);
  h.dom.window.dispatchEvent(new h.dom.window.Event("load"));
  await pending;
  assert.deepEqual(h.calls.load, ["https://eu-assets.i.posthog.com/static/array.js"]);
});

test("loads the EU script once and initializes the cookieless config", async () => {
  const h = await run();
  assert.deepEqual(h.calls.load, ["https://eu-assets.i.posthog.com/static/array.js"]);
  assert.equal(h.calls.init.length, 1);
  const [key, config] = h.calls.init[0];
  assert.equal(key, "phc_test");
  assert.equal(config.api_host, "https://eu.i.posthog.com");
  assert.equal(config.cookieless_mode, "always");
  assert.equal(config.enable_heatmaps, true);
  assert.equal(config.autocapture, true);
  assert.equal(config.capture_pageleave, true);
  assert.equal(config.disable_session_recording, true);
  assert.equal(config.disable_surveys, true);
  assert.equal(config.respect_dnt, true);
});

test("suppresses loader failures", async () => {
  const h = harness();
  h.deps.load = () => Promise.reject(new Error("blocked"));
  await initAnalytics(h.deps);
  assert.equal(h.calls.init.length, 0);
});
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `node --test tests/analytics.test.js`
Expected: FAIL — `Cannot find module '../js/analytics.js'`.

- [ ] **Step 3: Write the minimal implementation**

Create `js/analytics.js`:

```js
const POSTHOG_KEY = "";
const POSTHOG_SCRIPT = "https://eu-assets.i.posthog.com/static/array.js";
const POSTHOG_HOST = "https://eu.i.posthog.com";
const PRODUCTION_HOST = "mynameisdaniil.dev";

const POSTHOG_CONFIG = {
  api_host: POSTHOG_HOST,
  cookieless_mode: "always",
  enable_heatmaps: true,
  autocapture: true,
  capture_pageview: true,
  capture_pageleave: true,
  disable_session_recording: true,
  disable_surveys: true,
  respect_dnt: true,
};

function defaultSchedule(win) {
  return (fn) => {
    if (typeof win.requestIdleCallback === "function") {
      win.requestIdleCallback(fn, { timeout: 3000 });
    } else {
      win.setTimeout(fn, 1200);
    }
  };
}

function defaultWhenReady(win, doc) {
  return (fn) => {
    if (doc.readyState === "complete") {
      fn();
    } else {
      win.addEventListener("load", fn, { once: true });
    }
  };
}

function loadScript(win, doc, url) {
  return new Promise((resolve, reject) => {
    const script = doc.createElement("script");
    script.async = true;
    script.crossOrigin = "anonymous";
    script.src = url;
    script.addEventListener("load", () => resolve());
    script.addEventListener("error", () => reject(new Error(`failed to load ${url}`)));
    doc.head.appendChild(script);
  });
}

export async function initAnalytics(deps = {}) {
  const win = deps.win ?? globalThis.window;
  const doc = deps.doc ?? globalThis.document;
  if (!win || !doc || win.location.hostname !== PRODUCTION_HOST) return;
  const key = deps.key ?? POSTHOG_KEY;
  if (!key) return;
  const schedule = deps.schedule ?? defaultSchedule(win);
  const whenReady = deps.whenReady ?? defaultWhenReady(win, doc);
  const load = deps.load ?? loadScript;
  const scriptUrl = deps.scriptUrl ?? POSTHOG_SCRIPT;

  await new Promise((resolve) => {
    whenReady(() => {
      schedule(() => {
        Promise.resolve()
          .then(() => load(win, doc, scriptUrl))
          .then(() => {
            win.posthog.init(key, POSTHOG_CONFIG);
          })
          .catch(() => {})
          .then(resolve);
      });
    });
  });
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `node --test tests/analytics.test.js`
Expected: PASS — 5 tests.

- [ ] **Step 5: Commit**

```bash
git add js/analytics.js tests/analytics.test.js
git commit -m "feat: add cookieless analytics module"
```

---

### Task 2: Outbound-link click events

**Files:**
- Modify: `js/analytics.js`
- Test: `tests/analytics.test.js`

**Interfaces:**
- Consumes: `initAnalytics(deps)` from Task 1.
- Produces: `wireClicks(doc, posthog) → void` (internal); emits `posthog.capture("outbound_click", { href, label, section })`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/analytics.test.js`:

```js
function click(win, el) {
  el.dispatchEvent(new win.MouseEvent("click", { bubbles: true }));
}

test("reports outbound clicks with href, label, and section", async () => {
  const h = await run();
  click(h.dom.window, h.dom.window.document.querySelector("#talks a"));
  assert.deepEqual(h.calls.capture, [
    ["outbound_click", { href: "/slides/fscala25/", label: "Deck", section: "talks" }],
  ]);
});

test("reports masthead links with section masthead", async () => {
  const h = await run();
  click(h.dom.window, h.dom.window.document.querySelector(".masthead a"));
  assert.equal(h.calls.capture.length, 1);
  assert.equal(h.calls.capture[0][1].section, "masthead");
});

test("ignores in-page fragment links and non-anchor clicks", async () => {
  const h = await run({
    html: `<!doctype html><html><body>
      <a class="skip" href="#paper">Skip</a>
      <p id="text">hello</p>
    </body></html>`,
  });
  const { window } = h.dom;
  click(window, window.document.querySelector(".skip"));
  click(window, window.document.querySelector("#text"));
  assert.equal(h.calls.capture.length, 0);
});

test("caps the label at 200 characters", async () => {
  const long = "x".repeat(250);
  const h = await run({
    html: `<!doctype html><html><body><section id="writing"><a href="https://a.example">${long}</a></section></body></html>`,
  });
  click(h.dom.window, h.dom.window.document.querySelector("#writing a"));
  assert.equal(h.calls.capture[0][1].label.length, 200);
});
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `node --test tests/analytics.test.js`
Expected: FAIL — the four new tests get `calls.capture` length 0 instead of 1 (and the deepEqual mismatch).

- [ ] **Step 3: Add the click wiring**

In `js/analytics.js`, add after `POSTHOG_CONFIG`:

```js
const LABEL_LIMIT = 200;

function sectionFor(anchor) {
  const section = anchor.closest("section[id]");
  return section ? section.id : "masthead";
}

function wireClicks(doc, posthog) {
  doc.addEventListener("click", (event) => {
    const anchor = event.target.closest?.("a[href]");
    if (!anchor) return;
    const href = anchor.getAttribute("href");
    if (!href || href.startsWith("#")) return;
    const label = anchor.textContent.trim().slice(0, LABEL_LIMIT);
    posthog.capture("outbound_click", { href, label, section: sectionFor(anchor) });
  });
}
```

Then in `initAnalytics`, inside the `.then(() => { ... })` block, call it after init:

```js
        .then(() => {
          win.posthog.init(key, POSTHOG_CONFIG);
          wireClicks(doc, win.posthog);
        })
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `node --test tests/analytics.test.js`
Expected: PASS — 9 tests.

- [ ] **Step 5: Commit**

```bash
git add js/analytics.js tests/analytics.test.js
git commit -m "feat: track outbound link clicks"
```

---

### Task 3: Scroll-depth events

**Files:**
- Modify: `js/analytics.js`
- Test: `tests/analytics.test.js`

**Interfaces:**
- Consumes: `initAnalytics(deps)` from Task 1; deps gains `frame(fn) → void`.
- Produces: `wireScroll(win, doc, posthog, frame) → void` (internal); emits `posthog.capture("scroll_depth", { percent })` once per threshold at 25/50/75/100.

- [ ] **Step 1: Write the failing test**

Append to `tests/analytics.test.js`:

```js
test("reports scroll depth once per threshold", async () => {
  const h = await run();
  const { window } = h.dom;
  Object.defineProperty(window, "innerHeight", { value: 800, configurable: true });
  Object.defineProperty(window, "scrollY", { value: 0, writable: true, configurable: true });
  Object.defineProperty(window.document.documentElement, "scrollHeight", {
    value: 4000,
    configurable: true,
  });
  const scroll = (y) => {
    window.scrollY = y;
    window.dispatchEvent(new window.Event("scroll"));
  };
  scroll(800);
  scroll(800);
  scroll(1600);
  scroll(2600);
  scroll(3400);
  assert.deepEqual(h.calls.capture, [
    ["scroll_depth", { percent: 25 }],
    ["scroll_depth", { percent: 50 }],
    ["scroll_depth", { percent: 75 }],
    ["scroll_depth", { percent: 100 }],
  ]);
});
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `node --test tests/analytics.test.js`
Expected: FAIL — `calls.capture` is empty, deepEqual mismatch.

- [ ] **Step 3: Add the scroll wiring**

In `js/analytics.js`, add after `sectionFor`:

```js
const SCROLL_THRESHOLDS = [25, 50, 75, 100];

function defaultFrame(win) {
  return (fn) => {
    if (typeof win.requestAnimationFrame === "function") {
      win.requestAnimationFrame(fn);
    } else {
      fn();
    }
  };
}

function wireScroll(win, doc, posthog, frame) {
  const fired = new Set();
  let ticking = false;
  const emit = () => {
    ticking = false;
    const percent =
      ((win.scrollY + win.innerHeight) / doc.documentElement.scrollHeight) * 100;
    for (const threshold of SCROLL_THRESHOLDS) {
      if (percent >= threshold && !fired.has(threshold)) {
        fired.add(threshold);
        posthog.capture("scroll_depth", { percent: threshold });
      }
    }
  };
  win.addEventListener(
    "scroll",
    () => {
      if (ticking) return;
      ticking = true;
      frame(emit);
    },
    { passive: true },
  );
}
```

Then in `initAnalytics`, add the `frame` dep next to `schedule`:

```js
  const frame = deps.frame ?? defaultFrame(win);
```

and call the wiring after `wireClicks`:

```js
          wireClicks(doc, win.posthog);
          wireScroll(win, doc, win.posthog, frame);
```

Finally, in the test harness, add `frame: (fn) => fn(),` to the `deps` object so the scroll test is synchronous:

```js
  const deps = {
    win: dom.window,
    doc: dom.window.document,
    key: "phc_test",
    load,
    schedule: (fn) => fn(),
    frame: (fn) => fn(),
  };
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `node --test tests/analytics.test.js`
Expected: PASS — 10 tests.

- [ ] **Step 5: Commit**

```bash
git add js/analytics.js tests/analytics.test.js
git commit -m "feat: track scroll depth"
```

---

### Task 4: Wire analytics into the site and rebuild artifacts

**Files:**
- Modify: `js/main.js`
- Modify: `README.md`
- Rebuild: `assets/app.min.js`, `sw.js`

**Interfaces:**
- Consumes: `initAnalytics()` from Tasks 1–3.
- Produces: analytics active on production once `POSTHOG_KEY` is set.

- [ ] **Step 1: Call analytics from boot**

In `js/main.js`, add the import after the typography import:

```js
import { initAnalytics } from "./analytics.js";
```

and after `boot();` add:

```js
initAnalytics();
```

- [ ] **Step 2: Document the setup in README**

Add a section at the end of `README.md`:

```markdown
## Analytics

The site uses PostHog Cloud (EU) in cookieless mode: pageviews, referrers,
outbound clicks, scroll depth, and heatmaps — no cookies and no consent banner.
`js/analytics.js` no-ops unless `POSTHOG_KEY` is set and the hostname is
`mynameisdaniil.dev`.

To enable it: create an EU PostHog project, enable **Cookieless** and
**Heatmaps** in project settings, put the `phc_…` key in `POSTHOG_KEY`, then run
`npm run build:app && npm run build:sw` and commit the rebuilt artifacts.
```

- [ ] **Step 3: Rebuild the app bundle and service worker**

Run: `npm run build:app && npm run build:sw`
Expected: `assets/app.min.js` changes size; `sw.js stamped: daniil-site-<newhash>`.

- [ ] **Step 4: Run the full test suite**

Run: `npm test`
Expected: PASS — 49 tests (39 existing + 10 analytics).

- [ ] **Step 5: Local smoke check (analytics must stay inert off-production)**

Run:

```bash
python3 -m http.server 8000 >/dev/null 2>&1 &
sleep 1
chromium --headless=new --no-sandbox --disable-dev-shm-usage \
  --user-data-dir="$HOME/snap/chromium/common/analytics-smoke" \
  --virtual-time-budget=8000 --dump-dom http://localhost:8000/ | grep -c posthog
```

Expected: `0` (no PostHog script injected on localhost). Then stop the server:

```bash
pkill -f "http.server 8000"
```

- [ ] **Step 6: Commit source and artifacts together**

```bash
git add js/main.js README.md assets/app.min.js sw.js
git commit -m "feat: enable cookieless analytics on the site"
```

- [ ] **Step 7: Verify generated artifacts are fresh**

Run: `npm run check`
Expected: exit 0 (no diff in rebuilt artifacts).

---

## Post-Plan Manual Steps (user)

1. Create a PostHog account at `eu.posthog.com` and a project for the site.
2. In project settings enable **Cookieless** and **Heatmaps**.
3. Put the project API key (`phc_…`) into `POSTHOG_KEY` in `js/analytics.js`.
4. `npm run build:app && npm run build:sw`, commit, push.
5. On the live site, confirm events arrive and `document.cookie` stays empty.
