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
