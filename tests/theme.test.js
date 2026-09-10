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
