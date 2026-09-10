import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { parseHash, createRouter } from "../js/router.js";

const ids = new Set(["about", "contact"]);

function sleep(win) {
  return new Promise((resolve) => win.setTimeout(resolve, 20));
}

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
  await sleep(dom.window);
  assert.equal(router.current, "contact");
  assert.deepEqual(focused, ["contact"]);
});

test("focus sets the hash and reports synchronously, deduping the echo", async () => {
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
  assert.equal(router.current, "contact");
  assert.deepEqual(focused, ["contact"]);

  await sleep(dom.window);
  assert.deepEqual(focused, ["contact"]);
});

test("focusing the same card twice reports once", async () => {
  const dom = new JSDOM("<html></html>", { url: "http://localhost/#about" });
  const focused = [];
  const router = createRouter({
    defaultCard: "about",
    cardIds: ids,
    onFocus: (id) => focused.push(id),
    win: dom.window,
  });

  router.focus("contact");
  router.focus("contact");
  assert.deepEqual(focused, ["contact"]);
  await sleep(dom.window);
  assert.deepEqual(focused, ["contact"]);
});

test("focus falls back to the default for unknown ids", async () => {
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
  await sleep(dom.window);
  assert.deepEqual(focused, ["about"]);
});

test("Escape focuses the default card", async () => {
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
  await sleep(dom.window);
  assert.deepEqual(focused, ["about"]);
});

test("rapid focus calls settle on the last card without duplicate reports", async () => {
  const dom = new JSDOM("<html></html>", { url: "http://localhost/#about" });
  const focused = [];
  const router = createRouter({
    defaultCard: "about",
    cardIds: ids,
    onFocus: (id) => focused.push(id),
    win: dom.window,
  });

  router.focus("contact");
  router.focus("about");
  assert.deepEqual(focused, ["contact", "about"]);
  assert.equal(router.current, "about");
  await sleep(dom.window);
  assert.deepEqual(focused, ["contact", "about"]);
});

test("an external hash change during a pending echo is not swallowed", async () => {
  const dom = new JSDOM("<html></html>", { url: "http://localhost/#about" });
  const focused = [];
  const router = createRouter({
    defaultCard: "about",
    cardIds: ids,
    onFocus: (id) => focused.push(id),
    win: dom.window,
  });

  router.focus("contact");
  dom.window.location.hash = "#about";
  await sleep(dom.window);
  assert.equal(router.current, "about");
  assert.deepEqual(focused, ["contact", "about"]);

  dom.window.location.hash = "#contact";
  await sleep(dom.window);
  assert.equal(router.current, "contact");
  assert.deepEqual(focused, ["contact", "about", "contact"]);
});
