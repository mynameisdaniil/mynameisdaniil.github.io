import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { typeset, unjustify } from "../src/index.js";

function setup() {
  const dom = new JSDOM(
    `<div id="root"><p>don't stop</p><p>"quoted" text...</p></div>`,
    { url: "http://localhost/" },
  );
  globalThis.document = dom.window.document;
  globalThis.window = dom.window;
  globalThis.Node = dom.window.Node;
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

test("destroy removes the resize listener", () => {
  const dom = setup();
  const root = dom.window.document.getElementById("root");
  const added = [];
  const removed = [];
  const originalAdd = dom.window.addEventListener.bind(dom.window);
  const originalRemove = dom.window.removeEventListener.bind(dom.window);
  dom.window.addEventListener = (type, fn, opts) => {
    if (type === "resize") added.push(fn);
    return originalAdd(type, fn, opts);
  };
  dom.window.removeEventListener = (type, fn, opts) => {
    if (type === "resize") removed.push(fn);
    return originalRemove(type, fn, opts);
  };
  const handle = typeset(root, { justify: false, awaitFonts: false, onResize: true });
  handle.destroy();
  assert.equal(added.length, 1);
  assert.equal(removed[0], added[0]);
});

test("typeset still runs when font readiness rejects", async () => {
  const dom = setup();
  const root = dom.window.document.getElementById("root");
  Object.defineProperty(dom.window.document, "fonts", {
    value: { ready: Promise.reject(new Error("no fonts")) },
    configurable: true,
  });
  typeset(root, { justify: false, awaitFonts: true, onResize: false });
  await Promise.resolve();
  await Promise.resolve();
  assert.match(root.textContent, /don\u2019t/);
});

test("injectStyles adds the protrusion rule only once", () => {
  const dom = setup();
  const root = dom.window.document.getElementById("root");
  typeset(root, { justify: false, awaitFonts: false, onResize: false, injectStyles: true });
  typeset(root, { justify: false, awaitFonts: false, onResize: false, injectStyles: true });
  assert.equal(dom.window.document.querySelectorAll("style[data-typeset]").length, 1);
});

test("typeset passes protrusion overrides through", () => {
  const dom = setup();
  const root = dom.window.document.getElementById("root");
  typeset(root, {
    justify: false,
    awaitFonts: false,
    onResize: false,
    protrusion: { right: { "\u2026": 0.05 } },
  });
  const ellipsis = [...root.querySelectorAll(".protrude-right")].find(
    (span) => span.textContent === "\u2026",
  );
  assert.ok(ellipsis);
  assert.equal(ellipsis.style.marginRight, "-0.05em");
});

test("unjustify resets the inline nowrap set by justification", () => {
  const dom = setup();
  const p = dom.window.document.querySelector("p");
  p.style.whiteSpace = "nowrap";
  unjustify(p);
  assert.equal(p.style.whiteSpace, "");
});

test("unjustify accepts an array of elements", () => {
  const dom = setup();
  const paragraphs = [...dom.window.document.querySelectorAll("p")];
  for (const p of paragraphs) p.style.whiteSpace = "nowrap";
  unjustify(paragraphs);
  for (const p of paragraphs) assert.equal(p.style.whiteSpace, "");
});
