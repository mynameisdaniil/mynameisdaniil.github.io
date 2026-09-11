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

test("destroy removes the resize listener without throwing", () => {
  const dom = setup();
  const root = dom.window.document.getElementById("root");
  const handle = typeset(root, { justify: false, awaitFonts: false, onResize: true });
  handle.destroy();
  dom.window.dispatchEvent(new dom.window.Event("resize"));
  assert.ok(true);
});
