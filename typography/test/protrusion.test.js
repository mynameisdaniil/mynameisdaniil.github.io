import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { protrusionFor, applyProtrusion, clearProtrusion } from "../src/protrusion.js";

test("protrusionFor knows left and right punctuation", () => {
  assert.equal(protrusionFor(".", "right"), 0.15);
  assert.equal(protrusionFor(",", "right"), 0.1);
  assert.equal(protrusionFor("-", "right"), 0.2);
  assert.equal(protrusionFor("-", "left"), 0.2);
  assert.equal(protrusionFor("«", "left"), 0.15);
  assert.equal(protrusionFor("»", "right"), 0.15);
  assert.equal(protrusionFor("a", "right"), 0);
  assert.equal(protrusionFor(".", "left"), 0);
  assert.equal(protrusionFor("\u201C", "left"), 0.15);
  assert.equal(protrusionFor("\u201D", "right"), 0.15);
  assert.equal(protrusionFor("\u2014", "right"), 0.15);
  assert.equal(protrusionFor("\u2013", "right"), 0.1);
  assert.equal(protrusionFor("\u2026", "right"), 0.15);
});

function makeParagraph() {
  const dom = new JSDOM('<p>"Hello, world.<br>Second line!</p>');
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
  assert.equal(left.textContent, '"');
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

test("protrudes punctuation after inline whitespace across element boundaries", () => {
  const dom = new JSDOM("<p>Hello.<em> </em><br>Second.</p>");
  globalThis.document = dom.window.document;
  globalThis.Node = dom.window.Node;
  globalThis.NodeFilter = dom.window.NodeFilter;
  const p = dom.window.document.querySelector("p");
  applyProtrusion(p);
  const dots = [...p.querySelectorAll(".protrude-right")].filter((s) => s.textContent === ".");
  assert.equal(dots.length, 2);
  assert.equal(dots[0].style.marginRight, "-0.15em");
});

test("protrudes a left character inside an inline element", () => {
  const dom = new JSDOM('<p><em>"Hello</em> world.</p>');
  globalThis.document = dom.window.document;
  globalThis.Node = dom.window.Node;
  globalThis.NodeFilter = dom.window.NodeFilter;
  const p = dom.window.document.querySelector("p");
  applyProtrusion(p);
  assert.equal(p.querySelector(".protrude-left").textContent, '"');
  assert.equal(p.querySelector(".protrude-left").style.marginLeft, "-0.1em");
});

test("protrusionFor applies per-side overrides", () => {
  assert.equal(protrusionFor(".", "right", { right: { ".": 0.05 } }), 0.05);
  assert.equal(protrusionFor(".", "right", { left: { ".": 0.05 } }), 0.15);
  assert.equal(protrusionFor(".", "right"), 0.15);
});
