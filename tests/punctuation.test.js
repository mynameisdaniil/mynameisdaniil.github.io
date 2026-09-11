import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { smartPunctuation, applySmartPunctuation } from "../js/punctuation.js";

test("smartPunctuation converts apostrophes, quotes, dashes, and ellipses", () => {
  assert.equal(smartPunctuation("Can't"), "Can\u2019t");
  assert.equal(smartPunctuation('"quoted"'), "\u201Cquoted\u201D");
  assert.equal(smartPunctuation("wait..."), "wait\u2026");
  assert.equal(smartPunctuation("1--2"), "1\u20132");
  assert.equal(smartPunctuation("a---b"), "a\u2014b");
});

test("applySmartPunctuation rewrites only text nodes", () => {
  const dom = new JSDOM('<p title="Can\'t">Can\'t <a href="https://x--y.example/a">link--text</a></p>');
  globalThis.document = dom.window.document;
  globalThis.NodeFilter = dom.window.NodeFilter;
  const p = dom.window.document.querySelector("p");
  applySmartPunctuation(p);
  assert.equal(p.getAttribute("title"), "Can't");
  assert.match(p.textContent, /Can\u2019t/);
  assert.match(p.textContent, /link\u2013text/);
  assert.equal(p.querySelector("a").getAttribute("href"), "https://x--y.example/a");
});

test("applySmartPunctuation skips code and pre", () => {
  const dom = new JSDOM("<p>don't <code>don't</code></p>");
  globalThis.document = dom.window.document;
  globalThis.NodeFilter = dom.window.NodeFilter;
  const p = dom.window.document.querySelector("p");
  applySmartPunctuation(p);
  assert.match(p.firstChild.nodeValue, /\u2019/);
  assert.equal(p.querySelector("code").textContent, "don't");
});
