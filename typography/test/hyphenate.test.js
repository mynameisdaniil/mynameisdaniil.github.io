import test from "node:test";
import assert from "node:assert/strict";
import { patterns, hyphenateFor, registerPatterns } from "../src/hyphenate.js";

test("the base module ships only en-us patterns and falls back", () => {
  assert.deepEqual(Object.keys(patterns), ["en-us"]);
  assert.deepEqual(hyphenateFor("ru")("hyphenation"), hyphenateFor("en-us")("hyphenation"));
});

test("registerPatterns adds languages", () => {
  const fake = { id: "xx", leftmin: 2, rightmin: 2, patterns: { a: "a1a" } };
  registerPatterns({ xx: fake });
  assert.equal(patterns.xx, fake);
});
