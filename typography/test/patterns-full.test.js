import test from "node:test";
import assert from "node:assert/strict";
import { patterns } from "../src/index.js";

test("the full entry registers ru patterns alongside en-us", () => {
  assert.ok(patterns["en-us"]);
  assert.ok(patterns.ru);
});
