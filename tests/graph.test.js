import test from "node:test";
import assert from "node:assert/strict";
import { snapToDevicePixel } from "../js/graph.js";

test("snapToDevicePixel rounds to whole pixels at dpr 1", () => {
  assert.equal(snapToDevicePixel(10.4, 1), 10);
  assert.equal(snapToDevicePixel(10.5, 1), 11);
  assert.equal(snapToDevicePixel(-3.6, 1), -4);
});

test("snapToDevicePixel rounds to the device pixel grid at fractional dpr", () => {
  assert.equal(snapToDevicePixel(10.1, 2), 10);
  assert.equal(snapToDevicePixel(10.3, 2), 10.5);
  assert.equal(snapToDevicePixel(10.2, 1.25), 10.4);
});

test("snapToDevicePixel output always lands on a device pixel boundary", () => {
  for (const dpr of [1, 1.25, 1.5, 2, 3]) {
    for (const value of [0.1, 12.34, 387.629, -56.78, 999.999]) {
      const snapped = snapToDevicePixel(value, dpr);
      assert.equal(Number.isInteger(Math.round(snapped * dpr * 1000) / 1000), true);
      assert.ok(Math.abs(snapped - value) <= 0.5 / dpr + 1e-9);
    }
  }
});
