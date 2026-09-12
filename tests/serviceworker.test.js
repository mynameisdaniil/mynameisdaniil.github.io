import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { cacheVersion } from "../scripts/build-sw.mjs";

const swPath = new URL("../sw.js", import.meta.url);

test("sw.js is stamped with the current cache version", async () => {
  const sw = readFileSync(swPath, "utf8");
  const version = await cacheVersion();
  assert.ok(sw.includes(`const CACHE = "${version}";`));
  assert.equal(sw.includes("__CACHE_VERSION__"), false);
});

test("sw.js precaches the shell assets", () => {
  const sw = readFileSync(swPath, "utf8");
  for (const asset of [
    "/index.html",
    "/assets/site.css",
    "/assets/fonts.css",
    "/assets/app.min.js",
  ]) {
    assert.ok(sw.includes(`"${asset}"`), `precache is missing ${asset}`);
  }
});
