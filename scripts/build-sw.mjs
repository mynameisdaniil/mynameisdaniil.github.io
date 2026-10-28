import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");

const SHELL_ASSETS = [
  "index.html",
  "assets/fonts.css",
  "assets/site.css",
  "assets/app.min.js",
  "habr/habradude/index.html",
  "habr/yaff/index.html",
  "js/article.js",
  "js/analytics.js",
  "js/print-lifecycle.js",
  "typography/dist/typeset-en.esm.js",
];

export { SHELL_ASSETS };

export async function cacheVersion() {
  const hash = createHash("sha256");
  for (const asset of SHELL_ASSETS) {
    hash.update(asset);
    hash.update(await readFile(join(root, asset)));
  }
  return `daniil-site-${hash.digest("hex").slice(0, 8)}`;
}

export async function buildServiceWorker() {
  const template = await readFile(join(root, "scripts", "sw.template.js"), "utf8");
  if (!template.includes("__CACHE_VERSION__")) {
    throw new Error("sw.template.js is missing the __CACHE_VERSION__ placeholder");
  }
  const version = await cacheVersion();
  await writeFile(join(root, "sw.js"), template.replace("__CACHE_VERSION__", version));
  return version;
}

if (import.meta.url === pathToFileURL(process.argv[1]).href) {
  buildServiceWorker().then((version) => console.log(`sw.js stamped: ${version}`));
}
