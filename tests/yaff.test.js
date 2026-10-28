import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync, statSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { JSDOM } from "jsdom";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const articleDir = join(root, "habr", "yaff");
const articlePath = join(articleDir, "index.html");
const articleUrl = "https://mynameisdaniil.dev/habr/yaff/";
const siteUrl = "https://mynameisdaniil.dev/";

let cached;

function article() {
  if (!cached) {
    const html = readFileSync(articlePath, "utf8");
    const dom = new JSDOM(html, { url: articleUrl });
    cached = { html, doc: dom.window.document };
  }
  return cached;
}

function prose() {
  const { doc } = article();
  return [...doc.querySelectorAll("main p")]
    .map((element) => element.textContent.replace(/\s+/g, " ").trim())
    .join(" ");
}

function localFile(raw) {
  const url = new URL(raw, articleUrl);
  assert.equal(url.origin, new URL(siteUrl).origin, `${raw} must stay on this site`);
  let path = decodeURIComponent(url.pathname);
  if (path.endsWith("/")) path += "index.html";
  return join(root, path);
}

test("article declares English metadata, canonical URL, and 2015 publication date", () => {
  const { doc } = article();
  assert.equal(doc.documentElement.lang, "en");

  assert.match(doc.title, /node-seq Revisited/);
  const description = doc.querySelector('meta[name="description"]')?.content ?? "";
  assert.ok(description.length >= 80, "description should explain the article");

  assert.equal(doc.querySelector('link[rel="canonical"]')?.href, articleUrl);
  assert.equal(doc.querySelector('meta[name="author"]')?.content, "Daniil Sobol");
  assert.equal(doc.querySelector('meta[property="og:type"]')?.content, "article");
  assert.equal(doc.querySelector('meta[property="og:url"]')?.content, articleUrl);
  assert.match(doc.querySelector('meta[property="og:title"]')?.content ?? "", /node-seq Revisited/);
  assert.ok(doc.querySelector('meta[property="og:description"]')?.content);
  assert.equal(
    doc.querySelector('meta[property="article:published_time"]')?.content,
    "2015-02-24",
  );
});

test("article is a static semantic shell with home navigation and source attribution", () => {
  const { doc } = article();

  assert.equal(doc.querySelector(".skip-link")?.getAttribute("href"), "#paper");
  const main = doc.querySelector("main");
  assert.ok(main, "article needs a main landmark");
  assert.equal(main.id, "paper");
  assert.ok(main.classList.contains("paper"));
  assert.ok(main.classList.contains("paper--article"));
  assert.equal(main.getAttribute("aria-labelledby"), "article-title");

  const headings = doc.querySelectorAll("h1");
  assert.equal(headings.length, 1);
  assert.equal(headings[0].id, "article-title");
  assert.equal(
    headings[0].textContent.trim(),
    "node-seq Revisited (About Asynchrony, Again)",
  );

  const homeLinks = [...doc.querySelectorAll("main, header")].flatMap((scope) => [
    ...scope.querySelectorAll('a[href="/"]'),
  ]);
  assert.ok(homeLinks.length >= 1, "article must link back home");

  assert.match(main.textContent, /Daniil Sobol/);
  const published = main.querySelector("time");
  assert.equal(published?.getAttribute("datetime"), "2015-02-24");
  assert.match(published.textContent, /24 February 2015/);
  assert.ok(
    main.querySelector('a[href="https://habr.com/ru/post/251307/"]'),
    "article must link to the Russian original",
  );

  assert.equal(doc.querySelector('script[src="/assets/app.min.js"]'), null);
  assert.equal(doc.querySelector('script[type="module"]')?.getAttribute("src"), "/js/article.js");
  assert.equal(doc.querySelector('link[rel="stylesheet"][href="/assets/site.css"]') !== null, true);
});

test("article preserves all seven code examples, including corrected syntax", () => {
  const { doc } = article();
  const expected = [
    "var fs = require('fs');\nvar Hash = require('hashish');\nvar Seq = require('seq');\n\nSeq()\n  .seq(function () {\n    fs.readdir(__dirname, this);\n  })\n  .flatten()\n  .parEach(function (file) {\n    fs.stat(__dirname + '/' + file, this.into(file));\n  })\n  .seq(function () {\n    var sizes = Hash.map(this.vars, function (s) { return s.size })\n    console.dir(sizes);\n  });",
    "var fs = require('fs');\nvar Hash = require('hashish');\nvar Seq = require('seq');\n\nSeq()\n  .seq(function () {\n    fs.readdir(__dirname, this);\n  })\n  .flatten()\n  .parEach(function (file) {\n    fs.stat(__dirname + '/' + file, this.into(file));\n  })\n  .catch(function (err) {\n    console.error(err);\n  })\n  .seq(function () {\n    var sizes = Hash.map(this.vars, function (s) { return s.size })\n    console.dir(sizes);\n  });",
    "var fs = require('fs');\nYAFF(['./', '../'])\n  .par(function (path) {\n    fs.readdir(path, this);\n  })\n  .par(function (path) {\n    fs.readdir(path, this);\n  })\n  .flatten()\n  .parMap(function (file) {\n    fs.stat(__dirname + '/' + file, this);\n  })\n  .map(function (stat) {\n    return stat.size;\n  })\n  .unflatten()\n  .finally(function (e, sizes) {\n    if (e)\n      throw e;\n    log(sizes);\n  });",
    "var listDirs = function (dirs, cb) {\n  YAFF(dirs)\n    // magic bubbles\n    .finally(cb);\n};",
    "YAFF(['one', 'two', 'three'])\n  .par(function (one) {\n    this(null, one);\n  })\n  .par(function (two) {\n    this(null, two);\n  })\n  .par(function (three) {\n    this(null, three);\n  })\n  .seq(function (one, two, three) {\n    this(null, one, three);\n  })\n  .seq(function (one, three) {\n    this(null, null);\n  })\n  .seq(function () {\n    this(null, 'and so on and so forth');\n  })",
    "var resizePhotos = function (photos, cb) {\n  YAFF(photos)\n    .parMap(function (photo) {\n      asyncResize(photo.image, photo.params, this);\n    }, 10)\n    .unflatten()\n    .finally(cb);\n};",
    "YAFF(['./'])\n  .mseq([\n    function (path1) {\n      fs.readdir(path1, this);\n    },\n    function (arg) {\n      this(null, arg);\n    }\n  ])\n  .flatten()\n  .parMap(function (file) {\n    fs.stat(__dirname + '/' + file, this);\n  })\n  .map(function (stat) {\n    return stat.size;\n  })\n  .unflatten()\n  .finally(function (e, sizes) {\n    log(sizes);\n  });",
  ];

  const blocks = [...doc.querySelectorAll("main pre")];
  assert.equal(blocks.length, expected.length);
  for (const pre of blocks) {
    assert.equal(pre.children.length, 1, "each code example is one block");
    assert.equal(pre.firstElementChild.tagName, "CODE");
    assert.equal(pre.querySelector("img"), null, "code must not be faked as a screenshot");
  }
  assert.deepEqual(
    blocks.map((pre) => pre.firstElementChild.textContent),
    expected,
  );
  assert.equal(/[\u0400-\u04FF]/.test(blocks.map((pre) => pre.textContent).join("")), false);
});

test("article keeps one local English diagram of the argument stack", () => {
  const { doc, html } = article();
  const figures = [...doc.querySelectorAll("main figure")];
  assert.equal(figures.length, 1);
  const image = figures[0].querySelector("img");
  assert.ok(image, "the diagram needs an image");
  assert.equal(new URL(image.getAttribute("src"), articleUrl).pathname, "/habr/yaff/flow.svg");
  assert.equal(image.getAttribute("width"), "1000");
  assert.equal(image.getAttribute("height"), "1280");

  const alt = image.getAttribute("alt") ?? "";
  assert.match(alt, /argument stack/i);
  assert.ok(alt.trim().split(/\s+/).length >= 8, `alt text is too terse: ${alt}`);
  assert.equal(/[\u0400-\u04FF]/.test(alt), false, `alt text must be English: ${alt}`);

  const svgPath = join(articleDir, "flow.svg");
  assert.ok(statSync(svgPath).isFile(), "flow.svg must be a local asset");
  const svg = readFileSync(svgPath, "utf8");
  assert.equal(/[\u0400-\u04FF]/.test(svg), false, "the diagram must not contain Cyrillic");
  assert.match(svg, /<title[^>]*>[^<]*argument stack/i);
  assert.match(svg, /PROGRAM/);
  assert.match(svg, /ARGUMENT STACK/);
  assert.match(svg, /WHAT&#8217;S HAPPENING|WHAT'S HAPPENING/);
  assert.match(svg, /\.unflatten\(\)/);
  assert.match(svg, /\.finally\(/);

  const caption = figures[0].querySelector("figcaption");
  assert.ok(caption, "the diagram needs a caption");
  assert.equal(caption.querySelector('a[href="flow.svg"]') !== null, true);
  assert.equal(/habrastorage/.test(html), false, "the diagram must be served locally");
});

test("all local article references resolve from the nested URL", () => {
  const { doc } = article();
  const references = [
    ...doc.querySelectorAll("main a[href], main img[src]"),
    ...doc.querySelectorAll("link[href], script[src]"),
  ];

  for (const element of references) {
    const raw = element.getAttribute("href") ?? element.getAttribute("src");
    if (!raw || raw.startsWith("#") || raw.startsWith("http") || raw.startsWith("mailto:")) {
      continue;
    }
    const file = localFile(raw);
    assert.ok(statSync(file).isFile(), `${raw} resolves to a missing file: ${file}`);
  }
});

test("translation covers the whole article in English", () => {
  const { html } = article();
  const text = prose();

  assert.equal(/[\u0400-\u04FF]/.test(html), false, "no untranslated Russian may remain");
  assert.ok(text.split(/\s+/).filter(Boolean).length > 900, "the article must not be summarized");

  for (const marker of [
    /Yet Another Flow Framework/,
    /late to the party/i,
    /Tesla Roadster/,
    /Fatal Flaw/,
    /Library of Dreams/,
    /argument stack/i,
    /curtsy/i,
    /npmjs\.org/,
    /fit of passion/i,
  ]) {
    assert.match(text, marker);
  }

  for (const href of [
    "https://github.com/substack/node-seq",
    "https://github.com/mynameisdaniil/yaff/blob/master/main.js",
    "https://github.com/mynameisdaniil/yaff/issues",
    "https://github.com/mynameisdaniil/yaff/pulls",
    "https://www.npmjs.com/package/yaff",
  ]) {
    assert.ok(
      article().doc.querySelector(`main a[href="${href}"]`),
      `article must keep the ${href} link`,
    );
  }
});

test("sitemap lists the article canonical URL", () => {
  const xml = readFileSync(join(root, "sitemap.xml"), "utf8");
  assert.match(xml, /<loc>https:\/\/mynameisdaniil\.dev\/habr\/yaff\/<\/loc>/);
});
