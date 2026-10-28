import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync, statSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { JSDOM } from "jsdom";
import yaml from "js-yaml";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const articleDir = join(root, "habr", "habradude");
const articlePath = join(articleDir, "index.html");
const articleUrl = "https://mynameisdaniil.dev/habr/habradude/";
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

test("article declares English document metadata, canonical URL, and publication date", () => {
  const { doc } = article();
  assert.equal(doc.documentElement.lang, "en");

  assert.match(doc.title, /Haskell for Absolute Beginners: A Codefights Walkthrough/);
  const description = doc.querySelector('meta[name="description"]')?.content ?? "";
  assert.ok(description.length >= 80, "description should explain the article");

  assert.equal(doc.querySelector('link[rel="canonical"]')?.href, articleUrl);
  assert.equal(doc.querySelector('meta[name="author"]')?.content, "Daniil Sobol");
  assert.equal(doc.querySelector('meta[property="og:type"]')?.content, "article");
  assert.equal(doc.querySelector('meta[property="og:url"]')?.content, articleUrl);
  assert.match(doc.querySelector('meta[property="og:title"]')?.content ?? "", /Haskell/);
  assert.ok(doc.querySelector('meta[property="og:description"]')?.content);
  assert.equal(
    doc.querySelector('meta[property="article:published_time"]')?.content,
    "2017-01-18",
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
  assert.equal(headings[0].textContent.trim(), "Haskell for Absolute Beginners: A Codefights Walkthrough");

  const homeLinks = [...doc.querySelectorAll('main, header')].flatMap((scope) => [
    ...scope.querySelectorAll('a[href="/"]'),
  ]);
  assert.ok(homeLinks.length >= 1, "article must link back home");

  assert.match(main.textContent, /Daniil Sobol/);
  const published = main.querySelector("time");
  assert.equal(published?.getAttribute("datetime"), "2017-01-18");
  assert.match(published.textContent, /18 January 2017/);
  assert.ok(
    main.querySelector('a[href="https://habr.com/ru/articles/319440/"]'),
    "article must link to the Russian original",
  );

  assert.equal(doc.querySelector('script[src="/assets/app.min.js"]'), null);
  assert.equal(doc.querySelector('script[type="module"]')?.getAttribute("src"), "/js/article.js");
  assert.equal(doc.querySelector('link[rel="stylesheet"][href="/assets/site.css"]') !== null, true);
});

test("article preserves all nineteen Haskell code examples exactly", () => {
  const { doc } = article();
  const expected = [
    "#!/usr/bin/env runhaskell\ndude month day n = …",
    "dude month day n = sum …",
    "dude month day n = sum [1, 3, 5, 7, 9]",
    "main = putStrLn (show (dude 1 1 1))",
    "main = putStrLn show dude 1 1 1",
    "main = ((putStrLn show) dude) 1 1 1",
    "main = putStrLn $ show $ dude 1 1 1",
    "dude month day n = sum $ take n $ …",
    "dude month day n = sum $ take n $ drop (day - 1) $ …",
    "months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]",
    "months = cycle [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]",
    "cycle list = list ++ cycle list",
    "months = cycle [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]\ndude month day n = sum months --infinite dude",
    "months = cycle [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]\nto_days max_days = …\ndude month day n = sum $ take n $ drop (day - 1) $ map to_days months",
    "months = cycle [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]\nto_days max_days = [1..max_days]\ndude month day n = sum $ take n $ drop (day - 1) $ map to_days months",
    "months = cycle [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]\nto_days max_days = [1..max_days]\ndude month day n = sum $ take n $ drop (day - 1) $ concat $ map to_days months",
    "months = cycle [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]\nto_days max_days = [1..max_days]\ndude month day n = sum $ take n $ drop (day - 1) $ concatMap to_days months",
    "number_of_wagons x = x*2 - 1",
    "#!/usr/bin/env runhaskell\nmonths = cycle [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]\nto_days max_days = [1..max_days]\nnumber_of_wagons x = x*2 - 1\ndude month day n = sum $ take n $ drop (day - 1) $ map number_of_wagons $ concatMap to_days $ drop (month - 1) months\nmain = putStrLn $ show $ dude 1 1 1",
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

test("article keeps six local illustrations in order with meaningful English alt text", () => {
  const { doc } = article();
  const expected = [
    ["crowbar.png", 400, 402, /crowbar/i],
    ["train_one.png", 800, 540, /one wagon/i],
    ["train_three.png", 800, 333, /three wagons/i],
    ["example_25.png", 800, 536, /25/],
    ["map.png", 800, 375, /map/i],
    ["lambda_plain.png", 800, 603, /lambda/i],
  ];

  const figures = [...doc.querySelectorAll("main figure")];
  assert.equal(figures.length, expected.length);
  figures.forEach((figure, index) => {
    const images = figure.querySelectorAll("img");
    assert.equal(images.length, 1, "each figure holds one illustration");
    const image = images[0];
    const [file, width, height, keyword] = expected[index];
    assert.equal(new URL(image.getAttribute("src"), articleUrl).pathname, `/habr/habradude/${file}`);
    assert.equal(image.getAttribute("width"), String(width));
    assert.equal(image.getAttribute("height"), String(height));
    assert.ok(statSync(join(articleDir, file)).isFile(), `${file} must be a local asset`);

    const alt = image.getAttribute("alt") ?? "";
    assert.match(alt, keyword);
    assert.ok(alt.trim().split(/\s+/).length >= 4, `alt text is too terse: ${alt}`);
    assert.equal(/[\u0400-\u04FF]/.test(alt), false, `alt text must be English: ${alt}`);
  });

  assert.equal(/habrastorage/.test(article().html), false, "illustrations must be served locally");
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

  const starterLinks = [...doc.querySelectorAll('main a[href="dude.hs"]')];
  assert.equal(starterLinks.length, 1, "the empty starter file stays linked locally");
  assert.equal(statSync(join(articleDir, "dude.hs")).size, 0, "dude.hs must stay empty");
});

test("translation covers the whole article in English", () => {
  const { html } = article();
  const text = prose();

  assert.equal(/[\u0400-\u04FF]/.test(html), false, "no untranslated Russian may remain");
  assert.ok(text.split(/\s+/).filter(Boolean).length > 1000, "the article must not be summarized");

  for (const marker of [
    /functional programming/i,
    /White Russians/,
    /burning down Moscow/i,
    /infinite list/i,
    /starts with the letter m/i,
    /Next time/,
  ]) {
    assert.match(text, marker);
  }

  assert.match(
    text,
    /online runner[^.]*four seconds/i,
    "the four-second observation must be attributed to the online runner",
  );
});

test("homepage Writing section points at the English article", () => {
  const content = yaml.load(readFileSync(join(root, "content.yml"), "utf8"));
  const writing = content.sections.find((section) => section.id === "writing");
  assert.ok(writing, "homepage needs its writing section");

  const links = [...writing.body.matchAll(/\[([^\]]+)\]\(([^)]+)\)/g)].map((match) => ({
    label: match[1],
    href: match[2],
  }));
  const articleLink = links.find(
    (link) => new URL(link.href, siteUrl).href === articleUrl,
  );
  assert.ok(articleLink, "writing section must link to the English article");
  assert.match(articleLink.label, /Haskell for Absolute Beginners/);
  assert.ok(
    links.some((link) => link.href === "https://habr.com/ru/post/251307/"),
    "the unrelated writing entry stays untouched",
  );
  assert.equal(
    links.some((link) => link.href === "https://habr.com/ru/post/319440/"),
    false,
    "the Russian article moves to the article page as the source link",
  );
});

test("sitemap lists the article canonical URL", () => {
  const xml = readFileSync(join(root, "sitemap.xml"), "utf8");
  assert.match(xml, /<loc>https:\/\/mynameisdaniil\.dev\/habr\/habradude\/<\/loc>/);
});

test("service worker cache version hashes the article runtime", async () => {
  const { SHELL_ASSETS } = await import("../scripts/build-sw.mjs");
  assert.ok(Array.isArray(SHELL_ASSETS));
  for (const asset of [
    "index.html",
    "assets/fonts.css",
    "assets/site.css",
    "assets/app.min.js",
    "habr/habradude/index.html",
    "js/article.js",
    "js/analytics.js",
    "js/print-lifecycle.js",
    "typography/dist/typeset-en.esm.js",
  ]) {
    assert.ok(SHELL_ASSETS.includes(asset), `cache hash must include ${asset}`);
  }
});

test("article stylesheet keeps one reading column and locally scrolling code", () => {
  const css = readFileSync(join(root, "assets", "site.css"), "utf8");
  const dom = new JSDOM(`<!doctype html><html><head><style>${css}</style></head></html>`);
  const sheet = dom.window.document.styleSheets[0];

  const flat = [];
  const walk = (rules, media) => {
    for (const rule of rules) {
      if (rule.type === 4) {
        walk(rule.cssRules, rule.conditionText);
      } else if (rule.selectorText) {
        flat.push({ rule, media });
      }
    }
  };
  walk(sheet.cssRules, "");

  const selected = (selector) =>
    flat.filter(({ rule }) =>
      rule.selectorText
        .split(",")
        .map((part) => part.trim())
        .includes(selector),
    );
  const value = (entry, property) => entry.rule.style.getPropertyValue(property);

  assert.ok(
    selected(".paper--article").some(
      (entry) => entry.media === "" && value(entry, "column-count") === "1",
    ),
    "the article is single-column by default",
  );
  assert.ok(
    selected(".paper--article").some(
      (entry) => entry.media.includes("min-width: 1920px") && value(entry, "column-count") === "1",
    ),
    "the article stays single-column on very wide screens",
  );
  assert.ok(
    selected(".paper--article").some(
      (entry) => entry.media === "print" && value(entry, "column-count") === "1",
    ),
    "the article stays single-column in print",
  );
  assert.ok(
    selected(".paper--article::before").some((entry) => value(entry, "display") === "none"),
    "article disables the homepage column separators",
  );
  assert.ok(
    selected(".paper--article pre").some(
      (entry) => entry.media === "" && value(entry, "overflow-x") === "auto",
    ),
    "wide code scrolls inside its own block",
  );
  assert.ok(
    selected(".paper--article pre").some(
      (entry) => entry.media === "print" && value(entry, "white-space") === "pre-wrap",
    ),
    "code wraps instead of clipping when printed",
  );

  const measure = selected(".page--article")
    .filter((entry) => entry.media === "")
    .map((entry) => Number.parseFloat(value(entry, "max-width")))
    .filter(Number.isFinite);
  assert.ok(
    measure.some((width) => width >= 800 && width <= 1000),
    `article reading measure should be about 800px, got ${measure.join(", ")}`,
  );

  assert.ok(
    selected(".paper").some(
      (entry) => entry.media.includes("min-width: 640px") && value(entry, "column-count") === "2",
    ),
    "homepage newspaper columns are unchanged",
  );
});
