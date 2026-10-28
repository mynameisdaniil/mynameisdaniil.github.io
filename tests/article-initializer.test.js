import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";

const PAGE = `<!doctype html><html><body>
  <main class="paper paper--article" id="paper"><p>A -- B.</p></main>
</body></html>`;

async function harness() {
  const dom = new JSDOM(PAGE, { url: "https://mynameisdaniil.dev/habr/habradude/" });
  const { window: win, window: { document: doc } } = dom;
  const calls = { typeset: [], analytics: 0, registered: [], refreshed: 0 };
  const typesetter = {
    refresh: () => {
      calls.refreshed += 1;
    },
  };
  const deps = {
    win,
    doc,
    typeset: (target) => {
      calls.typeset.push(target);
      return typesetter;
    },
    initAnalytics: () => {
      calls.analytics += 1;
    },
    register: (url) => {
      calls.registered.push(url);
      return Promise.resolve();
    },
  };
  const { initArticle } = await import("../js/article.js");
  return { dom, win, doc, calls, deps, initArticle };
}

test("typesets only the article paper and registers the root service worker", async () => {
  const h = await harness();
  h.initArticle(h.deps);

  assert.equal(h.calls.typeset.length, 1);
  assert.equal(h.calls.typeset[0], h.doc.getElementById("paper"));
  assert.deepEqual(h.calls.registered, ["/sw.js"]);
  assert.equal(h.calls.analytics, 1);
});

test("beforeprint clears protrusion and unjustifies, afterprint refreshes the typesetter", async () => {
  const h = await harness();
  Object.defineProperty(h.win, "matchMedia", {
    value: () => ({ matches: false }),
    configurable: true,
  });
  h.initArticle(h.deps);

  const paragraph = h.doc.querySelector("p");
  paragraph.style.whiteSpace = "nowrap";
  const protrusion = h.doc.createElement("span");
  protrusion.className = "protrude-left";
  protrusion.textContent = "A";
  paragraph.prepend(protrusion);

  h.win.dispatchEvent(new h.win.Event("beforeprint"));
  assert.equal(paragraph.style.whiteSpace, "");
  assert.equal(paragraph.querySelector(".protrude-left"), null);

  h.win.dispatchEvent(new h.win.Event("afterprint"));
  assert.equal(h.calls.refreshed, 1);
});

test("afterprint waits for print media to end before refreshing", async () => {
  const h = await harness();
  const listeners = [];
  const printMedia = {
    matches: true,
    addEventListener: (type, listener) => listeners.push({ type, listener }),
  };
  Object.defineProperty(h.win, "matchMedia", {
    value: () => printMedia,
    configurable: true,
  });
  h.initArticle(h.deps);

  h.win.dispatchEvent(new h.win.Event("afterprint"));
  assert.equal(h.calls.refreshed, 0);
  assert.equal(listeners.length, 1);
  assert.equal(listeners[0].type, "change");

  listeners[0].listener({ matches: false });
  assert.equal(h.calls.refreshed, 1);
});
