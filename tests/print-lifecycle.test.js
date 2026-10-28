import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { JSDOM } from "jsdom";
import { installPrintLifecycle } from "../js/print-lifecycle.js";

const PAGE = `<!doctype html><html><body>
  <main id="paper"><p>One</p><p>Two</p></main>
  <p id="outside">Outside</p>
</body></html>`;

function setup({ printMatches = false } = {}) {
  const dom = new JSDOM(PAGE);
  const { window: win } = dom;
  const doc = win.document;
  const root = doc.getElementById("paper");
  const calls = { cleared: [], unjustified: [], refreshed: 0 };
  const listeners = [];
  Object.defineProperty(win, "matchMedia", {
    value: () => ({
      matches: printMatches,
      addEventListener: (type, listener) => listeners.push({ type, listener }),
    }),
    configurable: true,
  });
  const typesetter = {
    refresh: () => {
      calls.refreshed += 1;
    },
  };
  return { win, doc, root, calls, listeners, typesetter };
}

function install(env) {
  installPrintLifecycle({
    win: env.win,
    root: env.root,
    clearProtrusion: (target) => env.calls.cleared.push(target),
    unjustify: (paragraphs) => env.calls.unjustified.push(paragraphs),
    typesetter: env.typesetter,
  });
}

test("beforeprint clears protrusion and unjustifies the paragraphs in the paper", () => {
  const env = setup();
  install(env);

  env.win.dispatchEvent(new env.win.Event("beforeprint"));

  assert.deepEqual(env.calls.cleared, [env.root]);
  assert.deepEqual(env.calls.unjustified, [[...env.root.querySelectorAll("p")]]);
  assert.equal(env.calls.unjustified[0].length, 2, "paragraphs outside the paper are untouched");
});

test("afterprint refreshes the typesetter immediately outside print media", () => {
  const env = setup({ printMatches: false });
  install(env);

  env.win.dispatchEvent(new env.win.Event("afterprint"));

  assert.equal(env.calls.refreshed, 1);
  assert.deepEqual(env.listeners, []);
});

test("afterprint defers the refresh until print media ends", () => {
  const env = setup({ printMatches: true });
  install(env);

  env.win.dispatchEvent(new env.win.Event("afterprint"));
  assert.equal(env.calls.refreshed, 0);
  assert.deepEqual(
    env.listeners.map(({ type }) => type),
    ["change"],
  );

  env.listeners[0].listener({ matches: true });
  assert.equal(env.calls.refreshed, 0, "a change into print media must not refresh");

  env.listeners[0].listener({ matches: false });
  assert.equal(env.calls.refreshed, 1);
});

test("afterprint tolerates a missing typesetter handle", () => {
  const env = setup({ printMatches: false });
  delete env.typesetter;
  install(env);

  assert.doesNotThrow(() => env.win.dispatchEvent(new env.win.Event("afterprint")));
});

test("both page initializers delegate print handling to the shared helper", () => {
  for (const file of ["js/main.js", "js/article.js"]) {
    const source = readFileSync(new URL(`../${file}`, import.meta.url), "utf8");
    assert.match(
      source,
      /import \{ installPrintLifecycle \} from "\.\/print-lifecycle\.js";/,
      `${file} must import the shared print lifecycle`,
    );
    assert.equal(
      source.includes('addEventListener("beforeprint"'),
      false,
      `${file} must not keep its own beforeprint handler`,
    );
    assert.equal(
      source.includes('addEventListener("afterprint"'),
      false,
      `${file} must not keep its own afterprint handler`,
    );
  }
});
