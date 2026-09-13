const POSTHOG_KEY = "";
const POSTHOG_SCRIPT = "https://eu-assets.i.posthog.com/static/array.js";
const POSTHOG_HOST = "https://eu.i.posthog.com";
const PRODUCTION_HOST = "mynameisdaniil.dev";

const POSTHOG_CONFIG = {
  api_host: POSTHOG_HOST,
  cookieless_mode: "always",
  enable_heatmaps: true,
  autocapture: true,
  capture_pageview: true,
  capture_pageleave: true,
  disable_session_recording: true,
  disable_surveys: true,
  respect_dnt: true,
};

function defaultSchedule(win) {
  return (fn) => {
    if (typeof win.requestIdleCallback === "function") {
      win.requestIdleCallback(fn, { timeout: 3000 });
    } else {
      win.setTimeout(fn, 1200);
    }
  };
}

function defaultWhenReady(win, doc) {
  return (fn) => {
    if (doc.readyState === "complete") {
      fn();
    } else {
      win.addEventListener("load", fn, { once: true });
    }
  };
}

function loadScript(win, doc, url) {
  return new Promise((resolve, reject) => {
    const script = doc.createElement("script");
    script.async = true;
    script.crossOrigin = "anonymous";
    script.src = url;
    script.addEventListener("load", () => resolve());
    script.addEventListener("error", () => reject(new Error(`failed to load ${url}`)));
    doc.head.appendChild(script);
  });
}

export async function initAnalytics(deps = {}) {
  const win = deps.win ?? globalThis.window;
  const doc = deps.doc ?? globalThis.document;
  if (!win || !doc || win.location.hostname !== PRODUCTION_HOST) return;
  const key = deps.key ?? POSTHOG_KEY;
  if (!key) return;
  const schedule = deps.schedule ?? defaultSchedule(win);
  const whenReady = deps.whenReady ?? defaultWhenReady(win, doc);
  const load = deps.load ?? loadScript;
  const scriptUrl = deps.scriptUrl ?? POSTHOG_SCRIPT;

  await new Promise((resolve) => {
    whenReady(() => {
      schedule(() => {
        Promise.resolve()
          .then(() => load(win, doc, scriptUrl))
          .then(() => {
            win.posthog.init(key, POSTHOG_CONFIG);
          })
          .catch(() => {})
          .then(resolve);
      });
    });
  });
}
