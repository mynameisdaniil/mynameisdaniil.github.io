const POSTHOG_KEY = "phc_C5tSXrtERrTTB47Nt8258rzH2gnLhqGbBwXZkXsqrb7S";
const POSTHOG_SCRIPT = "https://omg.mynameisdaniil.dev/static/array.js";
const POSTHOG_HOST = "https://omg.mynameisdaniil.dev";
const PRODUCTION_HOST = "mynameisdaniil.dev";

const POSTHOG_CONFIG = {
  api_host: POSTHOG_HOST,
  ui_host: "https://eu.posthog.com",
  cookieless_mode: "always",
  enable_heatmaps: true,
  autocapture: true,
  capture_pageview: true,
  capture_pageleave: true,
  disable_session_recording: true,
  disable_surveys: true,
  respect_dnt: true,
};

const LABEL_LIMIT = 200;

function sectionFor(anchor) {
  const section = anchor.closest("section[id]");
  return section ? section.id : "masthead";
}

const SCROLL_THRESHOLDS = [25, 50, 75, 100];

function defaultFrame(win) {
  return (fn) => {
    if (typeof win.requestAnimationFrame === "function") {
      win.requestAnimationFrame(fn);
    } else {
      fn();
    }
  };
}

function wireScroll(win, doc, posthog, frame) {
  const fired = new Set();
  let ticking = false;
  const emit = () => {
    ticking = false;
    const percent =
      ((win.scrollY + win.innerHeight) / doc.documentElement.scrollHeight) * 100;
    for (const threshold of SCROLL_THRESHOLDS) {
      if (percent >= threshold && !fired.has(threshold)) {
        fired.add(threshold);
        posthog.capture("scroll_depth", { percent: threshold });
      }
    }
  };
  win.addEventListener(
    "scroll",
    () => {
      if (ticking) return;
      ticking = true;
      frame(emit);
    },
    { passive: true },
  );
}

function wireClicks(doc, posthog) {
  doc.addEventListener("click", (event) => {
    const anchor = event.target.closest?.("a[href]");
    if (!anchor) return;
    const href = anchor.getAttribute("href");
    if (!href || href.startsWith("#")) return;
    const label = anchor.textContent.trim().slice(0, LABEL_LIMIT);
    posthog.capture("outbound_click", { href, label, section: sectionFor(anchor) });
  });
}

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
  const frame = deps.frame ?? defaultFrame(win);
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
            wireClicks(doc, win.posthog);
            wireScroll(win, doc, win.posthog, frame);
          })
          .catch(() => {})
          .then(resolve);
      });
    });
  });
}
