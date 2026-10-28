import { typeset, unjustify, clearProtrusion } from "../typography/dist/typeset-en.esm.js";
import { installPrintLifecycle } from "./print-lifecycle.js";
import { initAnalytics } from "./analytics.js";

export function initArticle(deps = {}) {
  const doc = deps.doc ?? document;
  const win = deps.win ?? window;
  const paper = deps.root ?? doc.getElementById("paper");
  if (!paper) return null;

  const runTypeset = deps.typeset ?? typeset;
  const clear = deps.clearProtrusion ?? clearProtrusion;
  const unjust = deps.unjustify ?? unjustify;
  const analytics = deps.initAnalytics ?? initAnalytics;
  const register =
    deps.register ??
    win.navigator.serviceWorker?.register.bind(win.navigator.serviceWorker);

  const typesetter = runTypeset(paper);
  installPrintLifecycle({
    win,
    root: paper,
    clearProtrusion: clear,
    unjustify: unjust,
    typesetter,
  });

  analytics();

  if (register) {
    Promise.resolve(register("/sw.js")).catch((err) =>
      console.warn("service worker registration failed", err),
    );
  }

  return typesetter;
}

if (typeof document !== "undefined") {
  initArticle();
}
