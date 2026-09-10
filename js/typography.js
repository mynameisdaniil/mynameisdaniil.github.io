import { applyProtrusion, clearProtrusion } from "./protrusion.js";

export function createTypography({ getHyphenator, typography = globalThis.typography }) {
  let raf = 0;
  let pending = null;

  function applyTo(el, lang = "en-us") {
    const paragraphs = [...el.querySelectorAll(".card-body p")];
    if (paragraphs.length === 0) return;

    try {
      clearProtrusion(el);
      typography.justifyContent(paragraphs, getHyphenator(lang));
      applyProtrusion(el);
    } catch (err) {
      console.error("typography: justification failed", err);
    }
  }

  function schedule(el, lang = "en-us") {
    cancelAnimationFrame(raf);
    pending = new Promise((resolve) => {
      raf = requestAnimationFrame(() => {
        applyTo(el, lang);
        pending = null;
        resolve();
      });
    });
    return pending;
  }

  async function flush() {
    if (pending) await pending;
  }

  return { applyTo, schedule, flush };
}
