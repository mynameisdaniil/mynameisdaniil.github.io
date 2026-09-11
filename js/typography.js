import { applyProtrusion, clearProtrusion } from "./protrusion.js";

export function createTypography({ getHyphenator, typography = globalThis.typography }) {
  function applyTo(el, lang = "en-us") {
    const paragraphs = [...el.querySelectorAll("p")];
    if (paragraphs.length === 0) return;

    try {
      if (!getComputedStyle(el).font) {
        console.warn(
          "typography: empty computed font shorthand; line measurement may be wrong (avoid font-kerning/font-variant-* on measured text)",
        );
      }
      clearProtrusion(el);
      typography.justifyContent(paragraphs, getHyphenator(lang));
      applyProtrusion(el);
    } catch (err) {
      console.error("typography: justification failed", err);
    }
  }

  return { applyTo };
}
