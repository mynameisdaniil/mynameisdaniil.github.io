import { justify } from "./justify.js";
import { hyphenateFor } from "./hyphenate.js";
import { applyProtrusion, clearProtrusion } from "./protrusion.js";
import { applySmartPunctuation } from "./punctuation.js";

const DEFAULTS = {
  selector: "p",
  lang: "en-us",
  hyphenate: true,
  justify: true,
  protrude: true,
  punctuation: true,
  onResize: true,
  awaitFonts: true,
  injectStyles: true,
  resizeDelay: 150,
};

function injectStylesOnce(doc) {
  if (doc.querySelector("style[data-typeset]")) return;
  const style = doc.createElement("style");
  style.setAttribute("data-typeset", "");
  style.textContent = ".protrude-left, .protrude-right { display: inline; }";
  doc.head.appendChild(style);
}

function resolveLang(el, fallback) {
  const tagged = el.closest ? el.closest("[lang]") : null;
  return (tagged && tagged.getAttribute("lang")) || fallback || "en-us";
}

export function typeset(root, options = {}) {
  const opts = { ...DEFAULTS, ...options };
  const doc = root.ownerDocument || document;

  if (opts.injectStyles) injectStylesOnce(doc);

  function run(el) {
    if (opts.punctuation) applySmartPunctuation(el);

    if (opts.justify) {
      const paragraphs = [...el.querySelectorAll(opts.selector)];
      if (paragraphs.length === 0) return;

      clearProtrusion(el);
      if (doc.fonts && !getComputedStyle(paragraphs[0]).font) {
        console.warn(
          "typeset: empty computed font shorthand; line measurement may be wrong (avoid font-kerning/font-variant-* on measured text)",
        );
      }

      const groups = new Map();
      for (const paragraph of paragraphs) {
        const lang = resolveLang(paragraph, opts.lang);
        if (!groups.has(lang)) groups.set(lang, []);
        groups.get(lang).push(paragraph);
      }

      try {
        for (const [lang, elements] of groups) {
          justify(elements, opts.hyphenate ? hyphenateFor(lang) : undefined);
        }
        if (opts.protrude) applyProtrusion(el);
      } catch (err) {
        console.error("typeset: justification failed", err);
      }
    } else if (opts.protrude) {
      clearProtrusion(el);
      applyProtrusion(el);
    }
  }

  const win = doc.defaultView || (typeof window !== "undefined" ? window : null);
  let resizeTimer = 0;

  function onResize() {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => run(root), opts.resizeDelay);
  }

  if (opts.onResize && win) win.addEventListener("resize", onResize);

  if (opts.awaitFonts && doc.fonts && doc.fonts.ready) {
    doc.fonts.ready.then(() => run(root)).catch(() => {});
  } else {
    run(root);
  }

  return {
    typeset: (el) => run(el || root),
    refresh: () => run(root),
    destroy() {
      if (win) win.removeEventListener("resize", onResize);
      clearTimeout(resizeTimer);
    },
  };
}
