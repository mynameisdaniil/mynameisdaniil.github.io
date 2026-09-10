import { loadContent, renderMarkdown, ContentError } from "./content.js";
import {
  createCard,
  setCardBody,
  markActiveLinks,
  createErrorCard,
} from "./card.js";
import { createGraph } from "./graph.js";
import { createRouter } from "./router.js";
import { initTheme } from "./theme.js";
import { createTypography } from "./typography.js";

const stage = document.getElementById("stage");
const cardsLayer = document.getElementById("cards");
const svg = document.getElementById("edges");
const live = document.getElementById("live");
const toggle = document.getElementById("theme-toggle");

initTheme({ button: toggle });

const typography = createTypography({
  getHyphenator: (lang) => globalThis.typography.hyphenateFor(lang),
});

try {
  const model = await loadContent("content.yml");
  document.title = model.meta.title;

  const cardsById = new Map();
  let router = null;

  for (const card of model.cards.values()) {
    const el = createCard(card, { onSelect: (id) => router.focus(id) });
    cardsById.set(card.id, el);
    cardsLayer.appendChild(el);
  }

  const graph = createGraph({
    stage,
    svg,
    cardsById,
    edges: model.edges,
  });

  function currentLang() {
    const card = model.cards.get(router.current);
    return card?.lang || "en-us";
  }

  function render(id) {
    const card = model.cards.get(id);
    const el = cardsById.get(id);
    setCardBody(el, renderMarkdown(card.body));
    graph.focusCard(id);
    el.querySelector(".card-title")?.focus({ preventScroll: true });
    for (const cardEl of cardsById.values()) markActiveLinks(cardEl, id);
    live.textContent = `Showing ${card.title}`;
    document.title =
      id === model.meta.defaultCard ? model.meta.title : `${card.title} — ${model.meta.title}`;
    typography.schedule(el, card.lang || "en-us");
  }

  router = createRouter({
    defaultCard: model.meta.defaultCard,
    cardIds: new Set(model.cards.keys()),
    onFocus: render,
  });

  let resizeTimer = 0;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => {
      typography.schedule(cardsById.get(router.current), currentLang());
    }, 150);
  });

  await document.fonts.ready;
  render(router.current);
  await typography.flush();
  stage.style.visibility = "visible";
  stage.setAttribute("aria-busy", "false");
} catch (err) {
  console.error(err);
  const details = err instanceof ContentError ? err.details : [String(err)];
  stage.appendChild(createErrorCard("Could not load content.", details));
  stage.style.visibility = "visible";
  stage.setAttribute("aria-busy", "false");
}
