import { loadContent, renderMarkdown, ContentError } from "./content.js";
import { typeset, unjustify, clearProtrusion } from "../typography/src/index-en.js";
import { initAnalytics } from "./analytics.js";

const masthead = document.querySelector(".masthead");
const titleEl = document.getElementById("site-title");
const subtitleEl = document.getElementById("site-subtitle");
const linksEl = document.getElementById("site-links");
const paper = document.getElementById("paper");
let typesetter = null;

window.addEventListener("beforeprint", () => {
  clearProtrusion(paper);
  unjustify([...paper.querySelectorAll("p")]);
});

window.addEventListener("afterprint", () => {
  const printMedia = window.matchMedia("print");
  if (printMedia.matches) {
    printMedia.addEventListener(
      "change",
      (event) => {
        if (!event.matches) typesetter?.refresh();
      },
      { once: true },
    );
  } else {
    typesetter?.refresh();
  }
});

function createLink(link) {
  const a = document.createElement("a");
  a.textContent = link.label;
  a.href = link.url;
  if (!link.url.startsWith("mailto:")) {
    a.target = "_blank";
    a.rel = "noopener noreferrer";
  }
  return a;
}

function renderLinks(container, links) {
  container.replaceChildren(...links.map(createLink));
}

function renderSection(section) {
  const el = document.createElement("section");
  el.id = section.id;
  if (section.lang) el.setAttribute("lang", section.lang);

  const heading = document.createElement("h2");
  heading.textContent = section.title;
  el.appendChild(heading);

  if (section.body) {
    const body = document.createElement("div");
    body.className = "section-body";
    body.innerHTML = renderMarkdown(section.body);
    el.appendChild(body);
  }

  if (section.links.length > 0) {
    const nav = document.createElement("nav");
    nav.className = "links-row section-links";
    nav.setAttribute("aria-label", `Links for ${section.title}`);
    renderLinks(nav, section.links);
    el.appendChild(nav);
  }

  return el;
}

function showError(details) {
  masthead.hidden = true;
  paper.replaceChildren();
  paper.classList.add("paper--error");

  const message = document.createElement("p");
  message.textContent = "Could not load content.";
  paper.append(message);
  if (details.length > 0) {
    const list = document.createElement("ul");
    for (const detail of details) {
      const item = document.createElement("li");
      item.textContent = detail;
      list.appendChild(item);
    }
    paper.append(list);
  }
}

async function boot() {
  try {
    const model = await loadContent("content.yml");
    document.title = model.meta.title;
    titleEl.textContent = model.meta.title;
    subtitleEl.textContent = model.meta.subtitle;
    renderLinks(linksEl, model.meta.links);
    paper.replaceChildren(...model.sections.map(renderSection));
    typesetter = typeset(paper);
  } catch (err) {
    console.error(err);
    showError(err instanceof ContentError ? err.details : [String(err)]);
  }
}

boot();

initAnalytics();

if ("serviceWorker" in navigator) {
  navigator.serviceWorker
    .register("/sw.js")
    .catch((err) => console.warn("service worker registration failed", err));
}
