export function createCard(card, { onSelect }) {
  const el = document.createElement("article");
  el.className = "card card--background";
  el.dataset.cardId = card.id;
  el.tabIndex = 0;
  el.setAttribute("aria-labelledby", `card-${card.id}-title`);

  const title = document.createElement("h1");
  title.className = "card-title";
  title.id = `card-${card.id}-title`;
  title.tabIndex = -1;
  title.textContent = card.title;

  const body = document.createElement("div");
  body.className = "card-body";
  body.hidden = true;

  const nav = document.createElement("nav");
  nav.className = "card-links";
  nav.setAttribute("aria-label", `Links for ${card.title}`);

  for (const link of card.links) {
    const a = document.createElement("a");
    a.textContent = link.label;
    if (link.url) {
      a.href = link.url;
      a.target = "_blank";
      a.rel = "noopener noreferrer";
    } else {
      a.href = `#${link.card}`;
      a.dataset.internal = "true";
    }
    nav.appendChild(a);
  }

  el.append(title, body, nav);

  el.addEventListener("click", (event) => {
    if (event.target.closest("a")) return;
    event.preventDefault();
    onSelect(card.id);
  });

  el.addEventListener("keydown", (event) => {
    if (event.target.closest("a")) return;
    if (event.key === "Enter" || event.key === " ") {
      event.preventDefault();
      onSelect(card.id);
    }
  });

  return el;
}

export function setCardState(el, state) {
  const focused = state === "focused";
  el.classList.toggle("card--focused", focused);
  el.classList.toggle("card--background", !focused);
}

export function setCardBody(el, html) {
  const body = el.querySelector(".card-body");
  if (!body || body.dataset.rendered === "true") return;
  body.innerHTML = html;
  body.hidden = false;
  body.dataset.rendered = "true";
}

export function markActiveLinks(el, activeId) {
  for (const a of el.querySelectorAll("a[data-internal]")) {
    if (a.getAttribute("href") === `#${activeId}`) {
      a.setAttribute("aria-current", "page");
    } else {
      a.removeAttribute("aria-current");
    }
  }
}

export function createErrorCard(message, details = []) {
  const el = document.createElement("article");
  el.className = "card card--error";

  const title = document.createElement("h1");
  title.className = "card-title";
  title.textContent = "Error";

  const body = document.createElement("div");
  body.className = "card-body";
  const p = document.createElement("p");
  p.textContent = message;
  body.appendChild(p);

  if (details.length > 0) {
    const list = document.createElement("ul");
    for (const detail of details) {
      const li = document.createElement("li");
      li.textContent = detail;
      list.appendChild(li);
    }
    body.appendChild(list);
  }

  el.append(title, body);
  return el;
}
