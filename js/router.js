export function parseHash(hash, cardIds, defaultCard) {
  const id = String(hash ?? "").replace(/^#/, "");
  return cardIds.has(id) ? id : defaultCard;
}

export function createRouter({ defaultCard, cardIds, onFocus, win = globalThis }) {
  let current = parseHash(win.location.hash, cardIds, defaultCard);
  let lastSet = null;

  function focus(id) {
    const next = parseHash(`#${id}`, cardIds, defaultCard);
    current = next;
    if (win.location.hash !== `#${next}`) {
      lastSet = `#${next}`;
      win.location.hash = next;
    }
    onFocus(next);
  }

  function onHashChange() {
    if (win.location.hash === lastSet) {
      lastSet = null;
      return;
    }
    current = parseHash(win.location.hash, cardIds, defaultCard);
    onFocus(current);
  }

  function onKeydown(event) {
    if (event.key === "Escape") focus(defaultCard);
  }

  win.addEventListener("hashchange", onHashChange);
  win.addEventListener("keydown", onKeydown);

  return {
    get current() {
      return current;
    },
    focus,
  };
}
