export function parseHash(hash, cardIds, defaultCard) {
  const id = String(hash ?? "").replace(/^#/, "");
  return cardIds.has(id) ? id : defaultCard;
}

export function createRouter({ defaultCard, cardIds, onFocus, win = globalThis }) {
  let current = parseHash(win.location.hash, cardIds, defaultCard);

  function focus(id) {
    const target = parseHash(`#${id}`, cardIds, defaultCard);
    if (win.location.hash !== `#${target}`) {
      win.location.hash = target;
      return;
    }
    if (target === current) return;
    current = target;
    onFocus(target);
  }

  function onHashChange(event) {
    if (event.newURL && event.newURL !== win.location.href) return;
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
