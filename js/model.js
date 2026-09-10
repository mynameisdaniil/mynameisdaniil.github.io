const ID_RE = /^[a-z0-9-]+$/;

export function buildModel(raw) {
  const errors = [];
  const cards = new Map();
  const edges = [];

  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    return {
      meta: { title: "", defaultCard: "" },
      cards,
      edges,
      errors: ["content.yml: root must be a mapping"],
    };
  }

  const metaRaw = raw.meta && typeof raw.meta === "object" ? raw.meta : {};
  const title = typeof metaRaw.title === "string" ? metaRaw.title : "";
  const defaultCard = typeof metaRaw.default_card === "string" ? metaRaw.default_card : "";
  const list = Array.isArray(raw.cards) ? raw.cards : [];

  if (!defaultCard) errors.push("meta.default_card is required");
  if (!Array.isArray(raw.cards)) errors.push("cards must be a list");

  for (const [i, c] of list.entries()) {
    const where = `cards[${i}]`;
    if (!c || typeof c !== "object" || Array.isArray(c)) {
      errors.push(`${where} must be a mapping`);
      continue;
    }

    const id = typeof c.id === "string" ? c.id : "";
    const cardTitle = typeof c.title === "string" ? c.title : "";
    if (!id) errors.push(`${where}.id is required`);
    else if (!ID_RE.test(id)) errors.push(`${where}.id must match [a-z0-9-]+`);
    if (!cardTitle) errors.push(`${where}.title is required`);
    if (id && cards.has(id)) errors.push(`duplicate card id "${id}"`);
    if (!id || cards.has(id)) continue;

    const links = [];
    const rawLinks = Array.isArray(c.links) ? c.links : [];
    if (c.links !== undefined && !Array.isArray(c.links)) {
      errors.push(`${where}.links must be a list`);
    }

    for (const [j, l] of rawLinks.entries()) {
      const lw = `${where}.links[${j}]`;
      if (!l || typeof l !== "object" || Array.isArray(l)) {
        errors.push(`${lw} must be a mapping`);
        continue;
      }
      const hasUrl = typeof l.url === "string" && l.url.length > 0;
      const hasCard = typeof l.card === "string" && l.card.length > 0;
      if (hasUrl === hasCard) {
        errors.push(`${lw} must have exactly one of url or card`);
        continue;
      }
      const fallback = hasUrl ? l.url : l.card;
      const label = typeof l.label === "string" && l.label ? l.label : fallback;
      links.push(hasUrl ? { label, url: l.url } : { label, card: l.card });
    }

    cards.set(id, {
      id,
      title: cardTitle,
      body: typeof c.body === "string" ? c.body : "",
      lang: typeof c.lang === "string" ? c.lang : "",
      links,
    });
  }

  const seenEdges = new Set();
  for (const card of cards.values()) {
    for (const link of card.links) {
      if (!link.card) continue;
      if (!cards.has(link.card)) {
        errors.push(`card "${card.id}" links to unknown card "${link.card}"`);
        continue;
      }
      const key = [card.id, link.card].sort().join("->");
      if (seenEdges.has(key)) continue;
      seenEdges.add(key);
      edges.push({ source: card.id, target: link.card });
    }
  }

  if (defaultCard && !cards.has(defaultCard)) {
    errors.push(`meta.default_card "${defaultCard}" does not exist`);
  }

  return { meta: { title, defaultCard }, cards, edges, errors };
}

export function isValid(model) {
  return model.errors.length === 0;
}
