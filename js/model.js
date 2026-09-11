const ID_RE = /^[a-z0-9-]+$/;

function parseLinks(rawLinks, where, errors) {
  const links = [];
  if (rawLinks === undefined) return links;
  if (!Array.isArray(rawLinks)) {
    errors.push(`${where} must be a list`);
    return links;
  }
  for (const [i, item] of rawLinks.entries()) {
    const path = `${where}[${i}]`;
    if (!item || typeof item !== "object" || Array.isArray(item)) {
      errors.push(`${path} must be a mapping`);
      continue;
    }
    if (typeof item.url !== "string" || item.url.length === 0) {
      errors.push(`${path} must have a url`);
      continue;
    }
    const label = typeof item.label === "string" && item.label ? item.label : item.url;
    links.push({ label, url: item.url });
  }
  return links;
}

export function buildModel(raw) {
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    return {
      meta: { title: "", subtitle: "", links: [] },
      sections: [],
      errors: ["content.yml: root must be a mapping"],
    };
  }

  const errors = [];
  const metaRaw = raw.meta && typeof raw.meta === "object" && !Array.isArray(raw.meta) ? raw.meta : {};
  const title = typeof metaRaw.title === "string" ? metaRaw.title : "";
  const subtitle = typeof metaRaw.subtitle === "string" ? metaRaw.subtitle : "";
  if (!title) errors.push("meta.title is required");
  if (!subtitle) errors.push("meta.subtitle is required");
  const metaLinks = parseLinks(metaRaw.links, "meta.links", errors);

  const sections = [];
  const seen = new Set();
  if (!Array.isArray(raw.sections)) {
    errors.push("sections must be a list");
  } else if (raw.sections.length === 0) {
    errors.push("sections must not be empty");
  }

  for (const [i, section] of (Array.isArray(raw.sections) ? raw.sections : []).entries()) {
    const where = `sections[${i}]`;
    if (!section || typeof section !== "object" || Array.isArray(section)) {
      errors.push(`${where} must be a mapping`);
      continue;
    }
    const id = typeof section.id === "string" ? section.id : "";
    const sectionTitle = typeof section.title === "string" ? section.title : "";
    const idValid = id !== "" && ID_RE.test(id);
    if (!id) errors.push(`${where}.id is required`);
    else if (!idValid) errors.push(`${where}.id must match [a-z0-9-]+`);
    else if (seen.has(id)) errors.push(`duplicate section id "${id}"`);
    if (!sectionTitle) errors.push(`${where}.title is required`);
    if (!idValid || seen.has(id)) continue;

    seen.add(id);
    sections.push({
      id,
      title: sectionTitle,
      body: typeof section.body === "string" ? section.body : "",
      lang: typeof section.lang === "string" ? section.lang : "",
      links: parseLinks(section.links, `${where}.links`, errors),
    });
  }

  return { meta: { title, subtitle, links: metaLinks }, sections, errors };
}

export function isValid(model) {
  return model.errors.length === 0;
}
