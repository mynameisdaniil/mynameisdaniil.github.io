import { buildModel } from "./model.js";

export class ContentError extends Error {
  constructor(message, details = []) {
    super(message);
    this.name = "ContentError";
    this.details = details;
  }
}

export async function loadContent(url, deps = {}) {
  const fetchImpl = deps.fetchImpl ?? globalThis.fetch;
  const yaml = deps.yaml ?? globalThis.jsyaml;

  const response = await fetchImpl(url);
  if (!response.ok) {
    throw new ContentError(`Failed to load ${url}: HTTP ${response.status}`);
  }

  const text = await response.text();
  const model = buildModel(yaml.load(text));
  if (model.errors.length > 0) {
    throw new ContentError(`${url} is invalid`, model.errors);
  }
  return model;
}

export function renderMarkdown(text, deps = {}) {
  if (!text) return "";
  const marked = deps.marked ?? globalThis.marked;
  return marked.parse(text);
}
