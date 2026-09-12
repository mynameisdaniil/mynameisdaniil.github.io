import yaml from "js-yaml";
import { marked } from "marked";
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
  const parser = deps.yaml ?? yaml;

  const response = await fetchImpl(url, { cache: "no-store" });
  if (!response.ok) {
    throw new ContentError(`Failed to load ${url}: HTTP ${response.status}`);
  }

  const text = await response.text();
  const model = buildModel(parser.load(text));
  if (model.errors.length > 0) {
    throw new ContentError(`${url} is invalid`, model.errors);
  }
  return model;
}

export function renderMarkdown(text, deps = {}) {
  if (!text) return "";
  const parser = deps.marked ?? marked;
  return parser.parse(text);
}
