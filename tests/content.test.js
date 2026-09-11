import test from "node:test";
import assert from "node:assert/strict";
import { loadContent, renderMarkdown, ContentError } from "../js/content.js";

const yaml = {
  load: () => ({
    meta: { title: "T", subtitle: "S", links: [] },
    sections: [{ id: "a", title: "A" }],
  }),
};

test("loads and validates content", async () => {
  const fetchImpl = async () => ({ ok: true, status: 200, text: async () => "raw" });
  const model = await loadContent("content.yml", { fetchImpl, yaml });
  assert.equal(model.meta.title, "T");
  assert.equal(model.sections.length, 1);
  assert.equal(model.sections[0].id, "a");
});

test("rejects on non-ok HTTP responses", async () => {
  const fetchImpl = async () => ({ ok: false, status: 404, text: async () => "" });
  await assert.rejects(
    () => loadContent("content.yml", { fetchImpl, yaml }),
    /Failed to load content.yml: HTTP 404/,
  );
});

test("rejects with details on invalid content", async () => {
  const fetchImpl = async () => ({ ok: true, status: 200, text: async () => "raw" });
  const badYaml = { load: () => ({ cards: [] }) };
  await assert.rejects(
    () => loadContent("content.yml", { fetchImpl, yaml: badYaml }),
    (err) => {
      assert.equal(err instanceof ContentError, true);
      assert.equal(err.details.includes("meta.title is required"), true);
      return true;
    },
  );
});

test("delegates markdown rendering and handles empty text", () => {
  const marked = { parse: (text) => `<p>${text}</p>` };
  assert.equal(renderMarkdown("hi", { marked }), "<p>hi</p>");
  assert.equal(renderMarkdown("", { marked }), "");
});
