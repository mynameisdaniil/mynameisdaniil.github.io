import test from "node:test";
import assert from "node:assert/strict";
import { buildModel, isValid } from "../js/model.js";

const validRaw = {
  meta: {
    title: "Daniil Sobol",
    subtitle: "distributed systems engineer",
    links: [
      { label: "GitHub", url: "https://github.com/mynameisdaniil" },
      { label: "Email", url: "mailto:sobol.daniil@gmail.com" },
    ],
  },
  sections: [
    {
      id: "projects",
      title: "Projects",
      body: "Hello.",
      links: [{ label: "ePortalD", url: "https://eportald.org/" }],
    },
    { id: "facts", title: "Facts" },
  ],
};

test("builds meta and sections with parsed links", () => {
  const model = buildModel(validRaw);
  assert.deepEqual(model.errors, []);
  assert.equal(model.meta.title, "Daniil Sobol");
  assert.equal(model.meta.subtitle, "distributed systems engineer");
  assert.deepEqual(model.meta.links[0], {
    label: "GitHub",
    url: "https://github.com/mynameisdaniil",
  });
  assert.equal(model.sections.length, 2);
  assert.equal(model.sections[0].id, "projects");
  assert.equal(model.sections[0].body, "Hello.");
  assert.deepEqual(model.sections[0].links, [{ label: "ePortalD", url: "https://eportald.org/" }]);
  assert.equal(model.sections[1].body, "");
  assert.equal(model.sections[1].lang, "");
  assert.equal(isValid(model), true);
});

test("requires meta.title and meta.subtitle", () => {
  const model = buildModel({ sections: [{ id: "a", title: "A" }] });
  assert.equal(model.errors.includes("meta.title is required"), true);
  assert.equal(model.errors.includes("meta.subtitle is required"), true);
});

test("validates meta.links shape and falls back to the url as label", () => {
  const model = buildModel({
    meta: { title: "T", subtitle: "S", links: [{ url: "https://x" }, { label: "No URL" }, "bad"] },
    sections: [{ id: "a", title: "A" }],
  });
  assert.equal(model.meta.links.length, 1);
  assert.equal(model.meta.links[0].label, "https://x");
  assert.equal(model.errors.includes("meta.links[1] must have a url"), true);
  assert.equal(model.errors.includes("meta.links[2] must be a mapping"), true);
});

test("requires a non-empty sections list", () => {
  const model = buildModel({ meta: { title: "T", subtitle: "S" } });
  assert.equal(model.errors.includes("sections must be a list"), true);
  const empty = buildModel({ meta: { title: "T", subtitle: "S" }, sections: [] });
  assert.equal(empty.errors.includes("sections must not be empty"), true);
});

test("validates section ids and titles", () => {
  const model = buildModel({
    meta: { title: "T", subtitle: "S" },
    sections: [
      { id: "Bad Id", title: "A" },
      { id: "b" },
      { id: "b", title: "Duplicate" },
    ],
  });
  assert.equal(model.errors.includes("sections[0].id must match [a-z0-9-]+"), true);
  assert.equal(model.errors.includes("sections[1].title is required"), true);
  assert.equal(model.errors.includes('duplicate section id "b"'), true);
  assert.equal(model.sections.length, 1);
  assert.equal(model.sections[0].id, "b");
});

test("parses section links and defaults unknown labels to the url", () => {
  const model = buildModel({
    meta: { title: "T", subtitle: "S" },
    sections: [{ id: "a", title: "A", links: [{ url: "https://x" }, { label: "Bad" }] }],
  });
  assert.deepEqual(model.sections[0].links, [{ label: "https://x", url: "https://x" }]);
  assert.equal(model.errors.includes("sections[0].links[1] must have a url"), true);
});

test("rejects a non-mapping root", () => {
  const model = buildModel("nope");
  assert.deepEqual(model.errors, ["content.yml: root must be a mapping"]);
  assert.deepEqual(model.sections, []);
});
