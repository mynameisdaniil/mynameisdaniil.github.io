import test from "node:test";
import assert from "node:assert/strict";
import { buildModel, isValid } from "../js/model.js";

const validRaw = {
  meta: { title: "Daniil Sobol", default_card: "about" },
  cards: [
    {
      id: "about",
      title: "Daniil Sobol",
      body: "Hello.",
      links: [
        { label: "GitHub", url: "https://github.com/mynameisdaniil" },
        { label: "Contact", card: "contact" },
      ],
    },
    {
      id: "contact",
      title: "Contact",
      links: [{ label: "Email", url: "mailto:sobol.daniil@gmail.com" }],
    },
  ],
};

test("builds a valid model and derives edges from internal links", () => {
  const model = buildModel(validRaw);
  assert.deepEqual(model.errors, []);
  assert.equal(model.meta.title, "Daniil Sobol");
  assert.equal(model.meta.defaultCard, "about");
  assert.equal(model.cards.size, 2);
  assert.deepEqual(model.edges, [{ source: "about", target: "contact" }]);
  assert.deepEqual(model.cards.get("about").links[1], { label: "Contact", card: "contact" });
  assert.equal(isValid(model), true);
});

test("flags duplicate ids and keeps the first card", () => {
  const model = buildModel({
    meta: { default_card: "a" },
    cards: [
      { id: "a", title: "A" },
      { id: "a", title: "B" },
    ],
  });
  assert.equal(model.errors.includes('duplicate card id "a"'), true);
  assert.equal(model.cards.get("a").title, "A");
});

test("requires meta.default_card", () => {
  const model = buildModel({ cards: [{ id: "a", title: "A" }] });
  assert.equal(model.errors.includes("meta.default_card is required"), true);
});

test("flags a default_card that does not exist", () => {
  const model = buildModel({ meta: { default_card: "nope" }, cards: [{ id: "a", title: "A" }] });
  assert.equal(model.errors.includes('meta.default_card "nope" does not exist'), true);
});

test("flags unknown internal link targets", () => {
  const model = buildModel({
    meta: { default_card: "a" },
    cards: [{ id: "a", title: "A", links: [{ label: "X", card: "ghost" }] }],
  });
  assert.equal(model.errors.includes('card "a" links to unknown card "ghost"'), true);
  assert.deepEqual(model.edges, []);
});

test("flags links with both url and card, or neither", () => {
  const model = buildModel({
    meta: { default_card: "a" },
    cards: [
      {
        id: "a",
        title: "A",
        links: [
          { label: "Both", url: "https://x", card: "a" },
          { label: "Neither" },
        ],
      },
    ],
  });
  assert.equal(model.errors.includes("cards[0].links[0] must have exactly one of url or card"), true);
  assert.equal(model.errors.includes("cards[0].links[1] must have exactly one of url or card"), true);
});

test("validates card ids and titles", () => {
  const model = buildModel({
    meta: { default_card: "Bad Id" },
    cards: [
      { id: "Bad Id", title: "A" },
      { id: "b" },
    ],
  });
  assert.equal(model.errors.includes("cards[0].id must match [a-z0-9-]+"), true);
  assert.equal(model.errors.includes("cards[1].title is required"), true);
});

test("defaults unknown link labels to the target", () => {
  const model = buildModel({
    meta: { default_card: "a" },
    cards: [{ id: "a", title: "A", links: [{ url: "https://x" }, { card: "a" }] }],
  });
  assert.equal(model.cards.get("a").links[0].label, "https://x");
  assert.equal(model.cards.get("a").links[1].label, "a");
});

test("dedupes bidirectional internal links into one undirected edge", () => {
  const model = buildModel({
    meta: { default_card: "about" },
    cards: [
      { id: "about", title: "About", links: [{ label: "Projects", card: "projects" }] },
      { id: "projects", title: "Projects", links: [{ label: "About", card: "about" }] },
    ],
  });
  assert.deepEqual(model.errors, []);
  assert.equal(model.edges.length, 1);
  assert.deepEqual(model.edges[0], { source: "about", target: "projects" });
});
