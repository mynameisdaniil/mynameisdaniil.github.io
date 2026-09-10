import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import { createCard, setCardState, setCardBody, markActiveLinks, createErrorCard } from "../js/card.js";

function setup() {
  const dom = new JSDOM("<div id='root'></div>");
  globalThis.document = dom.window.document;
  globalThis.Node = dom.window.Node;
  return dom;
}

const card = {
  id: "about",
  title: "Daniil Sobol",
  body: "Hello.",
  links: [
    { label: "GitHub", url: "https://github.com/mynameisdaniil" },
    { label: "Contact", card: "contact" },
  ],
};

test("createCard builds the structure and links", () => {
  setup();
  const el = createCard(card, { onSelect: () => {} });

  assert.equal(el.tagName, "ARTICLE");
  assert.equal(el.dataset.cardId, "about");
  assert.equal(el.classList.contains("card--background"), true);
  assert.equal(el.querySelector(".card-title").textContent, "Daniil Sobol");
  assert.equal(el.querySelector(".card-title").tabIndex, -1);

  const [github, contact] = el.querySelectorAll(".card-links a");
  assert.equal(github.getAttribute("href"), "https://github.com/mynameisdaniil");
  assert.equal(github.getAttribute("target"), "_blank");
  assert.equal(github.getAttribute("rel"), "noopener noreferrer");
  assert.equal(contact.getAttribute("href"), "#contact");
  assert.equal(contact.dataset.internal, "true");
});

test("clicking a background card selects it, links are left alone", () => {
  const dom = setup();
  const selected = [];
  const el = createCard(card, { onSelect: (id) => selected.push(id) });
  dom.window.document.body.appendChild(el);

  el.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true }));
  assert.deepEqual(selected, ["about"]);

  const contact = el.querySelector("a[data-internal]");
  contact.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true, cancelable: true }));
  assert.deepEqual(selected, ["about"]);

  const github = el.querySelector('a[target="_blank"]');
  github.addEventListener("click", (event) => event.preventDefault());
  github.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true, cancelable: true }));
  assert.deepEqual(selected, ["about"]);
});

test("Enter and Space select a card", () => {
  const dom = setup();
  const selected = [];
  const el = createCard(card, { onSelect: (id) => selected.push(id) });
  el.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
  el.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: " ", bubbles: true }));
  assert.deepEqual(selected, ["about", "about"]);
});

test("setCardState toggles focused/background classes", () => {
  setup();
  const el = createCard(card, { onSelect: () => {} });
  setCardState(el, "focused");
  assert.equal(el.classList.contains("card--focused"), true);
  assert.equal(el.classList.contains("card--background"), false);
  setCardState(el, "background");
  assert.equal(el.classList.contains("card--focused"), false);
  assert.equal(el.classList.contains("card--background"), true);
});

test("setCardBody renders once and unhides", () => {
  setup();
  const el = createCard(card, { onSelect: () => {} });
  setCardBody(el, "<p>one</p>");
  setCardBody(el, "<p>two</p>");
  const body = el.querySelector(".card-body");
  assert.equal(body.hidden, false);
  assert.equal(body.innerHTML, "<p>one</p>");
});

test("markActiveLinks sets aria-current on the matching internal link", () => {
  setup();
  const el = createCard(card, { onSelect: () => {} });
  markActiveLinks(el, "contact");
  assert.equal(el.querySelector("a[data-internal]").getAttribute("aria-current"), "page");
  markActiveLinks(el, "about");
  assert.equal(el.querySelector("a[data-internal]").hasAttribute("aria-current"), false);
});

test("createErrorCard shows the message and details", () => {
  setup();
  const el = createErrorCard("Could not load content.", ["first problem", "second problem"]);
  assert.equal(el.classList.contains("card--error"), true);
  assert.match(el.textContent, /Could not load content\./);
  assert.match(el.textContent, /first problem/);
  assert.equal(el.querySelectorAll("li").length, 2);
});
