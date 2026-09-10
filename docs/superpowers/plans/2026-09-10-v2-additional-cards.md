# v2 Additional Cards Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Grow `content.yml` from one card to five (About, Projects, Writing, Facts, Contact) and dedupe bidirectional internal links so hub edges render as single lines.

**Architecture:** Content-only change plus one small pure-logic fix in `buildModel`. No new modules; the existing graph renders 5 nodes unchanged.

**Tech Stack:** YAML content, ES modules, `node:test`, js-yaml devDependency.

**Spec:** `docs/superpowers/specs/2026-09-10-v2-additional-cards-design.md`

## Global Constraints

- Exact card content, labels, and URLs come from the spec; do not paraphrase beyond the listed typo fixes.
- Internal links (`card:`) create graph edges; external links (`url:`) never do.
- Facts render as separate paragraphs (blank lines in the YAML block scalar), not a list.
- Every task ends with a commit; do not commit `node_modules/` or `.superpowers/`.

---

### Task 1: Dedupe undirected edges in `buildModel` (TDD)

**Files:**
- Modify: `js/model.js` (edge derivation loop)
- Test: `tests/model.test.js` (append one test)

**Interfaces:**
- Consumes: `buildModel(raw) → { meta, cards, edges, errors }` (existing).
- Produces: `edges` contains at most one entry per unordered card pair; first encountered direction wins.

- [ ] **Step 1: Write the failing test**

Append to `tests/model.test.js`:

```js
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
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
node --test tests/model.test.js
```

Expected: FAIL — `model.edges.length` is 2, not 1.

- [ ] **Step 3: Implement the dedupe**

In `js/model.js`, replace the edge derivation block:

```js
  for (const card of cards.values()) {
    for (const link of card.links) {
      if (!link.card) continue;
      if (!cards.has(link.card)) {
        errors.push(`card "${card.id}" links to unknown card "${link.card}"`);
      } else {
        edges.push({ source: card.id, target: link.card });
      }
    }
  }
```

with:

```js
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
```

- [ ] **Step 4: Run the full suite to verify it passes**

```bash
npm test
```

Expected: all tests PASS (the pre-existing edge tests are one-directional and unaffected).

- [ ] **Step 5: Commit**

```bash
git add js/model.js tests/model.test.js
git commit -m "fix: dedupe undirected edges in content model"
```

---

### Task 2: Five cards in `content.yml`

**Files:**
- Modify: `content.yml` (replace entirely with the YAML below)

**Interfaces:**
- Consumes: `buildModel` validation and the graph engine.
- Produces: 5 cards, 4 unique edges (`about` ↔ each other card), `default_card: about`.

- [ ] **Step 1: Replace `content.yml` with exactly:**

```yaml
meta:
  title: Daniil Sobol
  default_card: about
cards:
  - id: about
    title: Daniil Sobol
    body: |
      I'm a software engineer working on distributed systems, Erlang, and Ethereum infrastructure. This site is a graph of cards — follow the links to look around.
    links:
      - label: Projects
        card: projects
      - label: Writing
        card: writing
      - label: Facts
        card: facts
      - label: Contact
        card: contact
      - label: GitHub
        url: https://github.com/mynameisdaniil
      - label: Email
        url: mailto:sobol.daniil@gmail.com
  - id: projects
    title: Projects
    body: |
      Erlang client for Ethereum Portal Network: [ePortalD](https://eportald.org/)

      Zig (C-compatible) client for Ethereum Portal Network: [libportal](https://libportal.net/)
    links:
      - label: ← About
        card: about
  - id: writing
    title: Writing
    body: |
      [My take on FP introductory article via Haskell](https://habr.com/ru/post/319440/)

      [YAFF — asynchronous stream-oriented programming before it became mainstream](https://habr.com/ru/post/251307/)
    links:
      - label: ← About
        card: about
  - id: facts
    title: Facts
    body: |
      Master's degree in signal processing

      Full-time Linux user since Ubuntu 8.04

      Have experience in nginx module writing

      I have /dev/ops which is a recursive link to myself

      Have experience in Node.js native module writing

      Do love syntax of Erlang

      My other CAP is a theorem

      I do understand Riak's code

      I have lost my ability to explain monads

      Can't decide if I dislike Java more than C++ or vice versa

      Have written this line at 3:43 in the morning

      Wrote several Lisp interpreters

      I've used Ansible back then before it became mainstream

      Containers pioneer (and I'm not talking about Docker)

      Can not advise you any continuous integration server

      Have written this line (and all other lines) using VIM

      RPC? Ask me how

      Do hate DNS

      My first language was BASIC; written on a piece of paper and interpreted in my head

      Stock exchange? Ask me how

      Actually I don't have any cool facts about Rust language. Guess this is a fact too
    links:
      - label: ← About
        card: about
  - id: contact
    title: Contact
    body: |
      The best way to contact me is email; I'm also around on these:
    links:
      - label: Email
        url: mailto:sobol.daniil@gmail.com
      - label: Telegram
        url: https://t.me/mynameisdaniil
      - label: GitHub
        url: https://github.com/mynameisdaniil
      - label: LinkedIn
        url: https://www.linkedin.com/in/mynameisdaniil/
      - label: Reddit
        url: https://www.reddit.com/user/mynameisdaniil
      - label: Stack Overflow
        url: https://stackoverflow.com/users/847216/mynameisdaniil
      - label: VK
        url: https://vk.com/mynameisdaniil
      - label: Instagram
        url: https://instagram.com/daniel_sable/
      - label: Facebook
        url: https://www.facebook.com/profile.php?id=100015560405467
      - label: Twitter
        url: https://twitter.com/mynameisdaniil
      - label: ← About
        card: about
```

- [ ] **Step 2: Verify the model headlessly**

```bash
node --input-type=module -e "
import { readFileSync } from 'node:fs';
import yaml from 'js-yaml';
import { buildModel } from './js/model.js';
const model = buildModel(yaml.load(readFileSync('content.yml', 'utf8')));
if (model.errors.length) { console.error(model.errors); process.exit(1); }
console.log('cards:', model.cards.size);
console.log('edges:', model.edges.length, model.edges.map((e) => e.source + '->' + e.target).join(', '));
console.log('default:', model.meta.defaultCard);
"
```

Expected:

```
cards: 5
edges: 4 about->projects, about->writing, about->facts, about->contact
default: about
```

- [ ] **Step 3: Run the full suite**

```bash
npm test
```

Expected: all tests PASS.

- [ ] **Step 4: Commit**

```bash
git add content.yml
git commit -m "content: add projects, writing, facts, and contact cards"
```

---

## Verification (after both tasks)

- `npm test` passes.
- Headless model check above prints 5 cards / 4 edges / default about.
- Human browser pass: five nodes; hub edges uniformly translucent (no doubled-dark
  lines); focus animation and background blur/scale; justified bodies; deep links
  `#projects`, `#writing`, `#facts`, `#contact`; back/forward; Esc returns to About.
