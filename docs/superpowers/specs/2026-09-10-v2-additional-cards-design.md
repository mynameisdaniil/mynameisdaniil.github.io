# v2 Additional Cards — Design Addendum

Date: 2026-09-10
Status: Approved (pending written-spec review)
Branch: `v2`
Parent spec: `docs/superpowers/specs/2026-09-10-v2-graph-cards-design.md`

## Goal

Grow `content.yml` from one card to five, migrating the old (master) content, and fix
the one graph behavior the new link structure exposes: bidirectional internal links
render as two exactly-overlapping SVG lines.

## Cards

| id | Title | Body | Links |
|---|---|---|---|
| `about` | Daniil Sobol | Updated intro (no longer "a single vertex") | Projects, Writing, Facts, Contact (internal), GitHub, Email |
| `projects` | Projects | Two paragraphs with the old project descriptions and inline links | ← About |
| `writing` | Writing | Two link paragraphs (the Habr articles) | ← About |
| `facts` | Facts | The 21 old facts, each its own paragraph | ← About |
| `contact` | Contact | One line | ← About; Email, Telegram, GitHub, LinkedIn, Reddit, Stack Overflow, VK, Instagram, Facebook, Twitter |

All cards are English; `lang` is omitted (defaults to `en-us`). The link structure is
hub-both-ways: About links to all four cards and each card links back to About.

## Content details

**`about` body:**

```
I'm a software engineer working on distributed systems, Erlang, and Ethereum infrastructure. This site is a graph of cards — follow the links to look around.
```

**`projects` body** (two paragraphs, inline links; keeps the old descriptions):

```
Erlang client for Ethereum Portal Network: [ePortalD](https://eportald.org/)

Zig (C-compatible) client for Ethereum Portal Network: [libportal](https://libportal.net/)
```

Links: `← About` only (the external URLs live in the body markdown; external links
never create graph edges either way).

**`writing` body** (two link paragraphs):

```
[My take on FP introductory article via Haskell](https://habr.com/ru/post/319440/)

[YAFF — asynchronous stream-oriented programming before it became mainstream](https://habr.com/ru/post/251307/)
```

Links: `← About` only.

**`facts` body** — the 21 facts from `git show master:contents.yaml`, one per
paragraph, with only these corrections: `expirience` → `experience`, `nodejs` →
`Node.js`, `vise versa` → `vice versa`, `lisp` → `Lisp`. Verbatim otherwise:

```
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
```

**`contact` body:** `The best way to contact me is email; I'm also around on these:`

Links (labels in this order):
- `Email` → `mailto:sobol.daniil@gmail.com`
- `Telegram` → https://t.me/mynameisdaniil
- `GitHub` → https://github.com/mynameisdaniil
- `LinkedIn` → https://www.linkedin.com/in/mynameisdaniil/
- `Reddit` → https://www.reddit.com/user/mynameisdaniil
- `Stack Overflow` → https://stackoverflow.com/users/847216/mynameisdaniil
- `VK` → https://vk.com/mynameisdaniil
- `Instagram` → https://instagram.com/daniel_sable/
- `Facebook` → https://www.facebook.com/profile.php?id=100015560405467
- `Twitter` → https://twitter.com/mynameisdaniil

Facts are paragraphs rather than a `<ul>` because `tex-linebreak`/protrusion only
process `.card-body p`; paragraphs keep the LaTeX justification and indent treatment.

## Engine fix: dedupe undirected edges

`buildModel` currently pushes one edge per internal link. With About ↔ card links,
each pair produces two overlapping `<line>` elements; the semi-transparent stroke
composites twice, making hub edges darker than they should be.

- Dedupe by sorted id pair (`[a, b].sort().join("->")`) when deriving edges.
- First encountered direction wins; navigation is unaffected (links still work both ways).
- Unit test: a model with `about → projects` and `projects → about` yields exactly one
  edge, either orientation.
- File: `js/model.js`, `tests/model.test.js`.

## Non-goals

- No new layout/animation behavior; 5 nodes already work.
- No changes to vendor assets, fonts, or the typography pipeline.
- No Russian copy in this pass.

## Verification

1. `npm test` — includes the new dedupe test; all pass.
2. Headless model check on the real `content.yml`: 5 cards, 4 unique edges, 0 errors.
3. Browser pass (human): 5 nodes; focus animation and background blur/scale; active-edge
   opacity uniform (no doubled-dark hub lines); each card's body justified; deep links
   `#projects`, `#writing`, `#facts`, `#contact`; back/forward; Esc returns to About.
