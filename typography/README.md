# typeset

Self-contained browser typography for justified prose:

- Knuth-Plass line breaking (TeX algorithm) via `tex-linebreak`
- Hyphenation via Hypher, with `en-us` and `ru` patterns bundled
- Hanging punctuation (character protrusion) at line edges
- Smart punctuation (`'` → `’`, paired `"` → `“ ”`, `...` → `…`, `--` → `–`, `---` → `—`)

It does not ship fonts, sizes, spacing, or layout — bring your own styles.

## Quick start

```html
<link rel="stylesheet" href="your-styles.css" />
<script src="dist/typeset.min.js"></script>
<script>
  typeset.typeset(document.body);
</script>
```

Or as a module (bundlers or `<script type="module">`):

```js
import { typeset } from "./typography/dist/typeset.esm.js";

const handle = typeset(document.querySelector("main"));
```

The first run happens automatically after `document.fonts.ready`; resizing
re-runs after 150 ms.

## Options

| option | default | meaning |
|---|---|---|
| `selector` | `"p"` | elements to justify inside the root |
| `lang` | `"en-us"` | fallback language; nearest ancestor `[lang]` wins |
| `hyphenate` | `true` | pass a Hypher hyphenator to the line breaker |
| `justify` | `true` | run Knuth-Plass justification |
| `protrude` | `true` | hanging punctuation at line edges |
| `punctuation` | `true` | smart quotes/dashes/ellipses in text nodes |
| `onResize` | `true` | debounced re-run on window resize |
| `awaitFonts` | `true` | wait for `document.fonts.ready` |
| `injectStyles` | `true` | inject `.protrude-left/right { display: inline }` |
| `resizeDelay` | `150` | resize debounce in ms |

Returns `{ typeset(el), refresh(), destroy() }`.

## Low-level API

`justify(elements, hyphenate)`, `unjustify(elements)`, `applyProtrusion(root)`,
`clearProtrusion(root)`, `protrusionFor(char, side)`, `smartPunctuation(text)`,
`applySmartPunctuation(root)`, `hyphenateFor(lang)`, `createHyphenator(patterns)`,
`patterns`.

## Font requirements

Use a serif/sans face with real regular/italic/bold faces, loaded before the first
run (`awaitFonts` handles this). **Never set `font-kerning` or `font-variant-*` on
text that is measured** — Chrome then returns an empty computed `font` shorthand and
the canvas measurement silently falls back to 10px sans-serif, producing wrong line
breaks. The package warns when this happens.

## Rebuilding

```sh
npm run build:typeset
```

Bundled third-party licenses: see `../NOTICE`.
