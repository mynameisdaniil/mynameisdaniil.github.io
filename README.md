# mynameisdaniil.dev

Personal site: a whitepaper-style document with justified, hyphenated typography
and a baked-in service worker. Sections and links come from `content.yml`; the
site is plain static files served by GitHub Pages — there is no build step at
deploy time.

## Quick start

```sh
npm install
npm run serve
```

Open http://localhost:8000.

## Editing content

Content lives in `content.yml`; no rebuild is needed after editing it.

```yaml
meta:
  title: Daniil Sobol
  subtitle: distributed systems engineer
  links:
    - label: GitHub
      url: https://github.com/mynameisdaniil
sections:
  - id: projects
    title: Projects
    body: |
      Markdown body. Inline links and paragraphs are supported.
    links: # optional; rendered as a separated row
      - label: ePortalD
        url: https://eportald.org/
    lang: en-us # optional; hyphenation language
```

Rules: `meta.title` and `meta.subtitle` are required; `sections` must be non-empty;
`id` is a lowercase slug and unique; each link requires `label` and `url` (see
`js/model.js` for the full validation). Markdown bodies are trusted and
rendered client-side; smart punctuation turns straight quotes/dashes into
typographic ones at render time.

## Commands

| Command | What it does |
|---|---|
| `npm test` | unit tests (`node --test` + jsdom) |
| `npm run check` | rebuilds generated artifacts and fails if committed copies are stale |
| `npm run build` | full build: fonts → favicon → typeset → app bundle → service worker |
| `npm run serve` | static server on :8000 |

Generated files are committed; after changing sources that feed them, run the
matching build and commit the result (CI enforces this via `npm run check`).

## Deploy

Merge into `master` and push — GitHub Pages serves the repository root and
`CNAME` maps it to `mynameisdaniil.dev`. Keep `CNAME`.

## Project layout

```
index.html            page shell (metadata, fonts, app bundle)
content.yml           site content
404.html              styled not-found page
robots.txt sitemap.xml
js/                   source modules (loaded through the app bundle)
assets/               site.css, fonts, app.min.js, icons, share image
typography/           reusable typeset package (own README)
sw.js                 generated service worker (edit scripts/sw.template.js)
scripts/              font/favicon/typeset/app/sw build steps
tests/                unit tests
docs/superpowers/     specs and implementation plans
NOTICE                third-party licenses
```

## For agents

A machine-readable summary of who this is, what they work on, and how to get in
touch is served at `/llms.txt`; the page also carries JSON-LD `Person` data.
Local agent instructions are not tracked in this repository.

## Analytics

The site uses PostHog Cloud (EU) in cookieless mode: pageviews, referrers,
outbound clicks, scroll depth, and heatmaps — no cookies and no consent banner.
`js/analytics.js` no-ops unless `POSTHOG_KEY` is set and the hostname is
`mynameisdaniil.dev`.

To enable it: create an EU PostHog project, enable **Cookieless** and
**Heatmaps** in project settings, put the `phc_…` key in `POSTHOG_KEY`, then run
`npm run build:app && npm run build:sw` and commit the rebuilt artifacts.

The SDK and capture requests go through `omg.mynameisdaniil.dev`, a PostHog
managed reverse proxy (EU), so ad blockers that target `posthog.com` do not
block analytics. The proxy's CNAME target lives in PostHog's organization proxy
settings; `api_host`/`ui_host`/`POSTHOG_SCRIPT` in `js/analytics.js` must match
that subdomain.
