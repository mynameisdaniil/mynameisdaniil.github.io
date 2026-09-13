# Cookieless PostHog Analytics — Design

Date: 2026-09-13
Status: Approved (pending written-spec review)
Branch: `master`

## Goal

Add privacy-friendly analytics to the site: pageviews, referrers, outbound-link
clicks, scroll depth, and click/scroll heatmaps — with **no cookies, no consent
banner, and no self-hosted infrastructure**.

## Non-Goals

- No session replay (incompatible with `cookieless_mode: "always"`; would require
  cookie-based consent, which was explicitly rejected).
- No Google Analytics / Clarity / Umami / Matomo / self-hosting.
- No consent banner or consent UI of any kind.
- No CSP changes, no new `<script>` tags in `index.html`, no changes to
  `content.yml` or the service worker logic.
- No tracking on localhost or non-production previews.

## Provider Decision

PostHog Cloud, **EU region**, free tier (1M events/month; heatmap data does not
count toward billing). The SDK is ~95KB gzip and is deliberately loaded *after*
the page is interactive, so the reading/typography experience never waits on it.
Cookieless mode makes the SDK set no cookies and no storage; user identity is a
privacy-preserving hash computed server-side. `respect_dnt` is enabled.

## Architecture

- `js/analytics.js` (new): the only new module. Exports
  `initAnalytics(deps) → void`; encapsulates script loading, PostHog init, and
  event wiring. All environment access is injectable for tests.
- `js/main.js`: calls `initAnalytics()` once at boot; does not await it.
- `assets/app.min.js`: rebuilt (bundles the module); `sw.js`: restamped because the
  shell asset hash changes. `index.html` is untouched.
- Service worker: PostHog requests are cross-origin, so the existing `fetch`
  handler ignores them. Nothing is precached; offline pages simply send nothing.

## Loading Strategy

1. `initAnalytics` returns immediately unless
   `win.location.hostname === "mynameisdaniil.dev"` **and** `POSTHOG_KEY` is set.
2. On `window.load`, schedule the loader via `requestIdleCallback` (with a
   `setTimeout` fallback); tests inject the scheduler.
3. The loader injects
   `https://eu-assets.i.posthog.com/static/array.js` as an async script (the
   standard PostHog snippet pattern with a `posthog` queue stub).
4. Any loader failure (adblocker, network) is caught silently; the site behaves
   identically without analytics.

## PostHog Configuration

```js
posthog.init(POSTHOG_KEY, {
  api_host: "https://eu.i.posthog.com",
  cookieless_mode: "always",
  enable_heatmaps: true,
  autocapture: true,
  capture_pageview: true,
  capture_pageleave: true,
  disable_session_recording: true,
  disable_surveys: true,
  respect_dnt: true,
});
```

`POSTHOG_KEY` is a module constant (`phc_…`). PostHog project settings must also
have **Cookieless** and **Heatmaps** enabled, otherwise cookieless events are
ignored server-side (documented PostHog requirement).

## Events

| Event | Source | Properties |
| --- | --- | --- |
| `$pageview` | PostHog (auto) | default |
| `$pageleave` | PostHog (auto, needed for scrollmap) | default |
| click/scroll heatmap data | PostHog (`enable_heatmaps` + autocapture) | default |
| `outbound_click` | delegated `click` listener on `document` | `href`, `label` (anchor text, capped at 200 chars), `section` (nearest `section[id]`, else `masthead`) |
| `scroll_depth` | passive `scroll` listener, rAF-throttled | `percent`: one event each at 25, 50, 75, 100 |

`outbound_click` covers all anchors: external links, social profiles, `mailto:`,
and internal deck links (`/slides/…`), so "which links do people open" is
answerable. Anchors whose `href` is a same-page `#` fragment (e.g. the skip link)
are ignored. `scroll_depth` fires at most once per threshold per page load;
completion is `(scrollY + innerHeight) / documentElement.scrollHeight`.

## Privacy

- `cookieless_mode: "always"` → no cookies, no `localStorage`/`sessionStorage`.
- `respect_dnt: true` → DNT visitors are not tracked.
- No personal data, no cross-site identifiers, no consent banner required.
- Data is stored in PostHog's EU region.

## Testing

`tests/analytics.test.js` (new, `node:test` + jsdom), with injected
window/document/scheduler/loader:

1. No-op when hostname is not `mynameisdaniil.dev`.
2. No-op when `POSTHOG_KEY` is empty.
3. Loader is scheduled after `load` (and not before).
4. Init receives the exact config above (`cookieless_mode`, heatmaps, pageleave,
   recording/surveys disabled, DNT).
5. `outbound_click` fires with `href`/`label`/`section` for a link inside a
   section, and with `section: "masthead"` for a masthead link.
6. `scroll_depth` fires once per threshold with the right `percent` values.
7. A throwing loader does not propagate.

Existing 39 tests stay green. `npm run check` is run after the artifact rebuild.

## Manual Setup (one-time, user)

1. Create a PostHog account on the EU cloud (`eu.posthog.com`) and a project for
   the site.
2. In project settings enable **Cookieless** and **Heatmaps**.
3. Copy the project API key (`phc_…`) into `POSTHOG_KEY` in `js/analytics.js`.
4. Rebuild (`npm run build:app`, `npm run build:sw`), commit, push.

Until the key is set, the module no-ops; deployment is safe.

## Verification

1. `npm test` and `npm run check` pass.
2. On production: PostHog shows `$pageview`, `$pageleave`, `outbound_click`, and
   `scroll_depth` events; heatmaps render for `/`.
3. `document.cookie` and `localStorage` remain empty after a visit.
4. Hard reload timing shows the site interactive before PostHog loads
   (analytics scheduled post-`load`).

## Definition of Done

- With `POSTHOG_KEY` set and deployed: pageviews, referrers, outbound clicks,
  scroll depth, and click/scroll heatmaps are visible in PostHog EU.
- No cookies or storage are set on production; no banner exists.
- No measurable impact on the site's first paint or typesetting.
- Tests and `npm run check` pass.
