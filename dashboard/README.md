# WUE Conditions Explorer dashboard

> **ALL DATA ON THIS SITE IS SIMULATED.** It comes from the synthetic SCUBA generator in `../synthetic_scuba/` and
> must not be mistaken for real utility records. The retailers are fictional, and the planted effects shown in answer
> key mode are assumptions written into the generator, not knowledge about real households.

A static dashboard (HTML, CSS and JavaScript with Observable Plot and D3 vendored in `app/vendor/`) with no build
step. It has four views (overview, usage and climate, rebates and savings, data quality) plus an about page, one filter
row (retailer and the answer key toggle), neutral styling in light and dark themes, and a table view behind every
chart. Answer key mode is off by default; `app/data/answer_key.json` is fetched only when a viewer turns it on. No value
is built from fewer than 10 accounts or households, and no individual account is shown.

## Updating the data

```
Rscript dashboard/prep/prepare_data.R      # rewrites dashboard/app/data/*.json from synthetic_scuba/
Rscript dashboard/tests/reconcile.R        # recomputes headline figures from the source tables and compares
```

The prep script uses the generator's R lockfile (`synthetic_scuba/renv.lock`). Only `answer_key.json` reads the
generator's planted effects (`synthetic_scuba/truth/` and `synthetic_scuba/reports/`); every other file comes from the
observed tables and the matched DiD estimates.

## Previewing and testing

```
cd dashboard
npm ci
npm run serve              # http://localhost:8788
npm test                   # Playwright smoke tests on desktop and phone widths
npx wrangler dev           # serves app/ the way Cloudflare will, including the _headers security policy
```

The smoke tests check that every view shows the simulated-data banner, that answer key mode is off by default and its
data is not fetched until requested, that no view scrolls horizontally at 375 pixels, that there are no console errors,
and that every chart has a table view.

## Deploying to Cloudflare (workers.dev)

The site is an assets-only Cloudflare Worker (`wrangler.jsonc`), served at
`https://wue-conditions-explorer.<your-subdomain>.workers.dev`. One-time setup:

1. In the Cloudflare dashboard, open **Workers & Pages** once so the account gets its `workers.dev` subdomain.
2. Create an API token from the **Edit Cloudflare Workers** template, and copy the **Account ID** from the account
   overview page.
3. In GitHub, add both as repository secrets named `CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID`
   (Settings, Secrets and variables, Actions).

After that, every merge to `main` that touches `dashboard/` runs the tests and deploys
(`.github/workflows/dashboard.yml`); the workflow can also be started by hand. To deploy from a laptop instead, run
`npx wrangler login` and then `npx wrangler deploy` in this folder.
