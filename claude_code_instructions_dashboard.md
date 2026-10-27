# WUE Conditions Explorer Dashboard: Instructions for Claude Code (Stub)

> **Status: first release decided (2026-10-26).** The decisions are recorded in section 10. Items marked "adopted default" were proposed by Claude Code and accepted without objection, and they can be overruled at any time. The original **To decide** wording is kept below each item for the record.

## 1. Goal and audience

The dashboard lets a user explore water use efficiency conditions across the five fictional retailers in the synthetic SCUBA dataset, and it shows how climate, demographics, rates and turf rebates relate to residential water use. It is built entirely on the synthetic data produced by `claude_code_instructions_synthetic_scuba.md`, so it can be shared and demonstrated without any privacy review.

**To decide.** We need to name the primary audience, which could be utility conservation staff, Metropolitan Water District program managers, analysts testing the M123 pipeline, or a general public audience. We also need to state the two or three questions the dashboard must answer in under a minute, since those questions will drive the layout.

## 2. Data inputs

The dashboard reads the observed tables under `synthetic_scuba/data/`, which are `usage.parquet`, `customers.csv`, `rebates.csv`, `acs_block_groups.csv`, `supplier_report.csv`, `climate_et_precip.csv` and the rate structures in `rates_owrs/`. It also reads the matched difference-in-differences savings estimates under `synthetic_scuba/estimates/`, which are computed from the observed tables alone. It never edits those files. Any aggregation it needs is computed by a separate preparation step described in section 5.

The `synthetic_scuba/truth/` folder holds the answer key, which includes the planted savings, latent attitudes and potential outcomes. These are quantities known to the generator because it planted them, not knowledge about real households, and a realistic dashboard would not have access to them.

**To decide.** We need to decide whether the dashboard ignores the truth folder entirely, or whether it offers a clearly labeled "answer key" mode that compares estimated savings with planted savings for teaching and validation purposes.

## 3. Guardrails

Every page carries a visible banner stating that all data is simulated and must not be mistaken for real utility records. The dashboard uses only the five fictional retailer names, and it shows no real utility names, addresses or parcel numbers. Individual account rows are never displayed alone, even though they are synthetic, because the dashboard should model the privacy habits that a real-data version would need.

**To decide.** We need to set a minimum group size for any aggregate shown (for example, no cell built from fewer than 10 accounts), and to decide whether this rule applies to the synthetic build or only to a future real-data version.

## 4. Candidate views

The list below is a starting menu rather than a commitment. We will choose, cut and order these views once we have looked at the data.

- A retailer overview shows accounts, single family share, median irrigable area, median income and billing frequency for each retailer.
- A climate panel shows monthly ET0 and precipitation by retailer, and it highlights the drought years from 2012 to 2015.
- A usage panel shows seasonal usage profiles by retailer and customer class, and it makes the coastal-versus-inland contrast and the 2015 drought drop visible.
- An efficiency panel shows usage against the indoor and outdoor water budget, using the pre-removal efficiency measure from the paper.
- A supplier panel shows gallons per capita per day, percent reduction relative to 2013, and enforcement counts over time.
- A rebate panel shows uptake over time and across geography, along with turf square footage removed and rebate dollars paid.
- A savings panel shows the matched difference-in-differences estimates of turf savings, with treated households against their matched controls, and, if answer key mode is adopted, it compares those estimates with the planted effect.
- A data quality panel shows missing rows, vacancy gaps, defaulted irrigable area and turnover, so users can see what the observation layer did.

**To decide.** We need to choose the views for the first release, the default filters (retailer, zip code, customer class and date range), and whether the geography uses a synthetic map of the fictional block group grid or stays non-spatial.

## 5. Data preparation layer

A preparation script reads the observed tables and writes small, pre-aggregated files that the dashboard loads directly. Keeping this step separate keeps the dashboard fast and makes every displayed number traceable to a script. The preparation step uses the same language and lockfile as the generator.

**To decide.** We need to settle the aggregation grain (retailer-month, zip-month or block-group-month) and the file format for the prepared data.

## 6. Technology

**To decide.** We need to choose the stack. The main candidates are listed below, and we will pick one after the data exists and the audience is settled.

- A static HTML page with embedded or bundled data is the easiest to share and host, and it needs no server.
- R Shiny or Quarto dashboards fit the R generator and the CaDC tool ecosystem, but Shiny needs a running server.
- Streamlit or Dash fit a Python fallback, and they also need a running server.
- Observable Framework produces a static site with strong charting, at the cost of a JavaScript build step.

## 7. Design

Charts follow accessible color practice, with colorblind-safe palettes, direct labels where possible, and a layout that works on a laptop and a phone. Units are stated on every axis, using CCF for account usage, gallons per capita per day for supplier figures, inches for ET and precipitation, and square feet for area.

**To decide.** We need to decide whether to apply Metropolitan Water District brand styling, California Data Collaborative styling, or a neutral style.

## 8. Validation

Every number on the dashboard must reconcile with the prepared data, and the prepared data must reconcile with the observed tables. A test script recomputes a handful of headline figures directly from the observed tables and compares them with what the dashboard displays.

**To decide.** We need to decide whether to add automated screenshot tests with Playwright, which is available in the cloud environment.

## 9. Output layout

**Decided (2026-10-26).** The preparation script lives in `dashboard/prep/` and writes its outputs to `dashboard/app/data/`, the static application lives in `dashboard/app/`, the tests in `dashboard/tests/`, and the Cloudflare configuration in `dashboard/wrangler.jsonc`, with a `dashboard/README.md` that repeats the synthetic data banner.

*Original item:* A tentative layout places the preparation script and its outputs under `dashboard/prep/`, the application under `dashboard/app/`, and the tests under `dashboard/tests/`, with a `dashboard/README.md` that repeats the synthetic data banner. We will confirm this once the stack is chosen.

## 10. Open questions log

We will record decisions here as we make them, with the date and a one-sentence rationale for each.

| Date | Decision | Rationale |
|---|---|---|
| 2026-10-26 | Styling is neutral, not Metropolitan or CaDC branded (user decision). | Brand styling on synthetic data could be mistaken for real agency results once the page is shared. |
| 2026-10-26 | The site is public, hosted on a `workers.dev` address with Cloudflare Workers static assets (user decision). | The data is synthetic and needs no privacy review, and the free `workers.dev` subdomain avoids DNS setup. |
| 2026-10-26 | Answer key mode ships, off by default and clearly labeled (user decision). | Comparing matched DiD estimates with the planted effect is the dataset's main teaching value, and it must never be mistaken for real-world truth. |
| 2026-10-26 | Primary audience: conservation program managers at Metropolitan and its member agencies, then analysts testing the estimator (adopted default). | They need to read results quickly, while analysts can drill into the report. |
| 2026-10-26 | The three questions: why residential use differs across retailers, what turf rebates saved and how sure we can be, and how the 2014 and 2015 conservation shows up in the data (adopted default). | These cover the generator's main structure and the estimator's main result. |
| 2026-10-26 | First release has four views (overview, usage and climate, rebates and savings, data quality) plus an about page; the efficiency and supplier panels are folded into usage and climate or deferred (adopted default). | Four views answer the three questions without crowding. |
| 2026-10-26 | Filters are one row above the content: retailer (all or one) and the answer key toggle. A date range filter is deferred (adopted default). | The panel is short (76 months) and one retailer filter scopes every view. |
| 2026-10-26 | No map; the fictional coordinates sit near latitude 0, longitude 0, so the views stay non-spatial (adopted default). | A basemap would place the retailers in the ocean and add nothing. |
| 2026-10-26 | Minimum cell size of 10 accounts or households, applied to the synthetic build too; household-level rows are never shown, and distributions are shown as quantiles (adopted default). | The dashboard should model the privacy habits a real-data version needs. |
| 2026-10-26 | Prepared data is JSON at retailer-month grain, written by `dashboard/prep/prepare_data.R` with the generator's R lockfile; answer key data lives in a separate file loaded only when the mode is on (adopted default). | Small files load fast, every number traces to one script, and the answer key stays separable. |
| 2026-10-26 | Stack: a static HTML page with Observable Plot and D3 vendored into the repository, no build step, deployed with `wrangler` from GitHub Actions (adopted default). | It needs no server, matches Workers static assets, and keeps deploys to one command. |
| 2026-10-26 | Tests: an R reconciliation script and Playwright smoke tests (banner on every view, answer key off by default, no horizontal scroll at phone width, no console errors) run in CI before deploy (adopted default). | Playwright is available, and the checks guard the guardrails. |

## 11. Definition of done

**To decide.** A draft definition is that the dashboard builds from a clean checkout with one command, reads only the prepared data, shows the synthetic data banner on every page, passes the reconciliation tests, and answers the questions named in section 1.
