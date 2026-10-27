// WUE Conditions Explorer. ALL DATA IS SIMULATED.
// Static app: loads prepared JSON from data/, renders with Observable Plot, and loads answer_key.json only when
// answer key mode is turned on. Every chart carries a legend (two or more series) and a table view.
"use strict";

const state = { view: "overview", retailer: "All retailers", answerKey: false, data: {}, key: null };
const ALL = "All retailers";
const FONT = 'system-ui, -apple-system, "Segoe UI", sans-serif';
const fmt0 = d3.format(",.0f"), fmt1 = d3.format(",.1f"), fmt2 = d3.format(",.2f");
const fmtPct = d3.format(".0%"), fmtSignedPct = d3.format("+.0%");
const parseMonth = d3.utcParse("%Y-%m");
const monthName = d3.utcFormat("%b %Y");
const MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

const css = (name) => getComputedStyle(document.documentElement).getPropertyValue(name).trim();
const retailers = () => state.data.meta.retailers;
const colorFor = (r) => (r === ALL ? css("--all") : css(`--series-${retailers().indexOf(r) + 1}`));
const ESTIMATE = () => css("--series-1"), PLANTED = () => css("--series-2"), PLACEBO = () => css("--series-3");
const shown = () => (state.retailer === ALL ? retailers() : [state.retailer]);

function el(tag, attrs = {}, ...children) {
  const n = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === "class") n.className = v;
    else if (k === "text") n.textContent = v;
    else n.setAttribute(k, v);
  }
  for (const c of children) if (c != null) n.append(c);
  return n;
}

// ---- Data loading ------------------------------------------------------------------------------------------------
async function loadJSON(name) {
  const r = await fetch(`data/${name}`);
  if (!r.ok) throw new Error(`Could not load ${name}`);
  return r.json();
}
async function loadAll() {
  const names = ["meta", "retailers", "usage_monthly", "climate_monthly", "supplier_monthly", "rebates", "savings", "quality"];
  const parts = await Promise.all(names.map((n) => loadJSON(`${n}.json`)));
  names.forEach((n, i) => (state.data[n] = parts[i]));
  for (const d of state.data.usage_monthly) d.date = parseMonth(d.month);
  for (const d of state.data.climate_monthly) d.date = parseMonth(d.month);
  for (const d of state.data.supplier_monthly) d.date = parseMonth(d.month);
  for (const d of state.data.quality.coverage) d.date = parseMonth(d.month);
}
async function ensureKey() {
  if (!state.key) state.key = await loadJSON("answer_key.json");
  return state.key;
}

// ---- Shared chart pieces -----------------------------------------------------------------------------------------
function baseOptions(width, height, extra = {}) {
  return {
    width, height, marginLeft: 56, marginRight: 20, marginTop: 16, marginBottom: 36,
    style: { background: "transparent", color: css("--text-2"), fontFamily: FONT, fontSize: "12px", overflow: "visible" },
    ...extra,
  };
}
const gridY = () => Plot.gridY({ stroke: css("--grid"), strokeOpacity: 1 });
const gridX = () => Plot.gridX({ stroke: css("--grid"), strokeOpacity: 1 });

// A chart card: title, subtitle, legend, plot, optional note, and a table view behind a toggle.
function chartCard(container, { title, subtitle, legend, render, table, note, height = 300 }) {
  container.replaceChildren();
  container.append(el("h3", { text: title }));
  if (subtitle) container.append(el("p", { class: "subtitle", text: subtitle }));
  if (legend && legend.length > 1) {
    const lg = el("div", { class: "legend", "aria-hidden": "true" });
    for (const item of legend) {
      const key = el("i", { class: item.shape || "" });
      key.style.background = item.color;
      lg.append(el("span", {}, key, document.createTextNode(item.label)));
    }
    container.append(lg);
  }
  const holder = el("div", { class: "plot" });
  container.append(holder);
  const width = Math.max(280, container.clientWidth - 32);
  const fig = render(width, height);
  if (fig) holder.append(fig);
  if (note) container.append(el("p", { class: "note", text: note }));
  if (table) {
    const btn = el("button", { class: "table-toggle", type: "button", "aria-expanded": "false", text: "Show table" });
    const wrap = el("div", { class: "table-wrap", hidden: "" });
    const t = el("table");
    const thead = el("thead"), tr = el("tr");
    for (const c of table.columns) tr.append(el("th", { scope: "col", text: c }));
    thead.append(tr);
    const tbody = el("tbody");
    for (const row of table.rows) {
      const r = el("tr");
      for (const v of row) r.append(el("td", { text: v == null ? "suppressed" : String(v) }));
      tbody.append(r);
    }
    t.append(thead, tbody);
    if (table.caption) t.prepend(el("caption", { class: "sr-only", text: table.caption }));
    wrap.append(t);
    btn.addEventListener("click", () => {
      const open = wrap.hidden;
      wrap.hidden = !open;
      btn.setAttribute("aria-expanded", String(open));
      btn.textContent = open ? "Hide table" : "Show table";
    });
    container.append(btn, wrap);
  }
}

// Crosshair tooltip for time series: snaps to the nearest date, lists every series, works with the keyboard.
const tooltip = () => document.getElementById("tooltip");
function showTooltip(x, y, title, rows) {
  const t = tooltip();
  t.replaceChildren(el("div", { class: "tt-title", text: title }));
  for (const r of rows) {
    const key = el("i"); key.style.background = r.color;
    t.append(el("div", { class: "tt-row" }, el("b", { text: r.value }), key, document.createTextNode(r.label)));
  }
  t.hidden = false;
  const pad = 14, w = t.offsetWidth, h = t.offsetHeight;
  t.style.left = `${Math.min(window.innerWidth - w - 8, x + pad)}px`;
  t.style.top = `${Math.max(8, Math.min(window.innerHeight - h - 8, y - h / 2))}px`;
}
const hideTooltip = () => (tooltip().hidden = true);

function crosshair(svg, dates, rowsAt, fmtDate = monthName) {
  const x = svg.scale("x");
  const opts = { top: 16, bottom: 36 };
  const H = +svg.getAttribute("height");
  const W = +svg.getAttribute("width");
  const line = document.createElementNS("http://www.w3.org/2000/svg", "line");
  line.setAttribute("stroke", css("--muted"));
  line.setAttribute("stroke-width", "1");
  line.setAttribute("y1", opts.top);
  line.setAttribute("y2", H - opts.bottom);
  line.style.display = "none";
  svg.append(line);
  svg.setAttribute("tabindex", "0");
  svg.style.cursor = "crosshair";
  let idx = dates.length - 1;
  const place = (i, clientX, clientY) => {
    idx = Math.max(0, Math.min(dates.length - 1, i));
    const d = dates[idx], px = x.apply(d);
    line.setAttribute("x1", px); line.setAttribute("x2", px);
    line.style.display = "";
    const rect = svg.getBoundingClientRect();
    const sx = clientX ?? rect.left + (px * rect.width) / W;
    const sy = clientY ?? rect.top + rect.height / 3;
    showTooltip(sx, sy, fmtDate(d), rowsAt(d));
  };
  svg.addEventListener("pointermove", (e) => {
    const rect = svg.getBoundingClientRect();
    const sx = ((e.clientX - rect.left) * W) / rect.width;
    const t = x.invert(sx);
    const i = d3.bisector((d) => d).center(dates, t);
    place(i, e.clientX, e.clientY);
  });
  svg.addEventListener("pointerleave", () => { line.style.display = "none"; hideTooltip(); });
  svg.addEventListener("focus", () => place(idx));
  svg.addEventListener("blur", () => { line.style.display = "none"; hideTooltip(); });
  svg.addEventListener("keydown", (e) => {
    if (e.key === "ArrowRight") { place(idx + 1); e.preventDefault(); }
    if (e.key === "ArrowLeft") { place(idx - 1); e.preventDefault(); }
  });
}

// Horizontal category charts: on narrow screens the category labels move above each row instead of a wide margin.
const NARROW = 560;
function catLayout(w, labelMargin, rows, yField) {
  if (w >= NARROW) return { marginLeft: labelMargin, y: {}, labels: [] };
  return { marginLeft: 12, y: { axis: null }, labels: [Plot.text(rows, { y: yField, text: yField, frameAnchor: "left", dy: -17, fill: css("--text-2"), fontSize: 11 })] };
}
const rowHeight = (w, n, base = 36) => n * (w < NARROW ? base + 16 : base) + 60;
const monthTicks = (w) => (w < NARROW ? [1, 3, 5, 7, 9, 11] : d3.range(1, 13));

function tipMark(data, options) {
  return Plot.tip(data, Plot.pointer({ ...options, fill: css("--surface"), stroke: css("--axis"), fontSize: 12 }));
}

// Conservation periods shaded behind time series.
function periods(y1, y2) {
  const p = [
    { start: parseMonth("2014-02"), end: parseMonth("2015-04"), label: "Voluntary call" },
    { start: parseMonth("2015-04"), end: parseMonth("2016-05"), label: "Mandatory" },
  ];
  return [
    Plot.rectX(p, { x1: "start", x2: "end", y1, y2, fill: css("--grid"), fillOpacity: 0.55 }),
    Plot.text(p, { x: "start", y: y2, text: "label", textAnchor: "start", dx: 4, dy: 10, fill: css("--text-2"), fontSize: 11 }),
  ];
}

function seriesByRetailer(rows, value) {
  return shown().map((r) => ({ r, color: colorFor(r), rows: rows.filter((d) => d.utility_name === r && d[value] != null) }));
}

function timeSeriesCard(container, { title, subtitle, rows, value, yLabel, fmtV, shade = true, yZero = true, note, extraTable }) {
  const series = seriesByRetailer(rows, value);
  const flat = series.flatMap((s) => s.rows);
  const dates = Array.from(new Set(flat.map((d) => +d.date))).sort((a, b) => a - b).map((t) => new Date(t));
  const yMax = d3.max(flat, (d) => d[value]) ?? 1, yMin = Math.min(0, d3.min(flat, (d) => d[value]) ?? 0);
  chartCard(container, {
    title, subtitle, note,
    legend: series.map((s) => ({ label: s.r, color: s.color })),
    render: (w, h) => {
      const svg = Plot.plot(baseOptions(w, h, {
        x: { type: "utc", label: null },
        y: { label: yLabel, domain: yZero ? [yMin, yMax * 1.08] : undefined, tickFormat: fmtV ? (v) => fmtV(v) : undefined },
        marks: [
          ...(shade ? periods(yZero ? yMin : d3.min(flat, (d) => d[value]), yMax * 1.08) : []),
          gridY(),
          Plot.ruleY([0], { stroke: css("--axis") }),
          ...series.map((s) => Plot.lineY(s.rows, { x: "date", y: value, stroke: s.color, strokeWidth: 2, curve: "linear" })),
        ],
      }));
      const lookup = new Map(series.map((s) => [s.r, new Map(s.rows.map((d) => [+d.date, d[value]]))]));
      crosshair(svg, dates, (d) => series.map((s) => {
        const v = lookup.get(s.r).get(+d);
        return { label: s.r, color: s.color, value: v == null ? "n/a" : (fmtV || fmt1)(v) };
      }));
      return svg;
    },
    table: {
      columns: ["Month", ...series.map((s) => s.r), ...(extraTable ? extraTable.columns : [])],
      rows: dates.map((d) => [d3.utcFormat("%Y-%m")(d), ...series.map((s) => {
        const v = s.rows.find((x) => +x.date === +d)?.[value];
        return v == null ? null : (fmtV || fmt1)(v);
      })]),
    },
  });
}

// ---- Views -------------------------------------------------------------------------------------------------------
function tile(label, value, sub, cls = "") {
  return el("div", { class: `tile ${cls}` }, el("div", { class: "label", text: label }), el("div", { class: "value", text: value }),
    sub ? el("div", { class: "sub", text: sub }) : null);
}

function renderOverview() {
  const D = state.data, R = state.retailer;
  const um = D.usage_monthly.filter((d) => d.utility_name === R);
  const y13 = d3.mean(um.filter((d) => d.month.startsWith("2013")), (d) => d.median_ccf);
  const late15 = d3.mean(um.filter((d) => d.month >= "2015-06" && d.month <= "2015-12"), (d) => d.pct_vs_2013);
  const pooled = D.savings.pooled[0];
  const tiles = document.getElementById("overview-tiles");
  tiles.replaceChildren(
    tile(`Median single family use in 2013 (${R})`, `${fmt1(y13)} CCF`, "per 30 days, average of monthly medians"),
    tile("Turf rebate savings, pooled estimate", `${fmt0(pooled.estimate)} gal/sq ft/yr`,
      `90% interval ${fmt0(pooled.lo90)} to ${fmt0(pooled.hi90)}; all ${pooled.n} treated households`),
    tile(`Use in June to December 2015 vs 2013 (${R})`, fmtSignedPct(late15), "median single family use, same months"),
  );
  if (state.answerKey && state.key) tiles.append(tile("Planted savings mean (answer key)", `${fmt1(state.key.planted_mean)} gal/sq ft/yr`,
    "over the same windows; known only because the data is simulated", "planted"));

  const cards = document.getElementById("retailer-cards");
  cards.replaceChildren(...D.retailers.map((r) => {
    const key = el("span", { class: "key" }); key.style.background = colorFor(r.name);
    const dl = el("dl");
    const add = (k, v) => dl.append(el("dt", { text: k }), el("dd", { text: v }));
    add("Accounts, Jan 2010", fmt0(r.accounts_jan2010));
    add("Single family share", fmtPct(r.sf_share));
    add("Median irrigable area", `${fmt0(r.median_irrigable_area_sf)} sq ft`);
    add("Median household income", `$${fmt0(r.median_income)}`);
    add("ET0 per year, 2011-2015", `${fmt0(r.et0_annual_2011_2015)} in`);
    add("Rain per year, 2011-2015", `${fmt0(r.precip_annual_2011_2015)} in`);
    add("Billing", r.billing);
    add("Rates", r.rate_type === "budget" ? "water budget" : "inclining blocks");
    return el("article", { class: `card ${R !== ALL && R !== r.name ? "dimmed" : ""}` },
      el("h4", {}, key, document.createTextNode(r.name)), el("p", { text: r.setting }), dl);
  }));

  const rows = D.retailers.filter((r) => R === ALL || r.name === R);
  chartCard(document.getElementById("chart-area"), {
    title: "Median irrigable area of single family homes",
    subtitle: "Square feet. Inland and valley lots are larger, which drives larger outdoor use.",
    height: rowHeight(document.getElementById("chart-area").clientWidth, rows.length, 34),
    render: (w, h) => { const L = catLayout(w, 140, rows, "name"); return Plot.plot(baseOptions(w, h, {
      marginLeft: L.marginLeft, marginRight: 70,
      y: { label: null, domain: rows.map((r) => r.name), padding: 0.3, ...L.y },
      x: { label: "Square feet", grid: false, nice: true },
      marks: [gridX(), Plot.ruleX([0], { stroke: css("--axis") }),
        Plot.barX(rows, { y: "name", x: "median_irrigable_area_sf", fill: (d) => colorFor(d.name), rx2y1: 4, rx2y2: 4 }),
        Plot.text(rows, { y: "name", x: "median_irrigable_area_sf", text: (d) => fmt0(d.median_irrigable_area_sf), dx: 6, textAnchor: "start", fill: css("--text") }),
        tipMark(rows, { y: "name", x: "median_irrigable_area_sf", title: (d) => `${d.name}\n${fmt0(d.median_irrigable_area_sf)} sq ft` }), ...L.labels],
    })); },
    table: { columns: ["Retailer", "Median irrigable area (sq ft)", "Median income", "Accounts"],
      rows: rows.map((r) => [r.name, fmt0(r.median_irrigable_area_sf), `$${fmt0(r.median_income)}`, fmt0(r.accounts_jan2010)]) },
  });
}

function renderUsage() {
  const D = state.data;
  const um = D.usage_monthly.filter((d) => d.utility_name !== ALL);
  timeSeriesCard(document.getElementById("chart-usage"), {
    title: "Median single family use", subtitle: "CCF per 30 days, monthly. Billing periods are scaled to 30 days so bimonthly retailers compare directly.",
    rows: um, value: "median_ccf", yLabel: "CCF per 30 days",
  });
  timeSeriesCard(document.getElementById("chart-vs2013"), {
    title: "Change from the same month of 2013", subtitle: "Median single family use, January 2014 onward. The 2015 mandate targeted about a 25 percent cut.",
    rows: um.filter((d) => d.month >= "2014-01"), value: "pct_vs_2013", yLabel: "Change vs 2013", fmtV: fmtSignedPct, yZero: false,
  });
  const seas = shown().flatMap((r) => d3.range(1, 13).map((m) => ({
    utility_name: r, month: m,
    v: d3.mean(um.filter((d) => d.utility_name === r && d.month < "2014-01" && +d.month.slice(5) === m), (d) => d.median_ccf),
  })));
  const et = shown().flatMap((r) => d3.range(1, 13).map((m) => ({
    utility_name: r, month: m, v: d3.mean(D.climate_monthly.filter((d) => d.utility_name === r && +d.month.slice(5) === m), (d) => d.et0_in),
  })));
  for (const [id, rows, title, sub, ylab] of [
    ["chart-seasonal", seas, "Seasonal pattern of use, 2010-2013", "Median single family use by calendar month, CCF per 30 days.", "CCF per 30 days"],
    ["chart-et0", et, "Reference evapotranspiration by month", "Mean monthly ET0, inches, 2010-2016. Coastal Surf City has the flattest cycle.", "Inches"],
  ]) {
    const series = shown().map((r) => ({ r, color: colorFor(r), rows: rows.filter((d) => d.utility_name === r) }));
    chartCard(document.getElementById(id), {
      title, subtitle: sub, legend: series.map((s) => ({ label: s.r, color: s.color })),
      render: (w, h) => {
        const svg = Plot.plot(baseOptions(w, h, {
          x: { label: null, domain: [1, 12], ticks: monthTicks(w), tickFormat: (m) => MONTHS[m - 1] },
          y: { label: ylab, zero: true },
          marks: [gridY(), Plot.ruleY([0], { stroke: css("--axis") }),
            ...series.map((s) => Plot.lineY(s.rows, { x: "month", y: "v", stroke: s.color, strokeWidth: 2 }))],
        }));
        crosshair(svg, d3.range(1, 13), (m) => series.map((s) => ({ label: s.r, color: s.color, value: fmt1(s.rows[m - 1].v) })),
          (m) => MONTHS[m - 1]);
        return svg;
      },
      table: { columns: ["Month", ...series.map((s) => s.r)], rows: d3.range(1, 13).map((m) => [MONTHS[m - 1], ...series.map((s) => fmt1(s.rows[m - 1].v))]) },
    });
  }
  timeSeriesCard(document.getElementById("chart-gpcd"), {
    title: "Residential gallons per capita per day (supplier report)",
    subtitle: "Calculated from production, the residential share and the service population, as in the State Water Board's monthly reports.",
    rows: D.supplier_monthly, value: "gpcd", yLabel: "R-GPCD",
  });
}

function quantileRows(q) {
  const g = d3.group(q, (d) => d.series);
  return Array.from(g, ([series, rows]) => {
    const at = (p) => rows.find((r) => Math.abs(r.p - p) < 1e-9).value;
    return { series, p5: at(0.05), p25: at(0.25), p50: at(0.5), p75: at(0.75), p95: at(0.95), n: rows[0].n };
  });
}

function renderSavings() {
  const D = state.data, S = D.savings, K = state.answerKey ? state.key : null;
  const st = S.pooled[0], mf = S.pooled[1];
  const tiles = document.getElementById("savings-tiles");
  tiles.replaceChildren(
    tile("Turf savings, stacked DiD (pooled)", `${fmt1(st.estimate)}`, `gal per sq ft of turf per year; 90% interval ${fmt1(st.lo90)} to ${fmt1(st.hi90)}`, "hero"),
    tile("Pooled household estimates", fmt1(mf.estimate), `random-effects pool; 90% interval ${fmt1(mf.lo90)} to ${fmt1(mf.hi90)}`),
    tile("Treated households analyzed", fmt0(S.cohort.households), `${S.cohort.window}; ${S.cohort.cotreated_in_window} also hold another rebate in the window`),
    tile("Placebo spread (noise floor)", `SD ${fmt0(S.placebo.sd)}`, `${S.placebo.n} untreated households given fake rebate dates; mean ${fmt1(S.placebo.mean)}`),
  );
  if (K) {
    const b = K.by_estimator.find((d) => d.estimator === "matched_corr_raw");
    tiles.append(tile("Planted mean (answer key)", fmt1(K.planted_mean), `matched DiD bias ${fmt1(b.bias)}; 90% intervals cover the planted value for ${fmtPct(b.coverage90)} of households`, "planted"));
  }
  document.getElementById("savings-scope").textContent = state.retailer === ALL
    ? "Pooled results cover all treated households. Choose a retailer to filter the uptake chart and highlight it in the by-retailer chart."
    : `Pooled results cover all retailers. The uptake chart shows ${state.retailer} only, and the by-retailer chart highlights it.`;

  // Uptake by year and type (stacked columns with a 2px surface gap; cells under 10 are suppressed).
  const types = ["turf", "high efficiency clothes washer", "weather based irrigation controller"];
  const tcol = { turf: css("--series-1"), "high efficiency clothes washer": css("--series-2"), "weather based irrigation controller": css("--series-3") };
  const src = state.retailer === ALL ? D.rebates.by_year : D.rebates.by_retailer_year.filter((d) => d.utility_name === state.retailer);
  const up = src.map((d) => ({ yr: String(d.yr), type: d.type, n: d.n }));
  const suppressed = up.filter((d) => d.n == null).length;
  const years = Array.from(new Set(up.map((d) => d.yr))).sort();
  chartCard(document.getElementById("chart-uptake"), {
    title: `Rebates by year of post-inspection (${state.retailer})`, subtitle: "Count of rebates by type.",
    legend: types.map((t) => ({ label: t, color: tcol[t], shape: "box" })),
    note: suppressed ? `${suppressed} cells with fewer than 10 rebates are suppressed and not drawn.` : null,
    render: (w, h) => Plot.plot(baseOptions(w, h, {
      x: { label: null, domain: years, padding: 0.35 }, y: { label: "Rebates", grid: false },
      color: { domain: types, range: types.map((t) => tcol[t]) },
      marks: [gridY(), Plot.ruleY([0], { stroke: css("--axis") }),
        Plot.barY(up.filter((d) => d.n != null), Plot.stackY({ x: "yr", y: "n", fill: "type", order: types, stroke: css("--surface"), strokeWidth: 2 })),
        tipMark(up.filter((d) => d.n != null), Plot.stackY({ x: "yr", y: "n", z: "type", order: types, title: (d) => `${d.type}, ${d.yr}\n${fmt0(d.n)} rebates` }))],
    })),
    table: { columns: ["Year", ...types], rows: years.map((y) => [y, ...types.map((t) => up.find((d) => d.yr === y && d.type === t)?.n ?? null)]) },
  });

  // Distribution of household estimates against the placebo noise floor (quantiles only, no individual rows).
  let qr = quantileRows(S.quantiles);
  if (K) qr = quantileRows(K.planted_quantiles).concat(qr);
  const qcol = (s) => (s.startsWith("Planted") ? PLANTED() : s.startsWith("Placebo") ? PLACEBO() : ESTIMATE());
  chartCard(document.getElementById("chart-distribution"), {
    title: "How spread out are household estimates?",
    subtitle: "Thin line: 5th to 95th percentile. Thick bar: 25th to 75th. Tick: median. Shaded band: the paper's estimated interquartile range (about 8 to 32).",
    legend: qr.map((d) => ({ label: d.series, color: qcol(d.series), shape: "box" })),
    height: rowHeight(document.getElementById("chart-distribution").clientWidth, qr.length, 46),
    note: "The placebo households never removed turf, so their spread is pure estimation noise. It is wider than the paper's whole interquartile range, which is why single-household estimates say little.",
    render: (w, h) => { const L = catLayout(w, 170, qr, "series"); return Plot.plot(baseOptions(w, h, {
      marginLeft: L.marginLeft, y: { label: null, domain: qr.map((d) => d.series), padding: 0.4, ...L.y }, x: { label: "gal per sq ft per year", nice: true },
      marks: [gridX(), Plot.rectX([{ a: 8, b: 32 }], { x1: "a", x2: "b", fill: css("--grid"), fillOpacity: 0.6 }),
        Plot.ruleX([0], { stroke: css("--axis") }),
        Plot.ruleY(qr, { y: "series", x1: "p5", x2: "p95", stroke: (d) => qcol(d.series), strokeWidth: 2 }),
        Plot.barX(qr, { y: "series", x1: "p25", x2: "p75", fill: (d) => qcol(d.series), insetTop: 4, insetBottom: 4, rx: 4 }),
        Plot.tickX(qr, { y: "series", x: "p50", stroke: css("--surface"), strokeWidth: 3 }),
        tipMark(qr, { y: "series", x: "p50", title: (d) => `${d.series} (n = ${d.n})\nmedian ${fmt1(d.p50)}\nIQR ${fmt1(d.p25)} to ${fmt1(d.p75)}\n5th-95th ${fmt0(d.p5)} to ${fmt0(d.p95)}` }), ...L.labels],
    })); },
    table: { columns: ["Series", "n", "5th", "25th", "Median", "75th", "95th"], rows: qr.map((d) => [d.series, d.n, fmt1(d.p5), fmt1(d.p25), fmt1(d.p50), fmt1(d.p75), fmt1(d.p95)]) },
  });

  // Estimator comparison: mean of household estimates with a 90% interval for the mean.
  const est = S.estimators;
  chartCard(document.getElementById("chart-estimators"), {
    title: "Estimators side by side",
    subtitle: "Mean of household estimates with a 90% interval for the mean, gal per sq ft per year." + (K ? " Orange line: planted mean." : ""),
    height: rowHeight(document.getElementById("chart-estimators").clientWidth, est.length),
    legend: K ? [{ label: "Estimate", color: ESTIMATE(), shape: "dot" }, { label: "Planted mean (answer key)", color: PLANTED() }] : null,
    render: (w, h) => { const L = catLayout(w, 250, est, "label"); return Plot.plot(baseOptions(w, h, {
      marginLeft: L.marginLeft, y: { label: null, domain: est.map((d) => d.label), padding: 0.4, ...L.y }, x: { label: "gal per sq ft per year", nice: true },
      marks: [gridX(), Plot.ruleX([0], { stroke: css("--axis") }),
        ...(K ? [Plot.ruleX([K.planted_mean], { stroke: PLANTED(), strokeWidth: 2 })] : []),
        Plot.ruleY(est, { y: "label", x1: "lo90", x2: "hi90", stroke: ESTIMATE(), strokeWidth: 2 }),
        Plot.dot(est, { y: "label", x: "mean", r: 5, fill: ESTIMATE(), stroke: css("--surface"), strokeWidth: 2 }),
        tipMark(est, { y: "label", x: "mean", title: (d) => `${d.label}\nmean ${fmt1(d.mean)} (90% ${fmt1(d.lo90)} to ${fmt1(d.hi90)})\nmedian ${fmt1(d.median)}, n = ${d.n}` }), ...L.labels],
    })); },
    note: "Naive pre-post has no control group, so it credits the drought drop to the rebate.",
    table: {
      columns: ["Estimator", "n", "Mean", "90% low", "90% high", "Median", ...(K ? ["Bias vs planted", "RMSE", "Coverage of 90% intervals"] : [])],
      rows: est.map((d) => {
        const k = K && K.by_estimator.find((b) => b.estimator === d.estimator);
        return [d.label, d.n, fmt1(d.mean), fmt1(d.lo90), fmt1(d.hi90), fmt1(d.median), ...(K ? [k ? fmt1(k.bias) : null, k ? fmt0(k.rmse) : null, k ? fmtPct(k.coverage90) : null] : [])];
      }),
    },
  });

  // Seasonal savings profile (pooled) and the event study.
  const sp = S.seasonal;
  chartCard(document.getElementById("chart-season-savings"), {
    title: "Savings by month of the year", subtitle: "Pooled stacked DiD with a seasonal interaction, gal per sq ft per month; band is the 90% interval.",
    legend: K ? [{ label: "Estimate", color: ESTIMATE() }, { label: "Planted (answer key)", color: PLANTED() }] : null,
    render: (w, h) => {
      const svg = Plot.plot(baseOptions(w, h, {
        x: { label: null, domain: [1, 12], ticks: monthTicks(w), tickFormat: (m) => MONTHS[m - 1] }, y: { label: "gal / sq ft / month", zero: true },
        marks: [gridY(), Plot.ruleY([0], { stroke: css("--axis") }),
          Plot.areaY(sp, { x: "month", y1: "lo90", y2: "hi90", fill: ESTIMATE(), fillOpacity: 0.12 }),
          Plot.lineY(sp, { x: "month", y: "estimate", stroke: ESTIMATE(), strokeWidth: 2 }),
          ...(K ? [Plot.lineY(K.seasonal, { x: "month", y: "planted", stroke: PLANTED(), strokeWidth: 2 })] : [])],
      }));
      crosshair(svg, d3.range(1, 13), (m) => [{ label: "Estimate", color: ESTIMATE(), value: fmt2(sp[m - 1].estimate) },
        ...(K ? [{ label: "Planted", color: PLANTED(), value: fmt2(K.seasonal[m - 1].planted) }] : [])], (m) => MONTHS[m - 1]);
      return svg;
    },
    table: { columns: ["Month", "Estimate", "90% low", "90% high", ...(K ? ["Planted"] : [])],
      rows: sp.map((d, i) => [MONTHS[i], fmt2(d.estimate), fmt2(d.lo90), fmt2(d.hi90), ...(K ? [fmt2(K.seasonal[i].planted)] : [])]) },
  });
  const ev = S.event_study;
  chartCard(document.getElementById("chart-event"), {
    title: "Event study", subtitle: "Savings by 3-month bin relative to the rebate, gal per sq ft per year, 90% intervals. Bins left of the line are before the application; the last pre bin is the reference.",
    render: (w, h) => Plot.plot(baseOptions(w, h, {
      x: { label: "3-month bins from the rebate", ticks: ev.map((d) => d.event_bin).filter((b) => b % 2 === 0) }, y: { label: "gal / sq ft / yr" },
      marks: [gridY(), Plot.ruleY([0], { stroke: css("--axis") }), Plot.ruleX([-0.5], { stroke: css("--muted") }),
        Plot.ruleX(ev, { x: "event_bin", y1: "lo90", y2: "hi90", stroke: ESTIMATE(), strokeWidth: 2 }),
        Plot.dot(ev, { x: "event_bin", y: "estimate", r: 4, fill: ESTIMATE(), stroke: css("--surface"), strokeWidth: 2 }),
        tipMark(ev, { x: "event_bin", y: "estimate", title: (d) => `bin ${d.event_bin}\n${fmt1(d.estimate)} (90% ${fmt1(d.lo90)} to ${fmt1(d.hi90)})` })],
    })),
    note: "No retailer shows a significant linear pre-trend (see the table).",
    table: { columns: ["Retailer", "Blocks", "Pre-trend slope per bin", "SE", "p-value"],
      rows: S.pretrend.map((d) => [d.utility_name, d.n_blocks, fmt2(d.slope), fmt2(d.se), fmt2(d.p)]) },
  });

  // By retailer.
  const br = S.by_retailer.slice().sort((a, b) => retailers().indexOf(a.utility_name) - retailers().indexOf(b.utility_name));
  const sel = (d) => state.retailer === ALL || d.utility_name === state.retailer;
  const kr = K ? new Map(K.by_retailer.map((d) => [d.utility_name, d.planted_mean])) : null;
  chartCard(document.getElementById("chart-by-retailer"), {
    title: "Household estimates by retailer", subtitle: "Mean with a 90% interval for the mean, gal per sq ft per year." + (K ? " Orange ticks: planted mean." : ""),
    height: rowHeight(document.getElementById("chart-by-retailer").clientWidth, br.length, 40),
    legend: K ? [{ label: "Estimate", color: ESTIMATE(), shape: "dot" }, { label: "Planted mean (answer key)", color: PLANTED() }] : null,
    render: (w, h) => { const L = catLayout(w, 150, br, "utility_name"); return Plot.plot(baseOptions(w, h, {
      marginLeft: L.marginLeft, y: { label: null, domain: br.map((d) => d.utility_name), padding: 0.4, ...L.y }, x: { label: "gal per sq ft per year", nice: true },
      marks: [gridX(), Plot.ruleX([0], { stroke: css("--axis") }),
        Plot.ruleY(br, { y: "utility_name", x1: "lo90", x2: "hi90", stroke: ESTIMATE(), strokeWidth: 2, strokeOpacity: (d) => (sel(d) ? 1 : 0.3) }),
        Plot.dot(br, { y: "utility_name", x: "mean", r: 5, fill: ESTIMATE(), fillOpacity: (d) => (sel(d) ? 1 : 0.3), stroke: css("--surface"), strokeWidth: 2 }),
        ...(K ? [Plot.tickX(br, { y: "utility_name", x: (d) => kr.get(d.utility_name), stroke: PLANTED(), strokeWidth: 3 })] : []),
        tipMark(br, { y: "utility_name", x: "mean", title: (d) => `${d.utility_name} (n = ${d.n})\nmean ${fmt1(d.mean)} (90% ${fmt1(d.lo90)} to ${fmt1(d.hi90)})` }), ...L.labels],
    })); },
    table: { columns: ["Retailer", "Households", "Mean", "90% low", "90% high", "Median", ...(K ? ["Planted mean"] : [])],
      rows: br.map((d) => [d.utility_name, d.n, fmt1(d.mean), fmt1(d.lo90), fmt1(d.hi90), fmt1(d.median), ...(K ? [fmt1(kr.get(d.utility_name))] : [])]) },
  });

  // Other rebate types: different units, so a table rather than one shared axis.
  const oc = document.getElementById("chart-other");
  const ko = K ? new Map(K.other_rebates.map((d) => [d.type, d.planted])) : null;
  const show = (d, v) => (d.unit.startsWith("share") ? fmtPct(v) : fmt1(v));
  chartCard(oc, {
    title: "Other rebate types", subtitle: "Same matching and DiD machinery. With about 50 households per type, wide intervals are expected.",
    render: () => null,
    note: "Washer savings should be flat across the year; controller savings should peak in summer. The seasonal test and the summer-minus-winter difference are in the table.",
    table: { columns: ["Rebate", "Households", "Estimate", "90% low", "90% high", "Unit", "Seasonality p-value", "Summer minus winter (CCF/month)", ...(K ? ["Planted"] : [])],
      rows: S.other_rebates.map((d) => [d.type, d.n, show(d, d.estimate), show(d, d.lo90), show(d, d.hi90), d.unit, fmt2(d.seasonal_p),
        `${fmt2(d.summer_minus_winter_ccf)} (SE ${fmt2(d.summer_minus_winter_se)})`, ...(K ? [show(d, ko.get(d.type))] : [])]) },
  });
  const btn = oc.querySelector(".table-toggle"); if (btn) btn.click();

  document.querySelector(".answer-key-only").hidden = !K;
  if (K) renderAnswerKey(K);
}

function renderAnswerKey(K) {
  const cal = K.calibration;
  chartCard(document.getElementById("chart-calibration"), {
    title: "Do higher planted savings get higher estimates?",
    subtitle: "Treated households grouped into fifths by planted savings (35 each). Mean estimate against mean planted value; the line marks perfect agreement.",
    render: (w, h) => Plot.plot(baseOptions(w, h, {
      x: { label: "Mean planted savings (gal / sq ft / yr)", nice: true }, y: { label: "Mean estimate", nice: true },
      marks: [gridY(), Plot.line([[d3.min(cal, (d) => Math.min(d.planted, d.estimated)), d3.min(cal, (d) => Math.min(d.planted, d.estimated))],
        [d3.max(cal, (d) => Math.max(d.planted, d.estimated)), d3.max(cal, (d) => Math.max(d.planted, d.estimated))]], { stroke: css("--muted") }),
        Plot.dot(cal, { x: "planted", y: "estimated", r: 6, fill: PLANTED(), stroke: css("--surface"), strokeWidth: 2 }),
        tipMark(cal, { x: "planted", y: "estimated", title: (d) => `fifth ${d.quint} (n = ${d.n})\nplanted ${fmt1(d.planted)}\nestimated ${fmt1(d.estimated)}` })],
    })),
    note: "Group means track the planted values only loosely; household noise dominates within each group.",
    table: { columns: ["Fifth", "Households", "Mean planted", "Mean estimate"], rows: cal.map((d) => [d.quint, d.n, fmt1(d.planted), fmt1(d.estimated)]) },
  });
  const ests = ["matched_corr_raw", "unmatched_did", "naive_pre_post"];
  const ecol = { matched_corr_raw: css("--series-1"), unmatched_did: css("--series-2"), naive_pre_post: css("--series-3") };
  const sc = K.scenarios.filter((d) => ests.includes(d.estimator));
  const pretty = (s) => s.replace(/_/g, " ");
  for (const d of sc) d.scenario_label = pretty(d.scenario);
  const scen = Array.from(new Set(sc.map((d) => d.scenario_label)));
  const lab = Object.fromEntries(K.scenarios.map((d) => [d.estimator, d.label]));
  chartCard(document.getElementById("chart-scenarios"), {
    title: "Bias in alternative planted worlds",
    subtitle: "The estimator stays fixed while the simulated world changes. Bias against the planted effect, gal per sq ft per year.",
    legend: ests.map((e) => ({ label: lab[e], color: ecol[e], shape: "dot" })),
    height: rowHeight(document.getElementById("chart-scenarios").clientWidth, scen.length, 34),
    render: (w, h) => { const L = catLayout(w, 160, scen.map((s) => ({ scenario_label: s })), "scenario_label"); return Plot.plot(baseOptions(w, h, {
      marginLeft: L.marginLeft, y: { label: null, domain: scen, padding: 0.4, ...L.y }, x: { label: "Bias (gal / sq ft / yr)", nice: true },
      marks: [gridX(), Plot.ruleX([0], { stroke: css("--axis") }),
        Plot.dot(sc, { y: "scenario_label", x: "bias", r: 5, fill: (d) => ecol[d.estimator], stroke: css("--surface"), strokeWidth: 2 }),
        tipMark(sc, { y: "scenario_label", x: "bias", title: (d) => `${d.scenario_label}: ${d.label}\nbias ${fmt1(d.bias)}, coverage ${fmtPct(d.coverage90)}` }), ...L.labels],
    })); },
    note: "Matched DiD stays near zero bias except under differential trends, where conservation-minded households trend down on their own. Parallel trends is the assumption it leans on most.",
    table: { columns: ["Scenario", "Estimator", "Bias", "RMSE", "Coverage of 90% intervals", "Description"],
      rows: K.scenarios.map((d) => [d.scenario.replace(/_/g, " "), d.label, fmt1(d.bias), fmt0(d.rmse), fmtPct(d.coverage90), d.description]) },
  });
}

function renderQuality() {
  const D = state.data, Q = D.quality;
  const cov = Q.coverage.filter((d) => shown().includes(d.utility_name));
  const months = Array.from(new Set(cov.map((d) => d.month))).sort();
  const max = 0.10;   // feed gaps run a few percent; the outage runs off this scale and is drawn at the darkest step
  chartCard(document.getElementById("chart-coverage"), {
    title: "Share of active locations with no billing row",
    subtitle: "By retailer and month. Darker cells mean more missing rows. The scale stops at 10 percent; the Inland Enclave outage in late 2013 (nearly every row missing) is drawn at the darkest step.",
    height: shown().length * 30 + 70,
    render: (w, h) => Plot.plot(baseOptions(w, h, {
      marginLeft: 150, x: { label: null, domain: months, tickFormat: (m) => (m.endsWith("-01") ? m.slice(0, 4) : ""), tickSize: 0 },
      y: { label: null, domain: shown() }, padding: 0.06,
      color: { type: "linear", domain: [0, max], range: [css("--seq-low"), css("--seq-high")], clamp: true, legend: true, label: "Share missing", tickFormat: fmtPct },
      marks: [Plot.cell(cov, { x: "month", y: "utility_name", fill: "missing_share" }),
        tipMark(cov, { x: "month", y: "utility_name", title: (d) => `${d.utility_name}, ${d.month}\n${d.missing_share == null ? "suppressed" : fmtPct(d.missing_share)} missing of ${fmt0(d.n)}` })],
    })),
    table: { columns: ["Month", ...shown()], rows: months.map((m) => [m, ...shown().map((r) => {
      const v = cov.find((d) => d.month === m && d.utility_name === r)?.missing_share; return v == null ? null : fmtPct(v); })]) },
  });
  const qr = Q.by_retailer.filter((d) => shown().includes(d.utility_name)).sort((a, b) => retailers().indexOf(a.utility_name) - retailers().indexOf(b.utility_name));
  const qc = document.getElementById("chart-quality-table");
  chartCard(qc, {
    title: "Data quality by retailer", subtitle: "Single family locations unless noted. Turnover is a real event (a new customer at the same home), not missing data.",
    render: () => null,
    note: `Duplicate turf rebate records were found at ${Q.duplicate_turf_locations} locations; the analysis drops them, as the paper did.`,
    table: { columns: ["Retailer", "Locations", "Defaulted irrigable area", "Missing irrigable area", "Missing household size", "Turnover per year", "Zero-usage rows (all classes)", "Negative reads", "Spike reads"],
      rows: qr.map((d) => [d.utility_name, fmt0(d.sf_locations), fmtPct(d.defaulted_area_share), fmtPct(d.missing_area_share), fmtPct(d.missing_hhsize_share),
        d3.format(".1%")(d.turnover_per_year), d3.format(".1%")(d.zero_usage_share), d.negative_reads >= 10 ? fmt0(d.negative_reads) : null, d.spikes >= 10 ? fmt0(d.spikes) : null]) },
  });
  const b = qc.querySelector(".table-toggle"); if (b) b.click();
}

function renderAbout() {
  const m = state.data.meta;
  document.getElementById("about-meta").textContent =
    `Prepared ${m.generated} from the ${m.profile} profile (root seed ${m.root_seed}). Minimum cell size: ${m.min_cell}.`;
}

// ---- App shell ---------------------------------------------------------------------------------------------------
const renderers = { overview: renderOverview, usage: renderUsage, savings: renderSavings, quality: renderQuality, about: renderAbout };
function render() {
  hideTooltip();
  for (const s of document.querySelectorAll(".view")) s.hidden = s.dataset.view !== state.view;
  for (const a of document.querySelectorAll(".tabs a")) {
    if (a.dataset.view === state.view) a.setAttribute("aria-current", "page"); else a.removeAttribute("aria-current");
  }
  document.getElementById("answer-key-notice").hidden = !state.answerKey;
  renderers[state.view]();
}

function applyTheme(t) {
  if (t === "auto") document.documentElement.removeAttribute("data-theme"); else document.documentElement.setAttribute("data-theme", t);
}

async function init() {
  try {
    await loadAll();
  } catch (e) {
    document.getElementById("main").prepend(el("p", { class: "banner", text: `The data could not be loaded: ${e.message}` }));
    return;
  }
  const sel = document.getElementById("retailer-select");
  for (const r of retailers()) sel.append(el("option", { value: r, text: r }));
  sel.addEventListener("change", () => { state.retailer = sel.value; render(); });
  const ak = document.getElementById("answer-key");
  ak.checked = false;
  ak.addEventListener("change", async () => {
    if (ak.checked) await ensureKey();
    state.answerKey = ak.checked;
    render();
  });
  const ts = document.getElementById("theme-select");
  let saved = "auto";
  try { saved = localStorage.getItem("wue-theme") || "auto"; } catch (e) { /* storage unavailable */ }
  ts.value = saved; applyTheme(saved);
  ts.addEventListener("change", () => {
    applyTheme(ts.value);
    try { localStorage.setItem("wue-theme", ts.value); } catch (e) { /* storage unavailable */ }
    render();
  });
  window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", render);
  const fromHash = () => {
    const v = location.hash.replace("#", "");
    state.view = renderers[v] ? v : "overview";
    render();
  };
  window.addEventListener("hashchange", fromHash);
  let lastW = window.innerWidth, timer = null;
  window.addEventListener("resize", () => {
    if (Math.abs(window.innerWidth - lastW) < 8) return;
    lastW = window.innerWidth; clearTimeout(timer); timer = setTimeout(render, 150);
  });
  fromHash();
}
init();
