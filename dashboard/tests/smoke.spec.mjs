// Guardrail checks: the simulated-data banner on every view, answer key off by default (and its data not even
// fetched until requested), no horizontal scroll, no console errors, and a table view for every chart.
import { test, expect } from "@playwright/test";
const views = ["overview", "usage", "savings", "quality", "about"];

for (const view of views) {
  test(`${view}: banner, no errors, no horizontal scroll, table views`, async ({ page }) => {
    const errors = [];
    page.on("console", (m) => { if (m.type() === "error") errors.push(m.text()); });
    page.on("pageerror", (e) => errors.push(e.message));
    await page.goto(`/#${view}`);
    await expect(page.locator(`#view-${view}`)).toBeVisible();
    await expect(page.locator(".banner")).toContainText("All data on this site is simulated");
    await page.waitForTimeout(400);
    const overflow = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
    expect(overflow).toBeLessThanOrEqual(0);
    const cards = page.locator(`#view-${view} .chart-card:visible`);
    const n = await cards.count();
    for (let i = 0; i < n; i++) await expect(cards.nth(i).locator(".table-toggle")).toHaveCount(1);
    expect(errors).toEqual([]);
  });
}

test("answer key is off by default and loads only on request", async ({ page }) => {
  const requested = [];
  page.on("request", (r) => requested.push(r.url()));
  await page.goto("/#savings");
  await expect(page.locator("#answer-key")).not.toBeChecked();
  await expect(page.locator("#answer-key-notice")).toBeHidden();
  await expect(page.locator(".tile.planted")).toHaveCount(0);
  expect(requested.some((u) => u.includes("answer_key.json"))).toBe(false);
  await page.locator("#answer-key").check();
  await expect(page.locator("#answer-key-notice")).toBeVisible();
  await expect(page.locator(".tile.planted")).toHaveCount(1);
  await expect(page.locator("#chart-scenarios")).toContainText("Bias in alternative planted worlds");
  expect(requested.some((u) => u.includes("answer_key.json"))).toBe(true);
});

test("retailer filter scopes the usage view", async ({ page }) => {
  await page.goto("/#usage");
  await page.selectOption("#retailer-select", "Valley Small Town");
  const legend = page.locator("#chart-usage .legend");
  await expect(legend).toHaveCount(0);   // a single series needs no legend box
  await page.locator("#chart-usage .table-toggle").click();
  await expect(page.locator("#chart-usage th").nth(1)).toHaveText("Valley Small Town");
});
