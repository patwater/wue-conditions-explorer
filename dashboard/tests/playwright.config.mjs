// Playwright smoke tests for the dashboard. Starts the local static server on port 8799.
import { defineConfig } from "@playwright/test";
export default defineConfig({
  testDir: ".",
  testMatch: /.*\.spec\.mjs/,
  timeout: 30000,
  use: { baseURL: "http://localhost:8799" },
  webServer: { command: "node tests/serve.mjs 8799", url: "http://localhost:8799", reuseExistingServer: true, cwd: ".." },
  projects: [
    { name: "desktop", use: { browserName: "chromium", viewport: { width: 1280, height: 900 } } },
    { name: "phone", use: { browserName: "chromium", viewport: { width: 375, height: 800 } } },
  ],
});
