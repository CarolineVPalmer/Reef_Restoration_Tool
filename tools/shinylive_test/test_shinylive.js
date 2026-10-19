// Diagnostic test of the Shinylive (in-browser) build: loads the app, runs the
// example analysis with light settings (B = 50) and then default settings,
// and records browser console messages, the progress messages shown by the app
// every 20 s, and screenshots. Used by .github/workflows/shinylive-test.yml.
const { chromium } = require("playwright");
const fs = require("fs");

async function findFrame(page, selector, timeoutMs) {
  const t0 = Date.now();
  while (Date.now() - t0 < timeoutMs) {
    for (const f of page.frames()) {
      try { if (await f.locator(selector).count() > 0) return f; } catch (e) {}
    }
    await page.waitForTimeout(500);
  }
  throw new Error("Timed out waiting for " + selector);
}

async function textOf(frame, sel) {
  try { return (await frame.locator(sel).allInnerTexts()).join(" | ").replace(/\s+/g, " ").slice(0, 300); }
  catch (e) { return ""; }
}

async function runOnce(page, frame, label, B, maxMinutes, log) {
  if (B !== null) {
    await frame.locator("#advanced").check();
    await frame.locator("#B").fill(String(B));
    await frame.locator("#B").press("Tab");
    await page.waitForTimeout(1000);
  }
  const t1 = Date.now();
  await frame.locator("#analyze").click();
  const out = { label, B };
  for (let i = 0; i < maxMinutes * 3; i++) {
    await page.waitForTimeout(20000);
    const done = await frame.locator("text=sites scored from").count();
    const progress = await textOf(frame, ".shiny-notification, .shiny-progress-notification");
    const modal = await textOf(frame, ".modal-body");
    log.push(`[${label} +${Math.round((Date.now() - t1) / 1000)} s] progress: ${progress} | modal: ${modal}`);
    if (modal) { out.status = "error shown: " + modal; break; }
    if (done > 0) { out.status = "ok"; break; }
  }
  out.seconds = (Date.now() - t1) / 1000;
  if (!out.status) out.status = `not finished after ${maxMinutes} min`;
  await page.screenshot({ path: `results/${label}.png`, fullPage: true });
  return out;
}

(async () => {
  fs.mkdirSync("results", { recursive: true });
  const log = [];
  const browser = await chromium.launch();
  const page = await (await browser.newContext({ viewport: { width: 1280, height: 900 } })).newPage();
  page.on("console", m => log.push(`[console ${m.type()}] ${m.text().slice(0, 500)}`));
  page.on("pageerror", e => log.push(`[page error] ${e.message.slice(0, 500)}`));
  page.on("frameattached", f => f.on && null);
  const results = [];
  const t0 = Date.now();
  await page.goto("http://localhost:8000/", { waitUntil: "load", timeout: 600000 });
  const frame = await findFrame(page, "#analyze", 600000);
  results.push({ step: "app ready", seconds: (Date.now() - t0) / 1000 });
  await frame.locator("#datafile").setInputFiles("app/data/example_transects.csv");
  await page.waitForTimeout(5000);
  results.push(await runOnce(page, frame, "light_B50", 50, 25, log));
  if (results[results.length - 1].status === "ok") {
    results.push(await runOnce(page, frame, "default_B200", 200, 40, log));
  }
  fs.writeFileSync("results/results.json", JSON.stringify(results, null, 2));
  fs.writeFileSync("results/browser_log.txt", log.join("\n"));
  console.log(JSON.stringify(results));
  await browser.close();
})();
