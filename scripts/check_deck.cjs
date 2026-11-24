// Browser smoke and overflow checks. npm install --no-save playwright
const { chromium } = require('playwright');
const fs = require('node:fs');
const base = (process.env.DECK_URL || 'http://127.0.0.1:8765/').replace(/\/?$/, '/');
(async () => {
  const browser = await chromium.launch({headless: true,
    executablePath: process.env.CHROME_PATH || undefined, args: ['--no-sandbox']});
  const page = await browser.newPage({viewport: {width: 1600, height: 900}});
  const errors = [], missing = [], external = [];
  page.on('pageerror', e => errors.push(e.message));
  page.on('response', r => { if (r.status() >= 400) missing.push([r.status(), r.url()]); });
  page.on('request', r => { if (!r.url().startsWith(base) && !r.url().startsWith('data:')) external.push(r.url()); });
  await page.goto(base + '?layers=all');
  await page.waitForFunction(() => window.courseReady, {timeout: 20000});
  await page.evaluate(() => document.fonts.ready);
  const ids = await page.locator('.slides section[data-source]').evaluateAll(nodes => nodes.map(s => s.id));
  const sources = await page.locator('.slides section[data-source]').evaluateAll(nodes => new Set(nodes.map(s => s.dataset.source)).size);
  if (sources !== 89) throw new Error(`Expected 89 source slides, got ${sources}`);
  fs.mkdirSync('/tmp/missing-data-shots', {recursive:true});
  const overflow = [];
  for (const id of ids) {
    await page.evaluate(id => {
      const s=document.getElementById(id); const i=Reveal.getIndices(s); Reveal.slide(i.h,i.v);
    }, id);
    await page.waitForTimeout(35);
    const result = await page.evaluate(() => {
      const s=Reveal.getCurrentSlide(); const grid=s.querySelector('.course-grid');
      const bound=grid.getBoundingClientRect(); const footer=document.querySelector('.mat-footer');
      const limit=footer ? footer.getBoundingClientRect().top : window.innerHeight-45;
      const wide=Array.from(grid.querySelectorAll('.course-card, .katex-display')).filter(c => c.scrollWidth > c.clientWidth+3);
      return {id:s.id,bottom:Math.round(bound.bottom),limit:Math.round(limit),wide:wide.length};
    });
    if (result.bottom > result.limit - 4 || result.wide) overflow.push(result);
    if (['p1-title','p1-riddle','p1-dgp','p1-mle','p1-sim','p2-rubin-1','p2-mi-coverage','p3-rct','p3-smcfcs-r'].includes(id))
      await page.screenshot({path:`/tmp/missing-data-shots/${id}.png`});
  }
  await page.keyboard.press('d');
  const core = await page.locator('.slides section[data-source]').count();
  if (core >= ids.length) throw new Error('Layer switch did not hide optional slides');
  await page.keyboard.press('d');
  if (await page.locator('.slides section[data-source]').count() !== ids.length) throw new Error('Layer switch did not restore slides');
  await page.keyboard.press('n');
  if (!await page.locator('.notes-panel').isVisible()) throw new Error('Notes did not open');
  await page.keyboard.press('n');
  const deepId = await page.locator('.slides section[data-layer="deep"]').first().getAttribute('id');
  await page.goto(`${base}#/${deepId}`);
  await page.waitForFunction(() => window.courseReady);
  if (await page.evaluate(() => Reveal.getCurrentSlide().id) !== deepId)
    throw new Error('Direct link to an optional slide failed');
  const download = await page.request.get(base + 'downloads/missing_data_practice.R');
  if (!download.ok() || !(await download.text()).includes('normal_imputations'))
    throw new Error('Standalone R download is missing');
  await page.goto(base + 'reading.html');
  if (await page.locator('.reading-slide').count() !== 89) throw new Error('Reading view lost slides');
  await page.pdf({path:'/tmp/missing-data-shots/reading.pdf',format:'A4',printBackground:true});
  const result = {sources,slides:ids.length,core,overflow,errors,missing,external:[...new Set(external)]};
  fs.writeFileSync('/tmp/missing-data-browser-check.json',JSON.stringify(result,null,2));
  console.log(JSON.stringify(result,null,2));
  await browser.close();
  if (errors.length || missing.length || external.length || overflow.length) process.exitCode=1;
})().catch(e=>{console.error(e);process.exit(1)});
