// Measure dense source slides and split at card/paragraph boundaries.
// The saved plan contains indices, never copies of content or calculated values.
const {chromium}=require('playwright');
const fs=require('node:fs');
(async()=>{
  const browser=await chromium.launch({headless:true,executablePath:process.env.CHROME_PATH,args:['--no-sandbox']});
  const page=await browser.newPage({viewport:{width:1600,height:900}});
  await page.goto('http://127.0.0.1:8765/?layers=all');
  await page.waitForFunction(()=>window.courseReady);
  await page.evaluate(()=>document.fonts.ready);
  if(await page.locator('.slides section[id*="-cont-"]').count())
    throw new Error('First generate with python3 scripts/build_deck.py --unpaginated and render.');
  const plan=await page.evaluate(()=>{
    const result={};
    for(const slide of document.querySelectorAll('.slides section[data-source]')){
      const index=Reveal.getIndices(slide);Reveal.slide(index.h,index.v);
      const grid=slide.querySelector('.course-grid');
      const limit=document.querySelector('.mat-footer').getBoundingClientRect().top-25;
      if(grid.getBoundingClientRect().bottom<=limit)continue;
      if(slide.classList.contains('course-title'))continue;
      const cards=[...grid.children];
      const fragments=[];
      for(let c=0;c<cards.length;c++){
        const original=cards[c];original.style.gridColumn='span 12';
        grid.replaceChildren(original);
        if(grid.getBoundingClientRect().bottom<=limit){
          fragments.push({card:c,start:0,end:original.children.length});continue;
        }
        const children=[...original.children];
        let start=0;
        while(start<children.length){
          const test=original.cloneNode(false);grid.replaceChildren(test);
          let end=start;
          while(end<children.length){
            test.append(children[end].cloneNode(true));
            if(grid.getBoundingClientRect().bottom>limit && end>start){test.lastElementChild.remove();break;}
            end++;
          }
          fragments.push({card:c,start,end});start=end;
        }
      }
      const pages=[];let current=[];grid.replaceChildren();
      for(const fragment of fragments){
        const original=cards[fragment.card];const card=original.cloneNode(false);
        for(let k=fragment.start;k<fragment.end;k++)card.append(original.children[k].cloneNode(true));
        grid.append(card);
        if(grid.getBoundingClientRect().bottom>limit && current.length){
          pages.push(current);current=[];grid.replaceChildren(card);
        }
        current.push(fragment);
      }
      if(current.length)pages.push(current);
      result[slide.id]=pages;
      grid.replaceChildren(...cards);
    }
    return result;
  });
  fs.writeFileSync('slides/layout.json',JSON.stringify(plan,null,2)+'\n');
  console.log(`Split ${Object.keys(plan).length} dense slides; ${Object.values(plan).reduce((s,p)=>s+p.length-1,0)} continuation slides.`);
  await browser.close();
})().catch(e=>{console.error(e);process.exit(1)});
