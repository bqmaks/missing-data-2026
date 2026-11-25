import {DEFAULTS, PRESETS, shown, expected, meanCurves, trajectories} from './locf-model.js';
import {element, button, controlsKeyboard, duration, courseColors} from './common.js';

export function locfWidget(d3, options = {}) {
  const root=element('div',undefined,{class:'mdw mdw-locf'});controlsKeyboard(root);
  let params={...DEFAULTS,...options}, arm='C', active='equal';
  const presetRow=element('div',undefined,{class:'mdw-presets'});root.append(presetRow);
  const presets=PRESETS.map(p=>button(p.label,()=>{params.pT=p.pT;params.pC=p.pC;active=p.id;sync();render();},presetRow));
  const svg=d3.select(root).append('svg').attr('viewBox','0 0 1000 400').attr('role','img')
    .attr('aria-label','Траектории пациентов и ожидаемые оценки эффекта LOCF');
  svg.append('title').text('Индивидуальные линии иллюстрируют перенос; оценки справа – аналитические ожидания.');
  const x=d3.scaleLinear().domain([0,12]).range([60,540]);
  const y=d3.scaleLinear().domain([25,65]).range([305,35]);
  const effect=d3.scaleLinear().domain([-16,16]).range([650,970]);
  svg.append('g').attr('transform','translate(0,305)').call(d3.axisBottom(x).tickValues([0,3,6,9,12]));
  svg.append('g').attr('transform','translate(60,0)').call(d3.axisLeft(y).ticks(4));
  svg.append('text').attr('x',300).attr('y',345).attr('text-anchor','middle').text('Месяц');
  const title=svg.append('text').attr('x',60).attr('y',20).attr('font-weight',600);
  svg.append('text').attr('x',650).attr('y',20).attr('font-weight',600).text('Эффект: лечение − контроль');
  const chart=svg.append('g'), means=svg.append('g'), bars=svg.append('g');
  const truthLine=svg.append('line').attr('y1',45).attr('y2',280).attr('stroke','#64748B').attr('stroke-dasharray','5 5');
  svg.append('g').attr('transform','translate(0,305)').call(d3.axisBottom(effect).tickValues([-16,-8,0,8,16]));
  svg.append('line').attr('x1',effect(0)).attr('x2',effect(0)).attr('y1',45).attr('y2',280).attr('stroke','#CBD5E1');
  const line=d3.line().x(d=>x(d.month)).y(d=>y(d.value));
  [[courseColors.observed,'наблюдения',null,1],[courseColors.missing,'скрытые','4 5',.5],[courseColors.imputed,'LOCF','7 4',1]].forEach(([c,t,dash,alpha],i)=>{
    svg.append('line').attr('x1',60+i*164).attr('x2',86+i*164).attr('y1',374).attr('y2',374).attr('stroke',c).attr('stroke-width',2).attr('stroke-dasharray',dash).attr('opacity',alpha);
    svg.append('text').attr('x',92+i*164).attr('y',380).text(t);
  });
  const status=element('p',undefined,{class:'mdw-status','aria-live':'polite'});root.append(status);
  const controls=element('div',undefined,{class:'mdw-ranges'});root.append(controls);
  const ranges={};
  for(const [key,label,min,max,step,factor] of [
    ['pT','Выбыло: лечение, %',0,60,5,100],['pC','Выбыло: контроль, %',0,60,5,100],
    ['cT','Изменение за визит: лечение',-3,1,.5,1],['cC','Изменение за визит: контроль',-3,1,.5,1]]) {
    const wrap=element('label'), text=element('span',label+' '), value=element('output');
    const input=element('input',undefined,{type:'range',min,max,step,'aria-label':label});
    text.append(value);wrap.append(text,input);controls.append(wrap);ranges[key]={input,value,factor};
    input.addEventListener('input',()=>{params[key]=Number(input.value)/factor;active=null;sync();render();});
  }
  const extra=element('div',undefined,{class:'mdw-extra'});root.append(extra);
  const armLabel=element('label','Траектории: '), armSelect=element('select',undefined,{'aria-label':'Рукав исследования'});
  [['C','Контроль'],['T','Лечение']].forEach(([v,t])=>armSelect.append(element('option',t,{value:v})));
  armLabel.append(armSelect);extra.append(armLabel);
  armSelect.addEventListener('change',()=>{arm=armSelect.value;render();});
  const timingLabel=element('label','Когда выбывают: '), timing=element('select',undefined,{'aria-label':'Когда выбывают'});
  [['0','рано'],['1','в середине'],['2','поздно']].forEach(([v,t])=>timing.append(element('option',t,{value:v})));
  timingLabel.append(timing);extra.append(timingLabel);
  timing.addEventListener('change',()=>{const k=Number(timing.value);params.lastVisits=[k,k+1];render();});
  const formulaLabel=element('label',undefined,{class:'mdw-check'}), check=element('input',undefined,{type:'checkbox'});
  formulaLabel.append(check,document.createTextNode('Показать формулу смещения'));extra.append(formulaLabel);
  const formula=element('p',undefined,{class:'mdw-formula'});root.append(formula);
  check.addEventListener('change',render);
  button('Сброс',()=>{params={...DEFAULTS,...options};arm='C';active='equal';timing.value='0';armSelect.value='C';check.checked=false;sync();render();},extra);
  function sync() {
    for(const [key,{input,value,factor}] of Object.entries(ranges)) {input.value=params[key]*factor;value.value=String(params[key]*factor);}
    presets.forEach((b,i)=>b.setAttribute('aria-pressed',String(active===PRESETS[i].id)));
  }
  function render() {
    const c=arm==='C'?params.cC:params.cT,p=arm==='C'?params.pC:params.pT;
    const people=trajectories(c,p,{...params,n:8,seed:1});
    const paths=people.flatMap(r=>{
      const observed={key:`${r.id}-obs`,color:courseColors.observed,alpha:.6,dash:null,points:r.points.filter(d=>d.observed).map(d=>({...d,value:d.y}))};
      if(!r.dropped)return [observed];
      return [observed,{key:`${r.id}-true`,color:courseColors.missing,alpha:.5,dash:'4 5',points:r.points.filter(d=>d.t>=r.last).map(d=>({...d,value:d.y}))},
        {key:`${r.id}-locf`,color:courseColors.imputed,alpha:1,dash:'7 4',points:r.points.filter(d=>d.t>=r.last).map(d=>({...d,value:d.locf}))}];
    });
    chart.selectAll('path.trajectory').data(paths,d=>d.key).join('path').attr('class','trajectory').attr('fill','none').attr('stroke',d=>d.color)
      .attr('stroke-dasharray',d=>d.dash).attr('stroke-width',1.5).attr('opacity',d=>d.alpha)
      .interrupt().transition().duration(duration(250)).attr('d',d=>line(d.points));
    chart.selectAll('circle').data(people.flatMap(r=>r.points.filter(p=>p.observed).map(p=>({...p,key:`${r.id}-${p.t}`}))),d=>d.key)
      .join('circle').attr('r',2.3).attr('fill',courseColors.observed).attr('cx',d=>x(d.month)).attr('cy',d=>y(d.y));
    chart.selectAll('path.marker').data(people.flatMap(r=>r.points.filter(p=>!p.observed).map(p=>({...p,key:`${r.id}-${p.t}`}))),d=>d.key)
      .join('path').attr('class','marker').attr('d',d3.symbol().type(d3.symbolTriangle).size(24))
      .attr('fill',courseColors.imputed).attr('opacity',1).attr('transform',d=>`translate(${x(d.month)},${y(d.locf)})`);
    const curves=meanCurves(c,p,params);
    means.selectAll('path').data(p === 0 ? ['truth'] : ['truth','locf'], d=>d).join('path').attr('fill','none').attr('stroke',d=>d==='truth'?'#1E293B':courseColors.imputed).attr('stroke-width',3.5)
      .interrupt().transition().duration(duration(250)).attr('d',k=>line(curves.map(d=>({...d,value:d[k]}))));
    const v=shown(params), e=expected(params);
    const values=[{key:'truth',label:'Истина',value:e.truth,text:v.truth},{key:'cc',label:'Анализ полных случаев',value:e.cc,text:v.cc},{key:'locf',label:'LOCF',value:e.locf,text:v.locf}];
    const groups=bars.selectAll('g').data(values,d=>d.key).join(enter=>{const g=enter.append('g');g.append('rect');g.append('text').attr('class','label');g.append('text').attr('class','value');return g;});
    groups.attr('transform',(_,i)=>`translate(0,${75+i*85})`);
    groups.select('rect').attr('height',24).attr('fill',d=>d.key==='locf'?courseColors.imputed:'#0F766E')
      .interrupt().transition().duration(duration(250)).attr('x',d=>Math.min(effect(0),effect(d.value))).attr('width',d=>Math.abs(effect(d.value)-effect(0)));
    groups.select('.label').attr('x',650).attr('y',-10).text(d=>d.label);
    groups.select('.value').attr('x',d=>Math.max(665,Math.min(955,effect(d.value)))).attr('y',18).attr('dx',d=>d.value>=0?6:-6).attr('text-anchor',d=>d.value>=0?'start':'end').text(d=>d.text);
    truthLine.interrupt().transition().duration(duration(250)).attr('x1',effect(e.truth)).attr('x2',effect(e.truth));
    title.text(arm==='C'?'Контроль':'Лечение');
    status.textContent=v.direction===0?'LOCF совпадает с истинным эффектом':`LOCF: ${v.locf} при истинном эффекте ${v.truth} – эффект ${v.direction<0?'занижен':'завышен'} на ${v.diff}`;
    formula.hidden=!check.checked;
    formula.textContent=`Смещение = (K − L̄) · (pC · cC − pT · cT); K = ${params.K}, L̄ = среднее(${params.lastVisits.join(", ")}), pC = ${params.pC}, cC = ${params.cC}, pT = ${params.pT}, cT = ${params.cT}. Точно: ${e.bias.toFixed(3)}.`;
    root.dataset.locf=v.locf;root.dataset.truth=v.truth;
  }
  sync();render();return root;
}
