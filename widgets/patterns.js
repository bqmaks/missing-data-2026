import {columnOrder, rowOrder, shuffled, makeDropout, makeGeneral} from './patterns-model.js';
import {element, button, controlsKeyboard, duration, courseColors} from './common.js';

export function patternsWidget(d3, options = {}) {
  const monotonic = makeDropout({n:10,p:8,completeRate:0.2});
  const datasets = options.datasets || [
    // Y1...Y8 обозначают исходные переменные, а не хронологию визитов.
    // Маска уже переставлена; имена закрепляем за столбцами один раз.
    {...monotonic, label:'Monotonic', columns:monotonic.columns.map((_,j)=>`Y${j+1}`)},
    {...makeGeneral({n:10,p:8,q:0.5}), label:'General'}
  ];
  const root = element('div', undefined, {class:'mdw mdw-patterns'});
  controlsKeyboard(root);
  const svg = d3.select(root).append('svg').attr('viewBox','0 0 1000 490')
    .attr('role','img').attr('aria-label','Паттерн пропущенных значений: сортировка строк и столбцов');
  svg.append('title').text('Синий – наблюдено; красный – пропущено.');
  const frame = svg.append('g').attr('class','pattern-frame');
  frame.append('rect').attr('class','pattern-border').attr('x',7).attr('y',4).attr('fill','white').attr('stroke','#1a1a1a').attr('stroke-width',1.5);
  frame.append('rect').attr('class','pattern-heading').attr('x',7).attr('y',4).attr('height',50).attr('fill','#f0f0f0').attr('stroke','#1a1a1a').attr('stroke-width',1.5);
  const headers = svg.append('g'), cells = svg.append('g').attr('class','pattern-cells');
  const panel = element('div', undefined, {class:'mdw-pattern-panel'});root.append(panel);
  const label = element('label','Тип паттерна');
  const select = element('select',undefined,{'aria-label':'Тип паттерна'});
  datasets.forEach((d,i)=>select.append(element('option',d.label,{value:i})));
  label.append(select);panel.append(label);
  let data, cols, rows;
  const sortCols = () => {cols=columnOrder(data.M);render();};
  const sortRows = () => {rows=rowOrder(data.M,cols);render();};
  button('Сортировать строки',sortRows,panel);
  button('Сортировать столбцы',sortCols,panel);
  const selectPattern=()=>{data=datasets[Number(select.value)];cols=data.columns.map((_,j)=>j);rows=shuffled(data.M.length,1100);render();};
  select.addEventListener('change',selectPattern);
  function render() {
    const rowHeight=Math.min(56,420/rows.length), colWidth=Math.min(110,600/cols.length), gap=4;
    frame.select('.pattern-border').attr('width',cols.length*colWidth-gap+10).attr('height',56+rows.length*rowHeight-gap+5);
    frame.select('.pattern-heading').attr('width',cols.length*colWidth-gap+10);
    const rendered=rows.flatMap((i,r)=>cols.map((j,c)=>({key:`${i}-${j}`,i,j,r,c,value:data.M[i][j]})));
    cells.selectAll('rect').data(rendered,d=>d.key).join(
      enter=>enter.append('rect').attr('class','pattern-cell').attr('x',d=>12+d.c*colWidth).attr('y',d=>60+d.r*rowHeight),
      update=>update,exit=>exit.remove()
    ).interrupt().attr('fill',d=>d.value?courseColors.missing:courseColors.observed)
      .transition().duration(duration(600)).attr('x',d=>12+d.c*colWidth).attr('y',d=>60+d.r*rowHeight)
      .attr('width',colWidth-gap).attr('height',rowHeight-gap);
    headers.selectAll('text').data(cols,j=>j).join('text').attr('class','variable')
      .text(j=>data.columns[j]).attr('text-anchor','middle')
      .interrupt().transition().duration(duration(600)).attr('transform',j=>`translate(${12+(cols.indexOf(j)+.5)*colWidth},35)`);
    root.dataset.dataset=data.id;root.dataset.rows=rows.length;
  }
  selectPattern();return root;
}
