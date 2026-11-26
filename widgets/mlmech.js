import {fitCompletePairs, conditionalMean, blendParameters, mixtureDensity, normalDensity,
        silverman, representativeMissing} from './mlmech-model.js';
import {element, controlsKeyboard, courseColors} from './common.js';

// Интерактив «Как ММП использует строки без САД» (D68): сквозная MAR-выборка.
// Слева – диаграмма рассеяния: полные пары, условные средние строк без САД на прямой
// E(САД | ИМТ) и их условные плотности; справа – маргинальная плотность САД.
// Ползунок λ задает вклад строк без САД: 0 – полные случаи, 1 – ММП.
export function mlMechanismWidget(d3, data) {
  const blue = courseColors.observed, red = courseColors.missing, black = '#1A1A1A';
  const fit = fitCompletePairs(data);
  const obsY = data.y.filter((_, i) => !data.M[i]);
  const bw = silverman(obsY);
  const reps = representativeMissing(data);
  const cc = blendParameters(data, 0, fit), ml = blendParameters(data, 1, fit);

  const root = element('div', undefined, {class: 'mdw mdw-mlmech'});
  controlsKeyboard(root);
  const svg = d3.select(root).append('svg').attr('viewBox', '0 0 1200 455').attr('role', 'img')
    .attr('aria-label', 'Строки без САД входят в оценки ММП своими условными распределениями');

  // Левая панель: ИМТ – САД.
  const L = {x0: 78, x1: 560, y0: 18, y1: 398};
  const allY = data.y;
  const sx = d3.scaleLinear().domain(d3.extent(data.x)).nice().range([L.x0, L.x1]);
  const sy = d3.scaleLinear().domain([d3.min(allY) - 3, d3.max(allY) + 3]).nice().range([L.y1, L.y0]);
  const gL = svg.append('g');
  gL.append('g').attr('transform', `translate(0,${L.y1})`).call(d3.axisBottom(sx).ticks(6));
  gL.append('g').attr('transform', `translate(${L.x0},0)`).call(d3.axisLeft(sy).ticks(6));
  gL.append('text').attr('x', (L.x0 + L.x1) / 2).attr('y', L.y1 + 44).attr('text-anchor', 'middle').text('ИМТ (кг/м²)');
  gL.append('text').attr('transform', `translate(${L.x0 - 56},${(L.y0 + L.y1) / 2}) rotate(-90)`)
    .attr('text-anchor', 'middle').text('САД (мм рт. ст.)');
  const clip = gL.append('g');
  const idx = data.x.map((_, i) => i);
  const obs = idx.filter(i => !data.M[i]), mis = idx.filter(i => data.M[i]);
  const hidden = clip.append('g').selectAll('path').data(mis).join('path')
    .attr('d', d3.symbol(d3.symbolCross, 22)).attr('transform', i => `translate(${sx(data.x[i])},${sy(data.y[i])}) rotate(45)`)
    .attr('fill', red).attr('opacity', 0);
  clip.append('g').selectAll('circle').data(obs).join('circle')
    .attr('cx', i => sx(data.x[i])).attr('cy', i => sy(data.y[i])).attr('r', 2.6)
    .attr('fill', blue).attr('opacity', .45);
  const condMeans = clip.append('g').selectAll('circle').data(mis).join('circle')
    .attr('cx', i => sx(data.x[i])).attr('cy', i => sy(conditionalMean(fit, data.x[i]))).attr('r', 3.2)
    .attr('fill', 'none').attr('stroke', red).attr('stroke-width', 1.2);
  // Условные плотности N(β0 + β1 x, σ²_{САД|ИМТ}) нескольких строк без САД, «лежа на боку».
  const s = Math.sqrt(fit.s2), widthBMI = 1.6, k = widthBMI * s * Math.sqrt(2 * Math.PI);
  const bells = clip.append('g').selectAll('g').data(reps).join('g');
  bells.append('line').attr('x1', i => sx(data.x[i])).attr('x2', i => sx(data.x[i]))
    .attr('y1', i => sy(conditionalMean(fit, data.x[i]) - 3 * s)).attr('y2', i => sy(conditionalMean(fit, data.x[i]) + 3 * s))
    .attr('stroke', red).attr('stroke-width', 1).attr('opacity', .6);
  bells.append('path').attr('fill', red).attr('fill-opacity', .18).attr('stroke', red).attr('stroke-width', 1.4)
    .attr('d', i => {
      const m = conditionalMean(fit, data.x[i]);
      const ys = d3.range(-3, 3.01, 0.1).map(z => m + z * s);
      return d3.line()(ys.map(v => [sx(data.x[i] + k * normalDensity(v, m, s)), sy(v)])
        .concat([[sx(data.x[i]), sy(m + 3 * s)]]).concat([[sx(data.x[i]), sy(m - 3 * s)]]));
    });
  const xr = sx.domain();
  const lineAt = (b0, b1) => [[sx(xr[0]), sy(b0 + b1 * xr[0])], [sx(xr[1]), sy(b0 + b1 * xr[1])]];
  gL.append('path').attr('d', d3.line()(lineAt(fit.b0, fit.b1))).attr('stroke', blue).attr('stroke-width', 3.2).attr('fill', 'none');
  gL.append('path').attr('d', d3.line()(lineAt(data.truth.intercept_SBP, data.truth.slope_SBP)))
    .attr('stroke', black).attr('stroke-width', 1.2).attr('fill', 'none');
  // Обратная регрессия E(ИМТ | САД): x = a + b·y. Истина – тонкая черная, оценка при текущем λ –
  // толстая синяя (D68): при λ = 0 это смещенная регрессия полных случаев, при λ = 1 – ММП.
  const yr = sy.domain();
  const lineBMI = (a, b) => [[sx(a + b * yr[0]), sy(yr[0])], [sx(a + b * yr[1]), sy(yr[1])]];
  const clipId = 'mlmech-clip';
  svg.append('defs').append('clipPath').attr('id', clipId).append('rect')
    .attr('x', L.x0).attr('y', L.y0).attr('width', L.x1 - L.x0).attr('height', L.y1 - L.y0);
  const reverseNow = gL.append('path').attr('clip-path', `url(#${clipId})`).attr('stroke', blue)
    .attr('stroke-width', 3.2).attr('fill', 'none');
  gL.append('path').attr('clip-path', `url(#${clipId})`)
    .attr('d', d3.line()(lineBMI(data.truth.intercept_BMI, data.truth.slope_BMI)))
    .attr('stroke', black).attr('stroke-width', 1.2).attr('fill', 'none');

  // Правая панель: маргинальная плотность САД.
  const R = {x0: 660, x1: 1170, y0: 18, y1: 398};
  const grid = d3.range(sy.domain()[0], sy.domain()[1] + 0.01, 0.5);
  const truthDens = grid.map(v => normalDensity(v, data.truth.mean_SBP, data.truth.sd_SBP));
  const ccDens = mixtureDensity(grid, data, 0, fit, bw), mlDens = mixtureDensity(grid, data, 1, fit, bw);
  const ymax = 1.08 * d3.max([...truthDens, ...ccDens, ...mlDens]);
  const rx = d3.scaleLinear().domain(sy.domain()).range([R.x0, R.x1]);
  const ry = d3.scaleLinear().domain([0, ymax]).range([R.y1, R.y0]);
  const gR = svg.append('g');
  gR.append('g').attr('transform', `translate(0,${R.y1})`).call(d3.axisBottom(rx).ticks(6));
  gR.append('g').attr('transform', `translate(${R.x0},0)`).call(d3.axisLeft(ry).ticks(4).tickFormat(d3.format('.3f')));
  gR.append('text').attr('x', (R.x0 + R.x1) / 2).attr('y', R.y1 + 44).attr('text-anchor', 'middle').text('САД (мм рт. ст.)');
  const dens = d3.line().x((_, k) => rx(grid[k])).y(v => ry(v));
  gR.append('path').attr('d', dens(ccDens)).attr('stroke', blue).attr('stroke-width', 2).attr('stroke-dasharray', '7 5').attr('fill', 'none');
  const current = gR.append('path').attr('stroke', blue).attr('stroke-width', 3.2).attr('fill', 'none');
  gR.append('path').attr('d', dens(truthDens)).attr('stroke', black).attr('stroke-width', 1.2).attr('fill', 'none');
  // Средние – треугольники под осью: истина (черный), CC (контур), текущее λ (залитый).
  const tri = (v, dx, stroke, fill) => gR.append('path').attr('d', d3.symbol(d3.symbolTriangle, 120)())
    .attr('stroke', stroke).attr('stroke-width', 1.6).attr('fill', fill)
    .attr('transform', `translate(${rx(v) + dx},${R.y1 + 12})`);
  tri(data.truth.mean_SBP, 0, black, black);
  tri(cc.mean_SBP, 0, blue, 'white');
  const triNow = tri(ml.mean_SBP, 0, blue, blue);

  // Управление: подпись с текущим λ, ползунок, переключатель скрытых значений – одной строкой.
  const controls = element('div', undefined, {class: 'mdw-mlmech-controls'}); root.append(controls);
  const lab = element('div', undefined, {class: 'mdw-mlmech-lambda'}); controls.append(lab);
  lab.append(element('span', 'Вклад строк без САД, λ = '));
  const out = element('output', '1.00'); lab.append(out);
  const sliderBox = element('div', undefined, {class: 'mdw-mlmech-slider'}); controls.append(sliderBox);
  const slider = element('input', undefined, {type: 'range', min: 0, max: 1, step: 0.05, value: 1,
    'aria-label': 'Вклад строк без САД: 0 – полные случаи, 1 – ММП'});
  sliderBox.append(slider);
  const ends = element('div', undefined, {class: 'mdw-mlmech-ends'});
  ends.append(element('span', '0 – полные случаи')); ends.append(element('span', '1 – ММП'));
  sliderBox.append(ends);
  const chk = element('label', undefined, {class: 'mdw-check'});
  const box = element('input', undefined, {type: 'checkbox'}); chk.append(box);
  chk.append(element('span', 'Скрытые значения')); controls.append(chk);

  // Таблица оценок.
  const table = element('table', undefined, {class: 'estimates mdw-mlmech-table'}); root.append(table);
  const fmt = (v, d) => (v < 0 ? '−' : '') + Math.abs(v).toFixed(d);
  const eqBMI = p => `${fmt(p.intercept_BMI, 2)} + ${fmt(p.slope_BMI, 3)} × САД`;
  const rows = [
    ['μ<sub>САД</sub>', p => fmt(p.mean_SBP, 1)], ['σ<sub>САД</sub>', p => fmt(p.sd_SBP, 1)],
    ['ρ<sub>XY</sub>', p => fmt(p.correlation, 3)], ['E(ИМТ | САД)', eqBMI]];
  // Без столбца θ̂_CD (D68): сравниваем истину, полные случаи и текущее λ.
  table.innerHTML = '<thead><tr><th>Параметры</th><th>θ</th><th>θ̂<sub>CC</sub></th>' +
    '<th class="mdw-now">θ̂(λ)</th></tr></thead><tbody>' + rows.map(([lbl, f], r) =>
      `<tr><th scope="row">${lbl}</th><td>${f(data.truth)}</td>` +
      `<td class="estimate-observed"><strong>${f(cc)}</strong></td><td class="estimate-observed" data-row="${r}"></td></tr>`).join('') + '</tbody>';

  function render() {
    const lambda = Number(slider.value);
    out.textContent = lambda.toFixed(2);
    const p = blendParameters(data, lambda, fit);
    condMeans.attr('opacity', lambda);
    bells.attr('opacity', lambda);
    hidden.attr('opacity', box.checked ? 0.5 : 0);
    current.attr('d', dens(mixtureDensity(grid, data, lambda, fit, bw)));
    reverseNow.attr('d', d3.line()(lineBMI(p.intercept_BMI, p.slope_BMI)));
    triNow.attr('transform', `translate(${rx(p.mean_SBP)},${R.y1 + 12})`);
    rows.forEach(([, f], r) => { table.querySelector(`td[data-row="${r}"]`).textContent = f(p); });
  }
  slider.addEventListener('input', render); box.addEventListener('change', render);
  render();
  return root;
}
