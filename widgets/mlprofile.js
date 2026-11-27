import {DEFAULTS, LIMITS, CHI2_1_95, simulate, kFromPercent, summary, lrCurves, profileRanges,
        profileGrid, axisCap, ellipsePoints, normalDensity, controlLabels, fmt, fmtPercent} from './mlprofile-model.js';
import {element, controlsKeyboard, courseColors} from './common.js';

// Интерактив W-MLPROFILE (D71): как строки только с ИМТ сдвигают оценку ММП для САД и почему
// оценка по полным случаям перестает согласовываться с данными (статистика 2Δℓ профильного
// правдоподобия). Все расчеты – в mlprofile-model.js; здесь только отрисовка и управление.
//
// Цвета по правилам курса (D21, D37): синий – оценки по наблюдаемым данным (полные случаи –
// штрих, ММП – толстая сплошная), черный пунктир – все данные до пропусков (только в симуляции),
// два дополнительных цвета спецификации – модели ММП с закрепленным параметром.
const blue = courseColors.observed, black = '#1A1A1A', gray = '#9AA1A9', light = '#CBD5E1', refColor = '#64748B';
const STYLE = {
  full: {color: black, width: 1.8, dash: '2 4', label: 'все данные до пропусков'},
  cc: {color: blue, width: 2.2, dash: '7 4', label: 'полные случаи'},
  ml: {color: blue, width: 3.2, dash: null, label: 'ММП с добавленными случаями'},
  fixedMean: {color: '#7C3AED', width: 2.2, dash: '8 3 2 3', label: 'ММП при закрепленном среднем САД'},
  fixedSd: {color: '#B45309', width: 2.2, dash: '12 4', label: 'ММП при закрепленном ст. отклонении САД'}
};
const PROFILE_POINTS = 49;
let instances = 0;

const styled = (sel, kind, width) => sel.attr('fill', 'none').attr('stroke', STYLE[kind].color)
  .attr('stroke-width', width ?? STYLE[kind].width).attr('stroke-dasharray', STYLE[kind].dash);

// Метки центров: кольцо – полные случаи, круг – ММП, крест – все данные, квадраты – закрепленные модели.
function addMarker(g, kind) {
  const c = STYLE[kind].color;
  if (kind === 'cc') return g.append('circle').attr('r', 6).attr('fill', '#fff').attr('stroke', c).attr('stroke-width', 2.4);
  if (kind === 'ml') return g.append('circle').attr('r', 6).attr('fill', c).attr('stroke', '#fff').attr('stroke-width', 1);
  if (kind === 'full') return g.append('path').attr('d', 'M-7,0H7M0,-7V7').attr('stroke', c).attr('stroke-width', 2.4);
  return g.append('rect').attr('x', -5).attr('y', -5).attr('width', 10).attr('height', 10).attr('fill', c);
}
const moveTo = (sel, px, py) => sel.attr('transform', `translate(${px},${py})`);

// Y в пикселях на кривой (точки в пикселях, x по возрастанию) при данном X.
function yAt(pts, X) {
  if (X <= pts[0][0]) return pts[0][1];
  for (let j = 1; j < pts.length; j++) if (X <= pts[j][0]) {
    const [x0, y0] = pts[j - 1], [x1, y1] = pts[j];
    return y0 + (y1 - y0) * (X - x0) / (x1 - x0);
  }
  return pts[pts.length - 1][1];
}

// Подпись линии-ориентира 3.84 не должна лежать на кривой: перебираем края и стороны линии.
function placeRefLabel(label, curvesPx, box, yRef) {
  const w = 36, h = 16;
  const candidates = [
    {anchor: 'end', x: box.x1 - 2, base: yRef - 4}, {anchor: 'start', x: box.x0 + 4, base: yRef - 4},
    {anchor: 'end', x: box.x1 - 2, base: yRef + h}, {anchor: 'start', x: box.x0 + 4, base: yRef + h}];
  const free = c => {
    const left = c.anchor === 'end' ? c.x - w : c.x, top = c.base - h + 2, bottom = c.base + 3;
    if (bottom > box.y1 + 2) return false;
    for (const pts of curvesPx) for (let X = left - 2; X <= left + w + 2; X += 2) {
      const Y = yAt(pts, X);
      if (Y >= top - 3 && Y <= bottom + 3) return false;
    }
    return true;
  };
  const c = candidates.find(free) ?? candidates[1];
  label.attr('x', c.x).attr('y', c.base).attr('text-anchor', c.anchor);
}

export function mlProfileWidget(d3, options = {}) {
  const uid = `mlprof-${++instances}`;
  // options.rho, options.psi1, options.percent – начальное состояние (для отдельной проверочной страницы).
  const state = {rho: options.rho ?? DEFAULTS.rho, psi1: options.psi1 ?? DEFAULTS.psi1, percent: options.percent ?? 0};
  let sim, curves, ranges, cap, k = 0, timer = null;

  const root = element('div', undefined, {class: 'mdw mdw-mlprof'});
  controlsKeyboard(root);
  const left = element('div', undefined, {class: 'mdw-mlprof-left'});
  const right = element('div', undefined, {class: 'mdw-mlprof-right'});
  root.append(left, right);

  // ---------- Регуляторы: подпись-формула над ползунком, каждый в своей строке ----------
  const ss = (sup, sub) => `<span class="mdw-ss"><span>${sup}</span><span>${sub}</span></span>`;
  const Mi = `<i>M</i>${ss('(<i>i</i>)', 'САД')}`, Yi = `<i>Y</i>${ss('(<i>i</i>)', 'ИМТ')}`;
  const slider = ([min, max, step], value, label) =>
    element('input', undefined, {type: 'range', min, max, step, value, 'aria-label': label});
  const row = (html, input, extra) => {
    const r = element('div', undefined, {class: 'mdw-mlprof-row'});
    const lab = element('div', undefined, {class: 'mdw-mlprof-label'}); lab.innerHTML = html;
    r.append(lab, input); if (extra) r.append(extra); left.append(r); return r;
  };
  const rhoInput = slider(LIMITS.rho, state.rho, 'Корреляция ИМТ и САД');
  row('<i>ρ</i><sub>XY</sub> = <output data-v="rho"></output> (<i>R</i><sup>2</sup> = <span data-v="r2"></span>%)' +
      '<small>X – ИМТ, Y – САД</small>', rhoInput);
  const psiInput = slider(LIMITS.psi1, state.psi1, 'Зависимость пропуска САД от ИМТ, ψ1');
  const note = element('div', undefined, {class: 'mdw-mlprof-note'});
  note.innerHTML = `Pr(${Mi} = 1) = <span data-v="all"></span> в целом; <span data-v="low"></span> при ИМТ = <i>μ</i> − <i>σ</i>; ` +
    `<span data-v="high"></span> при ИМТ = <i>μ</i> + <i>σ</i>`;
  row(`logit Pr(${Mi} = 1 | ${Yi}) = <i>ψ</i><sub>0</sub> + <i>ψ</i><sub>1</sub> × (${Yi} − <i>μ</i><sub>ИМТ</sub>) / <i>σ</i><sub>ИМТ</sub>;` +
      `&ensp;<i>ψ</i><sub>1</sub> = <output data-v="psi1"></output>, <i>ψ</i><sub>0</sub> = <span data-v="psi0"></span>`, psiInput, note);
  const pctInput = slider(LIMITS.percent, state.percent, 'Доля добавленных случаев с ИМТ, но без САД, %');
  const playLine = element('div', undefined, {class: 'mdw-mlprof-play'});
  const playButton = element('button', 'Добавлять постепенно', {type: 'button', 'aria-pressed': 'false'});
  playLine.append(pctInput, playButton);
  row('Доля добавленных случаев с ИМТ, но без САД: <output data-v="pct"></output>' +
      '<small>всего таких случаев: <span data-v="K"></span></small>', playLine);
  const val = name => root.querySelector(`[data-v="${name}"]`);

  // ---------- Легенда ----------
  const legend = element('div', undefined, {class: 'mdw-mlprof-legend'}); left.append(legend);
  for (const kind of ['full', 'cc', 'ml', 'fixedMean', 'fixedSd']) {
    const item = element('span'); legend.append(item);
    const sw = d3.select(item).append('svg').attr('viewBox', '0 0 46 16').attr('aria-hidden', 'true');
    styled(sw.append('line').attr('x1', 1).attr('x2', 45).attr('y1', 8).attr('y2', 8), kind, Math.min(STYLE[kind].width, 2.6));
    moveTo(addMarker(sw, kind), 23, 8);
    item.append(element('span', STYLE[kind].label));
  }

  // ---------- Диаграмма рассеяния и панель «распределение САД» ----------
  // Размеры viewBox равны ширине колонок на слайде 1600 × 900: текст SVG не мельче 16 px.
  const SW = 764, SH = 404, P = {x0: 62, x1: 562, y0: 30, y1: 356}, D = {x0: 588, x1: 760};
  const scatter = d3.select(left).append('svg').attr('viewBox', `0 0 ${SW} ${SH}`).attr('role', 'img')
    .attr('aria-label', 'ИМТ и САД: полные случаи, засечки ИМТ строк без САД и 95%-е эллипсы оценок');
  const sx = d3.scaleLinear().domain([14, 40]).range([P.x0, P.x1]);
  const sy = d3.scaleLinear().domain([90, 180]).range([P.y1, P.y0]);
  const dx = d3.scaleLinear().domain([0, 0.035]).range([D.x0, D.x1]);
  const defs = scatter.append('defs');
  defs.append('clipPath').attr('id', `${uid}-plot`).append('rect')
    .attr('x', P.x0).attr('y', P.y0).attr('width', P.x1 - P.x0).attr('height', P.y1 - P.y0);
  defs.append('clipPath').attr('id', `${uid}-dens`).append('rect')
    .attr('x', D.x0).attr('y', P.y0 - 4).attr('width', D.x1 - D.x0 + 4).attr('height', P.y1 - P.y0 + 4);
  scatter.append('g').attr('transform', `translate(0,${P.y1})`).call(d3.axisBottom(sx).tickValues(d3.range(15, 41, 5)));
  scatter.append('g').attr('transform', `translate(${P.x0},0)`).call(d3.axisLeft(sy).tickValues(d3.range(100, 181, 20)));
  scatter.append('text').attr('x', (P.x0 + P.x1) / 2).attr('y', P.y1 + 44).attr('text-anchor', 'middle').text('ИМТ (кг/м²)');
  scatter.append('text').attr('transform', `translate(${P.x0 - 46},${(P.y0 + P.y1) / 2}) rotate(-90)`)
    .attr('text-anchor', 'middle').text('САД (мм рт. ст.)');
  scatter.append('line').attr('x1', D.x0).attr('x2', D.x0).attr('y1', P.y0).attr('y2', P.y1).attr('stroke', gray);
  scatter.append('text').attr('x', D.x0 + 6).attr('y', P.y0 - 10).text('распределение САД');

  const plot = scatter.append('g').attr('clip-path', `url(#${uid}-plot)`);
  const gPoints = plot.append('g'), gRug = plot.append('g');
  const regLine = plot.append('line').attr('stroke', blue).attr('stroke-width', 1.4).attr('opacity', .8);
  const ell = {};
  for (const kind of ['fixedMean', 'fixedSd', 'cc', 'ml', 'full']) ell[kind] = styled(plot.append('path'), kind);
  const ccGuide = scatter.append('path').attr('fill', 'none').attr('stroke', blue).attr('stroke-width', 1.3).attr('stroke-dasharray', '7 4');
  const mlGuide = scatter.append('path').attr('fill', 'none').attr('stroke', blue).attr('stroke-width', 1.5).attr('stroke-dasharray', '2 3');
  const centers = {};
  for (const kind of ['fixedMean', 'fixedSd', 'cc', 'full', 'ml']) centers[kind] = addMarker(scatter.append('g'), kind);
  const dens = scatter.append('g').attr('clip-path', `url(#${uid}-dens)`);
  const densPath = {};
  for (const kind of ['cc', 'ml', 'full']) densPath[kind] = styled(dens.append('path'), kind);
  const yGrid = d3.range(90, 180.01, 0.5);
  const ellipse = d3.line().x(p => sx(p[0])).y(p => sy(p[1]));
  const density = est => d3.line()(yGrid.map(v => [dx(normalDensity(v, est.muY, est.varY)), sy(v)]));

  // ---------- Карточки ----------
  const cards = element('div', undefined, {class: 'mdw-mlprof-cards'}); right.append(cards);
  const card = (title, estimate) => {
    const c = element('div', undefined, {class: 'mdw-mlprof-card'}); cards.append(c);
    c.innerHTML = `<div class="mdw-card-title">${title}</div><div class="mdw-card-body">` +
      `<span class="mdw-card-value${estimate ? ' is-estimate' : ''}"></span><span class="mdw-card-sub"></span></div>`;
    return {value: c.querySelector('.mdw-card-value'), sub: c.querySelector('.mdw-card-sub')};
  };
  const cardMean = card('Среднее САД, ММП', true), cardSd = card('Ст. отклонение САД, ММП', true);
  const cardLrMean = card('2Δℓ для среднего САД'), cardLrSd = card('2Δℓ для ст. отклонения САД');

  // ---------- Панели профиля ----------
  const profilesBox = element('div', undefined, {class: 'mdw-mlprof-profiles'}); right.append(profilesBox);
  function profilePanel(which, name) {
    const W = 318, H = 250, Q = {x0: 40, x1: 310, y0: 34, y1: 218};
    const svg = d3.select(profilesBox).append('svg').attr('viewBox', `0 0 ${W} ${H}`).attr('role', 'img')
      .attr('aria-label', `Профильное правдоподобие: ${name}`);
    const id = `${uid}-${which}`;
    svg.append('defs').append('clipPath').attr('id', id).append('rect')
      .attr('x', Q.x0).attr('y', Q.y0).attr('width', Q.x1 - Q.x0).attr('height', Q.y1 - Q.y0);
    const title = svg.append('text').attr('class', 'mdw-mlprof-title').attr('x', 2).attr('y', 18);
    const x = d3.scaleLinear().range([Q.x0, Q.x1]), y = d3.scaleLinear().range([Q.y1, Q.y0]);
    const gx = svg.append('g').attr('transform', `translate(0,${Q.y1})`), gy = svg.append('g').attr('transform', `translate(${Q.x0},0)`);
    const ref = svg.append('line').attr('x1', Q.x0).attr('x2', Q.x1).attr('stroke', refColor).attr('stroke-width', 1.3).attr('stroke-dasharray', '3 3');
    const refLabel = svg.append('text').attr('class', 'mdw-mlprof-ref').text('3.84');
    const ccLine = styled(svg.append('line').attr('y1', Q.y0).attr('y2', Q.y1), 'cc', 1.4);
    const seg = svg.append('line').attr('stroke', blue).attr('stroke-width', 6).attr('y1', Q.y1);
    const curve = svg.append('path').attr('clip-path', `url(#${id})`).attr('fill', 'none').attr('stroke', blue).attr('stroke-width', 2.6);
    const fullTick = svg.append('line').attr('stroke', black).attr('stroke-width', 2.4).attr('y1', Q.y1 - 10).attr('y2', Q.y1 + 2);
    const mlDot = svg.append('circle').attr('r', 5.5).attr('fill', blue).attr('stroke', '#fff').attr('stroke-width', 1).attr('cy', Q.y1);
    const pick = est => which === 'mean' ? est.muY : est.sdY;
    return {
      rescale() {
        x.domain(ranges[which]); y.domain([0, cap]);
        gx.call(d3.axisBottom(x).ticks(5).tickFormat(d3.format('~f')));
        gy.call(d3.axisLeft(y).tickValues([0, cap / 2, cap]));
        ref.attr('y1', y(CHI2_1_95)).attr('y2', y(CHI2_1_95));
        ccLine.attr('x1', x(pick(sim.ccEstimate))).attr('x2', x(pick(sim.ccEstimate)));
        seg.attr('x1', x(pick(sim.ccEstimate))).attr('x2', x(pick(sim.ccEstimate)));
        fullTick.attr('x1', x(pick(sim.fullEstimate))).attr('x2', x(pick(sim.fullEstimate)));
      },
      update(s) {
        const lr = which === 'mean' ? s.lrMean : s.lrSd;
        const pts = profileGrid(sim, k, which, ranges[which], PROFILE_POINTS).map(([v, l]) => [x(v), y(Math.min(l, 1.5 * cap))]);
        curve.attr('d', d3.line()(pts));
        seg.attr('y2', y(Math.min(lr, cap)));
        mlDot.attr('cx', x(pick(s.ml)));
        title.text(`${name}: 2Δℓ = ${fmt(lr, 1)}`);
        placeRefLabel(refLabel, [pts], Q, y(CHI2_1_95));
      }
    };
  }
  const profiles = [profilePanel('mean', 'Среднее САД'), profilePanel('sd', 'Стандартное отклонение САД')];

  // ---------- Рост статистики с долей добавленных случаев ----------
  const GW = 650, GH = 234, G = {x0: 44, x1: 636, y0: 34, y1: 190};
  const growth = d3.select(right).append('svg').attr('viewBox', `0 0 ${GW} ${GH}`).attr('role', 'img')
    .attr('aria-label', 'Статистика 2Δℓ для оценок по полным случаям в зависимости от доли добавленных случаев');
  growth.append('text').attr('class', 'mdw-mlprof-title').attr('x', 2).attr('y', 18)
    .text('Статистика отношения правдоподобия 2Δℓ для оценки по полным случаям');
  const gxScale = d3.scaleLinear().domain([0, 100]).range([G.x0, G.x1]), gyScale = d3.scaleLinear().range([G.y1, G.y0]);
  growth.append('g').attr('transform', `translate(0,${G.y1})`)
    .call(d3.axisBottom(gxScale).tickValues(d3.range(0, 101, 20)).tickFormat(v => `${v}%`));
  const gyAxis = growth.append('g').attr('transform', `translate(${G.x0},0)`);
  growth.append('text').attr('x', (G.x0 + G.x1) / 2).attr('y', G.y1 + 40).attr('text-anchor', 'middle')
    .text('доля добавленных случаев с ИМТ, но без САД');
  const gRef = growth.append('line').attr('x1', G.x0).attr('x2', G.x1).attr('stroke', refColor).attr('stroke-width', 1.3).attr('stroke-dasharray', '3 3');
  const gRefLabel = growth.append('text').attr('class', 'mdw-mlprof-ref').text('3.84');
  const growthLine = d3.line().x(p => gxScale(p[0])).y(p => gyScale(p[1]));
  const track = {
    mean: growth.append('path').attr('fill', 'none').attr('stroke', gray).attr('stroke-width', 1.4),
    sd: growth.append('path').attr('fill', 'none').attr('stroke', gray).attr('stroke-width', 1.4).attr('stroke-dasharray', '6 4')};
  const now = {
    mean: growth.append('path').attr('fill', 'none').attr('stroke', blue).attr('stroke-width', 3),
    sd: growth.append('path').attr('fill', 'none').attr('stroke', blue).attr('stroke-width', 3).attr('stroke-dasharray', '6 4')};
  const tip = {
    mean: growth.append('circle').attr('r', 6).attr('fill', blue),
    sd: growth.append('rect').attr('x', -5.5).attr('y', -5.5).attr('width', 11).attr('height', 11).attr('fill', blue)};
  // Легенда – в левом верхнем углу, где обе кривые близки к нулю.
  const lg = growth.append('g').attr('transform', `translate(${G.x0 + 14},${G.y0 + 8})`);
  lg.append('line').attr('x2', 30).attr('stroke', blue).attr('stroke-width', 3);
  lg.append('circle').attr('cx', 30).attr('r', 5).attr('fill', blue);
  lg.append('text').attr('x', 42).attr('y', 5).text('среднее САД');
  lg.append('line').attr('x1', 168).attr('x2', 198).attr('stroke', blue).attr('stroke-width', 3).attr('stroke-dasharray', '6 4');
  lg.append('rect').attr('x', 193.5).attr('y', -4.5).attr('width', 9).attr('height', 9).attr('fill', blue);
  lg.append('text').attr('x', 210).attr('y', 5).text('стандартное отклонение САД');
  const series = which => curves[which].map((v, j) => [100 * j / sim.K, v]);

  // ---------- Пересчет ----------
  function rebuild() {
    sim = simulate({rho: state.rho, psi1: state.psi1});
    curves = lrCurves(sim); ranges = profileRanges(sim);
    cap = axisCap(Math.max(...curves.mean, ...curves.sd));
    const L = controlLabels(sim);
    val('rho').textContent = fmt(L.rho, 2); val('r2').textContent = L.r2Percent;
    val('psi1').textContent = fmt(L.psi1, 2); val('psi0').textContent = fmt(L.psi0, 2);
    val('all').textContent = fmtPercent(L.shareAll); val('low').textContent = fmtPercent(L.shareLow);
    val('high').textContent = fmtPercent(L.shareHigh); val('K').textContent = sim.K;

    gPoints.selectAll('circle').data(sim.cc, i => i).join('circle').attr('r', 3)
      .attr('fill', blue).attr('fill-opacity', .5).attr('cx', i => sx(sim.x[i])).attr('cy', i => sy(sim.y[i]));
    gRug.selectAll('line').data(sim.xOnly, i => i).join('line').attr('stroke-width', 1.6)
      .attr('x1', i => sx(sim.x[i])).attr('x2', i => sx(sim.x[i])).attr('y1', P.y1 - 18).attr('y2', P.y1 - 2);
    const cc = sim.ccEstimate, [xa, xb] = d3.extent(sim.x);
    regLine.attr('x1', sx(xa)).attr('x2', sx(xb))
      .attr('y1', sy(cc.muY + cc.slope * (xa - cc.muX))).attr('y2', sy(cc.muY + cc.slope * (xb - cc.muX)));
    ell.full.attr('d', ellipse(ellipsePoints(sim.fullEstimate)));
    ell.cc.attr('d', ellipse(ellipsePoints(cc)));
    moveTo(centers.cc, sx(cc.muX), sy(cc.muY));
    moveTo(centers.full, sx(sim.fullEstimate.muX), sy(sim.fullEstimate.muY));
    ccGuide.attr('d', `M${sx(cc.muX)},${sy(cc.muY)}H${D.x0}`);
    densPath.full.attr('d', density(sim.fullEstimate));
    densPath.cc.attr('d', density(cc));

    profiles.forEach(p => p.rescale());
    gyScale.domain([0, cap]);
    gyAxis.call(d3.axisLeft(gyScale).tickValues([0, cap / 2, cap]));
    gRef.attr('y1', gyScale(CHI2_1_95)).attr('y2', gyScale(CHI2_1_95));
    for (const w of ['mean', 'sd']) track[w].attr('d', growthLine(series(w)));
    placeRefLabel(gRefLabel, ['mean', 'sd'].map(w => series(w).map(([a, b]) => [gxScale(a), gyScale(b)])), G, gyScale(CHI2_1_95));
    update();
  }

  function update() {
    k = kFromPercent(sim, state.percent);
    const s = summary(sim, k), added = new Set(sim.xOnly.slice(0, k));
    val('pct').textContent = `${state.percent}%`;
    gRug.selectAll('line').attr('stroke', i => added.has(i) ? blue : light)
      .sort((a, b) => added.has(a) - added.has(b));
    ell.ml.attr('d', ellipse(ellipsePoints(s.ml)));
    ell.fixedMean.attr('d', ellipse(ellipsePoints(s.fixedMeanModel)));
    ell.fixedSd.attr('d', ellipse(ellipsePoints(s.fixedSdModel)));
    moveTo(centers.ml, sx(s.ml.muX), sy(s.ml.muY));
    moveTo(centers.fixedMean, sx(s.fixedMeanModel.muX), sy(s.fixedMeanModel.muY));
    moveTo(centers.fixedSd, sx(s.fixedSdModel.muX), sy(s.fixedSdModel.muY));
    // При k = 0 закрепленные модели совпадают с полными случаями и не рисуются.
    for (const kind of ['fixedMean', 'fixedSd']) {
      ell[kind].attr('visibility', k > 0 ? 'visible' : 'hidden');
      centers[kind].attr('visibility', k > 0 ? 'visible' : 'hidden');
    }
    mlGuide.attr('d', `M${sx(s.ml.muX)},${P.y1}V${sy(s.ml.muY)}H${D.x0}`);
    densPath.ml.attr('d', density(s.ml));

    cardMean.value.textContent = fmt(s.ml.muY, 1);
    cardMean.sub.innerHTML = `полные случаи: ${fmt(s.cc.muY, 1)},<br>все данные: ${fmt(s.full.muY, 1)}`;
    cardSd.value.textContent = fmt(s.ml.sdY, 1);
    cardSd.sub.innerHTML = `полные случаи: ${fmt(s.cc.sdY, 1)},<br>все данные: ${fmt(s.full.sdY, 1)}`;
    cardLrMean.value.textContent = fmt(s.lrMean, 1); cardLrMean.sub.textContent = `√(2Δℓ) = ${fmt(s.rootMean, 1)}`;
    cardLrSd.value.textContent = fmt(s.lrSd, 1); cardLrSd.sub.textContent = `√(2Δℓ) = ${fmt(s.rootSd, 1)}`;

    profiles.forEach(p => p.update(s));
    for (const w of ['mean', 'sd']) {
      const part = series(w).slice(0, k + 1);
      now[w].attr('d', growthLine(part));
      moveTo(tip[w], gxScale(100 * k / sim.K), gyScale(curves[w][k]));
    }
  }

  // ---------- Управление ----------
  function stop() {
    if (timer) clearInterval(timer);
    timer = null;
    playButton.textContent = 'Добавлять постепенно'; playButton.setAttribute('aria-pressed', 'false');
  }
  function setPercent(p) { state.percent = p; pctInput.value = p; update(); }
  playButton.addEventListener('click', () => {
    if (timer) return stop();
    if (matchMedia('(prefers-reduced-motion: reduce)').matches) return setPercent(100);
    if (state.percent >= 100) setPercent(0);
    playButton.textContent = 'Остановить'; playButton.setAttribute('aria-pressed', 'true');
    timer = setInterval(() => {
      setPercent(Math.min(100, state.percent + 1));
      if (state.percent >= 100) stop();
    }, 60);
  });
  rhoInput.addEventListener('input', () => { stop(); state.rho = Number(rhoInput.value); rebuild(); });
  psiInput.addEventListener('input', () => { stop(); state.psi1 = Number(psiInput.value); rebuild(); });
  pctInput.addEventListener('input', () => { stop(); state.percent = Number(pctInput.value); update(); });

  rebuild();
  return root;
}
