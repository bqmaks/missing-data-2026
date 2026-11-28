import {DEFAULTS, LIMITS, simulate, kFromPercent, summary, lrCurves, profileRanges,
        profileGrid, axisCap, ellipsePoints, normalDensity, controlLabels, fmt, fmtPercent} from './mlprofile-model.js';
import {PROFILE_PARAMS} from './mlprofile-config.js';
import {element, controlsKeyboard, courseColors} from './common.js';

// Интерактив W-MLPROFILE (D71, D94): как строки только с ИМТ сдвигают оценку ММП для САД и почему
// оценка по полным случаям перестает согласовываться с данными (статистика отношения
// правдоподобий −2 log Λ(c), Λ(c) = L_p(c) / L(θ̂_ML; Y_obs) – относительное профильное
// правдоподобие, D99). Все расчеты – в mlprofile-model.js; здесь только отрисовка и управление.
//
// Обозначения – как во всей презентации (D21, D37, D38, D42, D44): complete data (все данные
// до пропусков, только в симуляции) – тонкая черная линия и черная точка среднего; оценки
// по наблюдаемым данным синие и в 2–3 раза толще: complete case – штрих и контурный маркер,
// ММП – сплошная и залитый маркер; пропуски (строки без САД) – красные засечки.
const blue = courseColors.observed, red = courseColors.missing, black = '#1A1A1A', gray = '#9AA1A9';
const STYLE = {
  full: {color: black, width: 1.3, dash: null, label: 'complete data'},
  cc: {color: blue, width: 2.6, dash: '8 5', label: 'complete case'},
  ml: {color: blue, width: 3.6, dash: null, label: 'maximum likelihood'}
};
const PROFILE_POINTS = 49;
const ICON_PLAY = '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="M4.5 2.5L13 8L4.5 13.5Z"/></svg>';
const ICON_PAUSE = '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="M4 3H7V13H4ZM9 3H12V13H9Z"/></svg>';
let instances = 0;

const styled = (sel, kind, width) => sel.attr('fill', 'none').attr('stroke', STYLE[kind].color)
  .attr('stroke-width', width ?? STYLE[kind].width).attr('stroke-dasharray', STYLE[kind].dash);

// Точки средних: черная – complete data, синее кольцо – complete case, синий круг – ММП.
function addMarker(g, kind) {
  if (kind === 'cc') return g.append('circle').attr('r', 6).attr('fill', '#fff').attr('stroke', blue).attr('stroke-width', 2.4);
  if (kind === 'ml') return g.append('circle').attr('r', 6.5).attr('fill', blue).attr('stroke', '#fff').attr('stroke-width', 1);
  return g.append('circle').attr('r', 5).attr('fill', black);
}
const moveTo = (sel, px, py) => sel.attr('transform', `translate(${px},${py})`);

// Подпись вида μ̂_CC внутри SVG: буква, нижний индекс и отдельный знак ˆ по центру буквы.
// Комбинируемый U+0302 шрифт слайда ставит со сдвигом вправо (D95), поэтому крышка – свой элемент.
const HAT_DX = {'μ': 4.6, 'σ': 4.7};
// scale – отношение размера шрифта к 16 px (HAT_DX подобраны для 16 px).
function hatLabel(g, x, y, sym, sub, scale = 1) {
  const t = g.append('text').attr('x', x).attr('y', y).text(sym);
  t.append('tspan').attr('baseline-shift', 'sub').attr('font-size', 12 * scale).text(sub);
  g.append('text').attr('x', x + HAT_DX[sym] * scale).attr('y', y - .5).attr('text-anchor', 'middle').text('ˆ');
  return t;
}

// Подпись −2 log Λ(…) для осей SVG (D99): аргумент Λ – буква с нижним индексом.
function lambdaLabel(sel, sym, sub) {
  sel.text(`−2 log Λ(${sym}`);
  sel.append('tspan').attr('baseline-shift', 'sub').attr('font-size', 12).text(sub);
  sel.append('tspan').text(')');
  return sel;
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
  row('<i>ρ</i><sub>XY</sub> = <output data-v="rho"></output> (<i>R</i><sup>2</sup> = <span data-v="r2"></span>%)', rhoInput);
  const psiInput = slider(LIMITS.psi1, state.psi1, 'Зависимость пропуска САД от ИМТ, ψ1');
  const note = element('div', undefined, {class: 'mdw-mlprof-note'});
  note.innerHTML = `Pr(${Mi} = 1) = <span data-v="all"></span>`;
  // Значения ψ₀ и ψ₁ стоят прямо в формуле (D94), жирным, без цвета.
  row(`logit Pr(${Mi} = 1 | ${Yi}) = <output data-v="psi0"></output> + <output data-v="psi1"></output>` +
      ` × (${Yi} − <i>μ</i><sub>ИМТ</sub>) / <i>σ</i><sub>ИМТ</sub>`, psiInput, note);
  const pctInput = slider(LIMITS.percent, state.percent, 'Добавлено строк без САД, %');
  const playLine = element('div', undefined, {class: 'mdw-mlprof-play'});
  const playButton = element('button', undefined, {type: 'button', class: 'mdw-mlprof-playbtn',
    'aria-pressed': 'false', 'aria-label': 'Добавлять постепенно', title: 'Добавлять постепенно'});
  playButton.innerHTML = ICON_PLAY;
  playLine.append(pctInput, playButton);
  row('Добавлено строк без САД: <output data-v="pct"></output>', playLine);
  const val = name => root.querySelector(`[data-v="${name}"]`);

  // ---------- Легенда ----------
  const legend = element('div', undefined, {class: 'mdw-mlprof-legend'}); left.append(legend);
  for (const kind of ['full', 'cc', 'ml']) {
    const item = element('span'); legend.append(item);
    const sw = d3.select(item).append('svg').attr('viewBox', '0 0 46 16').attr('aria-hidden', 'true');
    styled(sw.append('line').attr('x1', 1).attr('x2', 45).attr('y1', 8).attr('y2', 8), kind);
    moveTo(addMarker(sw, kind), 23, 8);
    item.append(element('span', STYLE[kind].label));
  }

  // ---------- Диаграмма рассеяния и маргинальная плотность САД справа ----------
  // Размеры viewBox равны ширине колонок на слайде 1600 × 900: текст SVG не мельче 16 px.
  const SW = 764, SH = 404, P = {x0: 62, x1: 562, y0: 16, y1: 356}, D = {x0: 588, x1: 760};
  const scatter = d3.select(left).append('svg').attr('viewBox', `0 0 ${SW} ${SH}`).attr('role', 'img')
    .attr('aria-label', 'ИМТ и САД: полные случаи, засечки ИМТ строк без САД и 95%-е эллипсы оценок');
  const sx = d3.scaleLinear().domain([14, 40]).range([P.x0, P.x1]);
  const sy = d3.scaleLinear().domain([85, 185]).range([P.y1, P.y0]);
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

  const plot = scatter.append('g').attr('clip-path', `url(#${uid}-plot)`);
  const gPoints = plot.append('g'), gRug = plot.append('g');
  const regLine = plot.append('line').attr('stroke', blue).attr('stroke-width', 1.4).attr('opacity', .8);
  const ell = {};
  for (const kind of ['cc', 'ml', 'full']) ell[kind] = styled(plot.append('path'), kind);
  const ccGuide = scatter.append('path').attr('fill', 'none').attr('stroke', blue).attr('stroke-width', 1.3).attr('stroke-dasharray', '7 4');
  const mlGuide = scatter.append('path').attr('fill', 'none').attr('stroke', blue).attr('stroke-width', 1.5).attr('stroke-dasharray', '2 3');
  const centers = {};
  for (const kind of ['cc', 'full', 'ml']) centers[kind] = addMarker(scatter.append('g'), kind);
  const dens = scatter.append('g').attr('clip-path', `url(#${uid}-dens)`);
  const densPath = {};
  for (const kind of ['cc', 'ml', 'full']) densPath[kind] = styled(dens.append('path'), kind);
  const yGrid = d3.range(85, 185.01, 0.5);
  const ellipse = d3.line().x(p => sx(p[0])).y(p => sy(p[1]));
  const density = est => d3.line()(yGrid.map(v => [dx(normalDensity(v, est.muY, est.varY)), sy(v)]));

  // ---------- Правая колонка: два раздела – μ_САД и σ_САД ----------
  const sections = element('div', undefined, {class: 'mdw-mlprof-sections'}); right.append(sections);
  function section(which) {
    const sec = element('div', undefined, {class: 'mdw-mlprof-section'}); sections.append(sec);
    const sym = which === 'mean' ? 'μ' : 'σ', hat = `<span class="mdw-hat">${sym}</span>`;
    const c = element('div', undefined, {class: 'mdw-mlprof-card'}); sec.append(c);
    c.innerHTML = `<div class="mdw-card-title"><i>${sym}</i><sub>САД</sub></div><div class="mdw-card-body">` +
      `<span class="mdw-card-main">${hat}<sub>ML</sub> = <span class="mdw-card-value" data-c="ml"></span></span>` +
      `<span class="mdw-card-sub"><span class="is-estimate">${hat}<sub>CC</sub> = <span data-c="cc"></span></span><br>` +
      `${hat}<sub>CD</sub> = <span data-c="cd"></span></span></div>`;
    const cell = name => c.querySelector(`[data-c="${name}"]`);
    return {ml: cell('ml'), cc: cell('cc'), cd: cell('cd'), panel: profilePanel(which, sym, sec)};
  }

  // Панель профиля (D99): по вертикали −2 log Λ(μ_САД) – статистика отношения правдоподобий
  // как функция значения параметра; над графиком – ее значение для оценки complete case.
  function profilePanel(which, sym, parent) {
    const W = 318, H = 232, Q = {x0: 58, x1: 310, y0: 30, y1: 204};
    const svg = d3.select(parent).append('svg').attr('viewBox', `0 0 ${W} ${H}`).attr('role', 'img')
      .attr('aria-label', `Профильное правдоподобие для ${sym} САД: −2 log Λ(${sym}_САД)`);
    const id = `${uid}-${which}`;
    svg.append('defs').append('clipPath').attr('id', id).append('rect')
      .attr('x', Q.x0).attr('y', Q.y0).attr('width', Q.x1 - Q.x0).attr('height', Q.y1 - Q.y0);
    // Заголовок −2 log Λ(μ̂_CC) = …: «−2 log Λ(» прижат правым краем к TITLE_X, далее буква с крышкой
    // (hatLabel), индекс CC и значение – продолжение того же текста.
    const TITLE_X = 125, TITLE_SCALE = 17 / 16;
    const titleHead = svg.append('g').attr('class', 'mdw-mlprof-titlegroup');
    titleHead.append('text').attr('x', TITLE_X).attr('y', 16).attr('text-anchor', 'end').text('−2 log Λ(');
    const titleText = hatLabel(titleHead, TITLE_X, 16, sym, 'CC', TITLE_SCALE);
    titleText.append('tspan').text(') = ');
    const titleValue = titleText.append('tspan');
    lambdaLabel(svg.append('text').attr('transform', `translate(16,${(Q.y0 + Q.y1) / 2}) rotate(-90)`)
      .attr('text-anchor', 'middle'), sym, 'САД');
    const x = d3.scaleLinear().range([Q.x0, Q.x1]), y = d3.scaleLinear().range([Q.y1, Q.y0]);
    const gx = svg.append('g').attr('transform', `translate(0,${Q.y1})`), gy = svg.append('g').attr('transform', `translate(${Q.x0},0)`);
    const ccLine = styled(svg.append('line').attr('y1', Q.y0).attr('y2', Q.y1), 'cc', 1.4);
    const seg = svg.append('line').attr('stroke', blue).attr('stroke-width', 6).attr('y1', Q.y1);
    const curve = svg.append('path').attr('clip-path', `url(#${id})`).attr('fill', 'none').attr('stroke', blue).attr('stroke-width', 2.6);
    // Complete data – тонкая черная вертикальная пунктирная линия во всю высоту (D98), как штрих complete case.
    const fullTick = svg.append('line').attr('stroke', black).attr('stroke-width', 1.3).attr('stroke-dasharray', '5 4')
      .attr('y1', Q.y0).attr('y2', Q.y1);
    const mlDot = svg.append('circle').attr('r', 5.5).attr('fill', blue).attr('stroke', '#fff').attr('stroke-width', 1).attr('cy', Q.y1);
    const pick = est => which === 'mean' ? est.muY : est.sdY;
    return {
      rescale() {
        x.domain(ranges[which]); y.domain([0, cap]);
        gx.call(d3.axisBottom(x).ticks(5).tickFormat(d3.format('~f')));
        gy.call(d3.axisLeft(y).tickValues([0, cap / 2, cap]));
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
        titleValue.text(fmt(lr, 1));
      }
    };
  }
  const parts = {mean: section('mean'), sd: section('sd')};

  // ---------- Рост статистики −2 log Λ для оценок complete case с долей добавленных строк ----------
  const GW = 650, GH = 222, G = {x0: 58, x1: 636, y0: 12, y1: 178};
  const growth = d3.select(right).append('svg').attr('class', 'mdw-mlprof-growth').attr('viewBox', `0 0 ${GW} ${GH}`).attr('role', 'img')
    .attr('aria-label', 'Статистика −2 log Λ для оценок complete case в зависимости от доли добавленных строк без САД');
  const gxScale = d3.scaleLinear().domain([0, 100]).range([G.x0, G.x1]), gyScale = d3.scaleLinear().range([G.y1, G.y0]);
  growth.append('g').attr('transform', `translate(0,${G.y1})`)
    .call(d3.axisBottom(gxScale).tickValues(d3.range(0, 101, 20)).tickFormat(v => `${v}%`));
  const gyAxis = growth.append('g').attr('transform', `translate(${G.x0},0)`);
  growth.append('text').attr('transform', `translate(16,${(G.y0 + G.y1) / 2}) rotate(-90)`)
    .attr('text-anchor', 'middle').text('−2 log Λ');
  growth.append('text').attr('x', (G.x0 + G.x1) / 2).attr('y', G.y1 + 40).attr('text-anchor', 'middle')
    .text('добавлено строк без САД');
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
  const lg = growth.append('g').attr('transform', `translate(${G.x0 + 14},${G.y0 + 12})`);
  lg.append('line').attr('x2', 30).attr('stroke', blue).attr('stroke-width', 3);
  lg.append('circle').attr('cx', 30).attr('r', 5).attr('fill', blue);
  hatLabel(lg, 42, 5, 'μ', 'CC');
  lg.append('line').attr('x1', 104).attr('x2', 134).attr('stroke', blue).attr('stroke-width', 3).attr('stroke-dasharray', '6 4');
  lg.append('rect').attr('x', 129.5).attr('y', -4.5).attr('width', 9).attr('height', 9).attr('fill', blue);
  hatLabel(lg, 146, 5, 'σ', 'CC');
  const series = which => curves[which].map((v, j) => [100 * j / sim.K, v]);

  // ---------- Пересчет ----------
  function rebuild() {
    sim = simulate({rho: state.rho, psi1: state.psi1, ...PROFILE_PARAMS});
    curves = lrCurves(sim); ranges = profileRanges(sim);
    cap = axisCap(Math.max(...curves.mean, ...curves.sd));
    const L = controlLabels(sim);
    val('rho').textContent = fmt(L.rho, 2); val('r2').textContent = L.r2Percent;
    val('psi1').textContent = fmt(L.psi1, 2); val('psi0').textContent = fmt(L.psi0, 2);
    val('all').textContent = fmtPercent(L.shareAll);

    gPoints.selectAll('circle').data(sim.cc, i => i).join('circle').attr('r', 2.6)
      .attr('fill', blue).attr('fill-opacity', .3).attr('cx', i => sx(sim.x[i])).attr('cy', i => sy(sim.y[i]));
    gRug.selectAll('line').data(sim.xOnly, i => i).join('line').attr('stroke', red).attr('stroke-width', 1.4)
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

    for (const w of ['mean', 'sd']) parts[w].panel.rescale();
    gyScale.domain([0, cap]);
    gyAxis.call(d3.axisLeft(gyScale).tickValues([0, cap / 2, cap]));
    for (const w of ['mean', 'sd']) track[w].attr('d', growthLine(series(w)));
    update();
  }

  function update() {
    k = kFromPercent(sim, state.percent);
    const s = summary(sim, k), added = new Set(sim.xOnly.slice(0, k));
    val('pct').textContent = `${state.percent}%`;
    // Засечки ИМТ строк без САД красные (D94): добавленные – полностью, остальные – бледные.
    gRug.selectAll('line').attr('stroke-opacity', i => added.has(i) ? 1 : .3)
      .sort((a, b) => added.has(a) - added.has(b));
    ell.ml.attr('d', ellipse(ellipsePoints(s.ml)));
    moveTo(centers.ml, sx(s.ml.muX), sy(s.ml.muY));
    mlGuide.attr('d', `M${sx(s.ml.muX)},${P.y1}V${sy(s.ml.muY)}H${D.x0}`);
    densPath.ml.attr('d', density(s.ml));

    for (const [w, key] of [['mean', 'muY'], ['sd', 'sdY']]) {
      parts[w].ml.textContent = fmt(s.ml[key], 1);
      parts[w].cc.textContent = fmt(s.cc[key], 1);
      parts[w].cd.textContent = fmt(s.full[key], 1);
      parts[w].panel.update(s);
    }
    for (const w of ['mean', 'sd']) {
      now[w].attr('d', growthLine(series(w).slice(0, k + 1)));
      moveTo(tip[w], gxScale(100 * k / sim.K), gyScale(curves[w][k]));
    }
  }

  // ---------- Управление ----------
  function stop() {
    if (timer) clearInterval(timer);
    timer = null;
    playButton.innerHTML = ICON_PLAY; playButton.setAttribute('aria-pressed', 'false');
    playButton.setAttribute('aria-label', 'Добавлять постепенно'); playButton.title = 'Добавлять постепенно';
  }
  function setPercent(p) { state.percent = p; pctInput.value = p; update(); }
  playButton.addEventListener('click', () => {
    if (timer) return stop();
    if (matchMedia('(prefers-reduced-motion: reduce)').matches) return setPercent(100);
    if (state.percent >= 100) setPercent(0);
    playButton.innerHTML = ICON_PAUSE; playButton.setAttribute('aria-pressed', 'true');
    playButton.setAttribute('aria-label', 'Остановить'); playButton.title = 'Остановить';
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
