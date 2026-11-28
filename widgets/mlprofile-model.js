// Расчетная часть виджета W-MLPROFILE. Чистые функции без DOM и без D3: тестируются в Node.
//
// Модель: X = ИМТ, Y = САД, (X, Y) - двумерное нормальное распределение.
// Пропуски только в Y (монотонный паттерн), механизм MAR:
//   logit Pr(M = 1 | X) = psi0 + psi1 * (X - muX) / sdX,
// psi0 подбирается так, чтобы средняя вероятность пропуска по выборке равнялась missShare.
//
// Оценки ММП по «полным случаям + k случаев только с X» считаются по факторизации
// f(x, y) = f(x) * f(y | x): маргинальная часть по всем включенным x, регрессия по полным случаям.
// Все дисперсии - оценки ММП (делитель n, а не n - 1).
//
// Статистика отношения правдоподобия для параметра, закрепленного на значении c:
//   2 * [ l(оценка ММП) - max l при ограничении ]   (профильное правдоподобие).

export const DEFAULTS = Object.freeze({
  n: 320,
  seed: 20261127,
  muX: 27, sdX: 4,      // ИМТ, кг/м2
  muY: 135, sdY: 15,    // САД, мм рт. ст.
  rho: 0.6,             // корреляция X и Y в генеральной совокупности
  psi1: 1.8,            // логарифм отношения шансов пропуска на 1 ст. отклонение X
  missShare: 0.375      // целевая доля пропусков в Y
});

/** Диапазоны регуляторов: [минимум, максимум, шаг]. */
export const LIMITS = Object.freeze({
  rho: [0, 0.9, 0.05],
  psi1: [0, 3, 0.05],
  percent: [0, 100, 1]
});

/** 95%-й квантиль хи-квадрат с 1 степенью свободы: линия-ориентир на графиках. */
export const CHI2_1_95 = 3.841458820694124;
/** 95%-й квантиль хи-квадрат с 2 степенями свободы: контур эллипса. */
export const CHI2_2_95 = 5.991464547107979;

const expit = (v) => 1 / (1 + Math.exp(-v));

/** Генератор равномерных чисел mulberry32. Тот же, что в согласованном наброске. */
export function mulberry32(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function stats(x, y, idx) {
  const m = idx.length;
  let mx = 0, my = 0;
  for (const i of idx) { mx += x[i]; my += y[i]; }
  mx /= m; my /= m;
  let Sxx = 0, Syy = 0, Sxy = 0;
  for (const i of idx) {
    Sxx += (x[i] - mx) ** 2; Syy += (y[i] - my) ** 2; Sxy += (x[i] - mx) * (y[i] - my);
  }
  return { m, mx, my, Sxx, Syy, Sxy };
}

/** Поиск минимума унимодальной функции методом золотого сечения. */
function golden(g, lo, hi, iterations) {
  const r = (Math.sqrt(5) - 1) / 2;
  let c = hi - r * (hi - lo), d = lo + r * (hi - lo), gc = g(c), gd = g(d);
  for (let it = 0; it < iterations; it++) {
    if (gc < gd) { hi = d; d = c; gd = gc; c = hi - r * (hi - lo); gc = g(c); }
    else { lo = c; c = d; gc = gd; d = lo + r * (hi - lo); gd = g(d); }
  }
  return (lo + hi) / 2;
}

const estimate = (muX, muY, varX, slope, resVar) => ({
  muX, muY, varX, slope, resVar,
  covXY: slope * varX,
  varY: slope * slope * varX + resVar,
  sdY: Math.sqrt(slope * slope * varX + resVar)
});

/**
 * Порождает данные и разбивает их на полные случаи и случаи только с X.
 * Случайные числа общие для всех настроек: x не зависит от rho и psi1,
 * поэтому при движении регуляторов картинка меняется плавно.
 */
export function simulate(params = {}) {
  const p = { ...DEFAULTS, ...params };
  const U = mulberry32(p.seed);
  const Z = () => Math.sqrt(-2 * Math.log(1 - U())) * Math.cos(2 * Math.PI * U());
  const x = [], y = [], u = [];
  for (let i = 0; i < p.n; i++) {
    const z1 = Z(), z2 = Z(), ui = U();
    x.push(p.muX + p.sdX * z1);
    y.push(p.muY + p.sdY * (p.rho * z1 + Math.sqrt(1 - p.rho * p.rho) * z2));
    u.push(ui);
  }
  const z = (v) => (v - p.muX) / p.sdX;
  let lo = -12, hi = 12;
  for (let it = 0; it < 50; it++) {
    const mid = (lo + hi) / 2;
    let s = 0;
    for (let i = 0; i < p.n; i++) s += expit(mid + p.psi1 * z(x[i]));
    if (s / p.n < p.missShare) lo = mid; else hi = mid;
  }
  const psi0 = (lo + hi) / 2;
  const probMissing = (v) => expit(psi0 + p.psi1 * z(v));
  const cc = [], xOnly = [];
  for (let i = 0; i < p.n; i++) (u[i] < probMissing(x[i]) ? xOnly : cc).push(i);

  const C = stats(x, y, cc);
  const F = stats(x, y, [...Array(p.n).keys()]);
  const slope = C.Sxy / C.Sxx;
  const resVar = (C.Syy - slope * C.Sxy) / C.m;

  // Накопленные суммы x и x^2: полные случаи, затем случаи только с X в порядке индекса.
  const cum = [];
  let s = 0, q = 0;
  for (const i of cc) { s += x[i]; q += x[i] * x[i]; }
  cum.push({ s, q });
  for (const i of xOnly) { s += x[i]; q += x[i] * x[i]; cum.push({ s, q }); }

  return {
    params: p, x, y, u, psi0, probMissing, cc, xOnly, K: xOnly.length,
    C, F, slope, resVar, cum,
    ccEstimate: estimate(C.mx, C.my, C.Sxx / C.m, slope, resVar),
    fullEstimate: estimate(F.mx, F.my, F.Sxx / F.m, F.Sxy / F.Sxx, (F.Syy - F.Sxy * F.Sxy / F.Sxx) / F.m)
  };
}

/** Число добавленных случаев по проценту на регуляторе. */
export const kFromPercent = (sim, percent) => Math.round(percent / 100 * sim.K);

/** Достаточные статистики по x для полных случаев и первых k случаев только с X. */
export function included(sim, k) {
  const m = sim.C.m + k, xbar = sim.cum[k].s / m;
  return { m, xbar, S: sim.cum[k].q - m * xbar * xbar };
}

/** Оценка ММП: маргинальная часть по включенным x, регрессия по полным случаям. */
export function mlEstimate(sim, k) {
  const I = included(sim, k);
  return estimate(I.xbar, sim.C.my + sim.slope * (I.xbar - sim.C.mx), I.S / I.m, sim.slope, sim.resVar);
}

/** Логарифм правдоподобия наблюдаемых данных (без константы) для произвольной оценки. */
export function logLik(sim, k, est) {
  const I = included(sim, k), C = sim.C;
  const beta0 = est.muY - est.slope * est.muX;
  const ssx = I.S + I.m * (I.xbar - est.muX) ** 2;
  const e = C.my - beta0 - est.slope * C.mx;
  const rss = C.m * e * e + C.Syy - 2 * est.slope * C.Sxy + est.slope * est.slope * C.Sxx;
  return -0.5 * (I.m * Math.log(est.varX) + ssx / est.varX + C.m * Math.log(est.resVar) + rss / est.resVar);
}

/**
 * Профильное правдоподобие для среднего Y, закрепленного на значении c.
 * Возвращает статистику lr = 2 * разность логарифмов правдоподобия и лучшую модель при ограничении.
 */
export function profileMean(sim, k, c) {
  const I = included(sim, k), C = sim.C, p = sim.params;
  const at = (mu) => {
    const dx = C.mx - mu, dy = C.my - c;
    const slope = (C.Sxy + C.m * dx * dy) / (C.Sxx + C.m * dx * dx);
    const e = dy - slope * dx;
    const resVar = (C.m * e * e + C.Syy - 2 * slope * C.Sxy + slope * slope * C.Sxx) / C.m;
    const varX = (I.S + I.m * (I.xbar - mu) ** 2) / I.m;
    return { value: I.m * Math.log(varX) + C.m * Math.log(resVar), slope, resVar, varX };
  };
  const mu = golden((m) => at(m).value, p.muX - 3 * p.sdX, p.muX + 3 * p.sdX, 50);
  const o = at(mu);
  const free = I.m * Math.log(I.S / I.m) + C.m * Math.log(sim.resVar);
  return { lr: Math.max(0, o.value - free), model: estimate(mu, c, o.varX, o.slope, o.resVar) };
}

/**
 * Профильное правдоподобие для стандартного отклонения Y, закрепленного на значении sd.
 * Ограничение: slope^2 * varX + resVar = sd^2; средние свободны.
 */
export function profileSd(sim, k, sd) {
  const I = included(sim, k), C = sim.C, p = sim.params, v = sd * sd;
  const unit = p.sdY / p.sdX;
  const rss = (b) => C.Syy - 2 * b * C.Sxy + b * b * C.Sxx;
  const h = (b, varX) => {
    const resVar = v - b * b * varX;
    if (resVar <= 1e-6 || varX <= 1e-6) return 1e12;
    return I.m * (Math.log(varX) + I.S / (I.m * varX)) + C.m * (Math.log(resVar) + rss(b) / (C.m * resVar));
  };
  const inner = (b) => {
    const hi = Math.min(3.75 * p.sdX * p.sdX, v / (b * b + 1e-9) - 1e-3);
    const varX = golden((q) => h(b, q), 0.03 * p.sdX * p.sdX, hi, 34);
    return { varX, value: h(b, varX) };
  };
  const slope = golden((b) => inner(b).value, -0.533 * unit, 1.6 * unit, 36);
  const o = inner(slope);
  const free = I.m * (Math.log(I.S / I.m) + 1) + C.m * (Math.log(sim.resVar) + 1);
  const resVar = v - slope * slope * o.varX;
  return {
    lr: Math.max(0, o.value - free),
    model: estimate(I.xbar, C.my + slope * (I.xbar - C.mx), o.varX, slope, resVar)
  };
}

/** Статистики для оценок по полным случаям при k = 0..K: кривые нижнего графика. */
export function lrCurves(sim) {
  const mean = [], sd = [];
  for (let k = 0; k <= sim.K; k++) {
    mean.push(profileMean(sim, k, sim.ccEstimate.muY).lr);
    sd.push(profileSd(sim, k, sim.ccEstimate.sdY).lr);
  }
  return { mean, sd };
}

/** Все числа для карточек и подписей при k добавленных случаях. */
export function summary(sim, k) {
  const ml = mlEstimate(sim, k);
  const pm = profileMean(sim, k, sim.ccEstimate.muY), ps = profileSd(sim, k, sim.ccEstimate.sdY);
  return {
    ml, cc: sim.ccEstimate, full: sim.fullEstimate,
    lrMean: pm.lr, lrSd: ps.lr, rootMean: Math.sqrt(pm.lr), rootSd: Math.sqrt(ps.lr),
    fixedMeanModel: pm.model, fixedSdModel: ps.model
  };
}

/** Числа для подписей к регуляторам. */
export function controlLabels(sim) {
  const p = sim.params;
  return {
    rho: p.rho, r2Percent: Math.round(100 * p.rho * p.rho),
    psi1: p.psi1, psi0: sim.psi0,
    shareAll: p.missShare,
    shareLow: sim.probMissing(p.muX - p.sdX),
    shareHigh: sim.probMissing(p.muX + p.sdX)
  };
}

/** Диапазоны осей панелей профиля: зависят от настроек, но не от k. */
export function profileRanges(sim) {
  const a = [sim.ccEstimate.muY, sim.fullEstimate.muY, mlEstimate(sim, sim.K).muY];
  const b = [sim.ccEstimate.sdY, sim.fullEstimate.sdY, mlEstimate(sim, sim.K).sdY];
  return {
    mean: [Math.floor(Math.min(...a) - 4), Math.ceil(Math.max(...a) + 4)],
    sd: [Math.floor((Math.min(...b) - 2) * 2) / 2, Math.ceil((Math.max(...b) + 2.5) * 2) / 2]
  };
}

/** Точки кривой профиля: [значение параметра, статистика]. */
export function profileGrid(sim, k, which, range, points = 33) {
  const f = which === "mean" ? profileMean : profileSd, out = [];
  for (let j = 0; j < points; j++) {
    const v = range[0] + j / (points - 1) * (range[1] - range[0]);
    out.push([v, f(sim, k, v).lr]);
  }
  return out;
}

/** Верхняя граница оси статистики: 20, 40, 60, далее кратно 50. */
export function axisCap(maxLr) {
  return maxLr <= 20 ? 20 : maxLr <= 40 ? 40 : maxLr <= 60 ? 60 : Math.ceil(maxLr / 50) * 50;
}

/** Точки 95%-го эллипса рассеяния в координатах данных. */
export function ellipsePoints(est, points = 72, level = CHI2_2_95) {
  const R = Math.sqrt(level), l11 = Math.sqrt(est.varX), l21 = est.covXY / l11;
  const l22 = Math.sqrt(Math.max(1e-9, est.varY - l21 * l21)), out = [];
  for (let j = 0; j <= points; j++) {
    const a = j / points * 2 * Math.PI;
    out.push([est.muX + R * l11 * Math.cos(a), est.muY + R * (l21 * Math.cos(a) + l22 * Math.sin(a))]);
  }
  return out;
}

/** Плотность нормального распределения: кривые в панели «распределение САД». */
export const normalDensity = (v, mean, variance) =>
  Math.exp(-((v - mean) ** 2) / (2 * variance)) / Math.sqrt(2 * Math.PI * variance);

/** Число с фиксированным количеством знаков, десятичная точка, знак минус U+2212. */
export function fmt(v, digits = 1) {
  const s = Math.abs(v).toFixed(digits);
  return (v < 0 && Number(s) !== 0 ? "−" : "") + s;
}

/** Процент без лишних нулей: 7, 73.4, 37.5. */
export const fmtPercent = (share) => String(Math.round(1000 * share) / 10) + "%";
