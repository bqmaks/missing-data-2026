// Модель интерактива «Как ММП использует строки без САД» (D68).
// Пропуски только в y (САД), x (ИМТ) наблюдается всегда. Регрессия y | x оценивается
// по полным парам: β0, β1 и σ²_{y|x} = RSS / n_obs (ММП). Строка без y входит в моменты
// условными ожиданиями E[y | x] = β0 + β1 x и E[y² | x] = (β0 + β1 x)² + σ²_{y|x}
// с весом λ: λ = 0 – только полные пары (CC), λ = 1 – оценки ММП (точка неподвижности EM).

export function fitCompletePairs({x, y, M}) {
  let n = 0, sx = 0, sy = 0;
  for (let i = 0; i < x.length; i++) if (!M[i]) { n++; sx += x[i]; sy += y[i]; }
  const mx = sx / n, my = sy / n;
  let sxx = 0, sxy = 0;
  for (let i = 0; i < x.length; i++) if (!M[i]) { sxx += (x[i] - mx) ** 2; sxy += (x[i] - mx) * (y[i] - my); }
  const b1 = sxy / sxx, b0 = my - b1 * mx;
  let rss = 0;
  for (let i = 0; i < x.length; i++) if (!M[i]) rss += (y[i] - b0 - b1 * x[i]) ** 2;
  return {b0, b1, s2: rss / n, nObs: n, xObs: mx, yObs: my};
}

export function conditionalMean(fit, x) { return fit.b0 + fit.b1 * x; }

// Взвешенные моменты (делитель – сумма весов, как в ММП) и параметры курса.
export function blendParameters(data, lambda, fit = fitCompletePairs(data)) {
  const {x, y, M} = data;
  let W = 0, Sx = 0, Sy = 0, Sxx = 0, Syy = 0, Sxy = 0;
  for (let i = 0; i < x.length; i++) {
    const w = M[i] ? lambda : 1;
    if (w === 0) continue;
    const ey = M[i] ? conditionalMean(fit, x[i]) : y[i];
    const eyy = M[i] ? ey * ey + fit.s2 : y[i] * y[i];
    W += w; Sx += w * x[i]; Sy += w * ey; Sxx += w * x[i] * x[i]; Syy += w * eyy; Sxy += w * x[i] * ey;
  }
  const mx = Sx / W, my = Sy / W;
  const vx = Sxx / W - mx * mx, vy = Syy / W - my * my, cxy = Sxy / W - mx * my;
  const slopeSBP = cxy / vx, slopeBMI = cxy / vy;
  return {
    mean_BMI: mx, mean_SBP: my, sd_BMI: Math.sqrt(vx), sd_SBP: Math.sqrt(vy),
    correlation: cxy / Math.sqrt(vx * vy),
    intercept_SBP: my - slopeSBP * mx, slope_SBP: slopeSBP,
    intercept_BMI: mx - slopeBMI * my, slope_BMI: slopeBMI, weight: W
  };
}

const SQRT2PI = Math.sqrt(2 * Math.PI);
export function normalDensity(v, m, s) { const z = (v - m) / s; return Math.exp(-0.5 * z * z) / (s * SQRT2PI); }

// Ширина ядра Сильвермана для наблюдаемых y.
export function silverman(values) {
  const n = values.length, m = values.reduce((a, b) => a + b, 0) / n;
  const sd = Math.sqrt(values.reduce((a, b) => a + (b - m) ** 2, 0) / (n - 1));
  return 1.06 * sd * n ** (-0.2);
}

// Маргинальная плотность y: ядерная оценка по наблюдаемым y плюс λ × условные плотности
// N(β0 + β1 x, σ²_{y|x}) строк без y (сглаженные тем же ядром), нормировка на сумму весов.
export function mixtureDensity(grid, data, lambda, fit = fitCompletePairs(data), h) {
  const {x, y, M} = data;
  const obs = y.filter((_, i) => !M[i]);
  const bw = h ?? silverman(obs);
  const sc = Math.sqrt(fit.s2 + bw * bw);
  let W = obs.length;
  const out = grid.map(() => 0);
  for (const v of obs) grid.forEach((g, k) => { out[k] += normalDensity(g, v, bw); });
  if (lambda > 0) for (let i = 0; i < x.length; i++) if (M[i]) {
    W += lambda;
    const m = conditionalMean(fit, x[i]);
    grid.forEach((g, k) => { out[k] += lambda * normalDensity(g, m, sc); });
  }
  return out.map(v => v / W);
}

// Несколько строк без y для показа условных плотностей: ближайшие к квантилям их x.
export function representativeMissing(data, probs = [0.1, 0.3, 0.5, 0.7, 0.9]) {
  const idx = data.x.map((_, i) => i).filter(i => data.M[i]).sort((a, b) => data.x[a] - data.x[b]);
  return [...new Set(probs.map(p => idx[Math.min(idx.length - 1, Math.round(p * (idx.length - 1)))]))];
}
