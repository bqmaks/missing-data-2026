import test from "node:test";
import assert from "node:assert/strict";
import {
  DEFAULTS, CHI2_2_95, simulate, kFromPercent, included, mlEstimate, logLik,
  profileMean, profileSd, lrCurves, summary, controlLabels, profileRanges, profileGrid,
  axisCap, ellipsePoints, fmt, fmtPercent
} from "../widgets/mlprofile-model.js";

const close = (a, b, eps = 1e-9) => assert.ok(Math.abs(a - b) < eps, `${a} vs ${b} (eps ${eps})`);
const sim0 = simulate();

test("данные по умолчанию воспроизводят согласованный набросок", () => {
  assert.equal(sim0.x.length, 320);
  assert.equal(sim0.K, 122);
  assert.equal(sim0.cc.length, 198);
  close(sim0.ccEstimate.muY, 130.9, 0.05);
  close(sim0.ccEstimate.sdY, 13.8, 0.05);
  close(sim0.fullEstimate.muY, 134.9, 0.05);
  close(sim0.fullEstimate.sdY, 15.0, 0.05);
  close(sim0.psi0, -0.78, 0.005);
});

test("без добавленных случаев оценка ММП совпадает с оценкой по полным случаям", () => {
  const ml = mlEstimate(sim0, 0), cc = sim0.ccEstimate;
  for (const key of ["muX", "muY", "varX", "covXY", "varY"]) close(ml[key], cc[key]);
  close(profileMean(sim0, 0, cc.muY).lr, 0, 1e-6);
  close(profileSd(sim0, 0, cc.sdY).lr, 0, 1e-6);
});

test("среднее Y по ММП: среднее полных случаев плюс наклон на сдвиг среднего X", () => {
  for (const k of [1, 40, 122]) {
    const I = included(sim0, k), ml = mlEstimate(sim0, k);
    close(ml.muY, sim0.C.my + sim0.slope * (I.xbar - sim0.C.mx));
    close(ml.varY, sim0.slope ** 2 * ml.varX + sim0.resVar);
  }
  close(mlEstimate(sim0, sim0.K).muX, sim0.fullEstimate.muX, 1e-9);
  close(mlEstimate(sim0, sim0.K).varX, sim0.fullEstimate.varX, 1e-9);
});

test("оценка ММП совпадает с пределом EM-алгоритма", () => {
  const k = 80, idx = sim0.cc.concat(sim0.xOnly.slice(0, k)), nObs = sim0.cc.length, n = idx.length;
  let t = { mx: sim0.C.mx, my: sim0.C.my, sxx: sim0.C.Sxx / nObs, sxy: sim0.C.Sxy / nObs, syy: sim0.C.Syy / nObs };
  for (let it = 0; it < 2000; it++) {
    let sx = 0, sy = 0, sxx = 0, sxy = 0, syy = 0;
    const b = t.sxy / t.sxx, cond = t.syy - t.sxy * t.sxy / t.sxx;
    idx.forEach((i, j) => {
      const xi = sim0.x[i], observed = j < nObs;
      const yi = observed ? sim0.y[i] : t.my + b * (xi - t.mx);
      sx += xi; sy += yi; sxx += xi * xi; sxy += xi * yi; syy += yi * yi + (observed ? 0 : cond);
    });
    const mx = sx / n, my = sy / n;
    t = { mx, my, sxx: sxx / n - mx * mx, sxy: sxy / n - mx * my, syy: syy / n - my * my };
  }
  const ml = mlEstimate(sim0, k);
  close(ml.muX, t.mx, 1e-6); close(ml.muY, t.my, 1e-5);
  close(ml.varX, t.sxx, 1e-6); close(ml.covXY, t.sxy, 1e-5); close(ml.varY, t.syy, 1e-4);
});

test("контрольные значения статистики при всех добавленных случаях", () => {
  const table = [
    [0.6, 1.8, 17.3, 1.8], [0.6, 1.0, 10.4, 0.6], [0.6, 3.0, 23.0, 3.0],
    [0.9, 2.0, 47.6, 11.9], [0.9, 3.0, 62.1, 18.6], [0.3, 3.0, 6.4, 0.3]
  ];
  for (const [rho, psi1, lrMean, lrSd] of table) {
    const sim = simulate({ rho, psi1 }), s = summary(sim, sim.K);
    close(s.lrMean, lrMean, 0.06); close(s.lrSd, lrSd, 0.06);
  }
  const s = summary(sim0, sim0.K);
  close(s.ml.muY, 135.2, 0.05); close(s.ml.sdY, 14.9, 0.05);
  close(s.rootMean, 4.2, 0.05); close(s.rootSd, 1.4, 0.05);
});

test("статистика равна удвоенной разности логарифмов правдоподобия", () => {
  for (const [rho, psi1, k] of [[0.6, 1.8, 122], [0.9, 3, 60], [0.3, 1, 100]]) {
    const sim = simulate({ rho, psi1 }), ml = mlEstimate(sim, k);
    const pm = profileMean(sim, k, sim.ccEstimate.muY), ps = profileSd(sim, k, sim.ccEstimate.sdY);
    close(pm.lr, 2 * (logLik(sim, k, ml) - logLik(sim, k, pm.model)), 1e-6);
    close(ps.lr, 2 * (logLik(sim, k, ml) - logLik(sim, k, ps.model)), 1e-6);
  }
});

test("модель при ограничении действительно удовлетворяет ограничению", () => {
  const k = 90, pm = profileMean(sim0, k, 131), ps = profileSd(sim0, k, 13.5);
  close(pm.model.muY, 131); close(ps.model.sdY, 13.5, 1e-9);
  assert.ok(ps.model.resVar > 0 && pm.model.resVar > 0);
});

test("профиль равен нулю в оценке ММП и растет при удалении от нее", () => {
  for (const k of [0, 30, 122]) {
    const ml = mlEstimate(sim0, k);
    close(profileMean(sim0, k, ml.muY).lr, 0, 1e-6);
    close(profileSd(sim0, k, ml.sdY).lr, 0, 1e-6);
    for (const [f, at] of [[profileMean, ml.muY], [profileSd, ml.sdY]]) {
      let previous = 0;
      for (const d of [0.5, 1, 2, 3]) {
        const v = f(sim0, k, at + d).lr;
        assert.ok(v > previous, `рост вправо на ${d}`); previous = v;
      }
      previous = 0;
      for (const d of [0.5, 1, 2, 3]) {
        const v = f(sim0, k, at - d).lr;
        assert.ok(v > previous, `рост влево на ${d}`); previous = v;
      }
    }
  }
});

test("поиск минимума совпадает с перебором по сетке", () => {
  for (const [rho, psi1] of [[0.6, 1.8], [0.9, 3], [0, 3]]) {
    const sim = simulate({ rho, psi1 }), k = sim.K, I = included(sim, k), C = sim.C;
    const v = sim.ccEstimate.sdY ** 2;
    let best = Infinity;
    for (let b = -2; b <= 6; b += 0.004) {
      for (let q = 0.5; q <= 40; q += 0.02) {
        const r = v - b * b * q;
        if (r <= 0) continue;
        const value = I.m * (Math.log(q) + I.S / (I.m * q)) +
          C.m * (Math.log(r) + (C.Syy - 2 * b * C.Sxy + b * b * C.Sxx) / (C.m * r));
        if (value < best) best = value;
      }
    }
    const free = I.m * (Math.log(I.S / I.m) + 1) + C.m * (Math.log(sim.resVar) + 1);
    close(profileSd(sim, k, sim.ccEstimate.sdY).lr, best - free, 0.01);

    const c = sim.ccEstimate.muY;
    let bestMean = Infinity;
    for (let mu = 18; mu <= 34; mu += 0.002) {
      const dx = C.mx - mu, dy = C.my - c, b = (C.Sxy + C.m * dx * dy) / (C.Sxx + C.m * dx * dx), e = dy - b * dx;
      const value = I.m * Math.log((I.S + I.m * (I.xbar - mu) ** 2) / I.m) +
        C.m * Math.log((C.m * e * e + C.Syy - 2 * b * C.Sxy + b * b * C.Sxx) / C.m);
      if (value < bestMean) bestMean = value;
    }
    close(profileMean(sim, k, c).lr, bestMean - (I.m * Math.log(I.S / I.m) + C.m * Math.log(sim.resVar)), 0.001);
  }
});

test("MCAR: свободный член равен логиту доли пропусков, статистика около нуля", () => {
  const sim = simulate({ psi1: 0 }), curves = lrCurves(sim);
  close(sim.psi0, Math.log(0.375 / 0.625), 1e-6);
  assert.ok(Math.max(...curves.mean) < 0.5 && Math.max(...curves.sd) < 0.5);
  const labels = controlLabels(sim);
  close(labels.shareLow, 0.375, 1e-6); close(labels.shareHigh, 0.375, 1e-6);
});

test("без связи X и Y данные по X не меняют вывод о Y", () => {
  const sim = simulate({ rho: 0, psi1: 3 }), s = summary(sim, sim.K);
  assert.ok(s.lrMean < 1 && s.lrSd < 0.5);
});

test("статистика выходит на плато при сильной зависимости пропуска от X", () => {
  const at = (psi1) => { const sim = simulate({ psi1 }); return summary(sim, sim.K).lrMean; };
  close(at(5), at(3), 1.0); close(at(10), at(3), 1.0);
});

test("общие случайные числа: x не зависит от настроек, y зависит только от rho", () => {
  const a = simulate({ rho: 0.3, psi1: 0.5 }), b = simulate({ rho: 0.3, psi1: 2.5 }), c = simulate({ rho: 0.8, psi1: 0.5 });
  assert.deepEqual(a.x, b.x); assert.deepEqual(a.x, c.x);
  assert.deepEqual(a.y, b.y); assert.notDeepEqual(a.y, c.y);
});

test("доля пропусков держится около целевой при любых настройках", () => {
  for (const rho of [0, 0.6, 0.9]) for (const psi1 of [0, 1, 2, 3]) {
    const sim = simulate({ rho, psi1 });
    assert.ok(Math.abs(sim.K / sim.x.length - DEFAULTS.missShare) < 0.04, `${sim.K} при rho=${rho}, psi1=${psi1}`);
  }
});

test("подписи к регуляторам", () => {
  const l = controlLabels(sim0);
  assert.equal(l.r2Percent, 36);
  assert.equal(fmt(l.psi0, 2), "−0.78");
  assert.equal(fmtPercent(l.shareLow), "7%");
  assert.equal(fmtPercent(l.shareHigh), "73.4%");
  assert.equal(fmtPercent(l.shareAll), "37.5%");
  assert.equal(fmt(-0.001, 2), "0.00");
});

test("кривые, диапазоны осей и перевод процента в число случаев", () => {
  const curves = lrCurves(sim0);
  assert.equal(curves.mean.length, sim0.K + 1);
  close(curves.mean[0], 0, 1e-6); close(curves.mean[sim0.K], 17.3, 0.06);
  assert.equal(kFromPercent(sim0, 0), 0);
  assert.equal(kFromPercent(sim0, 100), sim0.K);
  assert.equal(kFromPercent(sim0, 50), 61);
  assert.deepEqual([axisCap(17.3), axisCap(23), axisCap(47.6), axisCap(62.1)], [20, 40, 60, 100]);
  const r = profileRanges(sim0);
  assert.ok(r.mean[0] < sim0.ccEstimate.muY && r.mean[1] > mlEstimate(sim0, sim0.K).muY);
  assert.ok(r.sd[0] < sim0.ccEstimate.sdY && r.sd[1] > sim0.fullEstimate.sdY);
  const grid = profileGrid(sim0, sim0.K, "mean", r.mean, 33);
  assert.equal(grid.length, 33);
  close(grid[0][0], r.mean[0]); close(grid[32][0], r.mean[1]);
});

test("точки эллипса лежат на 95%-м контуре", () => {
  const est = mlEstimate(sim0, 60), det = est.varX * est.varY - est.covXY ** 2;
  for (const [px, py] of ellipsePoints(est, 24)) {
    const dx = px - est.muX, dy = py - est.muY;
    close((est.varY * dx * dx - 2 * est.covXY * dx * dy + est.varX * dy * dy) / det, CHI2_2_95, 1e-6);
  }
});
