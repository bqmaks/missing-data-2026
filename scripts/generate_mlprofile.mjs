// Выгружает состояние интерактива W-MLPROFILE для статичного варианта печати (D71):
// настройки по умолчанию, 100% добавленных случаев. Данные берутся из widgets/mlprofile-model.js,
// а не разыгрываются в R заново: генератор mulberry32 в R не воспроизводится.
// Запуск: node scripts/generate_mlprofile.mjs  ->  data/ml-profile.json
import {writeFileSync} from 'node:fs';
import {simulate, summary, lrCurves, profileRanges, profileGrid, axisCap, ellipsePoints,
        controlLabels, CHI2_1_95} from '../widgets/mlprofile-model.js';

const r = (v, d = 4) => Math.round(v * 10 ** d) / 10 ** d;
const est = e => Object.fromEntries(Object.entries(e).map(([key, v]) => [key, r(v, 6)]));
const pts = a => a.map(p => p.map(v => r(v)));

const sim = simulate(), k = sim.K, s = summary(sim, k), curves = lrCurves(sim), ranges = profileRanges(sim);
const L = controlLabels(sim);
const out = {
  note: 'Создано scripts/generate_mlprofile.mjs из widgets/mlprofile-model.js: настройки по умолчанию, 100% добавленных случаев.',
  params: sim.params, psi0: r(L.psi0, 6), K: sim.K, k,
  shares: {all: L.shareAll, low: r(L.shareLow, 6), high: r(L.shareHigh, 6)},
  x: sim.x.map(v => r(v)), y: sim.y.map(v => r(v)),
  cc: sim.cc.map(i => i + 1), xOnly: sim.xOnly.map(i => i + 1),   // номера строк с 1, как в R
  estimates: {full: est(sim.fullEstimate), cc: est(sim.ccEstimate), ml: est(s.ml),
              fixedMean: est(s.fixedMeanModel), fixedSd: est(s.fixedSdModel)},
  ellipses: {full: pts(ellipsePoints(sim.fullEstimate)), cc: pts(ellipsePoints(sim.ccEstimate)),
             ml: pts(ellipsePoints(s.ml)), fixedMean: pts(ellipsePoints(s.fixedMeanModel)),
             fixedSd: pts(ellipsePoints(s.fixedSdModel))},
  lr: {mean: r(s.lrMean, 6), sd: r(s.lrSd, 6)},
  ranges, cap: axisCap(Math.max(...curves.mean, ...curves.sd)), chi2: CHI2_1_95,
  profiles: {mean: pts(profileGrid(sim, k, 'mean', ranges.mean, 121)), sd: pts(profileGrid(sim, k, 'sd', ranges.sd, 121))},
  curves: {mean: curves.mean.map(v => r(v)), sd: curves.sd.map(v => r(v))}
};
writeFileSync(new URL('../data/ml-profile.json', import.meta.url), JSON.stringify(out) + '\n');
console.log(`data/ml-profile.json: K = ${sim.K}, 2Δℓ = ${s.lrMean.toFixed(1)} / ${s.lrSd.toFixed(1)}`);
