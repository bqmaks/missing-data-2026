import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { fitCompletePairs, blendParameters, mixtureDensity, representativeMissing } from "../widgets/mlmech-model.js";

const data = JSON.parse(readFileSync(new URL("../data/ml-mechanism.json", import.meta.url), "utf8"));
const close = (a, b, tol = 1e-8) => assert.ok(Math.abs(a - b) < tol, `${a} vs ${b}`);

test("при λ = 1 оценки совпадают с аналитическим ММП из R", () => {
  const p = blendParameters(data, 1);
  for (const k of Object.keys(data.ml)) close(p[k], data.ml[k]);
});

test("при λ = 0 – моменты полных пар (среднее САД по CC)", () => {
  const p = blendParameters(data, 0);
  const obs = data.y.filter((_, i) => !data.M[i]);
  close(p.mean_SBP, obs.reduce((a, b) => a + b, 0) / obs.length);
  close(p.weight, obs.length);
});

test("регрессия E(САД | ИМТ) одинакова при любом λ (точка неподвижности EM)", () => {
  const fit = fitCompletePairs(data);
  for (const lambda of [0, 0.3, 0.7, 1]) {
    const p = blendParameters(data, lambda, fit);
    close(p.slope_SBP, fit.b1, 1e-8);
    close(p.intercept_SBP, fit.b0, 1e-6);
  }
});

test("μ̂_САД при λ = 1 равна ȳ_obs + β̂1 (x̄_all − x̄_obs)", () => {
  const fit = fitCompletePairs(data);
  const xall = data.x.reduce((a, b) => a + b, 0) / data.x.length;
  close(blendParameters(data, 1, fit).mean_SBP, fit.yObs + fit.b1 * (xall - fit.xObs));
});

test("маргинальная плотность-смесь нормирована", () => {
  const grid = Array.from({ length: 3001 }, (_, k) => 40 + k * 0.06);
  for (const lambda of [0, 1]) {
    const f = mixtureDensity(grid, data, lambda);
    close(f.reduce((a, b) => a + b, 0) * 0.06, 1, 2e-3);
  }
});

test("для показа выбраны строки без САД", () => {
  const r = representativeMissing(data);
  assert.ok(r.length >= 3 && r.every((i) => data.M[i] === 1));
});
