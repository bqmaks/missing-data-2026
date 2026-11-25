import test from "node:test";
import assert from "node:assert/strict";
import { expected, meanCurves, trajectories, shown, fmt1, DEFAULTS, PRESETS } from "../widgets/locf-model.js";

const close = (a, b, eps = 1e-9) => assert.ok(Math.abs(a - b) < eps, `${a} vs ${b}`);

test("истина и анализ полных случаев равны 4.0 при параметрах слайда", () => {
  const e = expected();
  close(e.truth, 4); close(e.cc, 4);
});

test("три сценария слайда: 3.0, 1.4 и 4.5 после округления", () => {
  const got = PRESETS.map((s) => expected({ pT: s.pT, pC: s.pC }).locf);
  close(got[0], 2.95); close(got[1], 1.375); close(got[2], 4.525);
});

test("смещение равно (K - L) * (pC * cC - pT * cT)", () => {
  for (const [pT, pC, cT, cC] of [[0.3, 0.3, -1, -2], [0.1, 0.5, 0.5, -1.5], [0.6, 0, -2, -2]]) {
    const e = expected({ pT, pC, cT, cC });
    close(e.bias, (DEFAULTS.K - 0.5) * (pC * cC - pT * cT));
  }
});

test("LOCF несмещен, когда никто не выбывает или нет тренда", () => {
  close(expected({ pT: 0, pC: 0 }).bias, 0);
  close(expected({ cT: 0, cC: 0, pT: 0.4, pC: 0.1 }).bias, 0);
});

test("средняя кривая LOCF на последнем визите совпадает с expected()", () => {
  const curve = meanCurves(DEFAULTS.cC, 0.45);
  close(curve.at(-1).locf, expected({ pC: 0.45 }).meanLocf.C);
  close(curve[0].locf, DEFAULTS.mu0);
});

test("траектории детерминированы и после выбывания заморожены", () => {
  const a = trajectories(-2, 0.5, { n: 8, seed: 3 }), b = trajectories(-2, 0.5, { n: 8, seed: 3 });
  assert.deepEqual(a, b);
  assert.equal(a.filter((r) => r.dropped).length, 4);
  for (const r of a.filter((r) => r.dropped))
    for (const pt of r.points.filter((pt) => pt.t > r.last)) {
      assert.equal(pt.observed, false);
      assert.equal(pt.locf, r.points[r.last].y);
    }
});

test("показанные значения совпадают со статичным слайдом", () => {
  assert.deepEqual(PRESETS.map((s) => shown({ pT: s.pT, pC: s.pC }).locf), ["3.0", "1.4", "4.5"]);
  assert.equal(fmt1(2.95), "3.0");
  const s = shown();
  assert.deepEqual([s.truth, s.cc, s.diff, s.direction], ["4.0", "4.0", "1.0", -1]);
  assert.equal(shown({ pT: 0, pC: 0 }).direction, 0);
});
