// Расчетная часть виджета W-LOCF. Чистые функции без DOM и без D3: тестируются в Node.
// Модель: два рукава РКИ, визиты t = 0..K. Среднее рукава g на визите t: mu0 + c_g * t.
// Выбывание MCAR: доля p_g пациентов рукава g выбывает; последний наблюденный визит
// выбывшего равновероятно берется из массива lastVisits (например, [0, 1] - раннее выбывание).

export const DEFAULTS = Object.freeze({
  K: 4,              // номер последнего визита (месяц = 3 * t)
  mu0: 50,           // общее исходное среднее
  cT: -1,            // изменение за визит, лечение
  cC: -2,            // изменение за визит, контроль
  pT: 0.30,          // доля выбывших, лечение
  pC: 0.30,          // доля выбывших, контроль
  lastVisits: [0, 1] // возможные последние визиты выбывших
});

export const PRESETS = Object.freeze([
  { id: "equal",   label: "Выбыли 30% и 30%",                  pT: 0.30, pC: 0.30 },
  { id: "control", label: "Выбыли 15% (лечение) и 45% (контроль)", pT: 0.15, pC: 0.45 },
  { id: "treat",   label: "Выбыли 45% (лечение) и 15% (контроль)", pT: 0.45, pC: 0.15 }
]);

const mean = (a) => a.reduce((s, v) => s + v, 0) / a.length;

/** Округление до одного знака «половина вверх»: 2.95 -> 3.0, как на статичном слайде. */
export const round1 = (x) => Math.round(x * 10 + 1e-9) / 10;
export const fmt1 = (x) => round1(x).toFixed(1);

/** Значения в том виде, в каком они показываются: разность считается по округленным числам. */
export function shown(params = {}) {
  const e = expected(params);
  const truth = round1(e.truth), locf = round1(e.locf);
  return { truth: fmt1(truth), cc: fmt1(e.cc), locf: fmt1(locf), diff: fmt1(Math.abs(locf - truth)), direction: Math.sign(round1(locf - truth)) };
}

/** Ожидаемые значения на последнем визите: истина, анализ полных случаев, LOCF. */
export function expected(params = {}) {
  const { K, mu0, cT, cC, pT, pC, lastVisits } = { ...DEFAULTS, ...params };
  const L = mean(lastVisits);
  const armTrue = (c) => mu0 + c * K;
  const armLocf = (c, p) => mu0 + c * ((1 - p) * K + p * L);
  const truth = armTrue(cT) - armTrue(cC);
  const locf = armLocf(cT, pT) - armLocf(cC, pC);
  return {
    truth,
    cc: truth,                                   // выбывание не зависит от исхода
    locf,
    bias: locf - truth,                          // = (K - L) * (pC * cC - pT * cT)
    meanTrue: { T: armTrue(cT), C: armTrue(cC) },
    meanLocf: { T: armLocf(cT, pT), C: armLocf(cC, pC) }
  };
}

/** Средняя траектория рукава: истинная и после LOCF, по визитам 0..K. */
export function meanCurves(c, p, params = {}) {
  const { K, mu0, lastVisits } = { ...DEFAULTS, ...params };
  const out = [];
  for (let t = 0; t <= K; t++) {
    // у выбывшего с последним визитом l значение на визите t равно значению на визите min(t, l)
    const frozen = mean(lastVisits.map((l) => Math.min(t, l)));
    out.push({ t, month: 3 * t, truth: mu0 + c * t, locf: mu0 + c * ((1 - p) * t + p * frozen) });
  }
  return out;
}

/** Детерминированный генератор (mulberry32) и нормальные числа (Бокс - Мюллер). */
export function rng(seed) {
  let a = seed >>> 0;
  const unif = () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const norm = () => Math.sqrt(-2 * Math.log(1 - unif())) * Math.cos(2 * Math.PI * unif());
  return { unif, norm };
}

/**
 * Индивидуальные траектории одного рукава - только для показа, в расчет столбцов не входят.
 * Выбывает ровно round(p * n) пациентов; последние визиты чередуются по lastVisits.
 * Возвращает [{ id, dropped, last, points: [{ t, month, y, observed, locf }] }].
 */
export function trajectories(c, p, { n = 8, seed = 1, sdBetween = 4, sdWithin = 1, ...params } = {}) {
  const { K, mu0, lastVisits } = { ...DEFAULTS, ...params };
  const g = rng(seed);
  const nDrop = Math.round(p * n);
  const rows = [];
  for (let i = 0; i < n; i++) {
    const b = g.norm() * sdBetween;
    const y = [];
    for (let t = 0; t <= K; t++) y.push(mu0 + b + c * t + g.norm() * sdWithin);
    const dropped = i < nDrop;
    const last = dropped ? lastVisits[i % lastVisits.length] : K;
    rows.push({
      id: i, dropped, last,
      points: y.map((v, t) => ({ t, month: 3 * t, y: v, observed: t <= last, locf: t <= last ? v : y[last] }))
    });
  }
  return rows;
}
