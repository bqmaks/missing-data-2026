// Расчетная часть виджета W-PATTERN. Чистые функции без DOM и без D3: тестируются в Node.
// M - матрица индикаторов пропуска: массив строк, M[i][j] = 1, если значение пропущено.

/** Число пропусков по столбцам и по строкам. */
export function counts(M) {
  const p = M[0].length;
  const col = Array.from({ length: p }, (_, j) => M.reduce((s, r) => s + r[j], 0));
  const row = M.map((r) => r.reduce((s, v) => s + v, 0));
  return { col, row };
}

/** Порядок столбцов по возрастанию числа пропусков (устойчивый). */
export function columnOrder(M) {
  const { col } = counts(M);
  return col.map((c, j) => [c, j]).sort((a, b) => a[0] - b[0] || a[1] - b[1]).map((d) => d[1]);
}

/** Порядок строк: по числу пропусков, при равенстве - по паттерну в заданном порядке столбцов (устойчивый). */
export function rowOrder(M, cols = M[0].map((_, j) => j)) {
  const key = (r) => cols.map((j) => r[j]).join("");
  return M.map((r, i) => [r.reduce((s, v) => s + v, 0), key(r), i])
    .sort((a, b) => a[0] - b[0] || (a[1] < b[1] ? -1 : a[1] > b[1] ? 1 : 0) || a[2] - b[2])
    .map((d) => d[2]);
}

/**
 * Ячейки, нарушающие монотонность при данном порядке столбцов:
 * наблюденное значение стоит правее пропущенного в той же строке.
 * Возвращает [{ i, j }] в исходных индексах.
 */
export function violations(M, cols) {
  const out = [];
  M.forEach((r, i) => {
    let seenMissing = false;
    for (const j of cols) {
      if (r[j] === 1) seenMissing = true;
      else if (seenMissing) out.push({ i, j });
    }
  });
  return out;
}

/**
 * Паттерн монотонный, если столбцы можно упорядочить так, что пропуск в столбце означает
 * пропуск во всех следующих. Это равносильно вложенности множеств пропусков столбцов,
 * а порядок по числу пропусков - единственный кандидат (с точностью до одинаковых столбцов).
 */
export function isMonotone(M) {
  return violations(M, columnOrder(M)).length === 0;
}

/** Группировка одинаковых строк, как в mice::md.pattern(): [{ pattern, n, rows }]. */
export function groupPatterns(M, cols = M[0].map((_, j) => j)) {
  const map = new Map();
  M.forEach((r, i) => {
    const k = cols.map((j) => r[j]).join("");
    if (!map.has(k)) map.set(k, { pattern: cols.map((j) => r[j]), n: 0, rows: [] });
    const g = map.get(k); g.n += 1; g.rows.push(i);
  });
  return [...map.values()].sort((a, b) => b.n - a.n || a.pattern.join("").localeCompare(b.pattern.join("")));
}

function mulberry32(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

/** Перестановка 0..n-1 (Фишер - Йетс) с фиксированным зерном. */
export function shuffled(n, seed) {
  const u = mulberry32(seed);
  const a = Array.from({ length: n }, (_, i) => i);
  for (let i = n - 1; i > 0; i--) {
    const j = Math.floor(u() * (i + 1));
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}

/** Таблица со слайда «Что такое пропущенные значения?»: 6 пациентов, 6 переменных. */
export const HOOK = Object.freeze({
  id: "hook",
  label: "Таблица из начала занятия",
  columns: ["Возраст", "Пол", "САД", "ДАД", "ИМТ", "ХС"],
  M: [
    [0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 1, 0],
    [0, 0, 1, 1, 0, 1],
    [0, 0, 0, 0, 0, 1],
    [0, 0, 0, 1, 1, 1],
    [0, 0, 1, 0, 0, 1]
  ]
});

/** Выбывание в лонгитудинальном исследовании: монотонный паттерн, строки и столбцы перемешаны. */
export function makeDropout({ n = 24, p = 5, completeRate = 0.45, seed = 7 } = {}) {
  const u = mulberry32(seed);
  const base = Array.from({ length: n }, () => {
    const r = u();
    // Полная строка с вероятностью completeRate; иначе выбывание после визита 1..p-1.
    const last = r < completeRate ? p : 1 + Math.floor(u() * (p - 1));
    return Array.from({ length: p }, (_, j) => (j < last ? 0 : 1));
  });
  const ro = shuffled(n, seed + 1), co = shuffled(p, seed + 2);
  return {
    id: "dropout",
    label: "Выбывание из исследования (перемешано)",
    columns: co.map((j) => `Визит ${j + 1}`),
    M: ro.map((i) => co.map((j) => base[i][j]))
  };
}

/** Общий паттерн: независимые пропуски в каждом столбце. */
export function makeGeneral({ n = 24, p = 5, q = 0.25, seed = 11 } = {}) {
  const u = mulberry32(seed);
  return {
    id: "general",
    label: "Независимые пропуски",
    columns: Array.from({ length: p }, (_, j) => `Y${j + 1}`),
    M: Array.from({ length: n }, () => Array.from({ length: p }, (_, j) => (j > 0 && u() < q ? 1 : 0)))
  };
}
