import test from "node:test";
import assert from "node:assert/strict";
import { isMonotone, columnOrder, rowOrder, violations, groupPatterns, shuffled, HOOK, makeDropout, makeGeneral, counts } from "../widgets/patterns-model.js";

test("таблица из начала занятия не монотонна", () => {
  assert.equal(isMonotone(HOOK.M), false);
  assert.ok(violations(HOOK.M, columnOrder(HOOK.M)).length > 0);
});

test("выбывание монотонно, хотя строки и столбцы перемешаны", () => {
  const d = makeDropout();
  assert.equal(isMonotone(d.M), true);
  const natural = d.M[0].map((_, j) => j);
  assert.ok(violations(d.M, natural).length > 0, "в исходном порядке столбцов монотонность не видна");
});

test("после сортировки каждая строка имеет вид 0...01...1", () => {
  const d = makeDropout({ n: 40, p: 6, seed: 3 });
  const cols = columnOrder(d.M);
  for (const i of rowOrder(d.M, cols)) assert.match(cols.map((j) => d.M[i][j]).join(""), /^0*1*$/);
});

test("независимые пропуски дают общий паттерн", () => {
  assert.equal(isMonotone(makeGeneral().M), false);
});

test("одномерный паттерн монотонен", () => {
  const M = [[0, 0, 1], [0, 0, 0], [0, 0, 1], [0, 0, 0]];
  assert.equal(isMonotone(M), true);
  assert.deepEqual(counts(M).col, [0, 0, 2]);
});

test("группировка паттернов как в md.pattern", () => {
  const g = groupPatterns(HOOK.M);
  assert.equal(g.reduce((s, d) => s + d.n, 0), 6);
  assert.deepEqual(g[0].pattern, [0, 0, 0, 0, 0, 1]);
  assert.equal(g[0].n, 2);
});

test("перемешивание детерминировано и является перестановкой", () => {
  const a = shuffled(10, 5);
  assert.deepEqual(a, shuffled(10, 5));
  assert.deepEqual([...a].sort((x, y) => x - y), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
});
