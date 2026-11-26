# missing-data-2026

Локальная презентация «Анализ неполных данных в R» по новым материалам
`course-inputs/missing_data_combined_v2.pptx`: 43 исходных слайда на 66 экранах.
Добавлены IPW, Monte Carlo, ММП/EM, переход к MI и два интерактива.
Плотные сравнения разделены на графики и таблицы; заметки доступны через `S`.

Используется [fb-presentation](https://github.com/bqmaks/fb-presentation), формат
`material-revealjs`. Расширение скопировано без изменений с commit
`c1d3d0296e8a2fbf8fb64416060fea815b47513a`; лицензия — [MIT](licenses/fb-presentation-MIT.txt).
Матрицы и формулы отображает локальный MathJax 2.7.9; для просмотра интернет не нужен.

## Локальная сборка

Проверено: Quarto 1.10.18, R 4.6.1, knitr 1.51 и установленный rmarkdown.
DGP и расчёты используют только base R. Установка R-пакетов, если они отсутствуют:

```r
install.packages(c("knitr", "rmarkdown"))
```

Редактируемый исходник — `index.qmd`. При сборке R генерирует одну выборку,
сохраняет CSV и строит таблицы/графики сквозного примера из неё. Отдельные
примеры используют явно заданные DGP и seeds. Таблица Monte Carlo читается из `data/`.

```bash
quarto preview index.qmd --to material-revealjs --host 127.0.0.1 --port 8765
quarto render index.qmd --to material-revealjs
```

Для просмотра откройте в обычном браузере адрес, который напечатает Quarto
(обычно `http://127.0.0.1:8765/index.html`). Оставьте процесс preview запущенным.
Так браузер получает HTML и все локальные ресурсы MathJax из одного каталога.
Интерактивы требуют HTTP: открытие двойным щелчком (`file://`) блокирует ES-модули.
Альтернативный просмотр уже собранных файлов:

```bash
python3 -m http.server 8765 --bind 127.0.0.1 --directory _site
```

Печатный вариант: `http://127.0.0.1:8765/index.html?print-pdf` → печать браузера
в PDF, альбомная ориентация, без колонтитулов браузера. Оба виджета заменяются
статичными иллюстрациями. Для передачи нужен весь `_site/`, а не один `index.html`.
Если старая вкладка сохраняет ошибку после пересборки, обновите её с `Ctrl+F5`.

HTML с ресурсами находится в `_site/`. Если стандартный кэш Quarto недоступен:

```bash
XDG_CACHE_HOME=/tmp/missing-data-quarto-cache quarto render index.qmd --to material-revealjs
```

Сборка не требует приватного PPTX: содержание уже перенесено в редактируемый QMD.
Исходные файлы `course-inputs/` не изменяются и не включаются в Git или сборку.

Персонаж презентации хранится в `assets/mice-mascot.svg`: мышь и кольцо разделены
на именованные группы, фон прозрачный. Вариант для таблицы `assets/mice-cell.svg`
использует ту же мордочку и уши, отдельную позу без кольца с лапками на линии
ячейки. После правки общего исходника обновите обе векторные версии:

```bash
python3 scripts/sync_title_mascot.py
python3 scripts/sync_cell_mascot.py
quarto render index.qmd --to material-revealjs
```

Титульный SVG остается самодостаточным: внешних изображений и шрифтов в нем нет.

## Данные и проверка

```bash
Rscript scripts/generate_example.R
Rscript tests/validate_example.R
Rscript tests/validate_advanced.R
node --test tests/*.test.js
# Полный пересчёт Monte Carlo, если менялись DGP или методы:
Rscript scripts/generate_simulation.R
```

В среде, где Node не показывает отдельные тесты из дочерних процессов:
`node --test --test-isolation=none tests/*.test.js` (проверено на Node 24).

DGP: n=1000; средние ИМТ/САД 27/135, SD 4/15, корреляция 0.6.
Seed выборки — 20261124, без подбора. Коэффициенты механизмов MCAR/MAR/MNAR
сохранены из PPTX. Числа самого PPTX заменены оценками по новой фиксированной выборке.

- [Описание CSV, seeds и методов](data/README.md).
- [Все численные оценки](data/estimates.csv).
- [Полные данные](data/complete.csv), [неполные варианты](data/observed.csv).
- [Генерация и расчёты](r/example.R), [новые примеры](r/advanced.R), [Monte Carlo](r/simulation.R).
- [Виджеты](widgets/): локальная D3 7.9.0, без сборки Node; [лицензия](licenses/d3-ISC.txt).
- [Соответствие слайдам и новый план](wiki/Lecture-plan.md).
- [Уточнения научных формулировок](wiki/Changelog-content.md).

Сквозная иллюстрация — одна выборка. Отдельно выполнены 3000 Monte Carlo-репликаций
с seed 20261153: результаты, смещения, покрытие и MCSE в
[data/simulation-summary.csv](data/simulation-summary.csv).

## Публикация

Разработка идёт локально. Стартовый пустой шаблон ранее отправлен на GitHub
(commit `0919249`); текущие изменения не публикуются. Новые push и размещение
готовой презентации в GitHub Pages выполняются по отдельному поручению.
Workflow автопубликации отсутствует. Прежний опубликованный сайт этим не снимается.

Договорённости и состояние: [wiki](wiki/Home.md), [AGENTS.md](AGENTS.md).
