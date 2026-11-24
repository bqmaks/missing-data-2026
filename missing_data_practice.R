# АНАЛИЗ ДАННЫХ С ПРОПУСКАМИ — точка входа в практикум (3 занятия).
# Откройте проект в RStudio и выполняйте блоки выбранного занятия по порядку.
# Из терминала: Rscript missing_data_practice.R 1 (либо 2, 3, all).
# Установка зависимостей: Rscript scripts/install_packages.R

args <- commandArgs(trailingOnly = TRUE)
lesson <- if (length(args)) args[1] else "1"
if (!lesson %in% c("1", "2", "3", "all")) stop("Выберите занятие: 1, 2, 3 или all")
if (!file.exists("r/00_dgp.R")) stop("Запустите скрипт из корня проекта missing-data-2026")

# Для работы в аудитории используем небольшой расчёт.
# Для таблиц презентации — scripts/precompute.R, не менее 2000 репликаций.
options(missingdata.S = 300L, missingdata.m = 20L, missingdata.maxit = 20L)
# Дорогой бутстреп запускается отдельно: options(missingdata.bootstrap = TRUE).

for (part in if (lesson == "all") 1:3 else as.integer(lesson)) {
  source(sprintf("practice/practice_part%d.R", part), echo = FALSE)
}
