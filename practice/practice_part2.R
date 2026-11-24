# ЗАНЯТИЕ 2. MICE, PMM, пассивная импутация и правила Рубина.
# Независимый запуск из корня проекта: source("practice/practice_part2.R")
source("r/00_dgp.R")
need_packages(c("mice", "ggplot2"))
dir.create("outputs", showWarnings = FALSE)
if (!interactive()) grDevices::cairo_pdf("outputs/practice-part2.pdf", width = 10, height = 6)
m <- getOption("missingdata.m", 20L)
iterations <- getOption("missingdata.maxit", 20L)

# МОДУЛЬ 3. Расширенная когорта из предоставленного практикума.
set.seed(202603)
n <- DGP$n
height_true <- rnorm(n, 170, 9)
bmi_base <- rnorm(n, 28, 5)
weight_true <- bmi_base * (height_true / 100)^2
age <- round(rnorm(n, 55, 10))
treatment <- factor(rbinom(n, 1, .5), labels = c("Control", "Active"))
sbp <- round(135 + 1.8 * (bmi_base - 28) + .25 * (age - 55) + rnorm(n, 0, 12), 1)
glucose_true <- round(exp(1.45 + .025 * (bmi_base - 28) + rnorm(n, 0, .22)) + 1.5, 2)

# ВАЖНО: пропуск глюкозы зависит от истинного ИМТ, который частично скрыт.
# Исходный пример НЕ является строгим MAR. Используем его для обсуждения
# спецификации и диагностики; номинальное покрытие проверяем на MAR в модуле 5.
cohort_l2 <- data.frame(age, treatment, sbp,
  height = round(ifelse(rbinom(n, 1, plogis(-2 + .04 * (age - 55))) == 1, NA, height_true), 1),
  weight = round(ifelse(rbinom(n, 1, plogis(-1.3 + .06 * (sbp - 135))) == 1, NA, weight_true), 1),
  glucose = ifelse(rbinom(n, 1, plogis(-1.1 - .10 * (bmi_base - 28))) == 1, NA, glucose_true))
cohort_l2$bmi <- with(cohort_l2, weight / (height / 100)^2)
print(colMeans(is.na(cohort_l2)))
print(mice::md.pattern(cohort_l2, plot = FALSE))

ini <- mice::mice(cohort_l2, m = 1, maxit = 0, printFlag = FALSE, seed = 301)
methods <- ini$method
predictors <- ini$predictorMatrix
methods["bmi"] <- "~ I(weight / (height / 100)^2)"
predictors[c("height", "weight"), "bmi"] <- 0
predictors[!rownames(predictors) %in% c("height", "weight", "bmi"), c("height", "weight")] <- 0
visit <- c("height", "weight", "bmi", "glucose")

# Сравнение norm / pmm ПОСЛЕ одинаковой настройки производных переменных.
method_norm <- methods; method_norm["glucose"] <- "norm"
method_pmm <- methods; method_pmm["glucose"] <- "pmm"
imp_norm <- mice::mice(cohort_l2, m = m, maxit = iterations, method = method_norm,
  predictorMatrix = predictors, visitSequence = visit, seed = 302, printFlag = FALSE)
imp <- mice::mice(cohort_l2, m = m, maxit = iterations, method = method_pmm,
  predictorMatrix = predictors, visitSequence = visit, seed = 303, printFlag = FALSE)
print(plot(imp))
print(mice::densityplot(imp, ~ weight + glucose + bmi))
print(tail(mice::convergence(imp, diagnostic = "all")))
print(imp$loggedEvents)

cat("Глюкоза: наблюдаемый диапазон:", range(cohort_l2$glucose, na.rm = TRUE), "\n")
cat("Диапазон импутаций norm:", range(as.matrix(imp_norm$imp$glucose)), "\n")
cat("Диапазон импутаций pmm:", range(as.matrix(imp$imp$glucose)), "\n")
stopifnot(all(as.matrix(imp$imp$glucose) %in% na.omit(cohort_l2$glucose)))
completed <- mice::complete(imp, "all")
stopifnot(all(vapply(completed, function(d)
  max(abs(d$bmi - d$weight / (d$height / 100)^2)) < 1e-8, logical(1))))

# МОДУЛЬ 4. Модель оценивается в КАЖДОМ заполненном датасете.
set.seed(202604)
fits <- with(imp, lm(sbp ~ bmi + age + treatment))
pooled <- mice::pool(fits)
print(summary(pooled, conf.int = TRUE))
print(pooled$pooled[c("term", "ubar", "b", "t", "df", "riv", "lambda", "fmi")])
fits_reduced <- with(imp, lm(sbp ~ bmi))
print(mice::D1(fits, fits_reduced))

# Опционально: Amelia получает другой набор информации (без роста и веса).
# Это иллюстрация интерфейса JM, а не сравнение методов при равных условиях.
if (isTRUE(getOption("missingdata.amelia", FALSE))) {
  need_packages("Amelia")
  am <- Amelia::amelia(cohort_l2[c("age", "treatment", "sbp", "bmi", "glucose")],
                      m = m, noms = "treatment", p2s = 0)
  print(summary(mice::pool(lapply(am$imputations, function(d)
    lm(sbp ~ bmi + age + treatment, data = d))), conf.int = TRUE))
}

# МОДУЛЬ 5. Готовый расчёт покрытия: 2000 репликаций, n = 500.
# Сравниваются mu2 и beta1: одиночная стохастика, несобственная MI,
# собственная MI при m = 5 / 20, ММП. У каждого покрытия указан MCSE.
# Пересчитать: Rscript scripts/precompute.R
set.seed(202605)
if (file.exists("static/data/mi_coverage.json")) {
  need_packages("jsonlite")
  coverage <- jsonlite::fromJSON("static/data/mi_coverage.json")
  print(coverage$summary[c("method", "term", "coverage", "mcse_coverage", "fmi")])
} else {
  message("Сначала выполните Rscript scripts/precompute.R для таблицы покрытия.")
}

# ЗАДАНИЯ
# 1. До запуска norm/pmm: какие значения может вернуть каждый метод?
# 2. Что меняется, если вернуть bmi в предикторы height и weight?
# 3. Измените m на 5, затем 50: сравните SE, df и FMI в pooled$pooled.
#    FMI не обязана уменьшаться при росте m — растёт точность её оценки.
# 4. Измените механизм пропуска glucose на зависимость только от sbp/age.
#    Теперь все вероятности пропуска задаются полностью наблюдаемыми данными.
saveRDS(list(cohort = cohort_l2, imp = imp, imp_norm = imp_norm,
  pooled = pooled, session = sessionInfo()), "outputs/part2-results.rds")
if (!interactive()) grDevices::dev.off()
