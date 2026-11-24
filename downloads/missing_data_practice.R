# АНАЛИЗ ДАННЫХ С ПРОПУСКАМИ — практикум, редакция 0.1.
# Самостоятельный файл: другие файлы проекта для запуска не нужны.
# В RStudio: выберите lesson ниже (1, 2, 3) и выполняйте блоки по порядку.
# Из терминала: Rscript missing_data_practice.R 1
# Зависимости: install.packages(c("MASS", "mice", "naniar", "ggplot2",
#   "smcfcs", "mitools", "survival", "MatchThem", "survey", "cobalt"))
# Для дополнительного бутстрепа: install.packages("bootImpute")
# options(missingdata.bootstrap = TRUE)  # 200 бутстреп-выборок × 2 импутации
args <- commandArgs(trailingOnly = TRUE)
lesson <- if (length(args)) args[1] else "1"
if (!lesson %in% c("1", "2", "3", "all")) stop("Занятие: 1, 2, 3 или all")
options(missingdata.S = 300L, missingdata.m = 20L, missingdata.maxit = 20L)

# Общая модель данных и оценки для презентации и трёх занятий.
# Библиотеку MASS не подключаем: она маскирует dplyr::select().
Sys.setenv(TZ = "Europe/Moscow")
DGP <- list(n = 500L, mu1 = 135, sd1 = 15, mu2 = 28, sd2 = 5,
            rho = 0.60, mcar = 0.30, intercept = -0.95,
            mar_slope = 0.08, mnar_slope = 0.24)
TRUTH <- with(DGP, c(mu2 = mu2, sd2 = sd2, rho = rho,
                   b1 = rho * sd2 / sd1, g1 = rho * sd1 / sd2))

need_packages <- function(packages) {
  absent <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) stop("Установите пакеты: install.packages(c(",
    paste(sprintf('"%s"', absent), collapse = ", "), "))", call. = FALSE)
}

make_complete <- function(n = DGP$n, rho = DGP$rho) {
  sigma <- matrix(c(DGP$sd1^2, rho * DGP$sd1 * DGP$sd2,
                    rho * DGP$sd1 * DGP$sd2, DGP$sd2^2), 2)
  xy <- MASS::mvrnorm(n, c(DGP$mu1, DGP$mu2), sigma)
  data.frame(sbp = xy[, 1], bmi = xy[, 2])
}

make_missing <- function(data, mechanism = "MAR") {
  p <- switch(mechanism,
    MCAR = rep(DGP$mcar, nrow(data)),
    MAR = plogis(DGP$intercept + DGP$mar_slope * (data$sbp - DGP$mu1)),
    MNAR = plogis(DGP$intercept + DGP$mnar_slope * (data$bmi - DGP$mu2)),
    stop("Неизвестный механизм"))
  data$bmi[rbinom(nrow(data), 1, p) == 1] <- NA_real_
  data
}

# Сводка регрессии без lm() внутри большого цикла симуляции.
pair_metrics <- function(x, y) {
  n <- length(y); dx <- x - mean(x); dy <- y - mean(y)
  sxx <- sum(dx^2); syy <- sum(dy^2); sxy <- sum(dx * dy)
  b1 <- sxy / sxx
  c(mu2 = mean(y), sd2 = sqrt(syy / (n - 1)),
    rho = sxy / sqrt(sxx * syy), b1 = b1, g1 = sxy / syy,
    se_mu2 = sqrt(syy / (n - 1) / n),
    se_b1 = sqrt(max(0, syy - sxy^2 / sxx) / (n - 2) / sxx))
}

analyse_methods <- function(data, full = NULL) {
  x <- data$sbp; y <- data$bmi; obs <- !is.na(y)
  n <- length(x); n_obs <- sum(obs)
  if (n_obs < 4 || var(x[obs]) == 0) stop("Слишком мало полных пар")
  cc <- pair_metrics(x[obs], y[obs])
  b1 <- unname(cc["b1"]); b0 <- mean(y[obs]) - b1 * mean(x[obs])
  fitted <- b0 + b1 * x
  residuals <- y[obs] - fitted[obs]
  noise_sd <- sqrt(sum(residuals^2) / (n_obs - 2))
  y_mean <- y_reg <- y_stoch <- y
  y_mean[!obs] <- mean(y[obs])
  y_reg[!obs] <- fitted[!obs]
  y_stoch[!obs] <- fitted[!obs] + rnorm(sum(!obs), 0, noise_sd)
  var1 <- mean((x - mean(x))^2)
  residual_var <- mean(residuals^2) # ММП, делитель n_obs
  var2 <- b1^2 * var1 + residual_var
  sxx <- sum((x[obs] - mean(x[obs]))^2)
  components <- c(sbp = b1^2 * var1 / n,
                  residual = residual_var / n_obs,
                  slope = (mean(x) - mean(x[obs]))^2 * residual_var / sxx)
  mle <- c(mu2 = b0 + b1 * mean(x), sd2 = sqrt(var2),
           rho = b1 * sqrt(var1 / var2), b1 = b1, g1 = b1 * var1 / var2,
           se_mu2 = sqrt(sum(components)), se_b1 = unname(cc["se_b1"]))
  out <- base::rbind(CCA = cc, Mean = pair_metrics(x, y_mean),
               Reg = pair_metrics(x, y_reg), StochReg = pair_metrics(x, y_stoch), MLE = mle)
  if (!is.null(full)) out <- base::rbind(Full = pair_metrics(full$sbp, full$bmi), out)
  out <- data.frame(method = rownames(out), out, row.names = NULL)
  # Здесь везде нормальное приближение; MI ниже использует t Барнарда–Рубина.
  out$cover_mu2 <- abs(out$mu2 - TRUTH["mu2"]) <= qnorm(.975) * out$se_mu2
  out$cover_b1 <- abs(out$b1 - TRUTH["b1"]) <= qnorm(.975) * out$se_b1
  attr(out, "variance_components") <- components
  attr(out, "completed") <- list(Mean = y_mean, Reg = y_reg, StochReg = y_stoch)
  out
}

run_simulation <- function(S = 300L, n = DGP$n, seed = 202602L) {
  set.seed(seed)
  out <- vector("list", S * 3L)
  at <- 0L
  for (s in seq_len(S)) {
    set.seed(seed + s)
    full <- make_complete(n)
    samples <- lapply(c("MCAR", "MAR", "MNAR"), function(mech) make_missing(full, mech))
    names(samples) <- c("MCAR", "MAR", "MNAR")
    for (mechanism in names(samples)) {
      at <- at + 1L
      data <- samples[[mechanism]]
      one <- analyse_methods(data, full)
      one$mechanism <- mechanism; one$replicate <- s
      one$missing_fraction <- mean(is.na(data$bmi))
      out[[at]] <- one
    }
  }
  raw <- do.call(base::rbind, out)
  groups <- split(raw, interaction(raw$mechanism, raw$method, drop = TRUE))
  summary <- do.call(base::rbind, lapply(groups, function(g) {
    row <- data.frame(mechanism = g$mechanism[1], method = g$method[1], S = nrow(g))
    for (v in names(TRUTH)) {
      row[[v]] <- mean(g[[v]])
      row[[paste0("mcse_", v)]] <- sd(g[[v]]) / sqrt(nrow(g))
      row[[paste0("emp_se_", v)]] <- sd(g[[v]])
      row[[paste0("rmse_", v)]] <- sqrt(mean((g[[v]] - TRUTH[v])^2))
    }
    for (v in c("mu2", "b1")) {
      coverage <- mean(g[[paste0("cover_", v)]])
      row[[paste0("coverage_", v)]] <- coverage
      row[[paste0("mcse_coverage_", v)]] <- sqrt(coverage * (1 - coverage) / nrow(g))
      row[[paste0("mean_se_", v)]] <- mean(g[[paste0("se_", v)]])
    }
    row$missing_fraction <- mean(g$missing_fraction)
    row$mcse_missing_fraction <- sd(g$missing_fraction) / sqrt(nrow(g))
    row
  }))
  rownames(summary) <- NULL
  list(summary = summary, raw = raw, seed = seed, S = S, n = n)
}

# Собственная нормальная MI: сначала sigma²* и beta*, затем остаточный шум.
# Для одной неполной переменной итерации FCS не нужны.
normal_imputations <- function(data, m = 20L, proper = TRUE) {
  obs <- !is.na(data$bmi)
  X <- base::cbind(1, data$sbp - mean(data$sbp))
  fit <- lm.fit(X[obs, , drop = FALSE], data$bmi[obs])
  df <- sum(obs) - ncol(X)
  sigma2 <- sum(fit$residuals^2) / df
  root <- chol(solve(crossprod(X[obs, , drop = FALSE])))
  lapply(seq_len(m), function(i) {
    s2 <- if (proper) df * sigma2 / rchisq(1, df) else sigma2
    beta <- fit$coefficients
    if (proper) beta <- beta + drop(t(root) %*% rnorm(ncol(X))) * sqrt(s2)
    out <- data
    out$bmi[!obs] <- drop(X[!obs, , drop = FALSE] %*% beta) + rnorm(sum(!obs), 0, sqrt(s2))
    out
  })
}

pool_normal <- function(datasets) {
  metrics <- vapply(datasets, function(d) pair_metrics(d$sbp, d$bmi), numeric(7))
  do.call(base::rbind, lapply(c("mu2", "b1"), function(term) {
    fit <- mice::pool.scalar(metrics[term, ], metrics[paste0("se_", term), ]^2,
                             n = nrow(datasets[[1]]), k = if (term == "mu2") 1 else 2)
    se <- sqrt(fit$t); margin <- qt(.975, fit$df) * se
    data.frame(term = term, estimate = fit$qbar, se = se, W = fit$ubar, B = fit$b,
               T = fit$t, df = fit$df, fmi = fit$fmi,
               lo = fit$qbar - margin, hi = fit$qbar + margin)
  }))
}

run_mi_coverage <- function(S = 2000L, seed = 202602L, n = DGP$n) {
  set.seed(seed)
  raw <- vector("list", S * 5L); at <- 0L
  for (s in seq_len(S)) {
    set.seed(seed + s)
    full <- make_complete(n)
    samples <- lapply(c("MCAR", "MAR", "MNAR"), function(mech) make_missing(full, mech))
    # Те же данные и тот же шум одиночной импутации, что в Части I.
    methods_all <- lapply(samples, function(d) analyse_methods(d, full))
    data <- samples[[2]]; methods <- methods_all[[2]]
    for (method in c("StochReg", "MLE")) {
      v <- methods[methods$method == method, ]
      at <- at + 1L
      raw[[at]] <- data.frame(method = method, replicate = s, term = c("mu2", "b1"),
        estimate = c(v$mu2, v$b1), se = c(v$se_mu2, v$se_b1),
        lo = c(v$mu2, v$b1) - qnorm(.975) * c(v$se_mu2, v$se_b1),
        hi = c(v$mu2, v$b1) + qnorm(.975) * c(v$se_mu2, v$se_b1), W = NA, B = NA, T = NA, df = NA, fmi = NA)
    }
    proper <- normal_imputations(data, 20, TRUE)
    sets <- list(Improper20 = normal_imputations(data, 20, FALSE), Proper5 = proper[1:5], Proper20 = proper)
    for (method in names(sets)) {
      at <- at + 1L; pooled <- pool_normal(sets[[method]])
      pooled$method <- method; pooled$replicate <- s
      raw[[at]] <- pooled[, names(raw[[1]])]
    }
  }
  raw <- do.call(base::rbind, raw)
  raw$covered <- raw$lo <= TRUTH[raw$term] & raw$hi >= TRUTH[raw$term]
  groups <- split(raw, interaction(raw$method, raw$term, drop = TRUE))
  summary <- do.call(base::rbind, lapply(groups, function(g) {
    p <- mean(g$covered)
    row <- data.frame(method = g$method[1], term = g$term[1], coverage = p,
      mcse_coverage = sqrt(p * (1 - p) / S), empirical_se = sd(g$estimate))
    for (v in c("estimate", "se", "W", "B", "T", "fmi")) {
      row[[v]] <- if (all(is.na(g[[v]]))) NA_real_ else mean(g[[v]])
      row[[paste0("mcse_", v)]] <- if (all(is.na(g[[v]]))) NA_real_ else sd(g[[v]]) / sqrt(S)
    }
    row
  }))
  rownames(summary) <- NULL
  list(summary = summary, raw = raw, seed = seed, S = S, n = n)
}


if (lesson %in% c("1", "all")) {
# ЗАНЯТИЕ 1. Природа пропусков, наивные методы, правдоподобие.
# Независимый запуск из корня проекта: source("practice/practice_part1.R")
need_packages(c("MASS", "naniar", "ggplot2"))
dir.create("outputs", showWarnings = FALSE)
if (!interactive()) grDevices::cairo_pdf("outputs/practice-part1.pdf", width = 10, height = 6)

# МОДУЛЬ 1. Сначала прогноз: будет ли среднее ИМТ по наблюдаемым занижено?
set.seed(202601)
n <- DGP$n
complete_data <- make_complete(n)
cohort <- data.frame(id = seq_len(n), age = round(rnorm(n, 55, 10)),
  treatment = factor(rbinom(n, 1, .5), labels = c("Control", "Active")),
  sbp = round(complete_data$sbp, 1), bmi = round(complete_data$bmi, 1))
cohort$glucose <- round(5.5 + .08 * (cohort$bmi - 28) + rnorm(n, 0, .7), 2)

# Этот механизм отличается от основной симуляции: здесь добавлен возраст.
# Это MAR относительно полной пары наблюдаемых предикторов (САД, возраст).
p <- plogis(-1.1 + .06 * (cohort$sbp - 135) + .03 * (cohort$age - 55))
cohort$bmi[rbinom(n, 1, p) == 1] <- NA_real_
cohort$glucose[rbinom(n, 1, .15) == 1] <- NA_real_
print(colMeans(is.na(cohort)))
print(naniar::vis_miss(cohort))
print(ggplot2::ggplot(cohort, ggplot2::aes(sbp, fill = is.na(bmi))) +
  ggplot2::geom_density(alpha = .4) + ggplot2::theme_minimal() +
  ggplot2::labs(x = "САД, мм рт. ст.", y = "Плотность", fill = "ИМТ пропущен"))
print(t.test(sbp ~ is.na(bmi), data = cohort))
# H0 — MCAR. Отклонение не доказывает MAR; неотклонение не доказывает MCAR.
print(naniar::mcar_test(cohort[c("age", "sbp", "bmi", "glucose")]))

# МОДУЛЬ 2. Единый DGP: по очереди MCAR / MAR / MNAR.
set.seed(202602)
full <- make_complete()
observed <- make_missing(full, "MAR")
comparison <- analyse_methods(observed, full)
print(comparison, digits = 4)
cat("Дисперсия среднего ММП по трём слагаемым:\n")
print(attr(comparison, "variance_components"))

# До запуска: каким методам ожидаете хорошее покрытие mu2? А beta1?
simulation <- run_simulation(S = getOption("missingdata.S", 300L))
sim_results_l1 <- simulation$summary
print(sim_results_l1[c("mechanism", "method", "mu2", "b1", "g1", "emp_se_mu2",
                      "rmse_mu2", "coverage_mu2", "mcse_coverage_mu2", "coverage_b1")], digits = 3)

# ЗАДАНИЯ
# 1. Сравните beta1 (ИМТ ~ САД) и gamma1 (САД ~ ИМТ) при MAR и MNAR.
#    Объясните результат через C ⟂ исход | предикторы модели.
# 2. Сравните формальную SE и эмпирический разброс; учитывайте MCSE покрытия.
# 3. Уберите возраст из доступных данных модуля 1. Станет ли механизм MNAR?
#    Возраст здесь генерируется независимо от ИМТ: удаление предиктора пропуска
#    само по себе НЕ обязано приводить к MNAR. Обоснуйте условную зависимость.
# 4. Замените make_complete(rho = .60) на rho = .30: что изменилось?
# 5. Дополнительно: реальные данные NHANES, BPSysAve и BMI.
#    Нужен пакет NHANES; не называйте механизм MAR только по результату теста.
if (isTRUE(getOption("missingdata.real_data", FALSE))) {
  need_packages("NHANES")
  data("NHANES", package = "NHANES", envir = environment())
  real_data <- unique(NHANES[c("ID", "Age", "BPSysAve", "BMI")])
  print(summary(real_data))
  print(naniar::vis_miss(real_data))
}

if (!interactive()) grDevices::dev.off()

}

if (lesson %in% c("2", "all")) {
# ЗАНЯТИЕ 2. MICE, PMM, пассивная импутация и правила Рубина.
# Независимый запуск из корня проекта: source("practice/practice_part2.R")
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
# Предрасчёт R: 2000 репликаций, n = 500; включён в файл.
coverage <- data.frame(
method = c("Improper20", "MLE", "Proper20", "Proper5", "StochReg", "Improper20", "MLE", "Proper20", "Proper5", "StochReg"),
term = c("b1", "b1", "b1", "b1", "b1", "mu2", "mu2", "mu2", "mu2", "mu2"),
coverage = c(0.906, 0.9415, 0.9465, 0.947, 0.8035, 0.937, 0.9535, 0.953, 0.9545, 0.869),
mcse_coverage = c(0.006525488487462, 0.00524774951765, 0.005031786462083, 0.005009540897128, 0.008885036578428, 0.005432816948877, 0.004708383480559, 0.004732388403333, 0.004659922209651, 0.0075445013089),
fmi = c(0.2895392740617, NA_real_, 0.464709598735, 0.4840162301179, NA_real_, 0.1807022162257, NA_real_, 0.3054747520048, 0.3288051318343, NA_real_))
print(coverage)

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

}

if (lesson %in% c("3", "all")) {
# ЗАНЯТИЕ 3. SMC-FCS, выживаемость, бутстреп и взвешивание.
# Независимый запуск из корня проекта: source("practice/practice_part3.R")
# Модули 6, 7, 9 выполняются по умолчанию. Дорогой модуль 8 включается явно:
# options(missingdata.bootstrap = TRUE, missingdata.nBoot = 200L)
need_packages(c("mice", "smcfcs", "mitools", "survival", "MatchThem", "survey", "cobalt"))
suppressPackageStartupMessages({
  library(mice)
  library(smcfcs)
  library(mitools)
  library(survival)
})
dir.create("outputs", showWarnings = FALSE)
if (!interactive()) grDevices::cairo_pdf("outputs/practice-part3.pdf", width = 10, height = 6)
M_IMP <- getOption("missingdata.m", 20L)
T_IMP <- getOption("missingdata.maxit", 20L)
RJ_LIMIT <- getOption("missingdata.rjlimit", 100000L)

# Не используем результат, если алгоритм сообщил о неудачных розыгрышах.
# На исходном нелинейном DGP такие отказы возможны даже при rjlimit = 100000.
# Разберите диагностику; повышение лимита увеличивает время расчёта.
smcfcs_checked <- function(...) {
  failures <- character()
  result <- withCallingHandlers(smcfcs::smcfcs(...), warning = function(w) {
    if (grepl("Rejection sampling failed", conditionMessage(w), fixed = TRUE))
      failures <<- c(failures, conditionMessage(w))
  })
  result$rejection_warnings <- failures
  result$rejection_ok <- !length(failures)
  if (!result$rejection_ok)
    message("SMC-FCS: есть отказы rejection sampling; результат исключён из итоговой таблицы.")
  result
}

# Вспомогательная функция: одна строка сводной таблицы.
# Для компактного сопоставления здесь нормальные приближённые ДИ ±1.96 SE;
# это не интервалы с поправкой степеней свободы и не проверка покрытия.
row_est <- function(label, est, se, term) {
  data.frame(method = label, term = term, estimate = est, se = se,
             lo = est - 1.96 * se, hi = est + 1.96 * se, row.names = NULL)
}

# ==============================================================================
# МОДУЛЬ 6: НЕЛИНЕЙНЫЙ ЭФФЕКТ. ПАССИВНАЯ ИМПУТАЦИЯ vs JAV vs SMC-FCS
# Модель анализа: САД ~ ИМТ + ИМТ^2 + возраст. Пропуски - в ИМТ, зависят от САД.
# ==============================================================================

set.seed(3001)
n6    <- 1000
age6  <- round(rnorm(n6, 55, 10))
bmi_c <- rnorm(n6, 28, 5) - 28                    # центрированный ИМТ
sbp6  <- 135 + 1.8 * bmi_c + 0.10 * bmi_c^2 + 0.25 * (age6 - 55) + rnorm(n6, 0, 12)
TRUE6 <- c(bmi_c = 1.8, bmi_c2 = 0.10)

full6 <- data.frame(sbp = sbp6, age = age6, bmi_c = bmi_c, bmi_c2 = bmi_c^2)

# Пропуск ИМТ зависит от исхода (САД): CCA здесь не защищён теоремой валидности
p_miss6 <- plogis(-1.0 + 0.04 * (sbp6 - mean(sbp6)))
miss6   <- rbinom(n6, 1, p_miss6) == 1
d6 <- full6
d6$bmi_c[miss6]  <- NA
d6$bmi_c2[miss6] <- NA
mean(miss6)                                        # доля пропусков

f6 <- sbp ~ bmi_c + bmi_c2 + age

# 0. Ориентир: полные данные без пропусков
fit_full6 <- lm(f6, data = full6)

# 1. Анализ полных случаев
fit_cca6 <- lm(f6, data = d6)

# 2. mice, пассивная импутация: импутируем bmi_c линейной моделью, квадрат пересчитываем
ini6  <- mice(d6, maxit = 0, printFlag = FALSE)
meth_p <- ini6$method
pred_p <- ini6$predictorMatrix
meth_p["bmi_c"]  <- "norm"
meth_p["bmi_c2"] <- "~ I(bmi_c^2)"
pred_p["bmi_c", "bmi_c2"] <- 0                     # квадрат не предсказывает сам себя
imp_pass <- mice(d6, m = M_IMP, maxit = T_IMP, method = meth_p, predictorMatrix = pred_p,
                 seed = 61, printFlag = FALSE)
pool_pass <- summary(pool(with(imp_pass, lm(sbp ~ bmi_c + bmi_c2 + age))))

# 3. mice, JAV ("just another variable"): bmi_c и bmi_c2 - две обычные переменные
meth_j <- ini6$method
meth_j[c("bmi_c", "bmi_c2")] <- "norm"
imp_jav <- mice(d6, m = M_IMP, maxit = T_IMP, method = meth_j, seed = 62, printFlag = FALSE)
pool_jav <- summary(pool(with(imp_jav, lm(sbp ~ bmi_c + bmi_c2 + age))))
# Проверьте: в заполненных данных JAV равенство bmi_c2 == bmi_c^2 нарушено
print(with(complete(imp_jav, 1), summary(bmi_c2 - bmi_c^2)))

# 4. SMC-FCS: модель анализа задаётся при импутации
#    method - по столбцам d6: sbp, age, bmi_c, bmi_c2
set.seed(63)
imp_smc <- smcfcs_checked(d6, smtype = "lm", smformula = "sbp ~ bmi_c + bmi_c2 + age",
                  method = c("", "", "norm", "bmi_c^2"),
                  m = M_IMP, numit = T_IMP, rjlimit = RJ_LIMIT, noisy = FALSE)
fits_smc <- with(imputationList(imp_smc$impDatasets), lm(sbp ~ bmi_c + bmi_c2 + age))
pool_smc <- MIcombine(fits_smc)
if (imp_smc$rejection_ok) print(summary(pool_smc))

# Сходимость: траектории коэффициентов модели анализа по итерациям.
# smcfcs 2.0.3: [импутация, коэффициент, итерация]. Проверяем размерность.
print(dim(imp_smc$smCoefIter))
stopifnot(length(dim(imp_smc$smCoefIter)) == 3L,
          dim(imp_smc$smCoefIter)[1] == M_IMP,
          dim(imp_smc$smCoefIter)[3] == T_IMP)
matplot(t(imp_smc$smCoefIter[, 3, ]), type = "l", lty = 1,
        xlab = "Итерация", ylab = "Коэффициент при bmi_c2")

# Сводная таблица для коэффициента при квадрате
get_lm   <- function(fit, term) c(coef(fit)[term], sqrt(diag(vcov(fit)))[term])
get_pool <- function(s, term) unlist(s[s$term == term, c("estimate", "std.error")])
tab6 <- rbind(
  row_est("Полные данные", get_lm(fit_full6, "bmi_c2")[1], get_lm(fit_full6, "bmi_c2")[2], "bmi_c2"),
  row_est("CCA",           get_lm(fit_cca6,  "bmi_c2")[1], get_lm(fit_cca6,  "bmi_c2")[2], "bmi_c2"),
  row_est("mice: пассивная", get_pool(pool_pass, "bmi_c2")[1], get_pool(pool_pass, "bmi_c2")[2], "bmi_c2"),
  row_est("mice: JAV",       get_pool(pool_jav,  "bmi_c2")[1], get_pool(pool_jav,  "bmi_c2")[2], "bmi_c2"),
  if (imp_smc$rejection_ok)
    row_est("smcfcs", coef(pool_smc)["bmi_c2"], sqrt(diag(vcov(pool_smc)))["bmi_c2"], "bmi_c2")
)
print(tab6, digits = 3)
cat("Истинное значение коэффициента при bmi_c2:", TRUE6["bmi_c2"], "\n")


# ==============================================================================
# МОДУЛЬ 7: МОДЕЛЬ КОКСА С ПРОПУСКАМИ В КОВАРИАТЕ
# Сравниваем: CCA; mice с индикатором события и риском Нельсона-Аалена
# (White & Royston, 2009); smcfcs с smtype = "coxph".
# ==============================================================================

set.seed(3002)
n7   <- 2000
age7 <- rnorm(n7, 55, 10)
trt7 <- rbinom(n7, 1, 0.5)
bmi7 <- rnorm(n7, 28, 5)
TRUE7 <- c(bmi = 0.06, age = 0.03, trt = -0.40)   # лог-отношения рисков
lp7  <- TRUE7["bmi"] * (bmi7 - 28) + TRUE7["age"] * (age7 - 55) + TRUE7["trt"] * trt7
t_ev <- rexp(n7, rate = 0.05 * exp(lp7))
t_ce <- runif(n7, 2, 12)                           # административное цензурирование
time7   <- pmin(t_ev, t_ce)
status7 <- as.integer(t_ev <= t_ce)
mean(status7)                                      # доля событий

# Пропуск ИМТ зависит от возраста и от факта события
p_miss7 <- plogis(-1.2 + 0.8 * status7 + 0.03 * (age7 - 55))
bmi_obs <- ifelse(rbinom(n7, 1, p_miss7) == 1, NA, bmi7)
d7 <- data.frame(time = time7, status = status7, age = age7, trt = trt7, bmi = bmi_obs)

f7 <- Surv(time, status) ~ bmi + age + trt

# 1. Анализ полных случаев
fit_cca7 <- coxph(f7, data = d7)

# 2. mice: в модель импутации входят status и H(T) Нельсона-Аалена, время - нет
d7m   <- d7
d7m$H <- nelsonaalen(d7m, time, status)
# Задаём предикторы ДО инициализации. Иначе mice может удалить H
# как почти коллинеарный time, хотя time мы не собирались использовать.
pred7 <- make.predictorMatrix(d7m)
pred7[,] <- 0
pred7["bmi", c("status", "H", "age", "trt")] <- 1
ini7  <- mice(d7m, maxit = 0, predictorMatrix = pred7, printFlag = FALSE)
meth7 <- ini7$method;  meth7["bmi"] <- "norm"
imp_wr <- mice(d7m, m = M_IMP, maxit = T_IMP, method = meth7, predictorMatrix = pred7,
               seed = 71, printFlag = FALSE)
stopifnot(imp_wr$predictorMatrix["bmi", "H"] == 1,
          imp_wr$predictorMatrix["bmi", "time"] == 0)
print(imp_wr$loggedEvents)
pool_wr <- summary(pool(with(imp_wr, coxph(Surv(time, status) ~ bmi + age + trt))))

# 3. smcfcs: method по столбцам d7 - time, status, age, trt, bmi
set.seed(72)
imp_smc7 <- smcfcs_checked(d7, smtype = "coxph", smformula = "Surv(time, status) ~ bmi + age + trt",
                   method = c("", "", "", "", "norm"), m = M_IMP, numit = T_IMP, rjlimit = RJ_LIMIT, noisy = FALSE)
fits_smc7 <- with(imputationList(imp_smc7$impDatasets),
                  coxph(Surv(time, status) ~ bmi + age + trt))
pool_smc7 <- MIcombine(fits_smc7)

tab7 <- rbind(
  row_est("CCA", coef(fit_cca7)["bmi"], sqrt(diag(vcov(fit_cca7)))["bmi"], "bmi"),
  row_est("mice + Нельсон-Аален", get_pool(pool_wr, "bmi")[1], get_pool(pool_wr, "bmi")[2], "bmi"),
  if (imp_smc7$rejection_ok)
    row_est("smcfcs (coxph)", coef(pool_smc7)["bmi"], sqrt(diag(vcov(pool_smc7)))["bmi"], "bmi")
)
print(tab7, digits = 3)
cat("Истинный log HR для ИМТ:", TRUE7["bmi"], "\n")


# ==============================================================================
# МОДУЛЬ 8: БУТСТРЕП + MI (сначала бутстреп, потом импутация)
# Bartlett & Hughes (2020). Данные модуля 6, импутация через mice.
# API проверен: bootImpute 1.3.0, bootMice(obsdata, nBoot, nImp, ...).
# ==============================================================================

if (isTRUE(getOption("missingdata.bootstrap", FALSE))) {
  need_packages("bootImpute")
  library(bootImpute)
  set.seed(3003)
  # 200 бутстреп-выборок, по 2 импутации в каждой
  boot_imps <- bootMice(d6, nBoot = getOption("missingdata.nBoot", 200L), nImp = 2, method = meth_p,
                        predictorMatrix = pred_p, maxit = 10, printFlag = FALSE)
  analyse6 <- function(dat) coef(lm(sbp ~ bmi_c + bmi_c2 + age, data = dat))
  boot_res <- bootImputeAnalyse(boot_imps, analyse6)
  print(boot_res)
}


# ==============================================================================
# МОДУЛЬ 9: ВЗВЕШИВАНИЕ ПО PROPENSITY SCORE ПОСЛЕ MI (подход within)
# Leyrat et al. (2019): объединяем оценки эффекта; исход включён в импутацию.
# API проверен с MatchThem 1.2.1; внутри датасета используются SE survey.
# ==============================================================================

{
  library(MatchThem)
  library(survey)
  set.seed(3004)
  n9   <- 1500
  age9 <- rnorm(n9, 55, 10)
  bmi9 <- rnorm(n9, 28, 5)
  # Назначение терапии зависит от конфаундеров: наблюдательное исследование
  trt9 <- rbinom(n9, 1, plogis(-0.2 + 0.08 * (bmi9 - 28) + 0.03 * (age9 - 55)))
  TRUE9 <- -8
  sbp9 <- 135 + 1.8 * (bmi9 - 28) + 0.25 * (age9 - 55) + TRUE9 * trt9 + rnorm(n9, 0, 12)
  bmi9_obs <- ifelse(rbinom(n9, 1, plogis(-1.0 + 0.03 * (sbp9 - 135))) == 1, NA, bmi9)
  d9 <- data.frame(sbp = sbp9, trt = trt9, age = age9, bmi = bmi9_obs)

  # Наивные ориентиры
  print(coef(lm(sbp ~ trt, data = d9))["trt"])              # без поправки на конфаундеры
  print(coef(lm(sbp ~ trt + bmi + age, data = d9))["trt"])  # регрессия по полным случаям

  # MI: исход (sbp) остаётся в модели импутации
  imp9 <- mice(d9, m = M_IMP, maxit = T_IMP, method = "norm", seed = 91, printFlag = FALSE)

  # Взвешивание в каждом заполненном датасете (within)
  w9   <- weightthem(trt ~ bmi + age, datasets = imp9, approach = "within",
                     method = "glm", estimand = "ATE")
  fit9 <- with(w9, svyglm(sbp ~ trt))
  pooled9 <- summary(pool(fit9), conf.int = TRUE)
  print(pooled9)

  # Баланс ковариат - в каждом датасете
  if (requireNamespace("cobalt", quietly = TRUE)) print(cobalt::bal.tab(w9))
  cat("Истинный эффект терапии:", TRUE9, "\n")
}


# ==============================================================================
# ЗАДАНИЯ
# Перед каждым запуском запишите прогноз одним предложением.
# ==============================================================================
# 6.1 Какой из четырёх способов ближе всего к истинному коэффициенту при bmi_c2?
#     Объясните результат JAV и пассивной импутации через понятие совместимости.
#     Если smcfcs отсутствует в таблице, сначала разберите rejection_warnings:
#     сравнивать его оценку с остальными до устранения отказов нельзя.
# 6.2 Замените механизм пропуска на зависящий только от возраста. Что стало с CCA
#     и почему (теорема валидности CCA из Части I)?
# 6.3 Добавьте в модель анализа взаимодействие bmi_c:age. Как изменятся smformula
#     и вектор method?
# 7.1 Увеличьте эффект ИМТ до log HR = 0.15 и добавьте в модель анализа квадрат ИМТ.
#     Как меняется расхождение между приближением White-Royston и smcfcs?
# 7.2 Замените в модели импутации H на log(time). Что происходит с оценкой?
# 8.1 Сравните бутстреп-интервал с интервалом по правилам Рубина из модуля 6.
# 9.1 Уберите sbp из модели импутации (обнулите столбец predictorMatrix).
#     Как изменилась оценка эффекта терапии?
# 9.2 Повторите с approach = "across" и сравните с within.

# ДОПОЛНИТЕЛЬНО: MNAR и чувствительность к delta (единицы ИМТ, кг/м²).
# Это сдвиг только скрытых значений; наблюдённые значения сохраняются.
set.seed(3086)
full_delta <- make_complete()
data_delta <- make_missing(full_delta, "MNAR")
missing_delta <- is.na(data_delta$bmi)
imputed_delta <- normal_imputations(data_delta, m = M_IMP)
delta_results <- do.call(rbind, lapply(seq(-2, 6, by = 1), function(delta) {
  shifted <- lapply(imputed_delta, function(d) {
    d$bmi[missing_delta] <- d$bmi[missing_delta] + delta
    d
  })
  result <- pool_normal(shifted)
  result$delta <- delta
  result
}))
print(delta_results[delta_results$term == "mu2", c("delta", "estimate", "lo", "hi")])
# Сравнение с истиной возможно только потому, что это симуляция.
# В реальном исследовании диапазон delta обосновывают предметно заранее.
saveRDS(list(nonlinear = tab6, survival = tab7, weighted = pooled9,
             delta = delta_results, events = list(passive = imp_pass$loggedEvents,
               jav = imp_jav$loggedEvents, survival = imp_wr$loggedEvents, weighting = imp9$loggedEvents),
             settings = list(m = M_IMP, iterations = T_IMP, rjlimit = RJ_LIMIT),
             diagnostics = list(smc_nonlinear = imp_smc$rejection_warnings,
                                smc_survival = imp_smc7$rejection_warnings),
             session = sessionInfo()), "outputs/part3-results.rds")
if (!interactive()) grDevices::dev.off()

}
