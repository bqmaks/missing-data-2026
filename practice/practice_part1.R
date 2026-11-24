# ЗАНЯТИЕ 1. Природа пропусков, наивные методы, правдоподобие.
# Независимый запуск из корня проекта: source("practice/practice_part1.R")
source("r/00_dgp.R")
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
