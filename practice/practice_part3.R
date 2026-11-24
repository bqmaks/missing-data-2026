# ЗАНЯТИЕ 3. SMC-FCS, выживаемость, бутстреп и взвешивание.
# Независимый запуск из корня проекта: source("practice/practice_part3.R")
# Модули 6, 7, 9 выполняются по умолчанию. Дорогой модуль 8 включается явно:
# options(missingdata.bootstrap = TRUE, missingdata.nBoot = 200L)
source("r/00_dgp.R")
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
