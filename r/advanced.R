# Дополнения combined_v2. Вычисления — base R; исходная выборка не меняется.
moment_parameters <- function(mu, S) {
  b <- S[1, 2] / S[1, 1]; a <- S[1, 2] / S[2, 2]
  setNames(c(mu, sqrt(diag(S)), cov2cor(S)[1, 2], mu[2] - b * mu[1], b,
             mu[1] - a * mu[2], a), names(population_parameters(example_config())))
}

ipw_fit <- function(d) {
  C <- !is.na(d$SBP)
  propensity <- glm(C ~ BMI, data = d, family = binomial())
  stopifnot(propensity$converged)
  pi <- predict(propensity, type = 'response')
  w <- 1 / pi[C]
  xy <- as.matrix(d[C, c('BMI', 'SBP')])
  mu <- colSums(xy * w) / sum(w)
  centered <- sweep(xy, 2, mu)
  S <- crossprod(centered, centered * w) / sum(w)
  list(parameters = moment_parameters(mu, S), pi = pi, w = w,
       ess = sum(w)^2 / sum(w^2), propensity = propensity)
}

normal_ml <- function(d) {
  fit <- lm(SBP ~ BMI, d)
  b <- coef(fit); vx <- mean((d$BMI - mean(d$BMI))^2)
  ve <- mean(residuals(fit)^2) # ML: n_obs, не n_obs - 2.
  mu <- c(mean(d$BMI), b[1] + b[2] * mean(d$BMI))
  S <- matrix(c(vx, b[2]*vx, b[2]*vx, b[2]^2*vx + ve), 2)
  list(mu = unname(mu), S = S, parameters = moment_parameters(mu, S))
}

normal_loglik <- function(d, mu, S) {
  ok <- !is.na(d$SBP)
  b <- S[1, 2] / S[1, 1]
  ve <- S[2, 2] - S[1, 2]^2 / S[1, 1]
  sum(dnorm(d$BMI, mu[1], sqrt(S[1, 1]), log = TRUE)) +
    sum(dnorm(d$SBP[ok], mu[2] + b * (d$BMI[ok] - mu[1]), sqrt(ve), log = TRUE))
}

normal_em <- function(d, tol = 1e-9, maxit = 2000L) {
  x <- d$BMI; y <- d$SBP; mis <- is.na(y)
  mu <- c(mean(x), mean(y, na.rm = TRUE))
  S <- diag(c(var(x), var(y, na.rm = TRUE)))
  trace <- function(k) data.frame(iteration = k, mean_SBP = mu[2],
    sd_SBP = sqrt(S[2, 2]), correlation = cov2cor(S)[1, 2],
    loglik = normal_loglik(d, mu, S))
  history <- trace(0)
  for (k in seq_len(maxit)) {
    old <- c(mu, S)
    b <- S[1, 2] / S[1, 1]
    ve <- S[2, 2] - S[1, 2]^2 / S[1, 1]
    ey <- y; ey[mis] <- mu[2] + b * (x[mis] - mu[1])
    ey2 <- ey^2; ey2[mis] <- ey2[mis] + ve
    mu <- c(mean(x), mean(ey))
    S <- matrix(c(mean(x^2), mean(x*ey), mean(x*ey), mean(ey2)), 2) - tcrossprod(mu)
    history <- rbind(history, trace(k))
    if (max(abs(c(mu, S) - old)) < tol) break
  }
  if (k == maxit) stop('EM did not converge')
  list(mu = mu, S = S, parameters = moment_parameters(mu, S), history = history)
}

binary_mar <- function(ex, seed = 20261150L) {
  d <- ex$complete; obese <- d$BMI >= 30
  # Вероятности пропуска САД: 0.80 при ожирении, 0.10 без него (D58; раньше 0.60 / 0.25).
  set.seed(seed); M <- rbinom(nrow(d), 1, ifelse(obese, .80, .10)) == 1
  z <- (30 - ex$config$mu['BMI']) / ex$config$sigma['BMI']
  mean_x <- ex$config$mu['BMI'] + ex$config$sigma['BMI'] *
    c(-dnorm(z)/pnorm(z), dnorm(z)/(1-pnorm(z)))
  truth_y <- ex$truth['intercept_SBP'] + ex$truth['slope_SBP'] * mean_x
  group <- do.call(rbind, lapply(0:1, function(g) {
    idx <- obese == g
    data.frame(group = c('ИМТ < 30', 'ИМТ ≥ 30')[g+1], n = sum(idx),
      n_obs = sum(idx & !M), observed_fraction = mean(!M[idx]), truth = truth_y[g+1],
      full = mean(d$SBP[idx]), CC = mean(d$SBP[idx & !M]))
  }))
  list(data = transform(d, obese = as.integer(obese), M = as.integer(M),
                        SBP_obs = ifelse(M, NA, d$SBP)), groups = group,
       overall_CC = mean(d$SBP[!M]), obese_full = mean(obese),
       obese_CC = mean(obese[!M]), seed = seed)
}

category_example <- function(seed = 20261151L) {
  set.seed(seed); n <- 800L
  male <- rbinom(n, 1, .5)
  BMI <- rnorm(n, 29 - 4*male, 2)
  SBP <- 95 + BMI + 12*male + rnorm(n, 0, 8)
  M <- rbinom(n, 1, .35) == 1
  sex <- factor(ifelse(M, 'Missing', ifelse(male == 1, 'Мужчины', 'Женщины')),
                levels = c('Женщины', 'Мужчины', 'Missing'))
  d <- data.frame(BMI, SBP, male, M = as.integer(M), sex)
  full <- lm(SBP ~ BMI + male, d); missing <- lm(SBP ~ BMI + sex, d)
  within <- lm(SBP ~ BMI, d[M, ])
  list(data = d, full = full, missing = missing, within = within,
       truth = c(95, 1, 12), seed = seed)
}

# Неразделимость (D63): Y ~ Bernoulli(θ), M ~ Bernoulli(ψ) независимо (MCAR), ограничение ψ = θ.
# Совместное правдоподобие ∝ θ^(s + n − n_obs) (1 − θ)^(2 n_obs − s), s = Σ_{M=0} Y.
distinctness_example <- function(B = 3000L, seed = 20261152L, n = 100L, theta = .3) {
  set.seed(seed)
  draws <- replicate(B, {
    y <- rbinom(n, 1, theta); M <- rbinom(n, 1, theta)
    no <- sum(M == 0); successes <- sum(y[M == 0])
    c(observed = if (no) successes/no else NA_real_, joint = (successes + n - no)/(n + no))
  })
  list(draws = t(draws), B = B, seed = seed, n = n, theta = theta)
}

# IPW на бинарном примере: π̂ – доля наблюдаемых САД в группе, вес w = 1/π̂.
# Оценка Хайека совпадает с Σ (n_g / n) × среднее полных случаев в группе.
binary_ipw <- function(b) {
  d <- b$data; ok <- d$M == 0
  pi_hat <- ave(1 - d$M, d$obese)
  w <- 1 / pi_hat
  list(pi = tapply(pi_hat, d$obese, unique), w = tapply(w, d$obese, unique),
       mean_SBP = sum(w[ok] * d$SBP[ok]) / sum(w[ok]))
}

# Демонстрационные данные для потери полных случаев и available case (D47):
# Y1 наблюдается полностью, в Y2, Y3, Y4 по 25% пропусков в непересекающихся
# строках (MCAR: строки распределены по блокам случайно, независимо от значений).
ac_demo <- function(seed = 20261154L, n = 1000L) {
  set.seed(seed)
  S <- matrix(.3, 3, 3); diag(S) <- 1
  X <- matrix(rnorm(n * 3), n) %*% chol(S)
  beta <- c(2, 1, .5, -.5)
  d <- data.frame(Y1 = beta[1] + X %*% beta[-1] + rnorm(n), Y2 = X[, 1], Y3 = X[, 2], Y4 = X[, 3])
  block <- sample(rep(1:4, length.out = n))
  obs <- d
  for (j in 1:3) obs[[j + 1]][block == j] <- NA
  list(full = d, observed = obs, block = block, beta = setNames(beta, c('(Intercept)', 'Y2', 'Y3', 'Y4')),
       predictor_cor = .3, seed = seed)
}

# Коэффициенты Y1 ~ Y2 + Y3 + Y4 по вектору средних и ковариационной матрице (lavaan).
sem_coef <- function(mu, S, n) {
  fit <- lavaan::sem('Y1 ~ Y2 + Y3 + Y4', sample.mean = mu, sample.cov = S,
                     sample.nobs = n, meanstructure = TRUE)
  pe <- lavaan::parameterEstimates(fit)
  pe <- pe[pe$lhs == 'Y1' & pe$op %in% c('~1', '~'), ]
  setNames(pe$est[match(c('', 'Y2', 'Y3', 'Y4'), ifelse(pe$op == '~1', '', pe$rhs))],
           c('(Intercept)', 'Y2', 'Y3', 'Y4'))
}

ac_estimates <- function(demo) {
  d <- demo$full; o <- demo$observed
  list(lm_full = coef(lm(Y1 ~ Y2 + Y3 + Y4, d)),
       sem_full = sem_coef(colMeans(d), cov(d), nrow(d)),
       lm_cc = coef(lm(Y1 ~ Y2 + Y3 + Y4, o)),
       sem_ac = sem_coef(colMeans(o, na.rm = TRUE), cov(o, use = 'pairwise.complete.obs'), nrow(o)),
       n_cc = sum(complete.cases(o)),
       n_pairs = crossprod(!is.na(as.matrix(o))),
       missing_share = colMeans(is.na(o)))
}

# Иллюстрация LOCF (D58, D59): по 5 участников в группах лечения и контроля, визиты 0, 3, 6,
# 9, 12 месяцев (t = 0, …, 4); среднее 50 + c_g × t, c_T = −1, c_C = −2 (модель прежнего
# интерактива); индивидуальный уровень N(0, 2²), наклон N(0, 0.3²), шум N(0, 0.7²).
# Два сценария выбывания на одних и тех же траекториях: участники 2 и 4 из пяти выбывают
# после визита 1 (3-й месяц) только в одной группе; схема задана заранее, без подбора.
# Ожидаемое смещение эффекта на 12-м месяце: −2 × 3 × 2/5 = −2.4 (ctrl), +1 × 3 × 2/5 = +1.2 (trt):
#   ctrl – выбывают в контроле: LOCF завышает исход контроля, эффект занижается;
#   trt  – выбывают в лечении (как в примере EMA 2010 для ухудшающихся состояний):
#          LOCF завышает исход в группе лечения, эффект завышается.
locf_trajectories <- function(seed = 20261157L, n = 5L) {
  set.seed(seed)
  visits <- 0:4; slopes <- c(Лечение = -1, Контроль = -2)
  early <- c(NA, 1, NA, 1, NA); none <- rep(NA, n)
  last_obs <- list(ctrl = list(Лечение = none, Контроль = early),
                   trt = list(Лечение = early, Контроль = none))
  out <- list()
  for (g in names(slopes)) for (i in seq_len(n)) {
    y <- 50 + rnorm(1, 0, 2) + (slopes[[g]] + rnorm(1, 0, .3)) * visits + rnorm(length(visits), 0, .7)
    row <- data.frame(group = g, id = i, visit = visits, month = 3 * visits, y = y)
    for (sc in names(last_obs)) {
      k <- last_obs[[sc]][[g]][i]
      observed <- if (is.na(k)) rep(TRUE, length(visits)) else visits <= k
      row[[paste0('observed_', sc)]] <- observed
      row[[paste0('locf_', sc)]] <- if (is.na(k)) y else ifelse(observed, y, y[k + 1])
    }
    out[[length(out) + 1]] <- row
  }
  do.call(rbind, out)
}

# Эффект «лечение − контроль» на последнем визите: по полным данным и после LOCF.
locf_effects <- function(d) {
  last <- d[d$month == max(d$month), ]
  eff <- function(col) unname(diff(tapply(last[[col]], last$group, mean)[c('Контроль', 'Лечение')]))
  c(full = eff('y'), ctrl = eff('locf_ctrl'), trt = eff('locf_trt'))
}

make_advanced <- function(ex) {
  b <- binary_mar(ex); demo <- ac_demo()
  list(binary = b, binary_ipw = binary_ipw(b), ipw = ipw_fit(ex$observed$MAR),
       ml = normal_ml(ex$observed$MAR), em = normal_em(ex$observed$MAR),
       category = category_example(), distinct = distinctness_example(),
       demo = demo, ac = ac_estimates(demo), locf = locf_trajectories())
}

export_advanced <- function(a, path = 'data') {
  save_csv <- function(x, f) write.csv(x, file.path(path, f), row.names = FALSE, na = 'NA')
  save_csv(a$binary$data, 'binary-mar.csv'); save_csv(a$binary$groups, 'binary-mar-estimates.csv')
  save_csv(data.frame(id = seq_along(a$ipw$pi), pi = a$ipw$pi), 'ipw-probabilities.csv')
  save_csv(data.frame(parameter = names(a$ml$parameters), IPW = a$ipw$parameters,
                     ML = a$ml$parameters, EM = a$em$parameters), 'advanced-estimates.csv')
  save_csv(a$em$history, 'em-trace.csv'); save_csv(a$category$data, 'missing-category.csv')
  save_csv(data.frame(parameter = c('intercept', 'BMI', 'male'), truth = a$category$truth,
    full = coef(a$category$full), missing_category = coef(a$category$missing)[1:3]),
    'missing-category-estimates.csv')
  save_csv(a$distinct$draws, 'distinctness-replicates.csv')
  save_csv(cbind(a$demo$observed, block = a$demo$block), 'ac-demo.csv')
  save_csv(a$locf, 'locf-example.csv')
  save_csv(data.frame(parameter = names(a$demo$beta), truth = a$demo$beta,
    lm_full = a$ac$lm_full, sem_full = a$ac$sem_full, lm_cc = a$ac$lm_cc, sem_ac = a$ac$sem_ac),
    'ac-demo-estimates.csv')
  # dput без хвостовых пробелов, чтобы файл проходил git diff --check.
  meta <- list(binary_seed = a$binary$seed, category_seed = a$category$seed,
            locf_seed = 20261157L,
            ac_demo = list(seed = a$demo$seed, n = nrow(a$demo$full), beta = unname(a$demo$beta),
              predictor_sd = 1, predictor_cor = a$demo$predictor_cor, error_sd = 1,
              missing = 'Y2, Y3, Y4: 25% each, disjoint rows, MCAR'),
            category = list(n=800, p_male=.5, BMI_means=c(female=29,male=25),
              BMI_sd=2, SBP_beta=c(95,1,12), SBP_error_sd=8, p_missing=.35),
            distinctness = a$distinct[c('B','seed','n','theta')])
  writeLines(sub(' +$', '', capture.output(dput(meta))), file.path(path, 'advanced-metadata.R'))
  invisible(a)
}
