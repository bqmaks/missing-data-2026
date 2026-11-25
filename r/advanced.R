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
  set.seed(seed); M <- rbinom(nrow(d), 1, ifelse(obese, .60, .25)) == 1
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

distinctness_example <- function(B = 3000L, seed = 20261152L, n = 100L, theta = .3) {
  set.seed(seed)
  draws <- replicate(B, {
    y <- rbinom(n, 1, theta); C <- rbinom(n, 1, theta)
    no <- sum(C); successes <- sum(y[C == 1])
    c(observed = if (no) successes/no else NA_real_, joint = (no+successes)/(n+no))
  })
  list(draws = t(draws), B = B, seed = seed, n = n, theta = theta)
}

make_advanced <- function(ex) {
  list(binary = binary_mar(ex), ipw = ipw_fit(ex$observed$MAR),
       ml = normal_ml(ex$observed$MAR), em = normal_em(ex$observed$MAR),
       category = category_example(), distinct = distinctness_example())
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
  dput(list(binary_seed = a$binary$seed, category_seed = a$category$seed,
            category = list(n=800, p_male=.5, BMI_means=c(female=29,male=25),
              BMI_sd=2, SBP_beta=c(95,1,12), SBP_error_sd=8, p_missing=.35),
            distinctness = a$distinct[c('B','seed','n','theta')]),
       file.path(path, 'advanced-metadata.R'))
  invisible(a)
}
