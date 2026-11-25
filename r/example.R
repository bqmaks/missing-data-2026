# Воспроизводимый пример, сохранённый в missing_data_combined_v2.pptx. Только base R.
# X = BMI (ИМТ); Y = SBP (САД). M = 1 означает пропуск только в SBP.
example_config <- function() {
  list(n = 1000L, mu = c(BMI = 27, SBP = 135),
       sigma = c(BMI = 4, SBP = 15), rho = 0.6,
       seed_data = 20261124L,
       seed_missing = c(MCAR = 20261125L, MAR = 20261126L, MNAR = 20261127L),
       seed_impute = c(MCAR = 20261135L, MAR = 20261136L, MNAR = 20261137L),
       p_mcar = 0.35, mar = c(intercept = -13, BMI = 0.45),
       mnar = c(intercept = -17.1, SBP = 0.12))
}

population_parameters <- function(cfg) {
  b <- cfg$rho * cfg$sigma['SBP'] / cfg$sigma['BMI']
  a <- cfg$rho * cfg$sigma['BMI'] / cfg$sigma['SBP']
  setNames(c(cfg$mu, cfg$sigma, cfg$rho,
             cfg$mu['SBP'] - b * cfg$mu['BMI'], b,
             cfg$mu['BMI'] - a * cfg$mu['SBP'], a),
           c('mean_BMI', 'mean_SBP', 'sd_BMI', 'sd_SBP', 'correlation',
             'intercept_SBP', 'slope_SBP', 'intercept_BMI', 'slope_BMI'))
}

estimate_pairs <- function(d) {
  d <- d[complete.cases(d[c('BMI', 'SBP')]), , drop = FALSE]
  forward <- coef(lm(SBP ~ BMI, data = d))
  reverse <- coef(lm(BMI ~ SBP, data = d))
  setNames(c(mean(d$BMI), mean(d$SBP), sd(d$BMI), sd(d$SBP),
             cor(d$BMI, d$SBP), forward, reverse),
           c('mean_BMI', 'mean_SBP', 'sd_BMI', 'sd_SBP', 'correlation',
             'intercept_SBP', 'slope_SBP', 'intercept_BMI', 'slope_BMI'))
}

pairwise_example <- function() {
  # Три блока по 4 строки: в каждом наблюдаются только две переменные.
  z <- c(-1.5, -0.5, 0.5, 1.5)
  d <- data.frame(Y1 = c(z, z, rep(NA_real_, 4)),
                  Y2 = c(z, rep(NA_real_, 4), z),
                  Y3 = c(rep(NA_real_, 4), z, -z))
  r <- cor(d, use = 'pairwise.complete.obs')
  list(data = d, correlation = r,
       n_pairs = crossprod(!is.na(as.matrix(d))),
       eigenvalues = eigen(r, symmetric = TRUE, only.values = TRUE)$values)
}

make_example <- function(cfg = example_config()) {
  RNGkind('Mersenne-Twister', 'Inversion', 'Rejection')
  set.seed(cfg$seed_data)
  z1 <- rnorm(cfg$n)
  z2 <- rnorm(cfg$n)
  complete <- data.frame(
    id = seq_len(cfg$n),
    BMI = cfg$mu['BMI'] + cfg$sigma['BMI'] * z1,
    SBP = cfg$mu['SBP'] + cfg$sigma['SBP'] *
      (cfg$rho * z1 + sqrt(1 - cfg$rho^2) * z2))
  probability <- list(
    MCAR = rep(cfg$p_mcar, cfg$n),
    MAR = plogis(cfg$mar[1] + cfg$mar[2] * complete$BMI),
    MNAR = plogis(cfg$mnar[1] + cfg$mnar[2] * complete$SBP))
  observed <- masks <- completed <- list()
  estimates <- data.frame(scenario = 'Full', method = 'Full', n = cfg$n,
                          n_obs = cfg$n, missing_fraction = 0,
                          as.list(estimate_pairs(complete)), check.names = FALSE)
  for (mechanism in names(probability)) {
    set.seed(cfg$seed_missing[[mechanism]])
    m <- rbinom(cfg$n, 1, probability[[mechanism]]) == 1L
    d <- complete
    d$SBP[m] <- NA_real_
    fit <- lm(SBP ~ BMI, data = d)
    prediction <- as.numeric(predict(fit, newdata = d[m, , drop = FALSE]))
    mean_fill <- regression_fill <- stochastic_fill <- d
    mean_fill$SBP[m] <- mean(d$SBP, na.rm = TRUE)
    regression_fill$SBP[m] <- prediction
    set.seed(cfg$seed_impute[[mechanism]])
    stochastic_fill$SBP[m] <- prediction + rnorm(sum(m), sd = sigma(fit))
    observed[[mechanism]] <- d
    masks[[mechanism]] <- m
    completed[[mechanism]] <- list(mean = mean_fill, regression = regression_fill,
                                   stochastic = stochastic_fill)
    variants <- c(list(CC = d[!m, , drop = FALSE]), completed[[mechanism]])
    for (method in names(variants)) {
      estimates <- rbind(estimates, data.frame(
        scenario = mechanism, method = method, n = nrow(variants[[method]]),
        n_obs = sum(!m), missing_fraction = mean(m),
        as.list(estimate_pairs(variants[[method]])), check.names = FALSE))
    }
  }
  list(config = cfg, truth = population_parameters(cfg), complete = complete,
       probabilities = probability, masks = masks, observed = observed,
       completed = completed, estimates = estimates, pairwise = pairwise_example())
}

export_example <- function(ex, path = 'data') {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  write_csv <- function(x, filename) {
    write.csv(x, file.path(path, filename), row.names = FALSE, na = 'NA',
              fileEncoding = 'UTF-8')
  }
  write_csv(ex$complete, 'complete.csv')
  observed <- ex$complete[c('id', 'BMI')]
  masks <- ex$complete['id']
  for (mechanism in names(ex$observed)) {
    observed[[paste0('SBP_', mechanism)]] <- ex$observed[[mechanism]]$SBP
    masks[[paste0('M_', mechanism)]] <- as.integer(ex$masks[[mechanism]])
    masks[[paste0('p_', mechanism)]] <- ex$probabilities[[mechanism]]
  }
  write_csv(observed, 'observed.csv')
  write_csv(masks, 'missingness.csv')
  write_csv(ex$estimates, 'estimates.csv')
  write_csv(data.frame(parameter = names(ex$truth), value = unname(ex$truth)), 'truth.csv')
  imputed <- do.call(rbind, lapply(names(ex$completed), function(scenario) {
    do.call(rbind, lapply(names(ex$completed[[scenario]]), function(method) {
      data.frame(scenario = scenario, method = method,
                 ex$completed[[scenario]][[method]], M = as.integer(ex$masks[[scenario]]))
    }))
  }))
  write_csv(imputed, 'imputed.csv')
  write_csv(ex$pairwise$data, 'pairwise-example.csv')
  dput(list(config = ex$config, RNG = RNGkind(), R = R.version.string),
       file = file.path(path, 'metadata.R'))
  invisible(ex)
}
