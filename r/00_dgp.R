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
