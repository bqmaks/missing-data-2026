# Независимые репликации DGP, общие данные для сравниваемых методов.
# Это отдельный эксперимент, не повторный подбор seed иллюстративной выборки.
# Параметры, для которых строим 95% ДИ (D54): среднее неполной переменной, корреляция,
# наклоны обеих регрессий.
interval_parameters <- c('mean_SBP', 'correlation', 'slope_SBP', 'slope_BMI')

fast_stats <- function(x, y) {
  n <- length(y); mx <- mean(x); my <- mean(y)
  vx <- var(x); vy <- var(y); cv <- cov(x, y)
  b <- cv/vx; a <- cv/vy; r <- cv/sqrt(vx*vy)
  residual_var <- sum((y - my - b*(x-mx))^2)/(n-2)
  residual_var_x <- sum((x - mx - a*(y-my))^2)/(n-2)
  se_mean <- sqrt(vy/n); se_slope <- sqrt(residual_var/((n-1)*vx))
  se_slope_x <- sqrt(residual_var_x/((n-1)*vy))
  # Обычные интервалы анализа «как будто данные полные» (после импутации – по n строк):
  # t для среднего и наклонов, z-преобразование Фишера для корреляции.
  t1 <- qt(.975, n-1); t2 <- qt(.975, n-2); hz <- qnorm(.975)/sqrt(n-3)
  ci <- c(lo_mean_SBP = my - t1*se_mean, hi_mean_SBP = my + t1*se_mean,
          lo_correlation = tanh(atanh(r) - hz), hi_correlation = tanh(atanh(r) + hz),
          lo_slope_SBP = b - t2*se_slope, hi_slope_SBP = b + t2*se_slope,
          lo_slope_BMI = a - t2*se_slope_x, hi_slope_BMI = a + t2*se_slope_x)
  list(parameters = setNames(c(mx,my,sqrt(vx),sqrt(vy),r,
    my-b*mx,b,mx-a*my,a), names(population_parameters(example_config()))),
    se_mean = se_mean, se_slope = se_slope, n = n,
    error_sd = sqrt(residual_var), ci = ci)
}

# ДИ Вальда для ММП по наблюдаемой информации (D54). Параметризация
# φ = (μ_ИМТ, μ_САД, log σ_ИМТ, log σ_САД, atanh ρ); гессиан −log L численный (optimHess,
# без случайных чисел), функции параметров – дельта-методом; корреляция – на шкале atanh.
ml_intervals <- function(d, ml = normal_ml(d)) {
  S <- ml$S; s1 <- sqrt(S[1,1]); s2 <- sqrt(S[2,2]); rho <- S[1,2]/(s1*s2)
  phi <- c(ml$mu, log(s1), log(s2), atanh(rho))
  nll <- function(p) {
    r <- tanh(p[5]); a1 <- exp(p[3]); a2 <- exp(p[4])
    -normal_loglik(d, p[1:2], matrix(c(a1^2, r*a1*a2, r*a1*a2, a2^2), 2))
  }
  V <- solve(optimHess(phi, nll))
  z <- qnorm(.975)
  wald <- function(est, g) { se <- sqrt(drop(t(g) %*% V %*% g)); c(est - z*se, est + z*se) }
  b <- rho*s2/s1; a <- rho*s1/s2
  c(setNames(wald(ml$mu[2], c(0,1,0,0,0)), c('lo_mean_SBP', 'hi_mean_SBP')),
    lo_correlation = tanh(phi[5] - z*sqrt(V[5,5])), hi_correlation = tanh(phi[5] + z*sqrt(V[5,5])),
    setNames(wald(b, c(0,0,-b,b,(1-rho^2)*s2/s1)), c('lo_slope_SBP', 'hi_slope_SBP')),
    setNames(wald(a, c(0,0,a,-a,(1-rho^2)*s1/s2)), c('lo_slope_BMI', 'hi_slope_BMI')))
}

run_simulation <- function(B = 3000L, seed = 20261153L, cfg = example_config()) {
  stopifnot(B >= 2000)
  RNGkind('Mersenne-Twister', 'Inversion', 'Rejection'); set.seed(seed)
  records <- vector('list', B * 3 * 7); pos <- 0L
  for (rep in seq_len(B)) {
    z <- rnorm(cfg$n)
    x <- cfg$mu['BMI'] + cfg$sigma['BMI'] * z
    y <- cfg$mu['SBP'] + cfg$sigma['SBP'] *
      (cfg$rho*z + sqrt(1-cfg$rho^2)*rnorm(cfg$n))
    full <- fast_stats(x, y)
    ps <- list(MCAR=rep(cfg$p_mcar,cfg$n), MAR=plogis(cfg$mar[1]+cfg$mar[2]*x),
               MNAR=plogis(cfg$mnar[1]+cfg$mnar[2]*y))
    for (scenario in names(ps)) {
      M <- rbinom(cfg$n,1,ps[[scenario]]) == 1; ok <- !M
      cc <- fast_stats(x[ok],y[ok])
      # Полные случаи по переменным (D44): ИМТ наблюдается полностью, поэтому μ_X и σ_X
      # считаются по всем n; остальное – по полным парам. Генератор случайных чисел не задействован.
      cc_report <- cc; cc_report$parameters[c('mean_BMI','sd_BMI')] <- c(mean(x), sd(x))
      pred <- cc$parameters['intercept_SBP'] + cc$parameters['slope_SBP']*x[M]
      ym <- yr <- ys <- y
      ym[M] <- mean(y[ok]); yr[M] <- pred
      ys[M] <- pred + rnorm(sum(M),sd=cc$error_sd)
      d <- data.frame(BMI=x, SBP=ifelse(M,NA,y))
      ipw <- ipw_fit(d)
      ml <- normal_ml(d)
      no_ci <- setNames(rep(NA_real_, 8), names(full$ci))
      variants <- list(Full=full, CC=cc_report, mean=fast_stats(x,ym),
        regression=fast_stats(x,yr), stochastic=fast_stats(x,ys),
        IPW=list(parameters=ipw$parameters, se_mean=NA_real_,se_slope=NA_real_,n=sum(ok), ci=no_ci),
        ML=list(parameters=ml$parameters,se_mean=NA_real_,se_slope=NA_real_,n=cfg$n,
                ci=ml_intervals(d, ml)))
      for (method in names(variants)) {
        v <- variants[[method]]; pos <- pos+1L
        records[[pos]] <- data.frame(replication=rep,scenario=scenario,method=method,
          as.list(v$parameters), se_mean=v$se_mean,se_slope=v$se_slope,n=v$n,
          n_obs=sum(ok), max_weight=if(method=='IPW') max(ipw$w) else NA_real_,
          ess=if(method=='IPW') ipw$ess else NA_real_, as.list(v$ci))
      }
    }
    if (rep %% 500 == 0) message('Monte Carlo: ',rep,' / ',B)
  }
  raw <- do.call(rbind, records)
  truth <- population_parameters(cfg)
  summary <- list(); k <- 0L
  for (s in unique(raw$scenario)) for (m in unique(raw$method)) {
    d <- raw[raw$scenario==s & raw$method==m,]
    for (p in names(truth)) {
      k <- k+1L; v <- d[[p]]
      se <- if (p=='mean_SBP') d$se_mean else if(p=='slope_SBP') d$se_slope else rep(NA_real_,B)
      crit <- if (p=='mean_SBP') qt(.975,d$n-1) else qt(.975,d$n-2)
      covered <- abs(v-truth[p]) <= crit*se
      coverage <- if(all(is.na(covered))) NA_real_ else mean(covered)
      summary[[k]] <- data.frame(scenario=s,method=m,parameter=p,B=B,truth=truth[p],
        mean=mean(v),bias=mean(v)-truth[p],empirical_se=sd(v),
        mcse_mean=sd(v)/sqrt(B),mean_model_se=if(all(is.na(se))) NA_real_ else mean(se),
        coverage=coverage,mcse_coverage=sqrt(coverage*(1-coverage)/B))
    }
  }
  list(raw=raw, summary=do.call(rbind,summary), intervals=interval_summary(raw, truth, B),
    metadata=list(B=B,seed=seed,config=cfg,
    CI='t intervals: mean df=n-1, slope df=n-2; ordinary complete-data SE after imputation',
    intervals=paste('95% CI for mean_SBP, correlation, slope_SBP, slope_BMI (simulation-intervals.csv):',
      'Full/CC/mean/regression/stochastic – t intervals with ordinary complete-data SE',
      '(df n-1 for mean, n-2 for slopes), Fisher z for correlation, n = analysed rows;',
      'ML – Wald intervals from observed information (numerical Hessian, delta method),',
      'correlation on atanh scale; no RNG used for intervals'),
    IPW='normalized weights, estimated logistic response model; no IPW CIs in this simulation',
    RNG=RNGkind(),R=R.version.string))
}

# Покрытие и средняя ширина 95% ДИ по репликациям (D54); MCSE покрытия – sqrt(p(1-p)/B),
# MCSE ширины – sd(ширины)/sqrt(B).
interval_summary <- function(raw, truth, B) {
  out <- list(); k <- 0L
  for (s in unique(raw$scenario)) for (m in unique(raw$method)) {
    d <- raw[raw$scenario==s & raw$method==m,]
    for (p in interval_parameters) {
      lo <- d[[paste0('lo_', p)]]; hi <- d[[paste0('hi_', p)]]
      if (all(is.na(lo))) next
      covered <- lo <= truth[p] & truth[p] <= hi; width <- hi - lo
      cov <- mean(covered); k <- k + 1L
      out[[k]] <- data.frame(scenario=s, method=m, parameter=p, B=B, truth=unname(truth[p]),
        coverage=cov, mcse_coverage=sqrt(cov*(1-cov)/B),
        mean_width=mean(width), mcse_width=sd(width)/sqrt(B))
    }
  }
  do.call(rbind, out)
}

export_simulation <- function(sim,path='data') {
  write.csv(sim$summary,file.path(path,'simulation-summary.csv'),row.names=FALSE,na='NA')
  write.csv(sim$intervals,file.path(path,'simulation-intervals.csv'),row.names=FALSE,na='NA')
  con <- gzfile(file.path(path,'simulation-replicates.csv.gz'),'wt')
  tryCatch(write.csv(sim$raw,con,row.names=FALSE,na='NA'),finally=close(con))
  # dput без хвостовых пробелов, чтобы файл проходил git diff --check.
  writeLines(sub(' +$', '', capture.output(dput(sim$metadata))), file.path(path,'simulation-metadata.R'))
}
