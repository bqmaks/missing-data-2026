# Независимые репликации DGP, общие данные для сравниваемых методов.
# Это отдельный эксперимент, не повторный подбор seed иллюстративной выборки.
fast_stats <- function(x, y) {
  n <- length(y); mx <- mean(x); my <- mean(y)
  vx <- var(x); vy <- var(y); cv <- cov(x, y)
  b <- cv/vx; a <- cv/vy
  residual_var <- sum((y - my - b*(x-mx))^2)/(n-2)
  list(parameters = setNames(c(mx,my,sqrt(vx),sqrt(vy),cv/sqrt(vx*vy),
    my-b*mx,b,mx-a*my,a), names(population_parameters(example_config()))),
    se_mean = sqrt(vy/n), se_slope = sqrt(residual_var/((n-1)*vx)), n = n,
    error_sd = sqrt(residual_var))
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
      variants <- list(Full=full, CC=cc_report, mean=fast_stats(x,ym),
        regression=fast_stats(x,yr), stochastic=fast_stats(x,ys),
        IPW=list(parameters=ipw$parameters, se_mean=NA_real_,se_slope=NA_real_,n=sum(ok)),
        ML=list(parameters=normal_ml(d)$parameters,se_mean=NA_real_,se_slope=NA_real_,n=cfg$n))
      for (method in names(variants)) {
        v <- variants[[method]]; pos <- pos+1L
        records[[pos]] <- data.frame(replication=rep,scenario=scenario,method=method,
          as.list(v$parameters), se_mean=v$se_mean,se_slope=v$se_slope,n=v$n,
          n_obs=sum(ok), max_weight=if(method=='IPW') max(ipw$w) else NA_real_,
          ess=if(method=='IPW') ipw$ess else NA_real_)
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
  list(raw=raw, summary=do.call(rbind,summary), metadata=list(B=B,seed=seed,config=cfg,
    CI='t intervals: mean df=n-1, slope df=n-2; ordinary complete-data SE after imputation',
    IPW='normalized weights, estimated logistic response model; no IPW/ML CIs in this simulation',
    RNG=RNGkind(),R=R.version.string))
}

export_simulation <- function(sim,path='data') {
  write.csv(sim$summary,file.path(path,'simulation-summary.csv'),row.names=FALSE,na='NA')
  con <- gzfile(file.path(path,'simulation-replicates.csv.gz'),'wt')
  tryCatch(write.csv(sim$raw,con,row.names=FALSE,na='NA'),finally=close(con))
  dput(sim$metadata,file.path(path,'simulation-metadata.R'))
}
