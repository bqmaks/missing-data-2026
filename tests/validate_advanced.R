source('r/example.R'); source('r/advanced.R'); source('r/simulation.R')
ex <- make_example(); a <- make_advanced(ex)
close <- function(x,y,tol=1e-6) stopifnot(max(abs(unname(x)-unname(y)))<tol)
# EM and an independent direct likelihood optimization must agree with factorization.
d <- ex$observed$MAR
objective <- function(p) {
  S <- matrix(c(exp(p[3]),tanh(p[5])*exp((p[3]+p[4])/2),
                tanh(p[5])*exp((p[3]+p[4])/2),exp(p[4])),2)
  -normal_loglik(d,p[1:2],S)
}
p0 <- c(a$ml$mu,log(diag(a$ml$S)),atanh(a$ml$parameters['correlation']))
opt <- optim(p0+c(.2,-.2,.01,-.01,.01),objective,method='BFGS',
             control=list(reltol=1e-12,maxit=1000))
stopifnot(opt$convergence==0)
close(opt$value,-normal_loglik(d,a$ml$mu,a$ml$S),1e-6)
close(a$em$parameters,a$ml$parameters,1e-6)
stopifnot(all(diff(a$em$history$loglik)>-1e-7))
close(a$ml$parameters[c('intercept_SBP','slope_SBP')],coef(lm(SBP~BMI,d)))
# IPW coefficients also equal independently weighted least squares in both directions.
ok <- !is.na(d$SBP)
close(a$ipw$parameters[c('intercept_SBP','slope_SBP')],coef(lm(SBP~BMI,d[ok,],weights=a$ipw$w)))
close(a$ipw$parameters[c('intercept_BMI','slope_BMI')],coef(lm(BMI~SBP,d[ok,],weights=a$ipw$w)))
close(a$ipw$parameters['mean_SBP'],weighted.mean(d$SBP[ok],a$ipw$w))
stopifnot(a$ipw$ess<=sum(ok),all(a$ipw$pi>0 & a$ipw$pi<1))
# Group true means integrate back to the DGP marginal mean.
prob <- pnorm((30-27)/4)
close(sum(a$binary$groups$truth*c(prob,1-prob)),135)
stopifnot(identical(a$category$data,category_example()$data),
          all(is.finite(a$distinct$draws)))
close(a$category$data$SBP[1:3],category_example()$data$SBP[1:3])
# Recompute published simulation coverage and bias from retained replicate data.
s <- read.csv('data/simulation-summary.csv')
r <- read.csv(gzfile('data/simulation-replicates.csv.gz'))
stopifnot(all(s$B==3000),nrow(r)==3000*3*7,all(is.finite(r$mean_SBP)))
for(i in seq_len(nrow(s))) {
  row <- s[i,];dd <- r[r$scenario==row$scenario & r$method==row$method,]
  stopifnot(nrow(dd)==3000,all(sort(dd$replication)==1:3000))
  v <- dd[[row$parameter]]
  close(mean(v)-row$truth,row$bias,1e-10)
  close(sd(v)/sqrt(3000),row$mcse_mean,1e-10)
  if(!is.na(row$coverage)) {
    is_mean <- row$parameter=='mean_SBP'
    se <- if(is_mean) dd$se_mean else dd$se_slope
    covered <- abs(v-row$truth)<=qt(.975,dd$n-if(is_mean) 1 else 2)*se
    close(mean(covered),row$coverage,1e-10)
    close(sqrt(mean(covered)*(1-mean(covered))/3000),row$mcse_coverage,1e-10)
  }
}
cat('OK: ML/EM, direct likelihood, IPW, group truth, new examples and 189 simulation summaries.\n')
