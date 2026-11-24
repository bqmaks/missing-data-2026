# Проверки статистических расчётов; запуск из корня проекта.
source("r/00_dgp.R")
need_packages(c("MASS", "mice", "jsonlite"))
set.seed(1701)
full <- make_complete()
observed <- make_missing(full)
res <- analyse_methods(observed, full)
obs <- !is.na(observed$bmi)
fit <- lm(bmi ~ sbp, observed)
cc <- res[res$method == "CCA", ]
mle <- res[res$method == "MLE", ]
stopifnot(abs(cc$b1 - coef(fit)[2]) < 1e-12,
          abs(cc$se_b1 - sqrt(vcov(fit)[2,2])) < 1e-12,
          abs(cc$b1 - mle$b1) < 1e-12,
          abs(res$g1[res$method == "Mean"] - cc$g1) < 1e-12,
          abs(res$mu2[res$method == "Reg"] - mle$mu2) < 1e-12)

set.seed(1702)
completed <- normal_imputations(observed)
stopifnot(all(vapply(completed, function(d)
  identical(d$bmi[obs], observed$bmi[obs]), logical(1))))
ours <- pool_normal(completed)
reference <- summary(mice::pool(lapply(completed, function(d) lm(bmi ~ sbp, d))), conf.int = TRUE)
b1 <- reference[reference$term == "sbp", ]; mine <- ours[ours$term == "b1", ]
stopifnot(abs(mine$estimate - b1$estimate) < 1e-10,
          abs(mine$se - b1$std.error) < 1e-10,
          abs(mine$lo - b1$conf.low) < 1e-10,
          abs(mine$hi - b1$conf.high) < 1e-10)

part1 <- jsonlite::fromJSON("static/data/sim_part1.json")
part2 <- jsonlite::fromJSON("static/data/mi_coverage.json")
stopifnot(part1$meta$S >= 2000L, part2$meta$S >= 2000L,
          length(unique(part1$summary$method)) == 6L)
for (method in c("MLE", "StochReg")) for (term in c("mu2", "b1")) {
  # Base R: avoid nonstandard evaluation for method names.
  a <- part1$summary[part1$summary$mechanism == "MAR" & part1$summary$method == method, ]
  b <- part2$summary[part2$summary$method == method & part2$summary$term == term, ]
  stopifnot(abs(a[[paste0("coverage_", term)]] - b$coverage) < 1e-12)
}
expected <- with(part1$summary, sqrt(coverage_mu2 * (1-coverage_mu2) / S))
stopifnot(max(abs(expected - part1$summary$mcse_coverage_mu2)) < 1e-11)
cat("OK: OLS and Anderson identities; observed data preservation; pooling and intervals match mice; shared controls and MCSE.\n")
