# Проверяем сохранённые данные, независимые расчётные тождества и общий DGP.
source('r/example.R')
ex <- make_example()
again <- make_example()
stopifnot(identical(ex, again), nrow(ex$complete) == 1000L,
          identical(ex$complete$id, seq_len(1000L)), !anyNA(ex$complete))
expected_truth <- c(27, 135, 4, 15, .6, 74.25, 2.25, 5.4, .16)
stopifnot(isTRUE(all.equal(unname(ex$truth), expected_truth)))

close <- function(x, y) stopifnot(isTRUE(all.equal(unname(x), unname(y), tolerance = 1e-11)))
for (s in names(ex$observed)) {
  m <- ex$masks[[s]]
  d <- ex$observed[[s]]
  stopifnot(identical(d$BMI, ex$complete$BMI), identical(is.na(d$SBP), m),
            identical(d$SBP[!m], ex$complete$SBP[!m]))
  p <- switch(s, MCAR = rep(.35, 1000), MAR = plogis(-13 + .45*d$BMI),
              MNAR = plogis(-17.1 + .12*ex$complete$SBP))
  close(ex$probabilities[[s]], p)
  cc <- d[!m, ]
  for (method in names(ex$completed[[s]])) {
    filled <- ex$completed[[s]][[method]]
    stopifnot(!anyNA(filled), identical(filled$BMI, ex$complete$BMI),
              identical(filled$SBP[!m], cc$SBP))
  }
  mean_data <- ex$completed[[s]]$mean
  close(mean(mean_data$SBP), mean(cc$SBP))
  close(var(mean_data$SBP), var(cc$SBP)*(nrow(cc)-1)/(nrow(d)-1))
  close(coef(lm(SBP~BMI, ex$completed[[s]]$regression)), coef(lm(SBP~BMI, cc)))
}

# Все 13 строк оценок проверяем по экспортированным CSV, через ковариационные
# тождества, независимо от estimate_pairs() и lm().
est <- read.csv('data/estimates.csv')
full <- read.csv('data/complete.csv')
obs <- read.csv('data/observed.csv')
imp <- read.csv('data/imputed.csv')
stopifnot(nrow(est) == 13L, nrow(imp) == 9000L)
close(as.matrix(full), as.matrix(ex$complete))
for (i in seq_len(nrow(est))) {
  row <- est[i, ]
  if (row$method == 'Full') d <- full
  else if (row$method == 'CC') {
    d <- data.frame(BMI = obs$BMI, SBP = obs[[paste0('SBP_', row$scenario)]])
    d <- d[complete.cases(d), ]
  } else d <- imp[imp$scenario == row$scenario & imp$method == row$method, ]
  mx <- mean(d$BMI); my <- mean(d$SBP)
  vx <- var(d$BMI); vy <- var(d$SBP); cv <- cov(d$BMI, d$SBP)
  values <- c(mx, my, sqrt(vx), sqrt(vy), cv/sqrt(vx*vy),
              my-cv/vx*mx, cv/vx, mx-cv/vy*my, cv/vy)
  close(as.numeric(row[names(ex$truth)]), values)
  stopifnot(row$n == nrow(d))
}
pairwise <- read.csv('data/pairwise-example.csv')
close(cor(pairwise, use='pairwise.complete.obs'), ex$pairwise$correlation)
close(sort(ex$pairwise$eigenvalues), c(-1,2,2))
stopifnot(!any(complete.cases(pairwise)), all(ex$pairwise$n_pairs[lower.tri(ex$pairwise$n_pairs)] == 4))
cat('OK: воспроизводимость; DGP и механизмы; неизменность наблюдаемых значений;\n',
    'все 13 строк оценок из CSV; тождества импутации; пример pairwise.\n', sep='')
