# Выполнять после полного запуска занятий 2 и 3.
need <- c("outputs/part2-results.rds", "outputs/part3-results.rds")
if (!all(file.exists(need))) stop("Сначала выполните практикум занятий 2 и 3")
source("r/00_dgp.R")
need_packages(c("jsonlite", "ggplot2", "mice"))
p2 <- readRDS(need[1]); p3 <- readRDS(need[2])
write <- function(x, name) jsonlite::write_json(x, paste0("static/data/", name),
  pretty = TRUE, auto_unbox = TRUE, digits = 12, na = "null")
write(list(meta = list(seed = 202603, n = nrow(p2$cohort), m = p2$imp$m,
  iterations = p2$imp$iteration, source = "practice/practice_part2.R",
  session = capture.output(p2$session)),
  missing_fraction = as.list(colMeans(is.na(p2$cohort))),
  pooled = summary(p2$pooled, conf.int = TRUE),
  observed_glucose = na.omit(p2$cohort$glucose),
  norm = as.numeric(as.matrix(p2$imp_norm$imp$glucose)),
  pmm = as.numeric(as.matrix(p2$imp$imp$glucose))), "mice_demo.json")
write(list(meta = list(seed_nonlinear = 3001, seed_survival = 3002,
  seed_weighted = 3004, seed_delta = 3086, source = "practice/practice_part3.R",
  seed_smc_nonlinear = 63, seed_smc_survival = 72, settings = p3$settings,
  session = capture.output(p3$session),
  description = "Single datasets, not a coverage simulation"),
  diagnostics = p3$diagnostics,
  nonlinear = p3$nonlinear, survival = p3$survival, weighted = p3$weighted,
  delta = p3$delta), "part3.json")
library(ggplot2)
theme_set(theme_minimal(base_size = 18))
glucose <- data.frame(value = c(na.omit(p2$cohort$glucose),
  as.numeric(as.matrix(p2$imp_norm$imp$glucose)), as.numeric(as.matrix(p2$imp$imp$glucose))),
  method = rep(c("Наблюдается", "norm", "pmm"), c(sum(!is.na(p2$cohort$glucose)),
    length(as.matrix(p2$imp_norm$imp$glucose)), length(as.matrix(p2$imp$imp$glucose)))))
p <- ggplot(glucose, aes(value, color = method, linetype = method)) + geom_density(linewidth = 1) +
  scale_color_manual(values = c("Наблюдается"="#1e293b", norm="#D55E00", pmm="#0072B2")) +
  labs(x = "Глюкоза, ммоль/л", y = "Плотность", color = NULL, linetype = NULL) +
  theme(legend.position="bottom")
ggsave("assets/generated/glucose.png", p, width = 11, height = 5, dpi = 140, bg = "white")
p <- ggplot(p3$nonlinear, aes(estimate, reorder(method, estimate))) +
  geom_vline(xintercept = .1, linetype = 2) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = .2, color="#0072B2") +
  geom_point(size=3, color="#0072B2") +
  labs(x = "Коэффициент при квадрате центрированного ИМТ", y = NULL)
ggsave("assets/generated/nonlinear.png", p, width = 11, height = 5, dpi = 140, bg = "white")
cat("Exported executed practice examples; no coverage claims for single datasets.\n")
