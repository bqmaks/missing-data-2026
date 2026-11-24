# Единственный расчёт опубликованных чисел. Запуск из корня проекта.
source("r/00_dgp.R")
need_packages(c("MASS", "mice", "jsonlite", "ggplot2"))
dir.create("static/data", recursive = TRUE, showWarnings = FALSE)
dir.create("assets/generated", recursive = TRUE, showWarnings = FALSE)
meta <- function(seed, S = 1L, n = DGP$n) list(seed = seed, S = S, n = n,
  date = as.character(Sys.Date()), R = R.version.string,
  packages = vapply(c("MASS", "mice", "jsonlite", "ggplot2"), function(p)
    as.character(packageVersion(p)), character(1)),
  session = capture.output(sessionInfo()))
write_json <- function(x, path) jsonlite::write_json(x, paste0("static/data/", path),
  pretty = TRUE, auto_unbox = TRUE, digits = 12, na = "null")
write_json(list(meta = meta(202601L), constants = DGP, truth = as.list(TRUTH)), "dgp.json")

set.seed(202601)
full <- make_complete()
samples <- lapply(c("MCAR", "MAR", "MNAR"), function(mech) make_missing(full, mech))
names(samples) <- c("MCAR", "MAR", "MNAR")
sample_metrics <- lapply(samples, function(d) analyse_methods(d, full))
write_json(list(meta = c(meta(202601L), list(selection = "Independent seeded sample; no selection by results")),
  full = full, masks = lapply(samples, function(d) is.na(d$bmi)),
  estimates = sample_metrics,
  completed = attr(sample_metrics$MAR, "completed")), "sample500.json")

cat("Part I: 2000 replications, three mechanisms ...\n")
sim <- run_simulation(S = 2000L)
write_json(list(meta = meta(sim$seed, sim$S, sim$n), summary = sim$summary,
  intervals = sim$raw[sim$raw$replicate <= 100, ]), "sim_part1.json")
cat("MI: 2000 replications, proper/improper, m=5/20 ...\n")
mi <- run_mi_coverage(S = 2000L)
write_json(list(meta = meta(mi$seed, mi$S, mi$n), summary = mi$summary,
  intervals = mi$raw[mi$raw$replicate <= 100, ]), "mi_coverage.json")

# Все графики ниже построены из тех же объектов, что записаны в JSON.
library(ggplot2)
theme_set(theme_minimal(base_size = 18) + theme(panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"), legend.position = "bottom"))
save_plot <- function(p, name, width = 11, height = 5) {
  ggsave(paste0("assets/generated/", name, ".png"), p, width = width,
         height = height, dpi = 140, bg = "white")
}
cloud <- do.call(rbind, lapply(names(samples), function(mech) {
  d <- full; d$mechanism <- mech
  d$status <- ifelse(is.na(samples[[mech]]$bmi), "Пропущено", "Наблюдается")
  d
}))
p <- ggplot(cloud, aes(sbp, bmi, color = status, shape = status)) +
  geom_point(alpha = .65, size = 1.8) +
  geom_hline(yintercept = DGP$mu2, linetype = 2, color = "#1e293b") +
  scale_color_manual(values = c("Наблюдается" = "#0072B2", "Пропущено" = "#D55E00")) +
  scale_shape_manual(values = c("Наблюдается" = 16, "Пропущено" = 1)) +
  labs(x = "САД, мм рт. ст.", y = "ИМТ, кг/м²", color = NULL, shape = NULL)
save_plot(p + subset(cloud, mechanism == "MAR"), "cohort")
save_plot(p + facet_wrap(~ mechanism), "mechanisms", 14, 5)

completed <- attr(sample_metrics$MAR, "completed")
imp_cloud <- do.call(rbind, lapply(names(completed), function(method) {
  data.frame(sbp = full$sbp, bmi = completed[[method]], method,
    status = ifelse(is.na(samples$MAR$bmi), "Импутировано", "Наблюдается"))
}))
save_plot(ggplot(imp_cloud, aes(sbp, bmi, color = status, shape = status)) +
  geom_point(alpha = .65, size = 1.7) + facet_wrap(~ method) +
  scale_color_manual(values = c("Наблюдается" = "#0072B2", "Импутировано" = "#D55E00")) +
  scale_shape_manual(values = c("Наблюдается" = 16, "Импутировано" = 17)) +
  labs(x = "САД, мм рт. ст.", y = "ИМТ, кг/м²", color = NULL, shape = NULL), "single-imputation", 14, 5)

intervals <- subset(sim$raw, mechanism == "MAR" & replicate <= 100)
intervals$method <- factor(intervals$method, c("Full", "CCA", "Mean", "Reg", "StochReg", "MLE"))
save_plot(ggplot(intervals, aes(x = mu2, y = replicate, color = cover_mu2)) +
  geom_segment(aes(x = mu2 - qnorm(.975) * se_mu2, xend = mu2 + qnorm(.975) * se_mu2,
                   yend = replicate), linewidth = .35) +
  geom_point(aes(shape = cover_mu2), size = .8) +
  geom_vline(xintercept = DGP$mu2, linetype = 2) + facet_wrap(~ method, nrow = 1) +
  scale_color_manual(values = c("FALSE" = "#D55E00", "TRUE" = "#0072B2")) +
  labs(x = "Средний ИМТ и 95% ДИ, кг/м²", y = "Репликация", color = "Содержит истину", shape = "Содержит истину"), "intervals", 14, 6)
save_plot(ggplot(mi$summary, aes(coverage, method, color = term, shape = term)) +
  geom_vline(xintercept = .95, linetype = 2) +
  geom_errorbar(aes(xmin = coverage - 1.96 * mcse_coverage,
                    xmax = coverage + 1.96 * mcse_coverage), orientation = "y", width = .15) +
  geom_point(size = 3) + facet_wrap(~ term) +
  labs(x = "Покрытие (усы: ±1.96 MCSE)", y = NULL, color = "Параметр", shape = "Параметр"), "mi-coverage")
cat("Saved verified data and figures.\n")
