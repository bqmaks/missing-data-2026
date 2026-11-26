# Представление результатов; статистика и DGP находятся в example.R.
# Семантическая палитра mice (D21); те же значения в CSS и widgets/common.js.
course_colors <- c(observed = "#006CC2", missing = "#B61A51", imputed = "#B61A51")
# Истина на всех графиках – тонкая черная линия; оценки – в 2–3 раза толще, цветом (D37).
truth_color <- "#1A1A1A"
truth_lwd <- 1.2
estimate_lwd <- 3.2
fmt <- function(x, digits = 2) formatC(x, digits = digits, format = 'f')
estimate <- function(ex, scenario = 'Full', method = 'Full') {
  ex$estimates[ex$estimates$scenario == scenario & ex$estimates$method == method, ]
}
# Регрессия с названиями переменных: «74.25 + 2.25 × ИМТ».
# bold = c(свободный член, наклон): выделить смещенные коэффициенты (D40).
equation_named <- function(x, direction = 'SBP', bold = c(FALSE, FALSE)) {
  a <- x[[paste0('intercept_', direction)]]
  b <- x[[paste0('slope_', direction)]]
  strong <- function(s, on) if (on) paste0('<strong>', s, '</strong>') else s
  paste0(strong(sub('^-', '−', fmt(a)), bold[1]), if (b >= 0) ' + ' else ' − ',
         strong(fmt(abs(b), if (direction == 'SBP') 2 else 3), bold[2]),
         if (direction == 'SBP') ' × ИМТ' else ' × САД')
}

# Таблицы оценок в едином стиле (D36, D39): строки – параметры μ_X … E(ИМТ | САД),
# столбцы – истина θ (черная) и оценки θ̂ с индексом; цвет оценки – цвет данных:
# наблюдаемые (CD, CC, IPW, ML) синие, заполненные импутацией красные.
theta_tex <- function(s) paste0('\\(', s, '\\)')
theta_labels <- c(theta_tex('\\mu_{\\text{ИМТ}}'), theta_tex('\\sigma_{\\text{ИМТ}}'),
                  theta_tex('\\mu_{\\text{САД}}'), theta_tex('\\sigma_{\\text{САД}}'),
                  theta_tex('\\rho_{XY}'),
                  theta_tex('\\mathrm{E}(\\text{САД}\\mid\\text{ИМТ})'),
                  theta_tex('\\mathrm{E}(\\text{ИМТ}\\mid\\text{САД})'))
# flags – список из bias_flags(): пять логических значений для μ_X … ρ_XY
# и пары (свободный член, наклон) для двух регрессий.
theta_cells <- function(x, flags = NULL) {
  if (is.null(flags)) flags <- list(scalar = rep(FALSE, 5), sbp = c(FALSE, FALSE), bmi = c(FALSE, FALSE))
  scalar <- c(fmt(x[['mean_BMI']], 1), fmt(x[['sd_BMI']], 1), fmt(x[['mean_SBP']], 1),
              fmt(x[['sd_SBP']], 1), fmt(x[['correlation']], 3))
  scalar <- ifelse(flags$scalar, paste0('<strong>', scalar, '</strong>'), scalar)
  c(scalar, equation_named(x, 'SBP', flags$sbp), equation_named(x, 'BMI', flags$bmi))
}
# Смещенные оценки выделяются жирным (D40) по Monte Carlo (3000 репликаций,
# data/simulation-summary.csv): |смещение| > 10 MCSE. Структурные смещения в курсе
# составляют 41–870 MCSE, прочие – не больше 6 MCSE. В регрессиях выделяется только
# смещенный коэффициент. Одна выборка смещение не определяет.
bias_flags <- function(sim, method, scenario, threshold = 10) {
  if (is.null(sim)) return(NULL)
  biased <- function(p) {
    r <- sim[sim$method == method & sim$scenario == scenario & sim$parameter == p, ]
    stopifnot(nrow(r) == 1)
    abs(r$bias) > threshold * r$mcse_mean
  }
  list(scalar = vapply(c('mean_BMI', 'sd_BMI', 'mean_SBP', 'sd_SBP', 'correlation'), biased, logical(1)),
       sbp = c(biased('intercept_SBP'), biased('slope_SBP')),
       bmi = c(biased('intercept_BMI'), biased('slope_BMI')))
}
theta_column <- function(x, index, class = 'estimate-observed', flags = NULL) {
  list(head = theta_tex(paste0('\\boldsymbol{\\hat\\theta}_{\\mathbf{', index, '}}')),
       values = theta_cells(x, flags), class = class)
}
cc_column <- function(ex, scenario, sim = NULL)
  theta_column(estimate(ex, scenario, 'CC'), 'CC', flags = bias_flags(sim, 'CC', scenario))
# cd_color = FALSE (по умолчанию, D42): начиная со слайда MCAR оценки по полным данным
# черные – это опорные значения; синим θ̂_CD только на слайде «Данные для примера».
theta_table <- function(ex, extra = list(), rows = 1:7, cd_color = FALSE) {
  cols <- c(list(list(head = theta_tex('\\boldsymbol{\\theta}'), values = theta_cells(ex$truth),
                      class = 'truth'),
                 theta_column(estimate(ex), 'CD', if (cd_color) 'estimate-observed' else 'truth')), extra)
  # Сырой HTML-блок: иначе Pandoc разбирает текст таблицы как markdown и съедает \\(.
  cat('\n```{=html}\n<table class="estimates theta-table"><thead><tr><th>Параметры</th>',
      paste0('<th>', vapply(cols, `[[`, character(1), 'head'), '</th>', collapse = ''),
      '</tr></thead><tbody>', sep = '')
  for (i in rows) {
    cat('<tr><th scope="row">', theta_labels[i], '</th>',
        paste0('<td class="', vapply(cols, `[[`, character(1), 'class'), '">',
               vapply(cols, function(col) col$values[i], character(1)), '</td>', collapse = ''),
        '</tr>', sep = '')
  }
  cat('</tbody></table>\n```\n')
}
truth_cd_table <- function(ex) theta_table(ex, cd_color = TRUE)

latex_matrix <- function(x, prefix = '', colors = NULL, headers = NULL) {
  x <- as.matrix(x)
  cells <- matrix(as.character(x), nrow(x), ncol(x))
  if (!is.null(colors)) cells[] <- paste0('\\color{', as.vector(colors), '}{', as.vector(cells), '}')
  rows <- apply(cells, 1, paste, collapse = ' & ')
  if (is.null(headers)) {
    body <- paste0('\\begin{bmatrix}', paste(rows, collapse = ' \\\\ '), '\\end{bmatrix}')
  } else {
    header <- paste0('\\text{', headers, '}', collapse = ' & ')
    body <- paste0('\\left[\\begin{array}{', paste(rep('r', ncol(x)), collapse = ''), '}',
                   header, ' \\\\ \\hline ', paste(rows, collapse = ' \\\\ '), '\\end{array}\\right]')
  }
  cat('\n$$\n', prefix, body, '\n$$\n', sep = '')
}

notation_example <- function() {
  y <- rbind(c(54,1,138,86,29.1,5.4), c(61,0,149,92,28.3,5.8),
             c(72,1,152,90,27.4,6.1), c(48,0,124,78,25.4,4.7),
             c(59,1,142,88,30.2,5.9), c(45,0,128,82,26.8,4.9))
  m <- rbind(c(0,0,0,0,0,1),c(0,0,0,0,1,0),c(0,0,1,1,0,1),
             c(0,0,0,0,0,1),c(0,0,0,1,1,1),c(0,0,1,0,0,1))
  observed <- y
  observed[m == 1] <- NA
  list(y = y, m = m, observed = observed)
}

# Самостоятельные учебные схемы; 1 означает пропуск в соответствующей ячейке Y.
pattern_examples <- function() {
  list(
    Univariate = cbind(0, 0, 0, c(rep(0, 5), rep(1, 5))),
    Monotone = rbind(c(0,0,0,0), c(0,0,0,0), c(0,0,0,1), c(0,0,0,1),
                     c(0,0,1,1), c(0,0,1,1), c(0,0,1,1), c(0,1,1,1),
                     c(0,1,1,1), c(0,1,1,1)),
    General = rbind(c(0,0,0,0), c(0,0,0,0), c(0,0,0,1), c(0,0,0,1),
                   c(0,0,1,1), c(0,0,1,0), c(0,1,0,1), c(0,1,0,0),
                   c(1,1,0,1), c(1,0,1,0)),
    Underidentified = cbind(0, 0,
                         c(0,0,0,0,0,1,1,1,1,1),
                         c(1,1,1,1,0,0,0,0,0,0))
  )
}

# Плотная сетка по мотивам Figure 4.1 в Flexible Imputation of Missing Data.
# SVG сохраняет широкие ячейки и тонкие белые разделители при масштабировании.
pattern_panel <- function(M, title, col_labels = NULL, col_notes = NULL, footer_h = 0,
                          width = 344, height = 620) {
  stopifnot(all(M %in% c(0, 1)))
  header <- 52; inset <- 5; gap <- 4
  cw <- (width - 2 * inset + gap) / ncol(M)
  ch <- (height - header - footer_h - 2 * inset + gap) / nrow(M)
  cat('<svg class="pattern-panel" xmlns="http://www.w3.org/2000/svg" ',
      'viewBox="0 0 ', width, ' ', height, '" role="img" aria-label="', title,
      ': синий – наблюдаемое значение, красный – пропуск">\n', sep = '')
  # Без общей рамки и серой полосы заголовка: фон панели прозрачный.
  cat('<title>', title, '</title>\n',
      '<text class="pattern-title" x="', width / 2, '" y="36" text-anchor="middle">', title, '</text>\n', sep = '')
  x_mid <- inset + (seq_len(ncol(M)) - 1) * cw + (cw - gap) / 2
  # Необязательные подписи столбцов сверху и примечания (например, доля NA) снизу.
  if (!is.null(col_labels)) cat(sprintf('<text class="pattern-col" x="%.2f" y="40" text-anchor="middle">%s</text>\n',
                                        x_mid, col_labels), sep = '')
  if (!is.null(col_notes)) cat(sprintf('<text class="pattern-note" x="%.2f" y="%.2f" text-anchor="middle" fill="%s">%s</text>\n',
                                       x_mid, height - footer_h / 2 + 10, col_notes$color, col_notes$text), sep = '')
  for (i in seq_len(nrow(M))) for (j in seq_len(ncol(M))) {
    cat(sprintf('<rect class="pattern-cell" data-row="%d" data-col="%d" data-missing="%d" x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>\n',
                i, j, M[i,j], inset + (j - 1) * cw, header + inset + (i - 1) * ch,
                cw - gap, ch - gap, course_colors[if (M[i,j]) 'missing' else 'observed']))
  }
  cat('</svg>\n')
}

# Схема потери полных случаев (D47, D52): 20 строк × 4 переменные, по 5 пропусков
# в Y2, Y3, Y4 в разных строках; под столбцами – доля пропусков в самой схеме.
cc_loss_mask <- function() rbind(matrix(0, 5, 4), cbind(0, diag(3)[rep(1:3, each = 5), ]))
cc_loss_panel <- function(M = cc_loss_mask()) {
  share <- colMeans(M)
  pattern_panel(M, '', col_labels = paste0('Y', seq_len(ncol(M))),
                col_notes = list(text = paste0(round(100 * share), '%'),
                                 color = ifelse(share > 0, course_colors[['missing']], course_colors[['observed']])),
                footer_h = 50, width = 350, height = 430)
}

# Стратифицированный анализ (D46): плотности ИМТ и САД в строках, где САД наблюдается
# (синие) и пропущен (красные; для САД это скрытые значения, известные только в симуляции),
# тонкая черная – все 1000 наблюдений. Столбцы – механизмы, общая шкала в каждом ряду.
# Узкие поля (D52): панели крупнее, промежутки между ними меньше.
plot_strat_density <- function(ex) {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  full <- ex$complete; scen <- c('MCAR', 'MAR', 'MNAR')
  par(mfrow = c(2, 3), mar = c(3.5, 4.6, 1.9, .3), mgp = c(2.3, .65, 0), family = 'sans', cex = 1.05)
  rows <- list(list(x = full$BMI, lab = 'ИМТ (кг/м²)'), list(x = full$SBP, lab = 'САД (мм рт. ст.)'))
  for (r in seq_along(rows)) {
    x <- rows[[r]]$x
    dens <- lapply(scen, function(s) {
      m <- ex$masks[[s]]
      list(all = density(x), obs = density(x[!m]), mis = density(x[m]))
    })
    ymax <- 1.06 * max(unlist(lapply(dens, function(d) c(d$all$y, d$obs$y, d$mis$y))))
    xr <- range(unlist(lapply(dens, function(d) range(d$all$x))))
    for (k in seq_along(scen)) {
      d <- dens[[k]]
      plot(NA, xlim = xr, ylim = c(0, ymax), main = if (r == 1) scen[k] else '', font.main = 1,
           cex.main = 1.35, xlab = rows[[r]]$lab, ylab = '',
           bty = 'l', las = 1, yaxt = 'n', xaxs = 'i', yaxs = 'i')
      # Подпись оси Y дальше от делений: у плотности САД длинные числа (0.035).
      if (k == 1) mtext('Плотность', side = 2, line = 3.4, cex = 1.05)
      axis(2, las = 1, labels = k == 1)
      grid(col = '#eeeeee')
      lines(d$obs, col = course_colors[['observed']], lwd = estimate_lwd)
      lines(d$mis, col = course_colors[['missing']], lwd = estimate_lwd)
      lines(d$all, col = truth_color, lwd = truth_lwd)
    }
  }
}

# Оценки по полным случаям при трех механизмах в одной таблице (D52): θ̂_CC синие,
# жирным – смещенные по Monte Carlo (D40); подпись столбца – механизм.
cc_mechanism_columns <- function(ex, sim) lapply(c('MCAR', 'MAR', 'MNAR'), function(s) {
  col <- cc_column(ex, s, sim)
  col$head <- paste0(col$head, '<br>', s)
  col
})

# Коэффициенты демо-регрессии одной строкой (D52): lm и lavaan::sem по моментам
# полных данных; оценки по полным данным черные (D42).
ac_coef_row <- function(coefs) {
  f <- function(x) sub('^-', '−', fmt(x, 3))
  cat('\n```{=html}\n<table class="estimates ac-row"><thead><tr>',
      paste0('<th>', c('(Intercept)', 'Y2', 'Y3', 'Y4'), '</th>', collapse = ''),
      '</tr></thead><tbody><tr>', paste0('<td>', f(coefs), '</td>', collapse = ''),
      '</tr></tbody></table>\n```\n', sep = '')
}

# Бинарный MAR и IPW (D54, D58): САД по группам ИМТ; синие – наблюдаемые, красные крестики –
# скрытые значения. В группе тонкая черная черта – истинное среднее, толстая синяя –
# среднее полных случаев. Общие средние – треугольники у оси Y (не линии через рисунок):
# черный залитый – истина, синий контурный – CC, синий залитый – IPW.
plot_binary_ipw <- function(ex, adv) {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  d <- adv$binary$data; g <- adv$binary$groups
  par(mar = c(3, 4.8, .6, .6), mgp = c(3, .8, 0), family = 'sans', cex = 1.05)
  # Горизонтальный разброс точек – только для рисунка; состояние RNG сессии восстанавливаем.
  seed_state <- if (exists('.Random.seed', globalenv())) get('.Random.seed', globalenv()) else NULL
  on.exit(if (!is.null(seed_state)) assign('.Random.seed', seed_state, globalenv()), add = TRUE)
  set.seed(20261156L)
  jitter_x <- function(k) runif(k, -.13, .13)
  plot(NA, xlim = c(.3, 2.55), ylim = range(d$SBP) + c(-3, 3), xaxt = 'n', xlab = '',
       ylab = 'САД (мм рт. ст.)', bty = 'l', las = 1)
  axis(1, at = 1:2, labels = g$group, tick = FALSE, cex.axis = 1.1)
  grid(nx = NA, ny = NULL, col = '#eeeeee')
  for (k in 1:2) {
    idx <- d$obese == k - 1; obs <- idx & d$M == 0; mis <- idx & d$M == 1
    points(k - .2 + jitter_x(sum(obs)), d$SBP[obs], pch = 16, cex = .55,
           col = adjustcolor(course_colors[['observed']], .35))
    points(k + .2 + jitter_x(sum(mis)), d$SBP[mis], pch = 4, cex = .55,
           col = adjustcolor(course_colors[['missing']], .7))
    segments(k - .42, g$CC[k], k + .42, g$CC[k], col = course_colors[['observed']], lwd = estimate_lwd)
    segments(k - .42, g$truth[k], k + .42, g$truth[k], col = truth_color, lwd = truth_lwd)
  }
  # Треугольники острием к оси Y, сдвинуты по горизонтали, чтобы близкие значения не сливались.
  u <- par('usr'); w <- .055 * (u[2] - u[1]); h <- .018 * (u[4] - u[3])
  tri <- function(y, k, col, fill) polygon(u[1] + k * 1.15 * w + c(0, w, w), y + c(0, h, -h),
                                           col = fill, border = col, lwd = 1.6, xpd = NA)
  tri(ex$truth[['mean_SBP']], 0, truth_color, truth_color)
  tri(adv$binary$overall_CC, 1, course_colors[['observed']], 'white')
  tri(adv$binary_ipw$mean_SBP, 2, course_colors[['observed']], course_colors[['observed']])
}

# Покрытие и средняя ширина 95% ДИ (D54, D66) по data/simulation-intervals.csv:
# строки – методы, столбцы – параметры × (покрытие, %; ширина); без цветового выделения.
uncertainty_table <- function(iv, scenario) {
  methods <- c(Full = 'CD', CC = 'CC', mean = 'MI', regression = 'RI', stochastic = 'SRI', ML = 'ML')
  # Без цвета (D66): в таблицах ДИ методы и значения черные.
  cls <- c(Full = 'truth', CC = 'truth', mean = 'truth', regression = 'truth', stochastic = 'truth', ML = 'truth')
  pars <- c(mean_SBP = '\\mu_{\\text{САД}}', correlation = '\\rho_{XY}',
            slope_SBP = '\\beta_{\\text{САД}\\mid\\text{ИМТ}}', slope_BMI = '\\beta_{\\text{ИМТ}\\mid\\text{САД}}')
  digits <- c(mean_SBP = 2, correlation = 3, slope_SBP = 3, slope_BMI = 4)
  cat('\n```{=html}\n<table class="estimates uncertainty-table"><thead><tr><th rowspan="2">Метод</th>',
      paste0('<th colspan="2">', theta_tex(pars), '</th>', collapse = ''), '</tr><tr>',
      strrep('<th>Покрытие, %</th><th>Ширина</th>', length(pars)), '</tr></thead><tbody>', sep = '')
  for (m in names(methods)) {
    cells <- vapply(names(pars), function(p) {
      r <- iv[iv$scenario == scenario & iv$method == m & iv$parameter == p, ]
      stopifnot(nrow(r) == 1)
      paste0('<td class="', cls[[m]], '">', fmt(100 * r$coverage, 1), '</td><td class="', cls[[m]], '">',
             fmt(r$mean_width, digits[[p]]), '</td>')
    }, character(1))
    cat('<tr><th scope="row" class="', cls[[m]], '">',
        theta_tex(paste0('\\boldsymbol{\\hat\\theta}_{\\mathbf{', methods[[m]], '}}')), '</th>',
        paste(cells, collapse = ''), '</tr>', sep = '')
  }
  cat('</tbody></table>\n```\n')
}

# Плотность двумерного нормального распределения с параметрами сквозного примера.
# Цвет: от белого к синему (D21); линии – эллипсы с расстоянием Махаланобиса 1, 2, 3.
bivariate_density_grid <- function(cfg = example_config(), n_grid = 241, k = 3.5) {
  mu <- unname(cfg$mu); s <- unname(cfg$sigma); rho <- cfg$rho
  Sigma <- matrix(c(s[1]^2, rho * s[1] * s[2], rho * s[1] * s[2], s[2]^2), 2)
  y1 <- seq(mu[1] - k * s[1], mu[1] + k * s[1], length.out = n_grid)
  y2 <- seq(mu[2] - k * s[2], mu[2] + k * s[2], length.out = n_grid)
  P <- solve(Sigma)
  d1 <- outer(y1 - mu[1], rep(1, n_grid)); d2 <- outer(rep(1, n_grid), y2 - mu[2])
  q <- P[1, 1] * d1^2 + 2 * P[1, 2] * d1 * d2 + P[2, 2] * d2^2
  norm <- 2 * pi * sqrt(det(Sigma))
  list(y1 = y1, y2 = y2, density = exp(-q / 2) / norm, mu = mu, Sigma = Sigma,
       levels = exp(-(1:3)^2 / 2) / norm)
}

plot_bivariate_density <- function(cfg = example_config(),
                                   xlab = expression(italic(y)[1]),
                                   ylab = expression(italic(y)[2])) {
  g <- bivariate_density_grid(cfg)
  pal <- colorRampPalette(c('#FFFFFF', course_colors[['observed']]))(200)
  op <- par(mar = c(4.2, 4.4, 0.6, 0.8), mgp = c(2.7, 0.8, 0), pty = 's'); on.exit(par(op))
  image(g$y1, g$y2, g$density, col = pal, useRaster = TRUE, las = 1,
        xlab = xlab, ylab = ylab)
  contour(g$y1, g$y2, g$density, levels = g$levels, drawlabels = FALSE, add = TRUE,
          col = adjustcolor('#1A1A1A', 0.55), lwd = 1.2)
  points(g$mu[1], g$mu[2], pch = 3, cex = 1.4, lwd = 2, col = '#1A1A1A')
  box()
}

# params – оценки, по которым строятся линии и точка среднего (ММП: без заполненных значений).
scatter <- function(ex, scenario = 'Full', method = 'CC', title = '', means = FALSE, params = NULL) {
  full <- ex$complete
  palette <- course_colors
  plot(full$BMI, full$SBP, type = 'n', xlab = 'ИМТ (кг/м²)', ylab = 'САД (мм рт. ст.)',
       xlim = range(full$BMI) + c(-0.4, 0.4), ylim = range(full$SBP) + c(-3, 3),
       main = title, bty = 'l', las = 1, cex.main = 1.05)
  grid(col = '#eeeeee')
  if (scenario == 'Full') {
    d <- full
    points(d$BMI, d$SBP, pch = 16, cex = 0.65, col = adjustcolor(palette['observed'], .3))
  } else {
    m <- ex$masks[[scenario]]
    observed_only <- method %in% c('CC', 'ML')
    d <- if (observed_only) ex$observed[[scenario]][!m, ] else ex$completed[[scenario]][[method]]
    # При импутации показываем скрытую истину бледнее, подстановку поверх нее.
    points(full$BMI[m], full$SBP[m], pch = 4, cex = .6,
           col = adjustcolor(palette['missing'], if (observed_only) 1 else .5))
    points(full$BMI[!m], full$SBP[!m], pch = 16, cex = .65,
           col = adjustcolor(palette['observed'], if (observed_only) .5 else .3))
    if (!observed_only) points(d$BMI[m], d$SBP[m], pch = 17, cex = .65,
                              col = palette['imputed'])
  }
  # Rug-разметка по осям (D44, D60, D67): ИМТ наблюдается всегда – синяя; САД – синие
  # наблюдаемые значения и красные пропуски (на месте скрытых значений) на всех слайдах,
  # включая импутацию; риски одной длины, подстановки отдельным слоем не рисуются.
  rug_col <- function(key, alpha) adjustcolor(palette[[key]], alpha)
  rug(full$BMI, side = 1, ticksize = .025, lwd = .6, col = rug_col('observed', .35))
  if (scenario == 'Full') {
    rug(full$SBP, side = 2, ticksize = .025, lwd = .6, col = rug_col('observed', .35))
  } else {
    rug(full$SBP[!m], side = 2, ticksize = .025, lwd = .6, col = rug_col('observed', .35))
    # D67: только цветовое различие, риски одной длины, без второго слоя подстановок.
    rug(full$SBP[m], side = 2, ticksize = .025, lwd = .6, col = rug_col('missing', .55))
  }
  tr <- ex$truth
  # Оценки по показанным данным; обе регрессии на общих осях X=ИМТ, Y=САД.
  if (is.null(params)) {
    b <- coef(lm(SBP ~ BMI, d)); a <- coef(lm(BMI ~ SBP, d))
  } else {
    b <- unname(params[c('intercept_SBP', 'slope_SBP')]); a <- unname(params[c('intercept_BMI', 'slope_BMI')])
  }
  # Цвет оценки – цвет данных, по которым она получена: полные данные и полные случаи
  # синие, заполненные импутацией – красные (D21, D37, D39).
  est_col <- if (scenario == 'Full' || method %in% c('CC', 'ML')) course_colors[['observed']] else course_colors[['imputed']]
  # Оценки толстые, без различия двух регрессий и без легенды.
  abline(b, col = est_col, lwd = estimate_lwd)
  if (abs(a[2]) > 1e-10) abline(a = -a[1] / a[2], b = 1 / a[2], col = est_col, lwd = estimate_lwd)
  # Истина поверх оценок: тонкие черные прямые из параметров DGP (D37).
  abline(tr[['intercept_SBP']], tr[['slope_SBP']], col = truth_color, lwd = truth_lwd)
  abline(a = -tr[['intercept_BMI']] / tr[['slope_BMI']], b = 1 / tr[['slope_BMI']],
         col = truth_color, lwd = truth_lwd)
  if (means) {
    # Средние: точка и пунктир до осей; истина черная, оценка цветом своих данных.
    u <- par('usr')
    mark <- function(x, y, col) {
      segments(x, u[3], x, y, col = col, lty = 2, lwd = 1.2)
      segments(u[1], y, x, y, col = col, lty = 2, lwd = 1.2)
      points(x, y, pch = 16, cex = 1.5, col = col)
    }
    # Полные случаи по переменным (D44): среднее ИМТ – по всем значениям ИМТ.
    if (!is.null(params)) mark(params[['mean_BMI']], params[['mean_SBP']], est_col)
    else mark(if (scenario != 'Full' && method == 'CC') mean(full$BMI) else mean(d$BMI), mean(d$SBP), est_col)
    mark(tr[['mean_BMI']], tr[['mean_SBP']], truth_color)
  }
}

plot_example <- function(ex, scenario = 'Full', method = 'CC', panels = FALSE, square = FALSE,
                         means = FALSE, params = NULL) {
  old <- par(no.readonly = TRUE)
  on.exit(par(old))
  par(mar = c(4.2, 4.7, if (square && !panels) .6 else 2.3, .7), mgp = c(2.8, .8, 0), family = 'sans')
  if (square) par(pty = 's')
  if (panels) {
    par(mfrow = c(1, 3))
    for (s in names(ex$observed)) scatter(ex, s, method, s, means = means)
  } else scatter(ex, scenario, method, means = means, params = params)
}
