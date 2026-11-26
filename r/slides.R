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
equation_named <- function(x, direction = 'SBP') {
  a <- x[[paste0('intercept_', direction)]]
  b <- x[[paste0('slope_', direction)]]
  paste0(fmt(a), if (b >= 0) ' + ' else ' − ',
         fmt(abs(b), if (direction == 'SBP') 2 else 3),
         if (direction == 'SBP') ' × ИМТ' else ' × САД')
}

# Таблицы оценок в едином стиле (D36, D39): строки – параметры μ_X … E(ИМТ | САД),
# столбцы – истина θ (черная) и оценки θ̂ с индексом; цвет оценки – цвет данных:
# наблюдаемые (CD, CC, IPW, ML) синие, заполненные импутацией красные.
theta_tex <- function(s) paste0('\\(', s, '\\)')
theta_labels <- c(theta_tex('\\mu_X'), theta_tex('\\sigma_X'), theta_tex('\\mu_Y'),
                  theta_tex('\\sigma_Y'), theta_tex('\\rho_{XY}'),
                  theta_tex('\\mathrm{E}(\\text{САД}\\mid\\text{ИМТ})'),
                  theta_tex('\\mathrm{E}(\\text{ИМТ}\\mid\\text{САД})'))
theta_cells <- function(x) c(fmt(x[['mean_BMI']], 1), fmt(x[['sd_BMI']], 1),
                             fmt(x[['mean_SBP']], 1), fmt(x[['sd_SBP']], 1),
                             fmt(x[['correlation']], 3), equation_named(x), equation_named(x, 'BMI'))
theta_column <- function(x, index, class = 'estimate-observed') {
  list(head = theta_tex(paste0('\\boldsymbol{\\hat\\theta}_{\\mathbf{', index, '}}')),
       values = theta_cells(x), class = class)
}
cc_column <- function(ex, scenario) theta_column(estimate(ex, scenario, 'CC'), 'CC')
imputation_columns <- function(ex, method) lapply(c('MCAR', 'MAR', 'MNAR'), function(s)
  theta_column(estimate(ex, s, method), s, 'estimate-imputed'))
theta_table <- function(ex, extra = list(), rows = 1:7) {
  cols <- c(list(list(head = theta_tex('\\boldsymbol{\\theta}'), values = theta_cells(ex$truth),
                      class = 'truth'),
                 theta_column(estimate(ex), 'CD')), extra)
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
truth_cd_table <- function(ex) theta_table(ex)

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
pattern_panel <- function(M, title) {
  stopifnot(all(M %in% c(0, 1)))
  width <- 344; height <- 620; header <- 52; inset <- 5; gap <- 4
  cw <- (width - 2 * inset + gap) / ncol(M)
  ch <- (height - header - 2 * inset + gap) / nrow(M)
  cat('<svg class="pattern-panel" xmlns="http://www.w3.org/2000/svg" ',
      'viewBox="0 0 ', width, ' ', height, '" role="img" aria-label="', title,
      ': синий – наблюдаемое значение, красный – пропуск">\n', sep = '')
  # Без общей рамки и серой полосы заголовка: фон панели прозрачный.
  cat('<title>', title, '</title>\n',
      '<text class="pattern-title" x="172" y="36" text-anchor="middle">', title, '</text>\n', sep = '')
  for (i in seq_len(nrow(M))) for (j in seq_len(ncol(M))) {
    cat(sprintf('<rect class="pattern-cell" data-row="%d" data-col="%d" data-missing="%d" x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>\n',
                i, j, M[i,j], inset + (j - 1) * cw, header + inset + (i - 1) * ch,
                cw - gap, ch - gap, course_colors[if (M[i,j]) 'missing' else 'observed']))
  }
  cat('</svg>\n')
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

scatter <- function(ex, scenario = 'Full', method = 'CC', title = '', means = FALSE) {
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
    d <- if (method == 'CC') ex$observed[[scenario]][!m, ] else ex$completed[[scenario]][[method]]
    # При импутации показываем скрытую истину бледнее, подстановку поверх нее.
    points(full$BMI[m], full$SBP[m], pch = 4, cex = .6,
           col = adjustcolor(palette['missing'], if (method == 'CC') 1 else .5))
    points(full$BMI[!m], full$SBP[!m], pch = 16, cex = .65,
           col = adjustcolor(palette['observed'], if (method == 'CC') .5 else .3))
    if (method != 'CC') points(d$BMI[m], d$SBP[m], pch = 17, cex = .65,
                              col = palette['imputed'])
  }
  tr <- ex$truth
  # Оценки по показанным данным; обе регрессии на общих осях X=ИМТ, Y=САД.
  b <- coef(lm(SBP ~ BMI, d)); a <- coef(lm(BMI ~ SBP, d))
  # Цвет оценки – цвет данных, по которым она получена: полные данные и полные случаи
  # синие, заполненные импутацией – красные (D21, D37, D39).
  est_col <- if (scenario == 'Full' || method == 'CC') course_colors[['observed']] else course_colors[['imputed']]
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
    mark(mean(d$BMI), mean(d$SBP), est_col)
    mark(tr[['mean_BMI']], tr[['mean_SBP']], truth_color)
  }
}

plot_example <- function(ex, scenario = 'Full', method = 'CC', panels = FALSE, square = FALSE,
                         means = FALSE) {
  old <- par(no.readonly = TRUE)
  on.exit(par(old))
  par(mar = c(4.2, 4.7, if (square && !panels) .6 else 2.3, .7), mgp = c(2.8, .8, 0), family = 'sans')
  if (square) par(pty = 's')
  if (panels) {
    par(mfrow = c(1, 3))
    for (s in names(ex$observed)) scatter(ex, s, method, s, means = means)
  } else scatter(ex, scenario, method, means = means)
}
