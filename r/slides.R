# Представление результатов; статистика и DGP находятся в example.R.
# Семантическая палитра mice (D21); те же значения в CSS и widgets/common.js.
course_colors <- c(observed = "#006CC2", missing = "#B61A51", imputed = "#B61A51")
fmt <- function(x, digits = 2) formatC(x, digits = digits, format = 'f')
estimate <- function(ex, scenario = 'Full', method = 'Full') {
  ex$estimates[ex$estimates$scenario == scenario & ex$estimates$method == method, ]
}
equation <- function(x, direction = 'SBP', latex = TRUE) {
  a <- x[[paste0('intercept_', direction)]]
  b <- x[[paste0('slope_', direction)]]
  paste0(fmt(a), if (b >= 0) ' + ' else ' − ',
         fmt(abs(b), if (direction == 'SBP') 2 else 3),
         if (direction == 'SBP') 'x' else 'y')
}
result_cells <- function(x) {
  c(fmt(x[['mean_BMI']], 1), fmt(x[['sd_BMI']], 1),
    fmt(x[['mean_SBP']], 1), fmt(x[['sd_SBP']], 1),
    fmt(x[['correlation']], 3), equation(x), equation(x, 'BMI'))
}
results_table <- function(ex, scenario = NULL, method = 'CC') {
  labels <- c('Среднее ИМТ', 'SD ИМТ', 'Среднее САД', 'SD САД',
              'Корреляция', 'САД ~ ИМТ', 'ИМТ ~ САД')
  cols <- list('Параметр' = labels, 'Истина' = result_cells(ex$truth),
               'Полные данные' = result_cells(estimate(ex)))
  if (!is.null(scenario)) {
    for (s in scenario) cols[[if (length(scenario) == 1) 'Полные случаи' else s]] <-
      result_cells(estimate(ex, s, method))
  }
  cat('<table class="estimates"><thead><tr>',
      paste0('<th>', names(cols), '</th>', collapse = ''), '</tr></thead><tbody>')
  for (i in seq_along(labels)) {
    cat('<tr>', paste0(ifelse(seq_along(cols) == 1, '<th scope="row">', '<td>'),
                       vapply(cols, `[`, character(1), i),
                       ifelse(seq_along(cols) == 1, '</th>', '</td>'), collapse = ''), '</tr>')
  }
  cat('</tbody></table>\n')
}

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
    Monotonic = rbind(c(0,0,0,0), c(0,0,0,0), c(0,0,0,1), c(0,0,0,1),
                     c(0,0,1,1), c(0,0,1,1), c(0,0,1,1), c(0,1,1,1),
                     c(0,1,1,1), c(0,1,1,1)),
    General = rbind(c(0,0,0,0), c(0,0,0,0), c(0,0,0,1), c(0,0,0,1),
                   c(0,0,1,1), c(0,0,1,0), c(0,1,0,1), c(0,1,0,0),
                   c(1,1,0,1), c(1,0,1,0)),
    Unidentified = cbind(0, 0,
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
  cat('<title>', title, '</title>\n',
      '<rect class="pattern-frame" x="1" y="1" width="342" height="618" fill="white"/>\n',
      '<rect class="pattern-heading" x="1" y="1" width="342" height="51" fill="#f0f0f0"/>\n',
      '<text class="pattern-title" x="172" y="36" text-anchor="middle">', title, '</text>\n', sep = '')
  for (i in seq_len(nrow(M))) for (j in seq_len(ncol(M))) {
    cat(sprintf('<rect class="pattern-cell" data-row="%d" data-col="%d" data-missing="%d" x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>\n',
                i, j, M[i,j], inset + (j - 1) * cw, header + inset + (i - 1) * ch,
                cw - gap, ch - gap, course_colors[if (M[i,j]) 'missing' else 'observed']))
  }
  cat('</svg>\n')
}

scatter <- function(ex, scenario = 'Full', method = 'CC', title = '') {
  full <- ex$complete
  palette <- course_colors
  plot(full$BMI, full$SBP, type = 'n', xlab = 'ИМТ, кг/м²', ylab = 'САД, мм рт. ст.',
       xlim = range(full$BMI) + c(-0.4, 0.4), ylim = range(full$SBP) + c(-3, 3),
       main = title, bty = 'l', las = 1, cex.main = 1.05)
  grid(col = '#eeeeee')
  if (scenario == 'Full') {
    d <- full
    points(d$BMI, d$SBP, pch = 16, cex = 0.65, col = adjustcolor(palette['observed'], .5))
  } else {
    m <- ex$masks[[scenario]]
    d <- if (method == 'CC') ex$observed[[scenario]][!m, ] else ex$completed[[scenario]][[method]]
    # При импутации показываем скрытую истину бледнее, подстановку поверх нее.
    points(full$BMI[m], full$SBP[m], pch = 4, cex = .6,
           col = adjustcolor(palette['missing'], if (method == 'CC') 1 else .5))
    points(full$BMI[!m], full$SBP[!m], pch = 16, cex = .65,
           col = adjustcolor(palette['observed'], .5))
    if (method != 'CC') points(d$BMI[m], d$SBP[m], pch = 17, cex = .65,
                              col = palette['imputed'])
  }
  # Обе регрессии показаны на общих осях X=ИМТ, Y=САД.
  b <- coef(lm(SBP ~ BMI, d)); a <- coef(lm(BMI ~ SBP, d))
  abline(b, col = course_colors[['observed']], lwd = 2.8)
  if (abs(a[2]) > 1e-10) abline(a = -a[1] / a[2], b = 1 / a[2],
                               col = '#7b3294', lwd = 2.8, lty = 2)
  legend('topleft', c('САД ~ ИМТ', 'ИМТ ~ САД'), col = c(course_colors[['observed']],'#7b3294'),
         lty = c(1,2), lwd = 2, bty = 'n', cex = .85)
}

plot_example <- function(ex, scenario = 'Full', method = 'CC', panels = FALSE) {
  old <- par(no.readonly = TRUE)
  on.exit(par(old))
  par(mar = c(4.2, 4.7, 2.3, .7), mgp = c(2.8, .8, 0), family = 'sans')
  if (panels) {
    par(mfrow = c(1, 3))
    for (s in names(ex$observed)) scatter(ex, s, method, s)
  } else scatter(ex, scenario, method)
}
