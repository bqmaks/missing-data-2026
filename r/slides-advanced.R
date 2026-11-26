# Простая таблица; сырой HTML-блок сохраняет формулы \\( … \\) в шапке (D39).
html_table <- function(columns, class = 'estimates', col_class = NULL) {
  if (is.null(col_class)) col_class <- rep('', length(columns))
  cat('\n```{=html}\n<table class="',class,'"><thead><tr>',
      paste0('<th scope="col">',names(columns),'</th>',collapse=''),'</tr></thead><tbody>',sep='')
  for(i in seq_len(length(columns[[1]]))) cat('<tr>',paste0(
    ifelse(seq_along(columns)==1,'<th scope="row">',paste0('<td class="',col_class,'">')),
    vapply(columns, function(x) as.character(x[i]), character(1)),
    ifelse(seq_along(columns)==1,'</th>','</td>'),collapse=''),'</tr>',sep='')
  cat('</tbody></table>\n```\n')
}
plot_cc_loss <- function() {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mar=c(4,4.6,1,1),mgp=c(2.8,.8,0),family='sans')
  p <- 1:30
  plot(p,100*.9^p,type='l',lwd=3,col=course_colors[['missing']],ylim=c(0,100),
    xlab='Число переменных p',ylab='Полных строк (%)',bty='l')
  lines(p,100*.95^p,lwd=3,col=course_colors[['observed']])
  legend('topright',c('10% NA в каждой','5% NA в каждой'),col=c(course_colors[['missing']],course_colors[['observed']]),lwd=3,bty='n')
}
# Missing category method (D55, D58): две квадратные панели. Цвет точек различает пол
# (оранжевый – женщины, бирюзовый – мужчины); строки с пропущенным полом – красные (D21).
# Тонкие черные – истинные условные прямые, толстые – оценки модели.
plot_category <- function(a) {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  d <- a$category$data; tr <- a$category$truth
  par(mfrow = c(1, 2), pty = 's', mar = c(4.2, 4.7, 2.4, .8), mgp = c(2.8, .8, 0), family = 'sans', cex = 1.05)
  # Цвета пола (D58): женщины – оранжевый, мужчины – бирюзовый; синий и красный – семантика курса.
  sex_cols <- c('#D97706', '#0F766E')
  xl <- range(d$BMI); yl <- range(d$SBP)
  truth_lines <- function() {
    abline(tr[1], tr[2], col = truth_color, lwd = truth_lwd)
    abline(tr[1] + tr[3], tr[2], col = truth_color, lwd = truth_lwd)
  }
  plot(d$BMI, d$SBP, pch = 16, cex = .6, col = adjustcolor(sex_cols[d$male + 1], .45), xlim = xl, ylim = yl,
       xlab = 'ИМТ (кг/м²)', ylab = 'САД (мм рт. ст.)', main = 'Пол известен', font.main = 1, bty = 'l', las = 1)
  b <- coef(a$category$full)
  abline(b[1], b[2], col = sex_cols[1], lwd = estimate_lwd)
  abline(b[1] + b[3], b[2], col = sex_cols[2], lwd = estimate_lwd)
  truth_lines()
  legend('topleft', c('Женщины', 'Мужчины'), col = sex_cols, pch = 16, bty = 'n')
  known <- d$M == 0
  plot(d$BMI[known], d$SBP[known], pch = 16, cex = .6, col = adjustcolor(sex_cols[d$male[known] + 1], .2),
       xlim = xl, ylim = yl, xlab = 'ИМТ (кг/м²)', ylab = 'САД (мм рт. ст.)',
       main = 'Категория «Missing»', font.main = 1, bty = 'l', las = 1)
  points(d$BMI[!known], d$SBP[!known], pch = 16, cex = .6, col = adjustcolor(course_colors[['missing']], .6))
  m <- coef(a$category$missing)
  abline(m[1], m[2], col = sex_cols[1], lwd = estimate_lwd)
  abline(m[1] + m[3], m[2], col = sex_cols[2], lwd = estimate_lwd)
  abline(m[1] + m[4], m[2], col = course_colors[['missing']], lwd = estimate_lwd)
  truth_lines()
}
plot_distinctness <- function(a) {
  d <- a$distinct;par(mar=c(4,4.6,2,1),mgp=c(2.8,.8,0),family='sans')
  plot(density(d$draws[,'observed']),col=course_colors[['observed']],lwd=3,xlim=c(0,.65),ylim=c(0,11),
       main='',xlab='Оценка θ',ylab='Плотность',bty='l')
  lines(density(d$draws[,'joint']),col='#0F766E',lwd=3)
  abline(v=d$theta,col=truth_color,lwd=truth_lwd)
  # Легенда без истинного θ (D64): вертикальная линия истины поясняется в заметках.
  legend('topright',c(expression(hat(theta)[obs]),expression(hat(theta)[joint])),
    col=c(course_colors[['observed']],'#0F766E'),lwd=c(3,3),bty='n')
}

# LOCF (D58, D59): три панели – полные траектории и два сценария выбывания (двое из пяти
# после 3-го месяца только в контроле или только в лечении). Наблюдаемые значения синие
# (лечение – темно-синий, контроль – светло-синий), скрытые – красные 50%, подставленные LOCF –
# красные 100%. Толстые линии – средние групп: по полным данным синие, после LOCF красные;
# тонкие черные в панелях LOCF – средние полных данных.
plot_locf <- function(a) {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  d <- a$locf; months <- sort(unique(d$month))
  blue <- c(Лечение = course_colors[['observed']], Контроль = '#7FB3E0')
  red <- c(Лечение = course_colors[['missing']], Контроль = '#DB8BA8')
  yl <- range(d$y) + c(-1, 1)
  par(mfrow = c(1, 3), mar = c(4.2, 4.3, 2.6, 5.6), mgp = c(2.6, .75, 0), family = 'sans', cex = 1, xpd = NA)
  panel <- function(title, ylab = 'Исход') {
    plot(NA, xlim = range(months), ylim = yl, xaxt = 'n', xlab = 'Месяц', ylab = ylab,
         main = title, font.main = 1, cex.main = 1.15, bty = 'l', las = 1)
    axis(1, at = months)
  }
  means <- function(col) tapply(d[[col]], list(d$month, d$group), mean)
  full_m <- means('y')
  label <- function(m, cols) {
    yv <- m[nrow(m), names(cols)]
    if (abs(diff(yv)) < 1.2) yv <- mean(yv) + c(.6, -.6) * sign(diff(rev(yv)) + 1e-9)
    text(max(months) + .5, yv, names(cols), col = cols, adj = 0, cex = .95)
  }
  panel('Полные данные')
  for (g in names(blue)) for (i in unique(d$id)) {
    r <- d[d$group == g & d$id == i, ]
    lines(r$month, r$y, col = adjustcolor(blue[[g]], .55), lwd = 1.3)
    points(r$month, r$y, pch = 16, cex = .6, col = blue[[g]])
  }
  for (g in names(blue)) lines(months, full_m[, g], col = blue[[g]], lwd = estimate_lwd)
  label(full_m, blue)
  for (sc in c('ctrl', 'trt')) {
    obs_col <- paste0('observed_', sc); locf_col <- paste0('locf_', sc)
    panel(c(ctrl = 'LOCF: выбывают в контроле', trt = 'LOCF: выбывают в лечении')[[sc]], ylab = '')
    for (g in names(blue)) for (i in unique(d$id)) {
      r <- d[d$group == g & d$id == i, ]; o <- r[[obs_col]]
      lines(r$month[o], r$y[o], col = adjustcolor(blue[[g]], .55), lwd = 1.3)
      points(r$month[o], r$y[o], pch = 16, cex = .6, col = blue[[g]])
      if (any(!o)) {
        k <- max(which(o))
        lines(r$month[k:nrow(r)], r$y[k:nrow(r)], col = adjustcolor(red[[g]], .5), lwd = 1.1, lty = 3)
        points(r$month[!o], r$y[!o], pch = 4, cex = .75, col = adjustcolor(red[[g]], .5))
        lines(r$month[k:nrow(r)], r[[locf_col]][k:nrow(r)], col = red[[g]], lwd = 1.3, lty = 2)
        points(r$month[!o], r[[locf_col]][!o], pch = 17, cex = .8, col = red[[g]])
      }
    }
    locf_m <- means(locf_col)
    for (g in names(blue)) lines(months, full_m[, g], col = truth_color, lwd = truth_lwd)
    for (g in names(red)) lines(months, locf_m[, g], col = red[[g]], lwd = estimate_lwd)
    label(locf_m, red)
  }
}

# Пространства параметров (θ, ψ) (D63): весь квадрат – разделимы (декартово произведение);
# диагональ θ = ψ и треугольник ψ ≤ θ – неразделимы.
plot_param_spaces <- function() {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  par(mfrow = c(1, 3), pty = 's', mar = c(4.4, 4.4, 3.6, 1), mgp = c(2.6, .7, 0), family = 'sans', cex = 1.05)
  fill <- '#C9CED4'; edge <- '#6B7480'
  panel <- function(title, sub) {
    plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = 'i', yaxs = 'i', xlab = expression(theta),
         ylab = '', las = 1, bty = 'n', cex.lab = 1.3)
    mtext(expression(psi), side = 2, line = 2.9, las = 1, cex = 1.3)
    mtext(title, side = 3, line = 1.5, cex = 1.3)
    mtext(sub, side = 3, line = .25, cex = 1, col = '#536574')
  }
  panel(expression(Omega[theta] %*% Omega[psi]), 'разделимы')
  rect(0, 0, 1, 1, col = fill, border = edge, lwd = 1.5)
  panel(expression(theta == psi), 'неразделимы')
  rect(0, 0, 1, 1, border = '#D5DAE0', lwd = 1)
  segments(0, 0, 1, 1, col = edge, lwd = 5)
  panel(expression(psi <= theta), 'неразделимы')
  rect(0, 0, 1, 1, border = '#D5DAE0', lwd = 1)
  polygon(c(0, 1, 1), c(0, 0, 1), col = fill, border = edge, lwd = 1.5)
}

# Статичный вариант интерактива «Как ММП использует строки без САД» (D68) для печати:
# состояние λ = 1 (ММП). Слева – полные пары, условные средние строк без САД на прямой
# E(САД | ИМТ) и несколько условных плотностей; справа – плотности САД: полные пары (штрих),
# смесь ММП (толстая синяя), истина (тонкая черная).
plot_ml_mechanism <- function(ex, a) {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  d <- ex$complete; m <- ex$masks$MAR; x <- d$BMI; y <- d$SBP
  fit <- lm(y ~ x, subset = !m); b <- coef(fit); s <- sqrt(mean(residuals(fit)^2))
  cm <- b[1] + b[2] * x
  par(mfrow = c(1, 2), mar = c(4.2, 4.6, 1, 1), mgp = c(2.8, .8, 0), family = 'sans', cex = 1.05)
  plot(x[!m], y[!m], pch = 16, cex = .55, col = adjustcolor(course_colors[['observed']], .45),
       xlim = range(x), ylim = range(y) + c(-3, 3), xlab = 'ИМТ (кг/м²)', ylab = 'САД (мм рт. ст.)', bty = 'l', las = 1)
  points(x[m], cm[m], pch = 1, cex = .6, col = course_colors[['missing']])
  ord <- which(m)[order(x[m])]; reps <- ord[round(c(.1, .3, .5, .7, .9) * (length(ord) - 1)) + 1]
  for (i in reps) {
    v <- cm[i] + seq(-3, 3, .1) * s; w <- 1.6 * s * sqrt(2 * pi) * dnorm(v, cm[i], s)
    polygon(c(x[i] + w, x[i], x[i]), c(v, cm[i] + 3 * s, cm[i] - 3 * s),
            col = adjustcolor(course_colors[['missing']], .18), border = course_colors[['missing']])
  }
  abline(b, col = course_colors[['observed']], lwd = estimate_lwd)
  abline(ex$truth[['intercept_SBP']], ex$truth[['slope_SBP']], col = truth_color, lwd = truth_lwd)
  # Обратная регрессия E(ИМТ | САД): ММП (толстая синяя) и истина (тонкая черная).
  mlp <- a$ml$parameters
  abline(a = -mlp[['intercept_BMI']] / mlp[['slope_BMI']], b = 1 / mlp[['slope_BMI']],
         col = course_colors[['observed']], lwd = estimate_lwd)
  abline(a = -ex$truth[['intercept_BMI']] / ex$truth[['slope_BMI']], b = 1 / ex$truth[['slope_BMI']],
         col = truth_color, lwd = truth_lwd)
  g <- seq(min(y) - 3, max(y) + 3, .5); h <- 1.06 * sd(y[!m]) * sum(!m)^(-.2)
  kd <- function(lambda) (rowSums(sapply(y[!m], function(v) dnorm(g, v, h))) +
    lambda * rowSums(sapply(cm[m], function(v) dnorm(g, v, sqrt(s^2 + h^2))))) / (sum(!m) + lambda * sum(m))
  tr <- dnorm(g, ex$truth[['mean_SBP']], ex$truth[['sd_SBP']])
  plot(g, kd(1), type = 'n', ylim = c(0, 1.08 * max(kd(0), kd(1), tr)), xlab = 'САД (мм рт. ст.)',
       ylab = 'Плотность', bty = 'l', las = 1)
  lines(g, kd(0), col = course_colors[['observed']], lwd = 2, lty = 2)
  lines(g, kd(1), col = course_colors[['observed']], lwd = estimate_lwd)
  lines(g, tr, col = truth_color, lwd = truth_lwd)
}
