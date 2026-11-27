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
# Missing category method (D55, D58, D72): две квадратные панели без заголовков. Цвет точек
# различает пол (оранжевый – женщины, бирюзовый – мужчины); строки с пропущенным полом –
# красные (D21). Истинные прямые не рисуем (D72): только оценки по данным – отдельная
# линейная регрессия САД на ИМТ в каждой группе, в пределах ИМТ этой группы.
category_strata <- function(d, group) {
  lapply(split(d, group, drop = TRUE), function(g) {
    b <- coef(lm(SBP ~ BMI, g)); x <- range(g$BMI)
    list(coef = b, x = x, y = b[1] + b[2] * x)
  })
}
plot_category <- function(a) {
  old <- par(no.readonly = TRUE); on.exit(par(old))
  d <- a$category$data
  par(mfrow = c(1, 2), pty = 's', mar = c(4.2, 4.7, 1, .8), mgp = c(2.8, .8, 0), family = 'sans', cex = 1.05)
  # Цвета пола (D58): женщины – оранжевый, мужчины – бирюзовый; синий и красный – семантика курса.
  group_cols <- c('Женщины' = '#D97706', 'Мужчины' = '#0F766E', 'Нет данных' = course_colors[['missing']])
  xl <- range(d$BMI); yl <- range(d$SBP)
  strata_lines <- function(fits, cols) for (k in names(fits))
    lines(fits[[k]]$x, fits[[k]]$y, col = cols[[k]], lwd = estimate_lwd)
  sex <- factor(ifelse(d$male == 1, 'Мужчины', 'Женщины'), levels = c('Женщины', 'Мужчины'))
  plot(d$BMI, d$SBP, pch = 16, cex = .6, col = adjustcolor(group_cols[as.character(sex)], .45), xlim = xl, ylim = yl,
       xlab = 'ИМТ (кг/м²)', ylab = 'САД (мм рт. ст.)', bty = 'l', las = 1)
  strata_lines(category_strata(d, sex), group_cols)
  legend('topleft', names(group_cols)[1:2], col = group_cols[1:2], pch = 16, bty = 'n')
  group <- factor(ifelse(d$M == 1, 'Нет данных', as.character(sex)), levels = names(group_cols))
  known <- d$M == 0
  plot(d$BMI[known], d$SBP[known], pch = 16, cex = .6, col = adjustcolor(group_cols[as.character(group[known])], .2),
       xlim = xl, ylim = yl, xlab = 'ИМТ (кг/м²)', ylab = 'САД (мм рт. ст.)', bty = 'l', las = 1)
  points(d$BMI[!known], d$SBP[!known], pch = 16, cex = .6, col = adjustcolor(group_cols[['Нет данных']], .6))
  strata_lines(category_strata(d, group), group_cols)
  legend('topleft', names(group_cols), col = group_cols, pch = 16, bty = 'n')
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

# Статичный вариант интерактива W-MLPROFILE (D71) для печати: настройки по умолчанию,
# 100% добавленных строк только с ИМТ. Данные выгружены из JS-модуля
# (node scripts/generate_mlprofile.mjs): генератор mulberry32 в R не воспроизводим.
# Стили как в интерактиве: синий – оценки по наблюдаемым данным (полные случаи – штрих,
# ММП – толстая сплошная), черный пунктир – все данные, фиолетовый и коричневый – модели
# ММП с закрепленным средним и стандартным отклонением САД.
plot_ml_profile <- function(path = 'data/ml-profile.json') {
  j <- jsonlite::fromJSON(path)
  old <- par(no.readonly = TRUE); on.exit(par(old))
  blue <- course_colors[['observed']]; ref_col <- '#64748B'
  st <- list(full = list(col = truth_color, lty = '24', lwd = 1.8, pch = 3),
             cc = list(col = blue, lty = '74', lwd = 2.2, pch = 1),
             ml = list(col = blue, lty = 'solid', lwd = estimate_lwd, pch = 19),
             fixedMean = list(col = '#7C3AED', lty = '8323', lwd = 2.2, pch = 15),
             fixedSd = list(col = '#B45309', lty = 'C4', lwd = 2.2, pch = 15))
  labels <- c(full = 'все данные до пропусков', cc = 'полные случаи', ml = 'ММП с добавленными случаями',
              fixedMean = 'ММП при закрепленном среднем САД', fixedSd = 'ММП при закрепленном ст. отклонении САД')
  e <- j$estimates; x <- j$x; y <- j$y
  layout(matrix(c(1, 2, 3, 4, 1, 2, 5, 5), 2, byrow = TRUE), widths = c(3.9, 1, 2.75, 2.75))
  par(family = 'sans', mgp = c(2.6, .7, 0), las = 1, cex = 1)

  # Диаграмма рассеяния: полные пары, засечки ИМТ строк без САД, 95%-е эллипсы и центры.
  par(mar = c(4.2, 4.6, 2.4, .3))
  plot(x[j$cc], y[j$cc], xlim = c(14, 40), ylim = c(90, 180), xaxs = 'i', yaxs = 'i', pch = 16, cex = .75,
       col = adjustcolor(blue, .5), xlab = 'ИМТ (кг/м²)', ylab = 'САД (мм рт. ст.)', bty = 'l')
  segments(x[j$xOnly], 91, x[j$xOnly], 95, col = blue, lwd = 1.2)
  xr <- range(x); cc <- e$cc; ml <- e$ml
  lines(xr, cc$muY + cc$slope * (xr - cc$muX), col = adjustcolor(blue, .8), lwd = 1.4)
  for (kind in c('fixedMean', 'fixedSd', 'cc', 'ml', 'full')) {
    p <- j$ellipses[[kind]]; s <- st[[kind]]
    lines(p[, 1], p[, 2], col = s$col, lty = s$lty, lwd = s$lwd)
  }
  segments(cc$muX, cc$muY, 40, cc$muY, col = blue, lty = '74', lwd = 1.2)
  segments(ml$muX, 90, ml$muX, ml$muY, col = blue, lty = '23', lwd = 1.4)
  segments(ml$muX, ml$muY, 40, ml$muY, col = blue, lty = '23', lwd = 1.4)
  for (kind in c('fixedMean', 'fixedSd', 'cc', 'full', 'ml')) {
    s <- st[[kind]]; v <- e[[kind]]
    points(v$muX, v$muY, pch = s$pch, col = s$col, bg = 'white', cex = 1.5, lwd = 2.2)
  }
  legend('topleft', legend = labels, col = sapply(st, `[[`, 'col'), lty = sapply(st, `[[`, 'lty'),
         lwd = 2, pch = sapply(st, `[[`, 'pch'), seg.len = 3.2, bg = adjustcolor('white', .85), box.col = NA,
         cex = .8, inset = c(.01, .01))
  title(main = sprintf('ρ = %.2f, ψ1 = %.2f; добавлены все %d строки без САД', j$params$rho, j$params$psi1, j$K),
        adj = 0, font.main = 1, cex.main = .9)

  # Нормальные плотности САД.
  par(mar = c(4.2, .3, 2.4, .6))
  g <- seq(90, 180, .5)
  plot(0, type = 'n', xlim = c(0, .035), ylim = c(90, 180), xaxs = 'i', yaxs = 'i', axes = FALSE, xlab = '', ylab = '')
  abline(v = 0, col = '#9AA1A9')
  for (kind in c('cc', 'ml', 'full')) {
    v <- e[[kind]]; s <- st[[kind]]
    lines(dnorm(g, v$muY, sqrt(v$varY)), g, col = s$col, lty = s$lty, lwd = s$lwd)
  }
  mtext('распределение САД', side = 3, line = .4, adj = .1, cex = .85)

  # Профили (D99): −2 log Λ(c) – статистика отношения правдоподобий как функция значения
  # параметра c, Λ(c) = L_p(c) / L(θ̂_ML; Y_obs).
  # «−2» – строкой: унарный минус plotmath ставит пробел между знаком и цифрой.
  profile_panel <- function(which, sym, lr, pick) {
    pr <- j$profiles[[which]]; rg <- j$ranges[[which]]
    par(mar = c(3, 3.8, 2.4, .8))
    plot(0, type = 'n', xlim = rg, ylim = c(0, j$cap), xaxs = 'i', yaxs = 'i', xlab = '',
         ylab = bquote('−2' ~ log ~ Lambda(.(sym)['САД'])), bty = 'l', yaxt = 'n')
    axis(2, at = c(0, j$cap / 2, j$cap))
    abline(h = j$chi2, col = ref_col, lty = '33', lwd = 1.3)
    abline(v = pick(e$cc), col = blue, lty = '74', lwd = 1.4)
    segments(pick(e$cc), 0, pick(e$cc), min(lr, j$cap), col = blue, lwd = 6, lend = 1)
    lines(pr[, 1], pr[, 2], col = blue, lwd = 2.6)
    segments(pick(e$full), 0, pick(e$full), .06 * j$cap, col = truth_color, lwd = 2.4)
    points(pick(e$ml), 0, pch = 19, col = blue, cex = 1.4, xpd = NA)
    text(rg[1], j$chi2, '3.84', adj = c(-.15, -.5), col = ref_col, cex = .85)
    mtext(bquote('−2' ~ log ~ Lambda(hat(.(sym))[CC]) == .(sprintf('%.1f', lr))),
          side = 3, line = .7, adj = 0, at = rg[1] - .14 * diff(rg), cex = .9)
  }
  profile_panel('mean', quote(mu), j$lr$mean, function(v) v$muY)
  profile_panel('sd', quote(sigma), j$lr$sd, function(v) v$sdY)

  # Рост статистики с долей добавленных строк.
  par(mar = c(4, 3.8, 2.4, .8))
  pct <- 100 * (seq_along(j$curves$mean) - 1) / j$K
  plot(0, type = 'n', xlim = c(0, 100), ylim = c(0, j$cap), xaxs = 'i', yaxs = 'i', xlab = '',
       ylab = quote('−2' ~ log ~ Lambda), bty = 'l', xaxt = 'n', yaxt = 'n')
  axis(1, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), '%'))
  axis(2, at = c(0, j$cap / 2, j$cap))
  abline(h = j$chi2, col = ref_col, lty = '33', lwd = 1.3)
  text(0, j$chi2, '3.84', adj = c(-.15, -.5), col = ref_col, cex = .85)
  lines(pct, j$curves$mean, col = blue, lwd = 3)
  lines(pct, j$curves$sd, col = blue, lwd = 3, lty = '64')
  points(100, tail(j$curves$mean, 1), pch = 19, col = blue, cex = 1.5, xpd = NA)
  points(100, tail(j$curves$sd, 1), pch = 15, col = blue, cex = 1.5, xpd = NA)
  legend('topleft', c('среднее САД', 'стандартное отклонение САД'), col = blue, lwd = 3, lty = c('solid', '64'),
         pch = c(19, 15), seg.len = 3, bty = 'n', cex = .85)
  mtext('доля добавленных случаев с ИМТ, но без САД', side = 1, line = 2.4, cex = .85)
  title(main = quote('Статистика отношения правдоподобий' ~ '−2' ~ log ~ Lambda ~ 'для оценки по полным случаям'),
        adj = 0, font.main = 1, cex.main = 1)
}
