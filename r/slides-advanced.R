html_table <- function(columns, class = 'estimates') {
  cat('<table class="',class,'"><thead><tr>',
      paste0('<th scope="col">',names(columns),'</th>',collapse=''),'</tr></thead><tbody>',sep='')
  for(i in seq_len(length(columns[[1]]))) cat('<tr>',paste0(
    ifelse(seq_along(columns)==1,'<th scope="row">','<td>'),
    vapply(columns, function(x) as.character(x[i]), character(1)),
    ifelse(seq_along(columns)==1,'</th>','</td>'),collapse=''),'</tr>',sep='')
  cat('</tbody></table>\n')
}
advanced_table <- function(ex, extra, label, rows = 1:7) {
  labels <- c('Среднее ИМТ','SD ИМТ','Среднее САД','SD САД','Корреляция','САД ~ ИМТ','ИМТ ~ САД')
  cols <- list('Параметр'=labels[rows], 'Истина'=result_cells(ex$truth)[rows],
    'Полные данные'=result_cells(estimate(ex))[rows],
    'Полные случаи'=result_cells(estimate(ex,'MAR','CC'))[rows])
  cols[[label]] <- result_cells(extra)[rows]; html_table(cols)
}
coverage_table <- function(sim) {
  methods <- c('Full','CC','mean','regression','stochastic')
  cols <- list('Метод'=c('Полные данные','Полные случаи','Среднее','Регрессионная','Стохастическая'))
  for (p in c('mean_SBP','slope_SBP')) for(s in c('MCAR','MAR')) {
    d <- sim[sim$parameter==p & sim$scenario==s,];d <- d[match(methods,d$method),]
    cols[[paste(if(p=='mean_SBP') 'Среднее' else 'Наклон',s)]] <-
      paste0(fmt(100*d$coverage,1),' ± ',fmt(100*d$mcse_coverage,1))
  }
  html_table(cols)
}
plot_ipw <- function(ex,a) {
  d <- ex$observed$MAR; ok <- !is.na(d$SBP)
  par(mfrow=c(1,2),mar=c(4,4.6,2,1),mgp=c(2.8,.8,0),family='sans')
  plot(d$BMI[ok],d$SBP[ok],pch=21,bg=adjustcolor(course_colors[['observed']],.35),col=course_colors[['observed']],
    cex=.4+sqrt(a$ipw$w)/2,xlab='ИМТ',ylab='САД',main='Размер точки ∝ √веса',bty='l')
  plot(d$BMI[ok],a$ipw$w,pch=16,col=adjustcolor(course_colors[['observed']],.5),
    xlab='ИМТ',ylab='Вес 1 / π̂',main='Большие веса – большой разброс',bty='l')
}
plot_cc_loss <- function() {
  par(mar=c(4,4.6,1,1),mgp=c(2.8,.8,0),family='sans')
  p <- 1:30
  plot(p,100*.9^p,type='l',lwd=3,col=course_colors[['missing']],ylim=c(0,100),
    xlab='Число переменных p',ylab='Полных строк, %',bty='l')
  lines(p,100*.95^p,lwd=3,col=course_colors[['observed']])
  legend('topright',c('10% NA в каждой','5% NA в каждой'),col=c(course_colors[['missing']],course_colors[['observed']]),lwd=3,bty='n')
}
plot_category <- function(a) {
  d <- a$category$data
  par(mfrow=c(1,2),mar=c(4,4.6,2,1),mgp=c(2.8,.8,0),family='sans')
  # Здесь цвет различает пол, а не наблюдаемые и импутированные значения.
  cols <- c(course_colors[['observed']],'#D97706')
  plot(d$BMI,d$SBP,pch=16,col=adjustcolor(cols[d$male+1],.5),
       xlab='ИМТ',ylab='САД',main='Пол известен',bty='l')
  b <- coef(a$category$full);abline(b[1:2],col=cols[1],lwd=3)
  abline(b[1]+b[3],b[2],col=cols[2],lwd=3)
  legend('topleft',c('Женщины','Мужчины'),col=cols,pch=16,bty='n')
  missing <- d[d$M==1,]
  plot(missing$BMI,missing$SBP,pch=16,col=adjustcolor(cols[missing$male+1],.5),
    xlab='ИМТ',ylab='САД',main='В категории Missing пол скрыт',bty='l')
  abline(a$category$within,col=course_colors[['missing']],lwd=3)
}
plot_distinctness <- function(a) {
  d <- a$distinct;par(mar=c(4,4.6,2,1),mgp=c(2.8,.8,0),family='sans')
  plot(density(d$draws[,'observed']),col=course_colors[['observed']],lwd=3,xlim=c(0,.65),ylim=c(0,11),
       main='',xlab='Оценка θ',ylab='Плотность',bty='l')
  lines(density(d$draws[,'joint']),col='#0F766E',lwd=3)
  abline(v=d$theta,lty=2);legend('topright',c('Только Y_obs','Учитывая M'),
    col=c(course_colors[['observed']],'#0F766E'),lwd=3,bty='n')
}
plot_em <- function(a) {
  h <- a$em$history;par(mfrow=c(1,3),mar=c(4,4.4,2,1),mgp=c(2.7,.8,0),family='sans')
  for (key in c('mean_SBP','correlation','loglik')) {
    plot(h$iteration,h[[key]],type='o',pch=16,cex=.55,col=course_colors[['observed']],lwd=2,bty='l',
      xlab='Итерация',ylab='',main=c(mean_SBP='Среднее САД',correlation='Корреляция',loglik='Наблюдаемое log L')[key])
    if(key!='loglik') abline(h=a$ml$parameters[key],lty=2,col='#0F766E')
  }
}
plot_locf_static <- function() {
  par(mfrow=c(1,2),mar=c(4,4.6,2,1),mgp=c(2.8,.8,0),family='sans')
  t <- 0:4;true <- 50-2*t
  frozen <- vapply(t,function(v) mean(pmin(v,c(0,1))),numeric(1))
  locf <- 50-2*(.7*t+.3*frozen)
  plot(3*t,true,type='o',col='#1E293B',lwd=3,ylim=c(25,65),xlab='Месяц',ylab='Среднее контроля',bty='l')
  lines(3*t,locf,type='o',col=course_colors[['imputed']],lwd=3)
  legend('bottomleft',c('Истина','После LOCF'),col=c('#1E293B',course_colors[['imputed']]),lwd=3,bty='n')
  pT <- c(.3,.15,.45);pC <- c(.3,.45,.15)
  effect <- -1*((1-pT)*4+pT*.5)+2*((1-pC)*4+pC*.5)
  at <- barplot(effect,horiz=TRUE,names.arg=c('30% / 30%','15% / 45%','45% / 15%'),
    col=course_colors[['imputed']],xlim=c(-16,16),xlab='Эффект: лечение − контроль',las=1)
  labels <- sprintf('%.1f', floor(effect*10+.5+1e-9)/10)
  abline(v=4,lty=2,lwd=2);text(effect+.5,at,labels,pos=4)
}
