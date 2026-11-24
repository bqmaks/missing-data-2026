"""Build editable Quarto/reading drafts from the reviewed source snapshot.

The PPTX is imported separately; normal builds never read ignored course-inputs.
All original slide IDs and notes remain traceable in wiki/traceability.csv.
"""
from pathlib import Path
import csv
import html
import json
import re
import subprocess
import zipfile
import sys
from xml.etree import ElementTree as ET
from copy import deepcopy

ROOT = Path(__file__).resolve().parents[1]
SOURCE = json.loads((ROOT / "slides/source-deck.json").read_text())
SLIDES = SOURCE["slides"]
MATH = {}


def math(tex):
    key = f"COURSEMATH{len(MATH):04d}END"
    MATH[key] = tex
    return f'<div class="equation">{key}</div>'


# Explicit transcription: readable LaTeX rather than PPTX pseudo-mathematics.
FORMULAS = {
    "f(Y, M | θ, ψ) =": r"f(Y,M\mid\theta,\psi)=f(Y\mid\theta)\Pr(M\mid Y,\psi)",
    "f(Y, M | θ*, ψ*)": r"f(Y,M\mid\theta^*,\psi^*)=f(Y\mid M,\theta^*)\Pr(M\mid\psi^*)",
    "L(θ, ψ | Y_obs, M) = ∫": r"\begin{aligned} L(\theta,\psi\mid Y_{\rm obs},M)&=\int f(Y_{\rm obs},Y_{\rm mis}\mid\theta)\\&\quad\cdot\Pr(M\mid Y_{\rm obs},Y_{\rm mis},\psi)\,dY_{\rm mis}\end{aligned}",
    "L(θ, ψ | Y_obs, M) = Pr": r"L(\theta,\psi\mid Y_{\rm obs},M)=\Pr(M\mid Y_{\rm obs},\psi)L_{\rm ign}(\theta\mid Y_{\rm obs})",
    "f(Y, M | θ, ψ)  =": r"f(Y,M\mid\theta,\psi)=f(M\mid Y_{\rm obs},Y_{\rm mis},\psi)f(Y_{\rm obs},Y_{\rm mis}\mid\theta)",
    "f(Y_obs, M | θ, ψ)  =  ∫": r"f(Y_{\rm obs},M\mid\theta,\psi)=\int f(M\mid Y_{\rm obs},Y_{\rm mis},\psi)f(Y_{\rm obs},Y_{\rm mis}\mid\theta)\,dY_{\rm mis}",
    "f(Y_obs, M | θ, ψ)  =  f": r"f(Y_{\rm obs},M\mid\theta,\psi)=f(M\mid Y_{\rm obs},\psi)\int f(Y_{\rm obs},Y_{\rm mis}\mid\theta)\,dY_{\rm mis}",
    "Y₂ | Y₁ = y₁": r"Y_2\mid Y_1=y_1\sim N\!\left(\mu_2+\rho\frac{\sigma_2}{\sigma_1}(y_1-\mu_1),\ \sigma_2^2(1-\rho^2)\right)",
    "g(θ̂)  ≈": r"g(\hat\theta)\ \dot\sim\ N\!\left(g(\theta),\ \frac{g'(\theta)^\top Vg'(\theta)}{n}\right)",
    "L(φ | Y_obs) = ∏": r"L(\phi\mid Y_{\rm obs})=\prod_{i=1}^{n_{\rm obs}}f_1(y_{i1})f_2(y_{i2}\mid y_{i1})\prod_{i=n_{\rm obs}+1}^{n}f_1(y_{i1})",
    "= [ ∏_{i=1..n}": r"=\underbrace{\prod_{i=1}^{n}f_1(y_{i1}\mid\mu_1,\sigma_1^2)}_{\text{all observations}}\ \underbrace{\prod_{i\in\mathcal O}f_2(y_{i2}\mid y_{i1},\beta_0,\beta_1,\sigma^2_{2\mid1})}_{\text{observed pairs}}",
    "L(φ | Y_obs) = [": r"L(\phi\mid Y_{\rm obs})=\left[\prod_{i=1}^{n} f_1(y_{i1}\mid\mu_1,\sigma_1^2)\right]\left[\prod_{i\in\mathcal O}f_2(y_{i2}\mid y_{i1},\beta_0,\beta_1,\sigma^2_{2\mid1})\right]",
    "μ̂₁ = ȳ₁,": r"\hat\mu_1=\bar y_1,\qquad \hat\sigma_1^2=\frac1n\sum_{i=1}^{n}(y_{i1}-\bar y_1)^2",
    "μ̂₁ = (1/n)": r"\hat\mu_1=\frac1n\sum_{i=1}^{n}y_{i1},\qquad\hat\sigma_1^2=\frac1n\sum_{i=1}^{n}(y_{i1}-\hat\mu_1)^2",
    "μ̂₂,MLE = ȳ₂,obs": r"\hat\mu_{2,\rm ML}=\bar y_{2,\rm obs}+\hat\beta_1(\bar y_1-\bar y_{1,\rm obs})",
    "Var(μ̂₂,MLE) =": r"\widehat{\operatorname{Var}}(\hat\mu_{2,\rm ML})=\frac{\hat\beta_1^2\hat\sigma_1^2}{n}+\frac{\hat\sigma^2_{2\mid1}}{n_{\rm obs}}",
    "+ (ȳ₁ − ȳ₁,obs)²": r"\quad+\frac{(\bar y_1-\bar y_{1,\rm obs})^2\hat\sigma^2_{2\mid1}}{\sum_{i\in\mathcal O}(y_{i1}-\bar y_{1,\rm obs})^2}",
    "θ̂_MI  =": r"\hat\theta_{\rm MI}=\frac1m\sum_{\ell=1}^{m}\hat\theta_\ell",
    "Ŵ = (1 / m)": r"\hat W=\frac1m\sum_{\ell=1}^{m}\widehat{\operatorname{Var}}(\hat\theta_\ell)",
    "B̂ = [1 / (m − 1)]": r"\hat B=\frac1{m-1}\sum_{\ell=1}^{m}(\hat\theta_\ell-\hat\theta_{\rm MI})^2",
    "V̂_MI = Ŵ": r"\hat V_{\rm MI}=\hat W+\left(1+\frac1m\right)\hat B",
    "95% ДИ: θ̂_MI": r"\hat\theta_{\rm MI}\ \pm\ t_{\nu_{\rm BR},\,0.975}\sqrt{\hat V_{\rm MI}}",
    "f(X | Y, Z)  ∝": r"f(X\mid Y,Z)\propto f(Y\mid X,Z;\beta)\,f(X\mid Z;\phi_X)",
}


def inline(text):
    text = html.escape(text)
    # IDs are stable; generated cross-references don't depend on slide numbering.
    def link(m):
        number = int(m.group(1))
        if not 1 <= number <= len(SLIDES):
            return m.group(0)
        target = SLIDES[number - 1]["id"]
        return f'<a class="slide-ref" href="#/{target}">связанный разбор</a>'
    return re.sub(r"слайд(?:е|а|у|ом)?\s+(\d+)(?!\d)", link, text, flags=re.I)


def para(text, heading=False):
    text = text.strip()
    for prefix, tex in FORMULAS.items():
        if text.startswith(prefix):
            return math(tex)
    if text.startswith("└"):
        return ''  # Group labels are included in the transcribed equation.
    if "\n" in text:
        return ''.join(para(t) for t in text.splitlines() if t.strip())
    tag = 'h3' if heading else 'p'
    cls = ' class="bullet"' if text.startswith(('•', '– ')) else ''
    return f'<{tag}{cls}>{inline(text)}</{tag}>'


def table(headers, rows):
    return '<table><thead><tr>' + ''.join(f'<th>{inline(str(h))}</th>' for h in headers) + '</tr></thead><tbody>' + ''.join(
        '<tr>' + ''.join(f'<td>{inline(str(c))}</td>' for c in row) + '</tr>' for row in rows) + '</tbody></table>'


def card(content, span=12, start=None, cls=''):
    position = f"{start} / span {span}" if start is not None else f"span {span}"
    return f'<div class="course-card {cls}" style="grid-column:{position}">{content}</div>'


def figure(path, caption, span=12):
    return card(f'<figure><img src="{path}" alt="{html.escape(caption)}"><figcaption>{inline(caption)}</figcaption></figure>', span, cls='image')


def source_layout(slide):
    body = [s for s in slide['shapes'] if 1.3 < s['position'][1] / 914400 < 7]
    body.sort(key=lambda s: (round(s['position'][1] / 914400, 1), s['position'][0]))
    if slide['number'] in (1, 43, 69):
        texts = [p for s in slide['shapes'] for p in s['paragraphs']]
        return card(''.join(para(p) for p in texts[1:]))
    out = []
    for shape in body:
        x, _, w, _ = [v / 914400 for v in shape['position']]
        start = max(1, min(12, round((x - .55) / 12.23 * 12) + 1))
        span = max(1, min(13 - start, round(w / 12.23 * 12)))
        if span >= 11:
            start, span = 1, 12
        if 'image' in shape:
            content = f'<img src="{shape["image"]}" alt="Иллюстрация к теме: {html.escape(get_title(slide))}">'
            out.append(card(content, span, start, 'image'))
        elif 'table' in shape:
            rows = shape['table']
            out.append(card(table(rows[0], rows[1:]), span, start))
        else:
            ps = shape['paragraphs']
            if any(p.lstrip().startswith(('library(', 'imp <-', 'imp<-')) for p in ps):
                first_code = next(i for i, p in enumerate(ps) if p.lstrip().startswith(('library(', 'imp <-', 'imp<-')))
                code = '\n'.join(ps[first_code:])
                if slide['id'] == 'p3-smcfcs-r':
                    code = code.replace('m = 20, numit = 20)', 'm = 20, numit = 20, rjlimit = 100000)')
                    code = code.replace('imp <- smcfcs(', 'imp <- smcfcs_checked(  # проверка отказов из R-практикума')
                    code = code.replace('summary(MIcombine(fits))', 'if (imp$rejection_ok) summary(MIcombine(fits))')
                content = ''.join(para(p, i == 0) for i, p in enumerate(ps[:first_code]))
                content += '<pre><code class="language-r">' + html.escape(code) + '</code></pre>'
            else:
                content = ''.join(para(p, i == 0 and len(ps) > 1 and len(p) < 100) for i, p in enumerate(ps))
            out.append(card(content, span, start))
    return ''.join(out)


def get_title(slide):
    if slide['number'] in (1, 43, 69):
        return slide['shapes'][0]['paragraphs'][0]
    candidates = [s for s in slide['shapes'] if .45 < s['position'][1] / 914400 < 1.3]
    return ' '.join(max(candidates, key=lambda s: s['position'][2])['paragraphs'])


def overrides():
    sample = json.loads((ROOT / 'static/data/sample500.json').read_text())
    sim = json.loads((ROOT / 'static/data/sim_part1.json').read_text())
    mi = json.loads((ROOT / 'static/data/mi_coverage.json').read_text())
    labels = {'Full':'Полные данные', 'CCA':'Полные случаи', 'Mean':'Среднее', 'Reg':'Регрессия',
              'StochReg':'Регрессия + шум', 'MLE':'ММП', 'Improper20':'MI без розыгрыша, m=20',
              'Proper5':'Собственная MI, m=5', 'Proper20':'Собственная MI, m=20'}
    replacements = {}
    rows = []
    for r in sample['estimates']['MAR']:
        if r['method'] == 'Full': continue
        rows.append([labels[r['method']], f'{r["mu2"]:.2f}', f'{r["mu2"]-1.96*r["se_mu2"]:.2f}–{r["mu2"]+1.96*r["se_mu2"]:.2f}', f'{r["rho"]:.2f}', f'{r["g1"]:.2f}'])
    nmis = sum(sample['masks']['MAR'])
    replacements['p1-riddle'] = card(para(f'Одна выборка: n = {len(sample["full"])}, ИМТ пропущен у {nmis} пациентов. Все методы работают с одними и теми же наблюдаемыми значениями.')) + card(table(['Метод', 'Средний ИМТ', '95% ДИ', 'Корреляция', 'САД ~ ИМТ'], rows)) + card(para('Сначала прогноз: чей центр интервала верен? Какой интервал учитывает потерю информации? Почему похожие точечные оценки ещё не означают одинаковый вывод?'))
    replacements['p1-cohort'] = card(para('Клинический сюжет', True) + para('САД измерено у всех пациентов. При высоком давлении человека раньше направляют к врачу, а антропометрию пропускают.') + para(f'В учебной выборке {len(sample["full"])} строк; ИМТ наблюдается у {len(sample["full"])-nmis}.') + para('Могут ли значения САД у пациентов без ИМТ помочь оценить средний ИМТ?'), 5) + figure('assets/generated/cohort.png', 'Одна воспроизводимая выборка; пустые кружки — скрытые от аналитика значения.', 7)
    replacements['p1-taxonomy'] = figure('assets/generated/mechanisms.png', 'Одинаковые полные данные, три механизма пропусков. Пунктир — популяционное среднее ИМТ.') + card(para('MCAR: вероятность пропуска постоянна. MAR: зависит только от наблюдаемого САД. MNAR: зависит от самого ИМТ, в том числе там, где он не наблюдается.'))
    replacements['p1-single-imputation'] = figure('assets/generated/single-imputation.png', 'Треугольники — импутации. Восстановление формы облака само по себе не гарантирует корректную SE.') + card(para('Среднее сжимает разброс; регрессия размещает значения на прямой; остаточный шум возвращает разброс. Сравниваем также среднее, оба наклона и доверительные интервалы.'))
    rows = []
    for method in ['Full', 'CCA', 'Mean', 'Reg', 'StochReg', 'MLE']:
        r = next(r for r in sim['summary'] if r['mechanism'] == 'MAR' and r['method'] == method)
        rows.append([labels[method], f'{r["mu2"]:.2f}', f'{r["sd2"]:.2f}', f'{r["rho"]:.2f}', f'{r["b1"]:.3f}', f'{r["g1"]:.2f}', f'{r["emp_se_mu2"]:.3f}', f'{r["rmse_mu2"]:.3f}', f'{100*r["coverage_mu2"]:.1f} ± {100*r["mcse_coverage_mu2"]:.1f}', f'{100*r["coverage_b1"]:.1f} ± {100*r["mcse_coverage_b1"]:.1f}'])
    replacements['p1-sim'] = card(para(f'MAR · n = {sim["meta"]["n"]} · {sim["meta"]["S"]} репликаций · seed = {sim["meta"]["seed"]}')) + card(table(['Метод','μ₂','σ₂','ρ','β₁','γ₁','Эмп. SE μ₂','RMSE μ₂','Покрытие μ₂, %','Покрытие β₁, %'], rows)) + card(para('После ± указан один MCSE покрытия (процентные пункты). Интервалы в Части I используют нормальное приближение. ММП восстанавливает маргинальные параметры при принятой модели; β₁ совпадает с оценкой по полным случаям.'))
    replacements['p1-zipper'] = figure('assets/generated/intervals.png', 'Первые 100 репликаций общего расчёта. Синие интервалы содержат истину; оранжевые — нет.')
    rows = [[labels[r['method']], 'μ₂' if r['term']=='mu2' else 'β₁', f'{100*r["coverage"]:.1f}', f'{100*r["mcse_coverage"]:.1f}', '—' if r['fmi'] is None else f'{r["fmi"]:.3f}'] for r in mi['summary']]
    replacements['p2-mi-coverage'] = card(table(['Метод', 'Параметр', 'Покрытие, %', 'MCSE, п.п.', 'Средняя FMI'], rows), 6) + figure('assets/generated/mi-coverage.png', f'{mi["meta"]["S"]} репликаций, n={mi["meta"]["n"]}; для MI — t-интервалы Барнарда–Рубина.', 6)
    replacements['p2-gibbs'] = card(para('Цепи нужно проверять, а не только запускать', True) + para('Просматривайте траектории среднего и SD импутаций. Устойчивый тренд, отсутствие перемешивания и loggedEvents требуют разбора.') + '<pre><code class="language-r">plot(imp)\nmice::convergence(imp, diagnostic = "all")\nimp$loggedEvents</code></pre>') + card(para('m — число заполненных наборов; T — число итераций цепи. Это разные настройки. PSRF близко к 1 — диагностический признак, который не доказывает правильность модели или MAR.'))
    wr_code = '''d7m <- d7  # time, status, age, trt, bmi
d7m$H <- mice::nelsonaalen(d7m, time, status)
pred <- mice::make.predictorMatrix(d7m); pred[,] <- 0
pred["bmi", c("status", "H", "age", "trt")] <- 1
meth <- mice::make.method(d7m); meth["bmi"] <- "norm"
imp <- mice::mice(d7m, m = 20, maxit = 20, method = meth,
  predictorMatrix = pred, seed = 71, printFlag = FALSE)
mice::pool(with(imp, survival::coxph(
  survival::Surv(time, status) ~ bmi + age + trt)))'''
    smc_code = '''# Исходный d7, без дополнительного столбца H
set.seed(72)
imp <- smcfcs_checked(d7, smtype = "coxph",
  smformula = "Surv(time, status) ~ bmi + age + trt",
  method = c("", "", "", "", "norm"),
  m = 20, numit = 20, rjlimit = 100000)
fits <- with(mitools::imputationList(imp$impDatasets),
  survival::coxph(survival::Surv(time, status) ~ bmi + age + trt))
if (imp$rejection_ok) mitools::MIcombine(fits)'''
    replacements['p3-surv-smcfcs'] = card(para('mice + приближение White–Royston', True) +
        '<pre><code class="language-r">' + html.escape(wr_code) + '</code></pre>') + card(
        para('SMC-FCS с моделью Кокса', True) + '<pre><code class="language-r">' + html.escape(smc_code) + '</code></pre>')
    return replacements


def build():
    from bundle_practice import main as bundle_practice
    bundle_practice()
    replacement = overrides()
    layout = json.loads((ROOT/'slides/layout.json').read_text()) if (ROOT/'slides/layout.json').exists() and '--unpaginated' not in sys.argv else {}
    qmd = ['''---
title: "Анализ данных с пропусками в биостатистике"
short-title: "Анализ неполных данных · R"
short-author: "Биостатистика · версия 0.1"
lang: ru
theme-color: "#0F766E"
theme-color-dark: "#115E59"
theme-color-light: "#CCFBF1"
format:
  material-revealjs:
    title-slide: false
    html-math-method: mathml
    transition: none
    navigation-mode: linear
    controls: true
    css: assets/course.css
    include-after-body:
      - text: '<script src="assets/course.js"></script>'
    theme-color: "#0F766E"
    theme-color-dark: "#115E59"
    theme-color-light: "#CCFBF1"
    slide-number: false
    code-copy: true
    auto-stretch: false
    center: false
    keyboard: true
---
''']
    reading = ['<!doctype html><html lang="ru"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Анализ неполных данных — чтение</title><link rel="stylesheet" href="assets/course.css"></head><body class="reading-page"><header><h1>Анализ данных с пропусками</h1><p>Первая редакция · три занятия · все слои и заметки докладчика.</p><p><a href="index.html">Открыть слайды</a> · <a href="downloads/missing_data_practice.R" download>R-файл</a> · <a href="downloads/missing-data-practice.zip">Практикум с расчётами (ZIP)</a> · <button onclick="window.print()">Печать</button></p></header>']
    trace, reviews, registry = [], [], []
    source_numeric = {'p1-formula-check','p1-locf-chart','p1-rho','p1-mirror','p1-rct','p2-em-trace','p2-why-mi','p2-pmm','p2-rubin-2','p3-compat-problem','p3-mnar-delta'}
    for slide in SLIDES:
        sid, number = slide['id'], slide['number']
        title = get_title(slide)
        if sid == 'p1-sim': title = 'Сравниваем смещение, разброс и покрытие на одних данных'
        if sid == 'p2-mi-coverage': title = 'MI учитывает неопределённость между заполнениями'
        body = replacement.get(sid) or source_layout(slide)
        source_body = source_layout(slide) if sid in replacement else None
        banner = ''
        if slide['review']:
            banner = '<div class="review-banner">TODO · Утверждения и ссылки этого слайда требуют подтверждения автора. Это рабочий материал.</div>'
            reviews.append((number, sid, slide['review']))
        if sid in source_numeric:
            banner += '<div class="review-banner">Числа перенесены из PPTX; этот расчёт ещё не воспроизведён в R.</div>'
        layer_ru = {'core':'Основная линия','deep':'Углублённо','appendix':'Справка'}[slide['layer']]
        heading_level = '#' if number in (1,43,69) else '##'
        if number in (1,43,69):
            title = {1:'I. Природа пропусков и правдоподобие',43:'II. Множественная импутация',69:'III. Границы применимости MI'}[number]
        attrs = f'#{sid} .course-slide .layer-{slide["layer"]} data-source="v2:{number}" data-layer="{slide["layer"]}"'
        if heading_level == '#': attrs += ' .course-title'
        content = f'<div class="course-kicker">ЗАНЯТИЕ {slide["part"]} · {layer_ru.upper()}</div>{banner}<div class="course-grid">{body}</div>'
        if sid in replacement:
            content += '<p class="source-label">Расчёт R: static/data · воспроизводимые параметры и seed в метаданных.</p>' if sid not in ('p2-gibbs', 'p3-surv-smcfcs') else ''
        notes = ''.join(para(n) for n in slide['notes'])
        if source_body:
            notes = '<p><strong>Редакция HTML:</strong> основной слайд обновлён по расчётам R или проверенному API. Числа исходного сценария ниже относятся к PPTX.</p>' + notes
        if sid in layout:
            tree = ET.fromstring('<root>' + re.sub(r'<img([^>]*?)(?<!/)>', r'<img\1/>', body) + '</root>')
            bodies = []
            for page in layout[sid]:
                cards = []
                for fragment in page:
                    original = tree[fragment['card']]
                    block = ET.Element('div', dict(original.attrib))
                    block.set('style', 'grid-column:span 12')
                    for child in list(original)[fragment['start']:fragment['end']]:
                        block.append(deepcopy(child))
                    cards.append(ET.tostring(block, encoding='unicode', method='html'))
                bodies.append(''.join(cards))
        else:
            bodies = [body]
        for page_index, page_body in enumerate(bodies):
            page_sid = sid if page_index == 0 else f'{sid}-cont-{page_index+1}'
            page_attrs = attrs.replace(f'#{sid} ', f'#{page_sid} ')
            page_title = title if page_index == 0 else title + ' · продолжение'
            page_content = f'<div class="course-kicker">ЗАНЯТИЕ {slide["part"]} · {layer_ru.upper()}</div>{banner}<div class="course-grid">{page_body}</div>'
            level = heading_level if page_index == 0 else '##'
            qmd.append(f'\n{level} {page_title} {{{page_attrs}}}\n\n```{{=html}}\n{page_content}\n```\n\n::: {{.notes}}\n\n```{{=html}}\n{notes}\n```\n\n:::\n')
            trace.append([f'v2:{number}', page_sid, slide['layer'], 'split' if len(bodies)>1 else 'rewritten' if sid in replacement else 'kept', slide['xml']])
        reading_notes = f'<details class="speaker-notes" open><summary>Заметки докладчика</summary>{notes}</details>'
        if source_body:
            reading_notes += f'<details class="speaker-notes"><summary>Исходный материал слайда (для сверки редакций)</summary><div class="course-grid">{source_body}</div></details>'
        reading.append(f'<article id="{sid}" class="reading-slide"><h2>{number}. {html.escape(title)}</h2>{content}{reading_notes}</article>')
        registry.append({'source':f'v2:{number}', 'id':sid, 'part':slide['part'], 'title':title, 'layer':slide['layer']})
    reading.append('</body></html>')
    # Render every formula once; a malformed equation fails the build.
    rendered = json.loads(subprocess.check_output(['node', str(ROOT/'scripts/math.cjs')], input=json.dumps(list(MATH.values())).encode()))
    documents = ['\n'.join(qmd), '\n'.join(reading)]
    for key, markup in zip(MATH, rendered):
        documents = [d.replace(key, markup) for d in documents]
    (ROOT/'index.qmd').write_text(documents[0])
    (ROOT/'reading.html').write_text(documents[1].replace('href="#/', 'href="#'))
    (ROOT/'slides/registry.json').write_text(json.dumps(registry, ensure_ascii=False, indent=2)+'\n')
    with (ROOT/'wiki/traceability.csv').open('w', newline='') as f:
        w=csv.writer(f, lineterminator='\n'); w.writerow(['source_slide','html_id','layer','action','notes']); w.writerows(trace)
    review_text = '# Открытые вопросы первой редакции\n\nСтатус: требуют подтверждения автора. На соответствующих слайдах есть видимая отметка TODO. Проверка API агентом не заменяет подтверждение содержательных утверждений.\n\n'
    for number, sid, flags in reviews:
        review_text += f'## v2:{number} — {sid}\n\n' + '\n\n'.join(flags) + '\n\n'
    review_text += '## Числа и интерактивность\n\nПересчитаны основная симуляция и сравнение MI (2000 репликаций), а также одна иллюстративная выборка. Для неё используется фиксированный seed без подбора по результатам; её оценки не обязаны совпадать со средними по симуляции. Изменение заданного в исходных инструкциях способа выбора «типичной» выборки отмечено для обсуждения.\n\nЧисленные примеры LOCF, РКИ, зависимости от корреляции, EM и специальных случаев пока перенесены из PPTX с явной отметкой об отсутствии воспроизведения в R. Сами практические модули проверяются отдельно; один запуск не подтверждает опубликованное покрытие.\n\nВ первой версии используются статические иллюстрации. Виджеты W1–W13 остаются спецификациями следующей итерации; их реализация не заявлена.\n'
    review_text += '\n## Диагностика SMC-FCS в практике\n\nПолный прогон нелинейного примера (m = 20, 20 итераций, rjlimit = 100000) дал 27 отказов rejection sampling. Его оценка исключена из итоговой таблицы и JSON. Нужен отдельный разбор вычислительной устойчивости; увеличение лимита само по себе не объявляется решением. Пример Кокса при тех же настройках прошёл без таких отказов.\n'
    (ROOT/'wiki/Open-questions.md').write_text(review_text)
    (ROOT/'downloads').mkdir(exist_ok=True)
    practice_readme = '''# Практикум: анализ данных с пропусками

Три занятия. Откройте распакованную папку как рабочую директорию RStudio.

Установите зависимости один раз: `source("scripts/install_packages.R")`.
Запускайте одно занятие: `source("practice/practice_part1.R")`,
`source("practice/practice_part2.R")` или `source("practice/practice_part3.R")`.
Из терминала: `Rscript missing_data_practice.R 1` (также 2 или 3).

Для чтения и редактирования используйте файлы practice/. Общие функции — r/00_dgp.R.
Результаты и диагностические PDF появятся в outputs/.

Перед третьим занятием дополнительный бутстреп включается командой
`options(missingdata.bootstrap = TRUE, missingdata.nBoot = 200L)`.
SMC-FCS требует больше времени; его результаты полезно рассчитать до занятия.

static/data/ содержит предрасчёты. Основные симуляции обновляются командой
`source("scripts/precompute.R")`: 2000 повторов, фиксированные seed, MCSE.
Код не устанавливает пакеты автоматически и не загружает внешние данные.
Один запуск примера не доказывает корректность метода или номинальное покрытие.
'''
    (ROOT/'downloads/README-practice.md').write_text(practice_readme)
    with zipfile.ZipFile(ROOT/'downloads/missing-data-practice.zip','w',zipfile.ZIP_DEFLATED) as z:
        z.writestr('README.md', practice_readme)
        for pattern in ['missing_data_practice.R','practice/*.R','r/*.R','scripts/install_packages.R','scripts/precompute.R','static/data/*.json']:
            for p in ROOT.glob(pattern): z.write(p, p.relative_to(ROOT))
    print(f'Built {len(SLIDES)} slides; {len(MATH)} rendered equations; {len(reviews)} review markers.')


if __name__ == '__main__':
    build()
