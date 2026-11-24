// =============================================================================
//  Material — тема презентаций для Quarto/Typst
//  Порт beamer-темы materialbeamer (Lorenzo Lancia) на Typst.
//
//  Файл подключается Quarto как template-partial и определяет функцию `slides`,
//  которую вызывает typst-show.typ.
// =============================================================================

// --- служебные метки -------------------------------------------------------
// <mat-slide>      — вид и заголовок слайда, ставится show-правилами заголовков
// <mat-slide-opts> — классы слайда ({.plain}, {.inverse}, …), ставится Lua-фильтром

#let mat-slide-mark(kind: "slide", title: none) = [
  #metadata((kind: kind, title: title))<mat-slide>
]

#let mat-slide-opts(..classes) = [
  #metadata((classes: classes.pos()))<mat-slide-opts>
]

// Roboto Condensed получается из переменного Roboto по оси wdth.
#let cond(body, ..args) = text(stretch: 75%, ..args, body)

// Цвет из YAML: pandoc экранирует «#», поэтому принимаем и "#2196F3", и "2196F3".
#let mat-color(value) = {
  if type(value) == color { return value }
  rgb("#" + str(value).replace("\\", "").replace("#", ""))
}

// Палитра темы, чтобы её видели функции верхнего уровня (#mat-block, callout).
#let mat-conf = state("mat-conf", (
  accent: rgb("#2196F3"),
  accent-dark: rgb("#1976D2"),
  body-fill: rgb("#F0F0F0"),
))

// Блок в стиле beamer-block / tcolorbox: цветная шапка + светлое тело с тенью.
// Определяется на верхнем уровне, чтобы его мог звать Lua-фильтр (#mat-block).
#let mat-block(title: none, accent: auto, body) = context {
  let conf = mat-conf.get()
  let accent = if accent == auto { conf.accent } else { accent }
  let shadow = 1mm

  let inner = {
    if title != none {
      block(width: 100%, fill: accent, inset: (x: 0.6em, y: 0.42em), above: 0pt, below: 0pt,
        text(fill: white, size: 1.05em, cond(title)))
    }
    block(width: 100%, fill: conf.body-fill, inset: (x: 0.6em, y: 0.6em), above: 0pt, below: 0pt,
      body)
  }

  // Тень рисуем прямоугольником под коробкой, поэтому нужна её высота.
  layout(size => {
    let h = measure(box(width: size.width, inner)).height
    block(width: 100%, height: h, breakable: false, {
      place(top + left, dx: shadow, dy: shadow,
        rect(width: 100%, height: 100%, fill: luma(0).transparentize(87%), stroke: none))
      place(top + left, inner)
    })
  })
}

// Слайд без рамки: {.plain} — только содержимое, {.inverse} — ещё и заливка темой.
#let mat-plain-slide(title: none, inverse: false, body) = context {
  let conf = mat-conf.get()
  page(margin: (x: 16mm, y: 14mm), {
    mat-slide-mark(kind: "plain")
    if inverse { mat-slide-opts("plain", "inverse") } else { mat-slide-opts("plain") }
    set text(fill: if inverse { white } else { black })
    show heading: it => it.body
    v(1fr)
    align(center + horizon, {
      if title != none {
        text(size: 2.2em, fill: if inverse { white } else { conf.accent-dark }, cond(title))
        v(0.6em)
      }
      body
    })
    v(1fr)
  })
}

// Переопределяет quarto-шный #callout из definitions.typ: коллауты рисуются
// как material-блоки, цвет шапки берётся из типа коллаута.
#let callout(
  body: [],
  title: "Callout",
  background_color: none,
  icon: none,
  icon_color: rgb("#2196F3"),
  body_background_color: none,
) = context {
  let accent = if lower(icon_color.to-hex()) == "#0758e5" {
    mat-conf.get().accent   // note → основной цвет темы
  } else { icon_color }
  mat-block(title: title, accent: accent, body)
}

// Тело слайда с классами вёрстки ({.center}, {.smaller}, {.larger}).
#let mat-vbox(vcenter: false, size: 1em, align-x: left, body) = {
  set text(size: size)
  set align(align-x)
  if vcenter {
    block(width: 100%, height: 100%, {
      v(1fr)
      body
      v(1fr)
    })
  } else {
    body
  }
}

// Колонки: `::: {.columns}` / `::: {.column width="40%"}`
// Ширины переводятся во fr, поэтому промежуток между колонками не ломает вёрстку.
#let mat-columns(gutter: 1.2em, valign: top, ..cols) = {
  let items = cols.pos()
  let widths = items.map(c => c.at("width", default: none))
  let sum-known = widths.filter(w => w != none).fold(0.0, (a, b) => a + b / 1%)
  let n-auto = widths.filter(w => w == none).len()
  let rest = calc.max(0.0, 100.0 - sum-known)
  let to-fr(w) = if w == none { rest / calc.max(1, n-auto) * 1fr } else { w / 1% * 1fr }
  grid(
    columns: widths.map(to-fr),
    column-gutter: gutter,
    align: valign,
    ..items.map(c => c.body),
  )
}

// =============================================================================
#let slides(
  // --- метаданные документа
  title: none,
  subtitle: none,
  authors: none,
  institute: none,
  date: none,
  short-title: none,
  short-author: none,
  logo: none,
  // --- геометрия
  aspect-ratio: "16-9",
  // --- типографика
  font: ("Roboto", "Liberation Sans", "DejaVu Sans"),
  codefont: ("Roboto Mono", "DejaVu Sans Mono"),
  fontsize: 20pt,
  lang: "ru",
  region: "RU",
  // --- палитра
  theme-color: rgb("#2196F3"),
  theme-color-dark: rgb("#1976D2"),
  theme-color-light: rgb("#BBDEFB"),
  block-color: rgb("#F0F0F0"),
  // --- поведение
  title-slide: true,
  section-slides: true,
  slide-toc: false,
  toc-title: "Содержание",
  slide-number: true,
  navigation: true,
  doc,
) = {
  // ---------------------------------------------------------------- размеры
  let nav-h = 7.5mm
  let title-h = 14mm
  let shade-h = 1.8mm
  let foot-h = 8mm
  let mx = 14mm
  // подписи для рамки считаем до размеров: от них зависит высота колонтитула
  let short-title = if short-title != none { short-title } else { title }
  let short-author = if short-author != none { short-author } else {
    if authors != none and authors != () {
      authors.map(a => a.name).join(", ")
    } else { none }
  }

  let nav-total = if navigation { nav-h } else { 0mm }
  let foot-total = if slide-number or short-author != none { foot-h } else { 0mm }

  mat-conf.update((
    accent: theme-color,
    accent-dark: theme-color-dark,
    body-fill: block-color,
  ))

  set document(
    title: if title != none { content-to-string(title) } else { none },
    author: if short-author != none { content-to-string(short-author) } else { "" },
  )

  // ------------------------------------------------------------------ текст
  set text(font: font, size: fontsize, lang: lang, region: region)
  set par(justify: false, leading: 0.65em, spacing: 0.9em)
  show raw: set text(font: codefont, size: 0.8em)
  show link: set text(fill: theme-color-dark)

  set list(marker: text(fill: theme-color, size: 0.9em)[•], indent: 0.6em, spacing: 0.75em)
  set enum(indent: 0.6em, spacing: 0.75em, numbering: n => text(fill: theme-color, [#n.]))
  set terms(separator: [ — ])

  show strong: set text(fill: theme-color-dark)
  show emph: set text(style: "italic")

  set table(inset: 6pt, stroke: (x, y) => if y == 0 { (bottom: 0.6pt + theme-color-dark) })
  show table.cell.where(y: 0): set text(fill: theme-color-dark, weight: "bold")

  show figure.caption: set text(size: 0.7em, fill: luma(70))
  set figure(gap: 0.6em)

  show raw.where(block: true): set block(
    fill: block-color, width: 100%, inset: 8pt, radius: 0pt,
  )
  show quote.where(block: true): it => block(
    width: 100%, inset: (left: 0.8em, y: 0.3em),
    stroke: (left: 2.5pt + theme-color-light), it.body,
  )

  // ------------------------------------------------------- рамка слайда
  let bar(h, fill-color, ..args) = block(
    width: 100%, fill: fill-color, inset: (x: mx), above: 0pt, below: 0pt,
    grid(rows: (h,), ..args),
  )

  let shade = rect(
    width: 100%, height: shade-h, stroke: none,
    fill: gradient.linear(luma(185), white, angle: 90deg),
  )

  let chrome = context {
    // Фон страницы наследует стиль текста с места разрыва, поэтому размеры
    // рамки задаём заново: иначе {.smaller} ужимал бы и навигацию с футером.
    set text(size: fontsize, font: font, fill: black, stretch: 100%)

    let pn = here().page()
    let total = counter(page).final().first()

    let marks = query(<mat-slide>).filter(m => m.location().page() <= pn)
    let info = if marks.len() > 0 { marks.last().value } else {
      (kind: "title", title: none)
    }
    let opts = query(<mat-slide-opts>).filter(m => m.location().page() == pn)
    let classes = if opts.len() > 0 { opts.last().value.classes } else { () }

    let is-plain = "plain" in classes
    let is-inverse = "inverse" in classes

    if is-inverse {
      place(top + left, rect(width: 100%, height: 100%, fill: theme-color, stroke: none))
    }

    if not is-plain {
      // ---- верхняя навигация
      if navigation {
        let sections = query(heading.where(level: 1))
        let passed = sections.filter(h => h.location().page() <= pn)
        let cur = if passed.len() > 0 { passed.len() - 1 } else { -1 }
        place(top + left, bar(nav-h, theme-color-dark,
          columns: (auto, 1fr),
          align: (horizon + left, horizon + right),
          text(size: fontsize * 0.5, fill: white, weight: "bold", cond(short-title)),
          {
            set text(size: fontsize * 0.46)
            stack(dir: ltr, spacing: 1.1em, ..sections.enumerate().map(((i, h)) => {
              let active = i == cur
              text(
                fill: if active { white } else { theme-color-light.transparentize(15%) },
                weight: if active { "bold" } else { "regular" },
                cond(h.body),
              )
            }))
          },
        ))
      }

      // ---- полоса заголовка слайда
      let has-title-bar = info.kind == "slide" and info.at("title", default: none) != none
      if has-title-bar {
        place(top + left, dy: nav-total, bar(title-h, theme-color,
          align: horizon + left,
          text(size: fontsize * 1.25, fill: white, cond(info.title)),
        ))
      }

      // ---- градиентная «тень» под верхними полосами
      if not is-inverse {
        place(top + left, dy: nav-total + if has-title-bar { title-h } else { 0mm }, shade)
      }

      // ---- нижний колонтитул
      if foot-total > 0mm {
        place(bottom + left, bar(foot-h, theme-color-dark,
          columns: (1fr, auto),
          align: (horizon + left, horizon + right),
          text(size: fontsize * 0.45, fill: white,
            if short-author != none { short-author } else { [] }),
          text(size: fontsize * 0.45, fill: white,
            if slide-number { [#pn / #total] } else { [] }),
        ))
      }
    }
  }

  set page(
    paper: if aspect-ratio == "4-3" { "presentation-4-3" } else { "presentation-16-9" },
    margin: (
      top: nav-total + title-h + 8mm,
      bottom: foot-total + 5mm,
      x: mx,
    ),
    numbering: none,
    background: chrome,
    header: none,
    footer: none,
  )

  // --------------------------------------------------------- титульный слайд
  if title-slide and (title != none or authors != none) {
    page(margin: (top: nav-total, bottom: foot-total, x: 0mm), {
      mat-slide-mark(kind: "title")
      v(2fr)
      block(width: 100%, inset: (x: mx), align(right, {
        if authors != none and authors != () {
          text(size: 1.1em, authors.map(a => a.name).join(", ", last: " & "))
          linebreak()
        }
        set text(size: 0.72em)
        cond({
          if institute != none and institute != () {
            institute.join(linebreak())
            linebreak()
          }
          if date != none { date }
        })
      }))
      v(1.2fr)
      block(width: 100%, fill: theme-color, inset: (x: mx, y: 13mm), {
        grid(columns: if logo != none { (1fr, auto) } else { (1fr,) }, column-gutter: 1em,
          align: horizon,
          {
            text(size: 2.4em, fill: white, title)
            if subtitle != none {
              linebreak()
              v(0.1em)
              text(size: 1.05em, fill: white, cond(subtitle))
            }
          },
          ..if logo != none { (logo,) } else { () },
        )
      })
      v(0.7fr)
    })
  }

  // ------------------------------------------------------------ слайд-оглавление
  if slide-toc {
    page({
      mat-slide-mark(kind: "slide", title: toc-title)
      context {
        let sections = query(heading.where(level: 1))
        v(0.5fr)
        for h in sections {
          block(above: 0.9em, below: 0.9em,
            link(h.location(), text(size: 1.05em, fill: theme-color-dark, h.body)))
        }
        v(1fr)
      }
    })
  }

  // ------------------------------------------------------------ заголовки
  show heading.where(level: 1): it => {
    if section-slides {
      pagebreak(weak: true)
      mat-slide-mark(kind: "section")
      v(1fr)
      align(center, {
        text(size: 1.8em, fill: theme-color-dark, cond(it.body))
        v(0.4em)
        line(length: 22%, stroke: 2.5pt + theme-color)
      })
      v(1.3fr)
      pagebreak(weak: true)
    } else {
      mat-slide-mark(kind: "section")
    }
  }

  show heading.where(level: 2): it => {
    pagebreak(weak: true)
    mat-slide-mark(kind: "slide", title: it.body)
  }

  show heading.where(level: 3): it => block(
    above: 0.9em, below: 0.5em,
    text(size: 1.02em, fill: theme-color-dark, weight: "medium", cond(it.body)),
  )
  show heading.where(level: 4): it => block(
    above: 0.8em, below: 0.4em,
    text(size: 0.95em, fill: luma(60), weight: "bold", it.body),
  )

  set heading(numbering: none, outlined: false)

  doc
}
