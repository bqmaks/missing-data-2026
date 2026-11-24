// Мост между YAML-метаданными Quarto и функцией slides() из typst-template.typ.
// Булевы опции печатаются всегда: значения по умолчанию заданы в _extension.yml,
// иначе `false` от пользователя нельзя отличить от «не задано».
#show: doc => slides(
$if(title)$
  title: [$title$],
$endif$
$if(subtitle)$
  subtitle: [$subtitle$],
$endif$
$if(by-author)$
  authors: (
$for(by-author)$
$if(it.name.literal)$
    ( name: [$it.name.literal$] ),
$endif$
$endfor$
  ),
$endif$
$if(institute)$
  institute: ($for(institute)$[$institute$],$endfor$),
$endif$
$if(date)$
  date: [$date$],
$endif$
$if(short-title)$
  short-title: [$short-title$],
$endif$
$if(short-author)$
  short-author: [$short-author$],
$endif$
$if(title-logo)$
  logo: image("$title-logo$", width: $if(title-logo-width)$$title-logo-width$$else$4cm$endif$),
$endif$
$if(aspect-ratio)$
  aspect-ratio: "$aspect-ratio$",
$endif$
$if(mainfont)$
  font: ("$mainfont$", "Liberation Sans", "DejaVu Sans"),
$endif$
$if(monofont)$
  codefont: ("$monofont$", "DejaVu Sans Mono"),
$endif$
$if(fontsize)$
  fontsize: $fontsize$,
$endif$
$if(lang)$
  lang: "$lang$",
$endif$
$if(region)$
  region: "$region$",
$endif$
$if(theme-color)$
  theme-color: mat-color("$theme-color$"),
$endif$
$if(theme-color-dark)$
  theme-color-dark: mat-color("$theme-color-dark$"),
$endif$
$if(theme-color-light)$
  theme-color-light: mat-color("$theme-color-light$"),
$endif$
$if(block-color)$
  block-color: mat-color("$block-color$"),
$endif$
$if(toc-title)$
  toc-title: [$toc-title$],
$endif$
  title-slide: $if(title-slide)$true$else$false$endif$,
  section-slides: $if(section-slides)$true$else$false$endif$,
  slide-toc: $if(slide-toc)$true$else$false$endif$,
  slide-number: $if(slide-number)$true$else$false$endif$,
  navigation: $if(navigation)$true$else$false$endif$,
  doc,
)
