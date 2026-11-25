# MathJax для локальных слайдов

Версия 2.7.9, соответствующая MathJax 2 API (`MathJax.Hub`) в установленном
revealjs-шаблоне Quarto 1.10.18. Источник:
https://registry.npmjs.org/mathjax/-/mathjax-2.7.9.tgz

Скопированы MathJax.js, TeX-AMS_HTML-full, extensions, input jax,
HTML-CSS output с TeX font data и WOFF-шрифтами. Другие output jax и семейства
шрифтов удалены. Это ресурсы для используемого TeX/HTML-CSS пути, не полная
универсальная поставка MathJax. Лицензия: `licenses/MathJax-Apache-2.0.txt`.

Quarto копирует каталог в `_site/`. CDN при показе слайдов не нужен.
