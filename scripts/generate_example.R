# Запуск из корня проекта: Rscript scripts/generate_example.R
source('r/example.R')
source('r/advanced.R')
example <- make_example()
export_example(example)
export_advanced(make_advanced(example))
print(example$estimates, row.names = FALSE, digits = 5)
cat('\nДанные и оценки сохранены в data/. Seeds: см. data/metadata.R\n')
