"""Make a single R file that can be used without a project checkout."""
from pathlib import Path
import json

ROOT = Path(__file__).resolve().parents[1]
header = '''# АНАЛИЗ ДАННЫХ С ПРОПУСКАМИ — практикум, редакция 0.1.
# Самостоятельный файл: другие файлы проекта для запуска не нужны.
# В RStudio: выберите lesson ниже (1, 2, 3) и выполняйте блоки по порядку.
# Из терминала: Rscript missing_data_practice.R 1
# Зависимости: install.packages(c("MASS", "mice", "naniar", "ggplot2",
#   "smcfcs", "mitools", "survival", "MatchThem", "survey", "cobalt"))
# Для дополнительного бутстрепа: install.packages("bootImpute")
# options(missingdata.bootstrap = TRUE)  # 200 бутстреп-выборок × 2 импутации
args <- commandArgs(trailingOnly = TRUE)
lesson <- if (length(args)) args[1] else "1"
if (!lesson %in% c("1", "2", "3", "all")) stop("Занятие: 1, 2, 3 или all")
options(missingdata.S = 300L, missingdata.m = 20L, missingdata.maxit = 20L)

'''


def main():
    result = header + (ROOT/'r/00_dgp.R').read_text() + '\n'
    for i in range(1, 4):
        text = (ROOT/f'practice/practice_part{i}.R').read_text()
        text = text.replace('source("r/00_dgp.R")\n', '')
        if i == 2:
            data = json.loads((ROOT/'static/data/mi_coverage.json').read_text())['summary']
            columns = []
            for key in ['method','term','coverage','mcse_coverage','fmi']:
                values = [json.dumps(row[key], ensure_ascii=False) if row[key] is not None else 'NA_real_' for row in data]
                columns.append(key+' = c('+', '.join(values)+')')
            start = text.index('if (file.exists("static/data/mi_coverage.json"))')
            end = text.index('\n# ЗАДАНИЯ', start)
            text = text[:start] + '# Предрасчёт R: 2000 репликаций, n = 500; включён в файл.\ncoverage <- data.frame(\n' + ',\n'.join(columns) + ')\nprint(coverage)\n' + text[end:]
        result += f'\nif (lesson %in% c("{i}", "all")) {{\n' + text + '\n}\n'
    (ROOT/'downloads').mkdir(exist_ok=True)
    (ROOT/'downloads/missing_data_practice.R').write_text(result)
    print('Built standalone R file for all three lessons.')


if __name__ == '__main__':
    main()
