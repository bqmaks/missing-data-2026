# Установка выполняется явно пользователем, а не при запуске занятия.
packages <- c("MASS", "dplyr", "tidyr", "ggplot2", "mice", "naniar",
              "jsonlite", "smcfcs", "mitools", "survival", "bootImpute",
              "MatchThem", "survey", "cobalt")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("Зависимости практикума готовы.\n")
