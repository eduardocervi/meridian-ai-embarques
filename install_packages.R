packages <- c(
  "shiny", "bslib", "DT", "pdftools", "httr2", "jsonlite", "base64enc",
  "readxl", "openxlsx", "dplyr", "tidyr", "stringr", "purrr", "htmltools"
)
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

# req_perform_parallel() está disponível nas versões atuais do httr2.
# Se a instalação for antiga, tente atualizar para aproveitar a extração simultânea.
if (requireNamespace("httr2", quietly = TRUE) && packageVersion("httr2") < "1.2.0") {
  message("Atualizando httr2 para habilitar processamento paralelo...")
  install.packages("httr2", repos = "https://cloud.r-project.org")
}
message("Pacotes prontos.")
