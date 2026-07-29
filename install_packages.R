pkgs <- c("shiny","bslib","DT","pdftools","httr2","jsonlite","base64enc","readxl","openxlsx","dplyr","tidyr","stringr","purrr","htmltools","DBI","RPostgres")
new <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if(length(new)) install.packages(new)
