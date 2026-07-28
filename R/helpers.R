read_control_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xlsm", "xls")) {
    sheets <- readxl::excel_sheets(path)
    target <- if ("CONTROLE" %in% toupper(sheets)) sheets[match("CONTROLE", toupper(sheets))] else sheets[1]
    x <- readxl::read_excel(path, sheet = target, .name_repair = "minimal")
    x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext == "csv") {
    x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  } else {
    stop("Formato de controle não suportado. Use .xlsx, .xlsm, .xls ou .csv.")
  }
  x
}

control_columns <- c(
  "URF", "IMPORTADOR", "EXPORTADOR", "MERCADORIA", "FATURA", "DI", "PROCESSO", "CRT",
  "TRANSPORTADORA", "DATA REGISTRO", "DATA LIBERAÇÃO", "ARQUIVO", "ATUALIZAÇÃO"
)

ensure_control_columns <- function(x) {
  for (nm in control_columns) if (!nm %in% names(x)) x[[nm]] <- NA
  x[, control_columns, drop = FALSE]
}

normalize_key <- function(x) {
  x <- toupper(trimws(as.character(x %||% "")))
  gsub("[^A-Z0-9]", "", x)
}

append_control_rows <- function(current, add) {
  current <- ensure_control_columns(current)
  add <- ensure_control_columns(add)
  if (!nrow(add)) return(list(control = current, added = integer(0), skipped = integer(0)))

  added <- integer(0)
  skipped <- integer(0)
  for (i in seq_len(nrow(add))) {
    crt <- normalize_key(add$CRT[i])
    fat <- normalize_key(add$FATURA[i])
    existing <- rep(FALSE, nrow(current))
    if (nrow(current)) {
      if (nzchar(crt)) existing <- existing | vapply(current$CRT, function(z) normalize_key(z) == crt, logical(1))
      if (nzchar(fat)) existing <- existing | vapply(current$FATURA, function(z) normalize_key(z) == fat, logical(1))
    }
    idx <- which(existing)
    if (length(idx)) {
      # Atualiza somente células vazias no registro já existente.
      j <- idx[1]
      for (nm in control_columns) {
        old <- current[[nm]][j]
        new <- add[[nm]][i]
        old_empty <- is.na(old) || !nzchar(trimws(as.character(old)))
        new_ok <- !is.na(new) && nzchar(trimws(as.character(new)))
        if (old_empty && new_ok) current[[nm]][j] <- new
      }
      skipped <- c(skipped, i)
    } else {
      current <- rbind(current, add[i, control_columns, drop = FALSE])
      added <- c(added, i)
    }
  }
  list(control = current, added = added, skipped = skipped)
}

safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  suppressWarnings(as.Date(x))
}

summary_kpis <- function(detail, mics, controle) {
  list(
    paginas = if (is.null(detail)) 0 else length(unique(detail$pagina_pdf)),
    mics = if (is.null(mics)) 0 else nrow(mics),
    revisar = if (is.null(mics) || nrow(mics) == 0) 0 else sum(mics$status_revisao == "Revisar", na.rm = TRUE),
    embarques = if (is.null(controle)) 0 else nrow(controle)
  )
}
