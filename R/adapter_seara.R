cell_value <- function(raw, row, col) {
  if (nrow(raw) < row || ncol(raw) < col) return(NA_character_)
  v <- raw[[col]][row]
  if (length(v) == 0 || is.na(v)) return(NA_character_)
  trimws(as.character(v))
}

infer_urf_filename <- function(filename) {
  stem <- tools::file_path_sans_ext(basename(filename))
  parts <- strsplit(stem, " - ", fixed=TRUE)[[1]]
  if (length(parts) < 2) return(NA_character_)
  toupper(trimws(tail(parts,1)))
}

parse_seara_report <- function(path, filename=basename(path)) {
  sheets <- readxl::excel_sheets(path)
  raw <- readxl::read_excel(path, sheet=sheets[1], col_names=FALSE, .name_repair="minimal")
  raw <- as.data.frame(raw, stringsAsFactors=FALSE, check.names=FALSE)
  meta <- data.frame(
    ARQUIVO=filename,
    URF=infer_urf_filename(filename),
    IMPORTADOR=cell_value(raw,5,2),
    EXPORTADOR=cell_value(raw,5,6),
    MERCADORIA=cell_value(raw,6,2),
    FATURA=cell_value(raw,6,6),
    DI=cell_value(raw,7,2),
    PROCESSO=cell_value(raw,7,6),
    CRT=cell_value(raw,8,6),
    TRANSPORTADORA=cell_value(raw,9,2),
    DAT=cell_value(raw,9,6),
    `DATA LIBERAÇÃO`=cell_value(raw,11,2),
    `DATA REGISTRO`=cell_value(raw,11,5),
    PESO_PREVISTO=cell_value(raw,19,5),
    stringsAsFactors=FALSE,
    check.names=FALSE
  )
  start <- 20L
  if (nrow(raw) >= start && ncol(raw) >= 8) {
    tab <- raw[start:nrow(raw),1:8,drop=FALSE]
    names(tab) <- c("DATA_EMISSAO","MIC_DTA","CAVALO","CARRETA","PESO_LIQUIDO","VALOR","SEQUENCIA","NOTA")
    keep <- apply(tab,1,function(z) any(!is.na(z) & nzchar(trimws(as.character(z)))))
    tab <- tab[keep,,drop=FALSE]
  } else tab <- data.frame()
  list(meta=meta, loads=tab, sheet=sheets[1])
}

seara_to_control <- function(parsed) {
  m <- parsed$meta
  ensure_control_columns(data.frame(
    URF=m$URF, IMPORTADOR=m$IMPORTADOR, EXPORTADOR=m$EXPORTADOR, MERCADORIA=m$MERCADORIA,
    FATURA=m$FATURA, DI=m$DI, PROCESSO=m$PROCESSO, CRT=m$CRT, TRANSPORTADORA=m$TRANSPORTADORA,
    `DATA REGISTRO`=m$`DATA REGISTRO`, `DATA LIBERAÇÃO`=m$`DATA LIBERAÇÃO`, ARQUIVO=m$ARQUIVO,
    ATUALIZAÇÃO=as.character(Sys.time()), check.names=FALSE, stringsAsFactors=FALSE
  ))
}
