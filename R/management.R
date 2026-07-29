`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

as_num_br <- function(x) {
  x <- as.character(x)
  x <- gsub("[^0-9,.-]", "", x)
  both <- grepl(",", x) & grepl("\\.", x)
  x[both] <- gsub("\\.", "", x[both])
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

parse_flexible_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- trimws(as.character(x))
  out <- suppressWarnings(as.Date(x))
  miss <- is.na(out)
  if (any(miss)) out[miss] <- suppressWarnings(as.Date(x[miss], format = "%d/%m/%Y"))
  out
}

processes_from_control <- function(control) {
  if (is.null(control) || !nrow(control)) return(data.frame())
  d <- ensure_control_columns(control)
  d$process_key <- ifelse(!is.na(d$PROCESSO) & nzchar(trimws(as.character(d$PROCESSO))), as.character(d$PROCESSO),
                          ifelse(!is.na(d$CRT) & nzchar(trimws(as.character(d$CRT))), as.character(d$CRT), as.character(d$FATURA)))
  d$client <- ifelse(!is.na(d$IMPORTADOR) & nzchar(trimws(as.character(d$IMPORTADOR))), d$IMPORTADOR, "Não identificado")
  d$status <- ifelse(!is.na(d$`DATA LIBERAÇÃO`) & nzchar(trimws(as.character(d$`DATA LIBERAÇÃO`))), "FINALIZADO",
                     ifelse(!is.na(d$CRT) & nzchar(trimws(as.character(d$CRT))), "PARCIAL", "EM ABERTO"))
  d$updated <- suppressWarnings(as.POSIXct(d$ATUALIZAÇÃO))
  d$updated[is.na(d$updated)] <- Sys.time()
  data.frame(
    PROCESSO = d$process_key,
    CLIENTE = d$client,
    IMPORTADOR = d$IMPORTADOR,
    EXPORTADOR = d$EXPORTADOR,
    FATURA = d$FATURA,
    DI = d$DI,
    CRT = d$CRT,
    PRODUTO = d$MERCADORIA,
    URF = d$URF,
    TRANSPORTADORA = d$TRANSPORTADORA,
    STATUS = d$status,
    ATUALIZADO_EM = d$updated,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

loads_from_mics <- function(mics) {
  if (is.null(mics) || !nrow(mics)) return(data.frame())
  gv <- function(row, n) as.character(row[[sprintf("item_%02d", n)]])
  rows <- lapply(seq_len(nrow(mics)), function(i) {
    r <- mics[i,,drop=FALSE]
    data.frame(
      MIC_DTA = r$mic_id,
      CRT = gv(r,23),
      FATURA = extract_invoice(gv(r,36)),
      EMISSAO = gv(r,6),
      TRANSPORTADORA = clean_transportadora(gv(r,1)),
      URF = infer_urf(gv(r,24)),
      PESO_BRUTO = as_num_br(gv(r,32)),
      MERCADORIA = gv(r,38),
      STATUS_REVISAO = r$status_revisao,
      stringsAsFactors=FALSE,
      check.names=FALSE
    )
  })
  do.call(rbind, rows)
}

pending_from_control <- function(control) {
  if (is.null(control) || !nrow(control)) return(data.frame())
  d <- ensure_control_columns(control)
  out <- list(); n <- 0L
  add <- function(i, tipo, prioridade, detalhe) {
    n <<- n + 1L
    out[[n]] <<- data.frame(PRIORIDADE=prioridade, TIPO=tipo, PROCESSO=as.character(d$PROCESSO[i]), FATURA=as.character(d$FATURA[i]), CRT=as.character(d$CRT[i]), DETALHE=detalhe, stringsAsFactors=FALSE)
  }
  for (i in seq_len(nrow(d))) {
    if (is.na(d$PROCESSO[i]) || !nzchar(trimws(as.character(d$PROCESSO[i])))) add(i,"Processo não identificado","Alta","Preencher ou validar o número do processo.")
    if (is.na(d$DI[i]) || !nzchar(trimws(as.character(d$DI[i])))) add(i,"DI pendente","Média","DI ainda não informada no controle.")
    if (is.na(d$URF[i]) || !nzchar(trimws(as.character(d$URF[i])))) add(i,"URF não identificada","Alta","Associar a URF/aduana ao embarque.")
    if (is.na(d$FATURA[i]) || !nzchar(trimws(as.character(d$FATURA[i])))) add(i,"Fatura não identificada","Crítica","Documento precisa de revisão.")
  }
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

movement_summary <- function(control) {
  if (is.null(control) || !nrow(control)) return(data.frame())
  d <- ensure_control_columns(control)
  ts <- suppressWarnings(as.POSIXct(d$ATUALIZAÇÃO))
  ts[is.na(ts)] <- Sys.time()
  data.frame(
    DATA = as.Date(ts),
    CLIENTE = ifelse(is.na(d$IMPORTADOR) | !nzchar(trimws(as.character(d$IMPORTADOR))), "Não identificado", d$IMPORTADOR),
    URF = d$URF,
    PROCESSO = d$PROCESSO,
    FATURA = d$FATURA,
    CRT = d$CRT,
    TRANSPORTADORA = d$TRANSPORTADORA,
    EVENTO = "REGISTRO_ATUALIZADO",
    ATUALIZADO_EM = ts,
    stringsAsFactors=FALSE,
    check.names=FALSE
  )
}

fmt_int <- function(x) format(as.integer(x %||% 0), big.mark=".", decimal.mark=",", scientific=FALSE)
