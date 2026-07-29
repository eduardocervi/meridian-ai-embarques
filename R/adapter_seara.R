# Adapter inteligente para relatórios operacionais SEARA
# V4.4: a IA interpreta semanticamente o layout da planilha. O R lê os valores e
# executa a consolidação; não dependemos de coordenadas fixas como B5/F6.

cell_value <- function(raw, row, col) {
  if (is.null(row) || is.null(col) || is.na(row) || is.na(col)) return(NA_character_)
  row <- as.integer(row); col <- as.integer(col)
  if (row < 1 || col < 1 || nrow(raw) < row || ncol(raw) < col) return(NA_character_)
  v <- raw[[col]][row]
  if (length(v) == 0 || is.na(v)) return(NA_character_)
  trimws(as.character(v))
}

excel_col_name <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 1) return(NA_character_)
  out <- ""
  while (n > 0) {
    r <- (n - 1) %% 26
    out <- paste0(LETTERS[r + 1], out)
    n <- (n - 1) %/% 26
  }
  out
}

excel_ref <- function(row, col) {
  if (is.null(row) || is.null(col) || is.na(row) || is.na(col)) return(NA_character_)
  paste0(excel_col_name(col), as.integer(row))
}

infer_urf_filename <- function(filename) {
  stem <- tools::file_path_sans_ext(basename(filename))
  parts <- strsplit(stem, " - ", fixed=TRUE)[[1]]
  if (length(parts) < 2) return(NA_character_)
  toupper(trimws(tail(parts,1)))
}

# Compacta somente células preenchidas e mantém suas coordenadas. A IA passa a
# entender rótulos/valores pela semântica, em vez de assumir posições fixas.
spreadsheet_grid_text <- function(raw, max_rows = 45L, max_cols = 14L) {
  nr <- min(nrow(raw), as.integer(max_rows))
  nc <- min(ncol(raw), as.integer(max_cols))
  if (nr < 1 || nc < 1) return("(planilha vazia)")
  lines <- character(0)
  for (r in seq_len(nr)) {
    vals <- character(0)
    for (c in seq_len(nc)) {
      v <- cell_value(raw, r, c)
      if (!is.na(v) && nzchar(v)) {
        v <- gsub("[\r\n]+", " ", v)
        v <- gsub("\\s+", " ", v)
        if (nchar(v) > 240) v <- paste0(substr(v, 1, 237), "...")
        vals <- c(vals, paste0(excel_ref(r,c), " = ", v))
      }
    }
    if (length(vals)) lines <- c(lines, paste(vals, collapse = " | "))
  }
  paste(lines, collapse = "\n")
}



# -----------------------------------------------------------------------------
# V4.6 - leitura semântica híbrida
# Primeiro localizamos rótulos no próprio Excel, sem coordenadas fixas.
# A IA é usada como fallback para campos não encontrados/ambíguos e para layouts
# desconhecidos. Isso reduz custo, tempo e o risco de deslocar campos vizinhos.
# -----------------------------------------------------------------------------
normalize_label <- function(x) {
  x <- trimws(toupper(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- gsub("[[:space:]\\r\\n]+", " ", x)
  x <- gsub("[^A-Z0-9/ ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

nonempty_cell <- function(raw, r, c) {
  v <- cell_value(raw, r, c)
  !is.na(v) && nzchar(trimws(v))
}

# Procura o valor associado a um rótulo. Não assume B5/F6 etc.; procura o
# rótulo em qualquer posição e prioriza células à direita, depois abaixo.
find_labeled_value <- function(raw, aliases, search_rows = NULL, search_cols = NULL) {
  if (is.null(search_rows)) search_rows <- seq_len(nrow(raw))
  if (is.null(search_cols)) search_cols <- seq_len(ncol(raw))
  aliases_n <- normalize_label(aliases)
  hits <- list()
  k <- 0L
  for (r in search_rows) for (c in search_cols) {
    lab <- normalize_label(cell_value(raw, r, c))
    if (!nzchar(lab)) next
    exact <- lab %in% aliases_n
    prefix <- any(vapply(aliases_n, function(a) nzchar(a) && (startsWith(lab, paste0(a," ")) || startsWith(lab, paste0(a,":"))), logical(1)))
    if (!exact && !prefix) next
    candidates <- list(
      c(r,c+1), c(r,c+2), c(r+1,c), c(r+1,c+1), c(r,c+3)
    )
    val <- NA_character_; vr <- NA_integer_; vc <- NA_integer_
    for (xy in candidates) {
      rr <- xy[1]; cc <- xy[2]
      if (rr>=1 && rr<=nrow(raw) && cc>=1 && cc<=ncol(raw) && nonempty_cell(raw,rr,cc)) {
        vv <- cell_value(raw,rr,cc)
        # não aceitar outro rótulo conhecido como valor
        if (normalize_label(vv) %in% aliases_n) next
        val <- vv; vr <- rr; vc <- cc; break
      }
    }
    if (!is.na(val) && nzchar(val)) {
      k <- k+1L
      hits[[k]] <- list(value=val, cell=excel_ref(vr,vc), label_cell=excel_ref(r,c), label=cell_value(raw,r,c))
    }
  }
  if (!length(hits)) return(NULL)
  hits[[1]]
}

semantic_field_dictionary <- function() list(
  importador=c("IMPORTADOR","IMPORTADOR/CONSIGNATARIO","CONSIGNATARIO"),
  exportador=c("EXPORTADOR","EXPORTADOR/REMETENTE","REMETENTE"),
  mercadoria=c("MERCADORIA","MERCADORIA/PRODUTO","PRODUTO","DESCRICAO DA MERCADORIA"),
  fatura=c("FATURA","FACTURA","FATURA COMERCIAL","FACTURA COMERCIAL"),
  di=c("DI","DECLARACAO DE IMPORTACAO","DECLARACAO IMPORTACAO"),
  processo=c("PROCESSO","PROCESSO / IMP","PROCESSO/IMP","IMP","PROCESSO IMP"),
  crt=c("CRT","CARTA DE PORTE","CARTA PORTE"),
  transportadora=c("TRANSPORTADORA","TRANSPORTADOR","EMPRESA TRANSPORTADORA"),
  dat=c("DAT"),
  data_liberacao=c("DATA LIBERACAO","DATA DE LIBERACAO","LIBERACAO"),
  data_registro=c("DATA REGISTRO","DATA DE REGISTRO","REGISTRO"),
  peso_previsto=c("PESO PREVISTO","PESO TOTAL PREVISTO","PESO PROCESSO","PESO TOTAL")
)

extract_fields_by_labels <- function(raw) {
  dict <- semantic_field_dictionary()
  out <- list()
  for (nm in names(dict)) {
    hit <- find_labeled_value(raw, dict[[nm]])
    if (is.null(hit)) out[[nm]] <- list(value=NULL, cell=NULL, confidence="low")
    else out[[nm]] <- list(value=hit$value, cell=hit$cell, confidence="high")
  }
  out
}

# Detecta a tabela de cargas por títulos, sem assumir linha ou coluna.
detect_load_table_by_headers <- function(raw) {
  canon <- list(
    data_emissao=c("DATA EMISSAO","DATA DE EMISSAO","EMISSAO"),
    mic_dta=c("MIC/DTA","MIC DTA","MIC","DTA"),
    cavalo=c("CAVALO","PLACA CAVALO","TRATOR","PLACA TRATOR"),
    carreta=c("CARRETA","PLACA CARRETA","REBOQUE","PLACA REBOQUE"),
    peso_liquido=c("PESO LIQUIDO","PESO LIQ","PESO"),
    valor=c("VALOR"),
    sequencia=c("SEQUENCIA","SEQ"),
    nota=c("NOTA","NF","NOTA FISCAL")
  )
  best <- NULL; best_n <- 0L
  for (r in seq_len(nrow(raw))) {
    cols <- setNames(as.list(rep(NA_integer_, length(canon))), names(canon))
    for (c in seq_len(ncol(raw))) {
      txt <- normalize_label(cell_value(raw,r,c)); if (!nzchar(txt)) next
      for (nm in names(canon)) if (txt %in% normalize_label(canon[[nm]])) cols[[nm]] <- c
    }
    nmatch <- sum(!is.na(unlist(cols)))
    if (nmatch > best_n) { best_n <- nmatch; best <- list(header_row=r, data_start_row=r+1L, columns=cols, confidence=if (nmatch>=6) "high" else if (nmatch>=4) "medium" else "low") }
  }
  if (is.null(best) || best_n < 4L) return(NULL)
  best
}

merge_layouts <- function(local_fields, local_load, ai_layout=NULL) {
  # Começa pela leitura local de alta confiança. IA só preenche lacunas.
  fields <- local_fields
  if (!is.null(ai_layout) && !is.null(ai_layout$fields)) {
    for (nm in names(fields)) {
      lv <- fields[[nm]]$value
      if ((is.null(lv) || is.na(lv) || !nzchar(trimws(as.character(lv)))) && !is.null(ai_layout$fields[[nm]])) fields[[nm]] <- ai_layout$fields[[nm]]
    }
  }
  lt <- local_load
  if (is.null(lt) && !is.null(ai_layout$load_table)) lt <- ai_layout$load_table
  if (is.null(lt)) lt <- list(header_row=NULL,data_start_row=NULL,columns=setNames(as.list(rep(NA_integer_,8)),c("data_emissao","mic_dta","cavalo","carreta","peso_liquido","valor","sequencia","nota")),confidence="low")
  list(fields=fields, load_table=lt, warnings=if (!is.null(ai_layout$warnings)) ai_layout$warnings else list())
}

report_layout_schema <- function() {
  field_schema <- list(
    type = "object", additionalProperties = FALSE,
    required = c("value", "cell", "confidence"),
    properties = list(
      value = list(type = c("string", "null")),
      cell = list(type = c("string", "null"), description = "Célula de origem, ex.: B7. Use null quando derivado de mais de uma célula."),
      confidence = list(type = "string", enum = c("high", "medium", "low"))
    )
  )
  col_schema <- list(type = c("integer", "null"), minimum = 1, maximum = 50)
  list(
    type = "object", additionalProperties = FALSE,
    required = c("fields", "load_table", "warnings"),
    properties = list(
      fields = list(
        type = "object", additionalProperties = FALSE,
        required = c("importador","exportador","mercadoria","fatura","di","processo","crt","transportadora","dat","data_liberacao","data_registro","peso_previsto"),
        properties = setNames(rep(list(field_schema), 12), c("importador","exportador","mercadoria","fatura","di","processo","crt","transportadora","dat","data_liberacao","data_registro","peso_previsto"))
      ),
      load_table = list(
        type = "object", additionalProperties = FALSE,
        required = c("header_row","data_start_row","columns","confidence"),
        properties = list(
          header_row = list(type = c("integer","null"), minimum = 1),
          data_start_row = list(type = c("integer","null"), minimum = 1),
          confidence = list(type = "string", enum = c("high","medium","low")),
          columns = list(
            type = "object", additionalProperties = FALSE,
            required = c("data_emissao","mic_dta","cavalo","carreta","peso_liquido","valor","sequencia","nota"),
            properties = list(
              data_emissao=col_schema, mic_dta=col_schema, cavalo=col_schema, carreta=col_schema,
              peso_liquido=col_schema, valor=col_schema, sequencia=col_schema, nota=col_schema
            )
          )
        )
      ),
      warnings = list(type = "array", items = list(type = "string"))
    )
  )
}

report_layout_prompt <- function(filename, sheet_name, grid_text) {
  paste0(
    "Você interpreta planilhas operacionais de comércio exterior para o MeridIAn Comex.\n",
    "O arquivo abaixo é um relatório operacional, normalmente SEARA, mas o layout pode variar entre versões.\n",
    "NÃO use posições fixas pré-concebidas. Identifique cada campo pelo rótulo, contexto e conteúdo da própria planilha.\n\n",
    "ARQUIVO: ", filename, "\nABA: ", sheet_name, "\n\n",
    "OBJETIVO: identificar corretamente os campos do processo e a estrutura da tabela de cargas.\n",
    "Campos desejados:\n",
    "- importador: empresa importadora, por exemplo SEARA ALIMENTOS LTDA; nunca confundir com DI ou processo.\n",
    "- exportador: fornecedor/exportador, por exemplo AGROFERTIL S.A; nunca confundir com processo.\n",
    "- mercadoria: descrição do produto/mercadoria.\n",
    "- fatura: número de fatura comercial, preservando zeros e separadores.\n",
    "- di: número da Declaração de Importação.\n",
    "- processo: número interno/IMP do processo.\n",
    "- crt: número do CRT/carta de porte.\n",
    "- transportadora: nome da transportadora.\n",
    "- dat: DAT quando existir.\n",
    "- data_liberacao e data_registro: datas correspondentes, se identificáveis.\n",
    "- peso_previsto: peso total previsto do processo quando explicitamente indicado.\n\n",
    "REGRAS IMPORTANTES:\n",
    "1. O texto foi extraído do Excel e cada valor vem precedido de sua coordenada (A1, B2...).\n",
    "2. Use SEMÂNTICA e proximidade entre rótulo e valor. Não suponha que B5/F5 etc. sejam sempre corretos.\n",
    "3. Preserve o valor exatamente como aparece, inclusive zeros à esquerda.\n",
    "4. Se um campo não estiver claro, retorne null e confiança low. Não desloque campos vizinhos para preencher lacunas.\n",
    "5. Identifique também a tabela de cargas. Informe a linha de cabeçalho, primeira linha de dados e o número das colunas correspondentes.\n",
    "6. A tabela costuma conter conceitos como Data Emissão, MIC/DTA, Cavalo, Carreta, Peso Líquido, Valor, Sequência e Nota, mas os títulos e posições podem variar.\n",
    "7. O nome do arquivo é apenas contexto; priorize o conteúdo da planilha.\n\n",
    "GRADE DE CÉLULAS NÃO VAZIAS:\n", grid_text,
    "\n\nRetorne somente o JSON estruturado solicitado."
  )
}

extract_report_output_text <- function(resp) {
  if (!is.null(resp$output_text) && length(resp$output_text) == 1 && nzchar(resp$output_text)) return(resp$output_text)
  if (!is.null(resp$output)) {
    for (out in resp$output) if (!is.null(out$content)) for (ct in out$content) {
      if (!is.null(ct$text) && length(ct$text) == 1 && nzchar(ct$text)) return(ct$text)
    }
  }
  stop("A OpenAI respondeu, mas não retornou o JSON da análise da planilha.")
}

call_openai_report_layout <- function(raw, filename, sheet_name, api_key, model = "gpt-5-mini") {
  api_key <- trimws(as.character(api_key)[1])
  if (is.na(api_key) || !nzchar(api_key)) stop("A importação inteligente requer OPENAI_API_KEY.")
  grid <- spreadsheet_grid_text(raw)
  body <- list(
    model = model,
    input = list(list(role="user", content=list(list(type="input_text", text=report_layout_prompt(filename, sheet_name, grid))))),
    text = list(format=list(type="json_schema", name="operational_report_layout", strict=TRUE, schema=report_layout_schema()))
  )
  if (grepl("^gpt-5", model)) body$reasoning <- list(effort="low")
  t0 <- Sys.time()
  resp <- httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_headers(Authorization=paste("Bearer",api_key), `Content-Type`="application/json") |>
    httr2::req_body_json(body, auto_unbox=TRUE) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries=3) |>
    httr2::req_perform()
  parsed <- httr2::resp_body_json(resp, simplifyVector=FALSE)
  ans <- jsonlite::fromJSON(extract_report_output_text(parsed), simplifyVector=FALSE)
  attr(ans,"elapsed_sec") <- as.numeric(difftime(Sys.time(),t0,units="secs"))
  ans
}

ai_field_value <- function(layout, name) {
  x <- layout$fields[[name]]
  if (is.null(x) || is.null(x$value)) return(NA_character_)
  v <- trimws(as.character(x$value)[1])
  if (!nzchar(v) || identical(tolower(v),"null")) NA_character_ else v
}

ai_field_cell <- function(layout, name) {
  x <- layout$fields[[name]]
  if (is.null(x) || is.null(x$cell)) return(NA_character_)
  v <- trimws(as.character(x$cell)[1]); if (!nzchar(v)) NA_character_ else v
}

ai_field_conf <- function(layout, name) {
  x <- layout$fields[[name]]
  if (is.null(x) || is.null(x$confidence)) return("low")
  as.character(x$confidence)[1]
}

extract_loads_from_ai_layout <- function(raw, layout) {
  lt <- layout$load_table
  if (is.null(lt) || is.null(lt$data_start_row) || is.na(as.integer(lt$data_start_row))) return(data.frame())
  start <- as.integer(lt$data_start_row)
  cols <- lt$columns
  targets <- c("data_emissao","mic_dta","cavalo","carreta","peso_liquido","valor","sequencia","nota")
  idx <- vapply(targets, function(nm) {
    v <- cols[[nm]]
    if (is.null(v) || is.na(as.integer(v))) NA_integer_ else as.integer(v)
  }, integer(1))
  if (all(is.na(idx)) || start > nrow(raw)) return(data.frame())

  out <- vector("list", nrow(raw)-start+1L); k <- 0L; blank_run <- 0L
  for (r in seq.int(start,nrow(raw))) {
    vals <- vapply(idx, function(c) if (is.na(c)) NA_character_ else cell_value(raw,r,c), character(1))
    key_present <- sum(!is.na(vals[c("mic_dta","cavalo","carreta","peso_liquido")]) & nzchar(vals[c("mic_dta","cavalo","carreta","peso_liquido")]))
    any_present <- any(!is.na(vals) & nzchar(vals))
    if (!any_present) {
      blank_run <- blank_run + 1L
      if (blank_run >= 4L && k > 0L) break
      next
    }
    blank_run <- 0L
    # Evita capturar rodapés/observações como carga. Exige ao menos 2 campos centrais.
    if (key_present < 2L) next
    k <- k + 1L
    out[[k]] <- data.frame(
      DATA_EMISSAO=vals["data_emissao"], MIC_DTA=vals["mic_dta"], CAVALO=vals["cavalo"], CARRETA=vals["carreta"],
      PESO_LIQUIDO=vals["peso_liquido"], VALOR=vals["valor"], SEQUENCIA=vals["sequencia"], NOTA=vals["nota"],
      SOURCE_ROW=r, stringsAsFactors=FALSE, check.names=FALSE
    )
  }
  if (!k) return(data.frame())
  do.call(rbind,out[seq_len(k)])
}

parse_seara_report <- function(path, filename=basename(path), api_key=NULL, model="gpt-5-mini") {
  sheets <- readxl::excel_sheets(path)
  if (!length(sheets)) stop("O arquivo não possui abas legíveis.")
  # Procura a aba com maior conteúdo útil, em vez de assumir cegamente a primeira.
  candidates <- lapply(sheets, function(sh) {
    x <- tryCatch(readxl::read_excel(path, sheet=sh, col_names=FALSE, .name_repair="minimal"), error=function(e) NULL)
    if (is.null(x)) return(NULL)
    x <- as.data.frame(x, stringsAsFactors=FALSE, check.names=FALSE)
    score <- sum(vapply(x, function(col) sum(!is.na(col) & nzchar(trimws(as.character(col)))), numeric(1)))
    list(sheet=sh, raw=x, score=score)
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("Não foi possível ler nenhuma aba do arquivo.")
  pick <- candidates[[which.max(vapply(candidates, `[[`, numeric(1), "score"))]]
  raw <- pick$raw; sheet_name <- pick$sheet
  if (!nrow(raw) || !ncol(raw)) stop("A aba selecionada do arquivo está vazia.")

  local_fields <- extract_fields_by_labels(raw)
  local_load <- detect_load_table_by_headers(raw)
  missing <- names(local_fields)[vapply(local_fields, function(x) is.null(x$value) || is.na(x$value) || !nzchar(trimws(as.character(x$value))), logical(1))]

  # Só chama IA quando há lacunas relevantes ou quando a tabela de cargas não foi reconhecida.
  ai_layout <- NULL
  key_ok <- !is.null(api_key) && length(api_key)==1 && !is.na(api_key) && nzchar(trimws(as.character(api_key)))
  need_ai <- length(intersect(missing, c("importador","exportador","mercadoria","fatura","di","processo","crt","transportadora"))) > 0 || is.null(local_load)
  if (need_ai && key_ok) {
    ai_layout <- tryCatch(call_openai_report_layout(raw, filename, sheet_name, api_key=api_key, model=model), error=function(e) {
      structure(list(message=conditionMessage(e)), class="ai_layout_error")
    })
  }
  ai_error <- inherits(ai_layout,"ai_layout_error")
  if (ai_error) ai_layout_clean <- NULL else ai_layout_clean <- ai_layout
  layout <- merge_layouts(local_fields, local_load, ai_layout_clean)

  fields <- c("importador","exportador","mercadoria","fatura","di","processo","crt","transportadora","dat","data_liberacao","data_registro","peso_previsto")
  low_fields <- fields[vapply(fields,function(x) ai_field_conf(layout,x)=="low",logical(1))]
  map_txt <- paste(vapply(fields,function(x) paste0(x,"=",ai_field_cell(layout,x)),character(1)),collapse="; ")
  elapsed <- if (!is.null(ai_layout_clean)) as.numeric(attr(ai_layout_clean,"elapsed_sec") %||% NA_real_) else 0
  method <- if (need_ai && !is.null(ai_layout_clean)) "Rótulos + IA" else if (need_ai && ai_error) "Rótulos (IA indisponível)" else "Rótulos semânticos"

  meta <- data.frame(
    ARQUIVO=filename,
    URF=infer_urf_filename(filename),
    IMPORTADOR=ai_field_value(layout,"importador"),
    EXPORTADOR=ai_field_value(layout,"exportador"),
    MERCADORIA=ai_field_value(layout,"mercadoria"),
    FATURA=ai_field_value(layout,"fatura"),
    DI=ai_field_value(layout,"di"),
    PROCESSO=ai_field_value(layout,"processo"),
    CRT=ai_field_value(layout,"crt"),
    TRANSPORTADORA=ai_field_value(layout,"transportadora"),
    DAT=ai_field_value(layout,"dat"),
    `DATA LIBERAÇÃO`=ai_field_value(layout,"data_liberacao"),
    `DATA REGISTRO`=ai_field_value(layout,"data_registro"),
    PESO_PREVISTO=ai_field_value(layout,"peso_previsto"),
    `MÉTODO`=method,
    `CONFIANÇA`=if (length(low_fields)) paste0("Revisar: ",paste(low_fields,collapse=", ")) else "Alta",
    `MAPEAMENTO`=map_txt,
    `TEMPO IA (s)`=round(elapsed,1),
    stringsAsFactors=FALSE, check.names=FALSE
  )
  loads <- extract_loads_from_ai_layout(raw, layout)
  warnings <- character(0)
  if (ai_error) warnings <- c(warnings, paste0("IA: ", ai_layout$message))
  if (!nrow(loads)) warnings <- c(warnings, "Nenhuma carga foi reconhecida automaticamente.")
  list(meta=meta, loads=loads, sheet=sheet_name, ai_layout=layout, warnings=warnings)
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
