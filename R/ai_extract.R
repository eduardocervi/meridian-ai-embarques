`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || identical(x, "")) y else x

mic_labels <- c(
  "Nome e endereço do transportador",
  "RUC / cadastro do transportador",
  "Trânsito aduaneiro",
  "Nº MIC/DTA",
  "Folha / total de folhas",
  "Data de emissão",
  "Aduana, cidade e país de partida",
  "Cidade e país de destino final",
  "Caminhão original - proprietário",
  "Cadastro do caminhão original",
  "Placa do caminhão",
  "Marca e número do caminhão",
  "Capacidade de arraste",
  "Ano do caminhão",
  "Semirreboque / reboque",
  "Caminhão substituto - proprietário",
  "Cadastro do caminhão substituto",
  "Placa do caminhão substituto",
  "Marca e número do caminhão substituto",
  "Capacidade de arraste do substituto",
  "Ano do substituto",
  "Semirreboque / reboque substituto",
  "Nº carta de porte / CRT",
  "Aduana de destino",
  "Moeda",
  "Origem das mercadorias",
  "Valor FOT",
  "Frete em US$",
  "Seguro em US$",
  "Tipo de volumes",
  "Quantidade de volumes",
  "Peso bruto",
  "Remetente / exportador",
  "Destinatário / importador",
  "Consignatário",
  "Documentos anexos / fatura",
  "Número de lacres / precintos",
  "Descrição das mercadorias",
  "Firma e selo do transportador",
  "Nº DTA, rota e prazo de transporte",
  "Firma e selo da Aduana de Partida",
  "Subtotal - quantidade de volumes",
  "Subtotal - peso bruto",
  "Totais da folha - quantidade de volumes",
  "Totais da folha - peso bruto",
  "Totais acumulados - quantidade de volumes",
  "Totais acumulados - peso bruto"
)

mic_schema_compact <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = c("mic_id", "folha", "total_folhas", "tipo_pagina", "valores", "confianca_baixa", "observacoes"),
    properties = list(
      mic_id = list(type = c("string", "null"), description = "Número do MIC/DTA do item 4 exatamente como impresso"),
      folha = list(type = c("integer", "null")),
      total_folhas = list(type = c("integer", "null")),
      tipo_pagina = list(type = "string", enum = c("principal", "continuacao", "indeterminado")),
      valores = list(
        type = "array",
        minItems = 47,
        maxItems = 47,
        items = list(type = c("string", "null")),
        description = "47 valores na ordem dos itens 1 a 47"
      ),
      confianca_baixa = list(
        type = "array",
        items = list(type = "integer", minimum = 1, maximum = 47),
        description = "Somente números dos itens cuja leitura é realmente incerta"
      ),
      observacoes = list(type = c("string", "null"))
    )
  )
}

mic_prompt_compact <- function() {
  paste(
    "Você é um extrator documental especializado em MIC/DTA.",
    "Leia a página visualmente pela geometria do formulário e pelos números impressos dos campos.",
    "Retorne um array 'valores' com EXATAMENTE 47 posições: posição 1 = item 1, ... posição 47 = item 47.",
    "Não repita rótulos dos campos. Isso reduz a resposta e acelera o processamento.",
    "REGRAS:",
    "1. Não invente, traduza, complete ou normalize dados. Preserve o texto impresso e zeros à esquerda.",
    "2. Se um item não existir nesta página ou estiver ilegível, use null.",
    "3. Item 4 é o número do MIC/DTA, no topo direito próximo ao QR Code. Leia com atenção máxima.",
    "4. Item 5 informa folha e total de folhas. Use para folha, total_folhas e tipo_pagina.",
    "5. Ignore carimbos, assinaturas, marcas d'água e QR Code quando não fizerem parte do campo.",
    "6. Em continuação, leia os campos repetidos e especialmente itens 42 a 47.",
    "7. Itens 23, 33, 34, 36 e 38 são críticos para o controle de embarques. Não os confunda com campos vizinhos.",
    "8. No item 36 preserve integralmente números de fatura e despacho, por exemplo 001-002-0000255.",
    "9. Marque em confianca_baixa apenas itens realmente ambíguos. Não inclua itens simplesmente vazios.",
    "10. Não escreva nada fora do JSON estruturado.",
    sep = "\n"
  )
}

extract_output_text <- function(resp) {
  if (!is.null(resp$output_text) && length(resp$output_text) == 1 && nzchar(resp$output_text)) return(resp$output_text)
  if (!is.null(resp$output)) {
    for (out in resp$output) {
      if (!is.null(out$content)) {
        for (ct in out$content) {
          if (!is.null(ct$text) && length(ct$text) == 1 && nzchar(ct$text)) return(ct$text)
        }
      }
    }
  }
  stop("A API respondeu, mas não foi possível localizar o JSON estruturado no retorno.")
}

build_openai_request <- function(image_path, api_key, model = "gpt-5-mini", detail = "high") {
  stopifnot(file.exists(image_path))
  api_key <- trimws(as.character(api_key)[1])
  if (is.na(api_key) || !nzchar(api_key)) stop("Informe uma OPENAI_API_KEY válida ou configure OPENAI_API_KEY.")
  if (!detail %in% c("high", "auto", "low")) detail <- "high"

  img_b64 <- base64enc::base64encode(image_path)
  mime <- if (grepl("\\.jpe?g$", image_path, ignore.case = TRUE)) "image/jpeg" else "image/png"
  data_url <- paste0("data:", mime, ";base64,", img_b64)

  body <- list(
    model = model,
    input = list(list(
      role = "user",
      content = list(
        list(type = "input_text", text = mic_prompt_compact()),
        list(type = "input_image", image_url = data_url, detail = detail)
      )
    )),
    text = list(
      format = list(
        type = "json_schema",
        name = "mic_extraction_compact",
        strict = TRUE,
        schema = mic_schema_compact()
      )
    )
  )
  if (grepl("^gpt-5", model)) body$reasoning <- list(effort = "low")

  httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_throttle(capacity = 60, fill_time_s = 60, realm = "openai-meridian")
}

parse_openai_response <- function(response) {
  if (inherits(response, "error")) stop(conditionMessage(response))
  parsed <- httr2::resp_body_json(response, simplifyVector = FALSE)
  txt <- extract_output_text(parsed)
  ans <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  attr(ans, "usage") <- parsed$usage %||% NULL
  attr(ans, "response_model") <- parsed$model %||% NULL
  ans
}

call_openai_vision <- function(image_path, api_key, model = "gpt-5-mini", detail = "high") {
  req <- build_openai_request(image_path, api_key, model, detail)
  t0 <- Sys.time()
  response <- httr2::req_perform(req)
  ans <- parse_openai_response(response)
  attr(ans, "elapsed_sec") <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ans
}

call_openai_vision_parallel <- function(image_paths, api_key, model = "gpt-5-mini", detail = "high", max_active = 2) {
  if (!length(image_paths)) return(list())
  max_active <- max(1L, min(as.integer(max_active), 4L))
  reqs <- lapply(image_paths, build_openai_request, api_key = api_key, model = model, detail = detail)
  t0 <- Sys.time()
  if (exists("req_perform_parallel", envir = asNamespace("httr2"), inherits = FALSE)) {
    responses <- httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE, max_active = max_active)
  } else {
    responses <- lapply(reqs, function(req) tryCatch(httr2::req_perform(req), error = function(e) e))
  }
  total_elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  per_page_est <- total_elapsed / max(1, length(image_paths))
  lapply(responses, function(resp) {
    if (inherits(resp, "error")) return(structure(list(error = conditionMessage(resp)), class = "mic_error"))
    ans <- tryCatch(parse_openai_response(resp), error = function(e) structure(list(error = conditionMessage(e)), class = "mic_error"))
    if (!inherits(ans, "mic_error")) attr(ans, "elapsed_sec") <- per_page_est
    ans
  })
}

normalize_page_result <- function(x, page_index, source_file, model_used = NA_character_) {
  vals <- x$valores
  if (length(vals) != 47) stop(sprintf("Página %s: IA retornou %s valores em vez de 47.", page_index, length(vals)))
  low <- unique(as.integer(unlist(x$confianca_baixa %||% integer(0))))
  low <- low[!is.na(low) & low >= 1 & low <= 47]

  values_chr <- vapply(vals, function(z) if (is.null(z)) NA_character_ else as.character(z), character(1))
  data.frame(
    arquivo = source_file,
    pagina_pdf = page_index,
    mic_id = as.character(x$mic_id %||% NA_character_),
    folha = as.integer(x$folha %||% NA_integer_),
    total_folhas = as.integer(x$total_folhas %||% NA_integer_),
    tipo_pagina = as.character(x$tipo_pagina %||% "indeterminado"),
    item = 1:47,
    rotulo = mic_labels,
    valor = values_chr,
    confianca = ifelse(1:47 %in% low, "baixa", ifelse(is.na(values_chr), "vazio", "alta")),
    observacao = ifelse(1:47 %in% low, as.character(x$observacoes %||% "Leitura marcada como incerta pela IA"), NA_character_),
    modelo = model_used,
    tempo_estimado_s = as.numeric(attr(x, "elapsed_sec") %||% NA_real_),
    stringsAsFactors = FALSE
  )
}

critical_items_missing <- function(x) {
  critical <- c(4, 5, 23, 33, 34, 36, 38)
  vals <- x$valores
  if (length(vals) != 47) return(TRUE)
  missing <- vapply(critical, function(i) is.null(vals[[i]]) || !nzchar(trimws(as.character(vals[[i]]))), logical(1))
  low <- as.integer(unlist(x$confianca_baixa %||% integer(0)))
  any(missing) || any(critical %in% low)
}

combine_unique <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  paste(unique(x), collapse = " | ")
}

consolidate_mics <- function(detail_df) {
  if (is.null(detail_df) || nrow(detail_df) == 0) return(data.frame())
  d <- detail_df
  d$mic_key <- ifelse(is.na(d$mic_id) | !nzchar(d$mic_id), paste0("SEM_MIC_PAG_", d$pagina_pdf), d$mic_id)
  keys <- unique(d$mic_key)
  rows <- lapply(keys, function(k) {
    z <- d[d$mic_key == k, , drop = FALSE]
    low_nonempty <- z$confianca == "baixa" & !is.na(z$valor) & nzchar(trimws(z$valor))
    out <- list(
      mic_id = if (startsWith(k, "SEM_MIC_PAG_")) NA_character_ else k,
      paginas = paste(sort(unique(z$pagina_pdf)), collapse = ", "),
      total_folhas = suppressWarnings(max(z$total_folhas, na.rm = TRUE)),
      status_revisao = if (any(low_nonempty, na.rm = TRUE) || startsWith(k, "SEM_MIC_PAG_")) "Revisar" else "Pronto"
    )
    if (!is.finite(out$total_folhas)) out$total_folhas <- NA_integer_
    for (i in 1:47) out[[sprintf("item_%02d", i)]] <- combine_unique(z$valor[z$item == i])
    as.data.frame(out, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

extract_invoice <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  patterns <- c(
    "(?i)(?:FACTURA(?:\\s+DE\\s+EXPORTACI[OÓ]N)?|FATURA)\\s*[:Nº°.-]*\\s*([0-9]{3}[-./][0-9]{3}[-./][0-9]{5,8})",
    "([0-9]{3}[-./][0-9]{3}[-./][0-9]{5,8})"
  )
  for (pat in patterns) {
    m <- regexec(pat, x, perl = TRUE)
    r <- regmatches(x, m)[[1]]
    if (length(r) >= 2 && nzchar(r[2])) return(r[2])
    if (length(r) == 1 && nzchar(r[1])) return(r[1])
  }
  NA_character_
}

clean_party <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  x <- gsub("(?i)\\b(NOMBRE|NOME)\\s*:\\s*", "", x, perl = TRUE)
  x <- gsub("(?i)\\bDIR\\s*:\\s*.*$", "", x, perl = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

clean_transportadora <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

infer_urf <- function(item24) {
  if (is.na(item24) || !nzchar(item24)) return(NA_character_)
  x <- toupper(iconv(item24, to = "ASCII//TRANSLIT"))
  if (grepl("FOZ DO IGUACU|FOZ DO IGUAZU|MULTILOG.*FOZ", x)) return("FOZ")
  if (grepl("GUAIRA", x)) return("GUAIRA")
  if (grepl("SANTA HELENA", x)) return("SANTA HELENA")
  if (grepl("MUNDO NOVO", x)) return("MUNDO NOVO")
  if (grepl("DIONISIO CERQUEIRA", x)) return("DIONISIO CERQUEIRA")
  NA_character_
}

suggest_control_rows <- function(mic_df, source_file = NA_character_) {
  if (is.null(mic_df) || nrow(mic_df) == 0) return(data.frame())
  getv <- function(row, n) {
    v <- row[[sprintf("item_%02d", n)]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)
  }
  rows <- lapply(seq_len(nrow(mic_df)), function(i) {
    r <- mic_df[i, , drop = FALSE]
    data.frame(
      URF = infer_urf(getv(r, 24)),
      IMPORTADOR = clean_party(getv(r, 34)),
      EXPORTADOR = clean_party(getv(r, 33)),
      MERCADORIA = getv(r, 38),
      FATURA = extract_invoice(getv(r, 36)),
      DI = NA_character_,
      PROCESSO = NA_character_,
      CRT = getv(r, 23),
      TRANSPORTADORA = clean_transportadora(getv(r, 1)),
      `DATA REGISTRO` = as.Date(NA),
      `DATA LIBERAÇÃO` = as.Date(NA),
      ARQUIVO = source_file,
      ATUALIZAÇÃO = as.POSIXct(Sys.time()),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  attr(out, "mic_ids") <- mic_df$mic_id
  out
}
