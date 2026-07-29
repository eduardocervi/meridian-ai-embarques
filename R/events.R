empty_events <- function() {
  data.frame(
    EVENT_ID=character(), DATA_HORA=as.POSIXct(character()), DATA_OPERACAO=as.Date(character()),
    EVENTO=character(), PROCESSO=character(), CLIENTE=character(), UNIDADE=character(), PRODUTO=character(),
    URF=character(), FATURA=character(), CRT=character(), MIC_DTA=character(), CARGAS=integer(),
    PESO=double(), USUARIO=character(), ARQUIVO=character(), CAMPO=character(), VALOR_ANTERIOR=character(),
    VALOR_NOVO=character(), DETALHE=character(), stringsAsFactors=FALSE, check.names=FALSE
  )
}

safe_chr <- function(x) { x <- as.character(x %||% NA_character_); ifelse(is.na(x), "", x) }

make_event <- function(evento, processo="", cliente="", unidade="", produto="", urf="", fatura="", crt="", mic="",
                       cargas=0L, peso=0, usuario="meridian", arquivo="", campo="", anterior="", novo="", detalhe="",
                       data_hora=Sys.time(), data_operacao=as.Date(data_hora)) {
  data.frame(
    EVENT_ID=paste0(format(data_hora, "%Y%m%d%H%M%OS3"), "-", sprintf("%06d", sample.int(999999,1))),
    DATA_HORA=as.POSIXct(data_hora), DATA_OPERACAO=as.Date(data_operacao), EVENTO=evento,
    PROCESSO=safe_chr(processo), CLIENTE=safe_chr(cliente), UNIDADE=safe_chr(unidade), PRODUTO=safe_chr(produto),
    URF=safe_chr(urf), FATURA=safe_chr(fatura), CRT=safe_chr(crt), MIC_DTA=safe_chr(mic),
    CARGAS=as.integer(cargas %||% 0), PESO=as.numeric(peso %||% 0), USUARIO=safe_chr(usuario), ARQUIVO=safe_chr(arquivo),
    CAMPO=safe_chr(campo), VALOR_ANTERIOR=safe_chr(anterior), VALOR_NOVO=safe_chr(novo), DETALHE=safe_chr(detalhe),
    stringsAsFactors=FALSE, check.names=FALSE
  )
}

append_events <- function(current, new_events) {
  if (is.null(current) || !nrow(current)) current <- empty_events()
  if (is.null(new_events) || !nrow(new_events)) return(current)
  dplyr::bind_rows(current, new_events)
}

control_business_key <- function(d) {
  d <- ensure_control_columns(d)
  paste(
    ifelse(is.na(d$IMPORTADOR),"",toupper(trimws(as.character(d$IMPORTADOR)))),
    ifelse(is.na(d$PROCESSO),"",toupper(trimws(as.character(d$PROCESSO)))),
    ifelse(is.na(d$FATURA),"",toupper(trimws(as.character(d$FATURA)))), sep="|"
  )
}

events_from_control_change <- function(old_control, new_control, usuario="meridian", arquivo="") {
  old <- ensure_control_columns(old_control); new <- ensure_control_columns(new_control)
  old$key <- control_business_key(old); new$key <- control_business_key(new)
  ev <- list(); k <- 0L
  fields <- intersect(c("URF","IMPORTADOR","EXPORTADOR","MERCADORIA","FATURA","DI","PROCESSO","CRT","TRANSPORTADORA","DATA REGISTRO","DATA LIBERAÇÃO"), names(new))
  for (i in seq_len(nrow(new))) {
    key <- new$key[i]; oi <- which(old$key == key)
    if (!length(oi)) {
      k <- k+1L; ev[[k]] <- make_event("NOVO_PROCESSO", processo=new$PROCESSO[i], cliente=new$IMPORTADOR[i], produto=new$MERCADORIA[i], urf=new$URF[i], fatura=new$FATURA[i], crt=new$CRT[i], usuario=usuario, arquivo=arquivo, detalhe="Processo incluído no estado atual")
      next
    }
    j <- oi[1]
    changed_any <- FALSE
    for (f in fields) {
      a <- safe_chr(old[[f]][j]); b <- safe_chr(new[[f]][i])
      if (!identical(a,b) && nzchar(b)) {
        changed_any <- TRUE; k <- k+1L
        et <- if (f == "DATA LIBERAÇÃO" && nzchar(b)) "PROCESSO_FINALIZADO" else if (f %in% c("DATA LIBERAÇÃO")) "STATUS_ALTERADO" else "PROCESSO_ATUALIZADO"
        ev[[k]] <- make_event(et, processo=new$PROCESSO[i], cliente=new$IMPORTADOR[i], produto=new$MERCADORIA[i], urf=new$URF[i], fatura=new$FATURA[i], crt=new$CRT[i], usuario=usuario, arquivo=arquivo, campo=f, anterior=a, novo=b, detalhe=paste("Alteração em", f))
      }
    }
    if (changed_any) invisible(NULL)
  }
  if (!length(ev)) return(empty_events())
  dplyr::bind_rows(ev)
}

normalize_report_loads <- function(parsed, usuario="meridian", arquivo="") {
  if (is.null(parsed) || is.null(parsed$loads) || !nrow(parsed$loads)) return(data.frame())
  m <- parsed$meta; x <- parsed$loads
  pick <- function(nms) { nm <- intersect(nms, names(x)); if(length(nm)) x[[nm[1]]] else rep(NA, nrow(x)) }
  data.frame(
    LOAD_KEY=paste(safe_chr(m$PROCESSO), safe_chr(pick(c("MIC_DTA","MIC","MIC.DTA"))), safe_chr(pick(c("SEQUENCIA","Sequencia","SEQUÊNCIA"))), sep="|"),
    PROCESSO=safe_chr(m$PROCESSO), CLIENTE=safe_chr(m$IMPORTADOR), UNIDADE=safe_chr(m$UNIDADE %||% ""), PRODUTO=safe_chr(m$MERCADORIA), URF=safe_chr(m$URF),
    FATURA=safe_chr(m$FATURA), CRT=safe_chr(m$CRT), MIC_DTA=safe_chr(pick(c("MIC_DTA","MIC","MIC.DTA"))),
    PESO=suppressWarnings(as.numeric(pick(c("PESO_LIQUIDO","PESO","Peso Liquido","PESO LÍQUIDO")))), USUARIO=usuario, ARQUIVO=arquivo,
    stringsAsFactors=FALSE, check.names=FALSE
  )
}

new_load_events <- function(existing_loads, candidate_loads, usuario="meridian") {
  if (is.null(candidate_loads) || !nrow(candidate_loads)) return(list(loads=existing_loads, events=empty_events()))
  if (is.null(existing_loads) || !nrow(existing_loads)) existing_loads <- candidate_loads[0,,drop=FALSE]
  existing_keys <- unique(safe_chr(existing_loads$LOAD_KEY))
  is_new <- !(candidate_loads$LOAD_KEY %in% existing_keys) | !nzchar(candidate_loads$LOAD_KEY)
  add <- candidate_loads[is_new,,drop=FALSE]
  if (!nrow(add)) return(list(loads=existing_loads, events=empty_events()))
  ev <- dplyr::bind_rows(lapply(seq_len(nrow(add)), function(i) make_event(
    "NOVA_CARGA", processo=add$PROCESSO[i], cliente=add$CLIENTE[i], unidade=add$UNIDADE[i], produto=add$PRODUTO[i], urf=add$URF[i], fatura=add$FATURA[i], crt=add$CRT[i], mic=add$MIC_DTA[i], cargas=1L, peso=add$PESO[i], usuario=usuario, arquivo=add$ARQUIVO[i], detalhe="Nova carga identificada na importação"
  )))
  list(loads=dplyr::bind_rows(existing_loads, add), events=ev)
}

period_bounds <- function(preset="Hoje", custom=NULL, today=Sys.Date()) {
  if (preset == "Hoje") return(c(today,today))
  if (preset == "Ontem") return(c(today-1,today-1))
  if (preset == "Últimos 7 dias") return(c(today-6,today))
  if (preset == "Últimos 30 dias") return(c(today-29,today))
  if (preset == "Este mês") return(c(as.Date(format(today,"%Y-%m-01")),today))
  if (preset == "Mês anterior") { first <- as.Date(format(today,"%Y-%m-01")); prev_end <- first-1; return(c(as.Date(format(prev_end,"%Y-%m-01")),prev_end)) }
  if (!is.null(custom) && length(custom)==2) return(as.Date(custom))
  c(today,today)
}

filter_events_period <- function(events, bounds) {
  if (is.null(events) || !nrow(events)) return(empty_events())
  dt <- as.Date(events$DATA_HORA)
  events[dt >= bounds[1] & dt <= bounds[2],,drop=FALSE]
}

movement_kpis <- function(events) {
  if (is.null(events) || !nrow(events)) return(list(clientes=0,processos=0,novos_processos=0,cargas=0,peso=0,unidades=0))
  list(
    clientes=length(unique(events$CLIENTE[nzchar(events$CLIENTE)])),
    processos=length(unique(events$PROCESSO[nzchar(events$PROCESSO)])),
    novos_processos=sum(events$EVENTO=="NOVO_PROCESSO",na.rm=TRUE),
    cargas=sum(events$CARGAS,na.rm=TRUE), peso=sum(events$PESO,na.rm=TRUE),
    unidades=length(unique(events$UNIDADE[nzchar(events$UNIDADE)]))
  )
}

movement_group_summary <- function(events) {
  if (is.null(events) || !nrow(events)) return(data.frame())
  events %>% dplyr::group_by(CLIENTE, UNIDADE, PRODUTO, URF) %>% dplyr::summarise(
    PROCESSOS_ALTERADOS=dplyr::n_distinct(PROCESSO[PROCESSO!=""]), NOVAS_CARGAS=sum(CARGAS,na.rm=TRUE), PESO_MOVIMENTADO=sum(PESO,na.rm=TRUE), .groups="drop"
  ) %>% dplyr::arrange(dplyr::desc(PESO_MOVIMENTADO), dplyr::desc(NOVAS_CARGAS))
}

productivity_summary <- function(events) {
  if (is.null(events) || !nrow(events)) return(data.frame())
  events %>% dplyr::group_by(ANALISTA=USUARIO) %>% dplyr::summarise(
    PROCESSOS_ATUALIZADOS=dplyr::n_distinct(PROCESSO[PROCESSO!=""]), NOVAS_CARGAS=sum(CARGAS,na.rm=TRUE), CLIENTES_ATENDIDOS=dplyr::n_distinct(CLIENTE[CLIENTE!=""]), PESO_MOVIMENTADO=sum(PESO,na.rm=TRUE), .groups="drop"
  ) %>% dplyr::arrange(dplyr::desc(PROCESSOS_ATUALIZADOS))
}

stale_process_alerts <- function(processes, events, days=3) {
  if (is.null(processes) || !nrow(processes)) return(data.frame())
  now <- Sys.Date(); p <- processes
  last_event <- rep(as.Date(NA), nrow(p))
  if (!is.null(events) && nrow(events)) {
    for (i in seq_len(nrow(p))) {
      z <- events[events$PROCESSO == safe_chr(p$PROCESSO[i]),,drop=FALSE]
      if (nrow(z)) last_event[i] <- max(as.Date(z$DATA_HORA),na.rm=TRUE)
    }
  }
  fallback <- as.Date(p$ATUALIZADO_EM)
  last_event[is.na(last_event)] <- fallback[is.na(last_event)]
  dias <- as.integer(now-last_event)
  alert <- p$STATUS != "FINALIZADO" & !is.na(dias) & dias >= as.integer(days)
  if (!any(alert,na.rm=TRUE)) return(data.frame())
  data.frame(PRIORIDADE=ifelse(dias[alert]>=days*2,"Crítica","Alta"), ALERTA="Sem atualização", PROCESSO=p$PROCESSO[alert], CLIENTE=p$CLIENTE[alert], URF=p$URF[alert], STATUS=p$STATUS[alert], ULTIMA_ATUALIZACAO=last_event[alert], DIAS_SEM_ALTERACAO=dias[alert], stringsAsFactors=FALSE)
}
