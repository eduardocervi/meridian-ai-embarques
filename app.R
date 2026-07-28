library(shiny)
library(bslib)
library(DT)
library(pdftools)
library(httr2)
library(jsonlite)
library(base64enc)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(htmltools)

source("R/ai_extract.R")
source("R/helpers.R")

brand_blue <- "#2f5faa"
brand_orange <- "#f28a32"

app_theme <- bs_theme(
  version = 5,
  bg = "#f5f7fb",
  fg = "#1b2430",
  primary = brand_blue,
  secondary = brand_orange,
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

brand_ui <- div(
  class = "meridian-brand",
  tags$img(src = "logo_meridian.jpeg", alt = "Meridian"),
  div(class = "brand-copy",
      div(class = "brand-title", "MERIDIAN | Controle de Embarques"),
      div(class = "brand-sub", "Extração inteligente de MIC/DTA"))
)

kpi_box <- function(label, value_output, orange = FALSE) {
  div(class = paste("kpi", if (orange) "orange" else ""),
      div(class = "kpi-label", label),
      div(class = "kpi-value", textOutput(value_output, inline = TRUE)))
}

ui <- page_navbar(
  title = brand_ui,
  theme = app_theme,
  id = "main_nav",
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  nav_panel(
    "Dashboard",
    div(class = "container-fluid py-4",
        div(class = "section-title", "Visão geral"),
        div(class = "section-sub", "Acompanhe o lote processado, pendências de revisão e o controle consolidado de embarques."),
        layout_columns(
          kpi_box("Páginas processadas", "kpi_paginas"),
          kpi_box("MICs identificados", "kpi_mics"),
          kpi_box("Pendentes de revisão", "kpi_revisar", TRUE),
          kpi_box("Embarques no controle", "kpi_embarques"),
          col_widths = c(3,3,3,3)
        ),
        br(),
        layout_columns(
          card(
            card_header("Últimos MICs extraídos"),
            DTOutput("dashboard_mics")
          ),
          card(
            card_header("Últimos embarques no controle"),
            DTOutput("dashboard_controle")
          ),
          col_widths = c(6,6)
        )
    )
  ),
  nav_panel(
    "Extração IA",
    div(class = "container-fluid py-4",
        div(class = "section-title", "Extrair MIC/DTA com Inteligência Artificial"),
        div(class = "section-sub", "Envie um PDF escaneado. Cada página será interpretada visualmente pela IA e convertida para uma estrutura de 47 itens."),
        layout_columns(
          card(
            card_header("1. Documento"),
            card_body(
              fileInput("mic_pdf", "PDF com os MIC/DTA", accept = ".pdf", buttonLabel = "Selecionar PDF", placeholder = "Nenhum arquivo selecionado"),
              uiOutput("pdf_info_ui"),
              uiOutput("page_range_ui"),
              checkboxInput("process_all", "Processar todas as páginas", value = TRUE),
              checkboxInput("auto_control", "Preencher o Controle automaticamente após a extração", value = TRUE),
              actionButton("run_ai", "Extrair com IA", class = "btn-primary w-100", icon = icon("wand-magic-sparkles")),
              uiOutput("processing_summary_ui"),
              tags$hr(class = "hr-soft"),
              div(class = "help-note", "Modo rápido: imagens JPEG otimizadas + chamadas simultâneas à API. Páginas com campos críticos duvidosos podem ser reenviadas automaticamente ao modelo de revisão.")
            )
          ),
          card(
            card_header("2. Pré-visualização"),
            card_body(
              uiOutput("preview_selector_ui"),
              div(class = "preview-wrap", imageOutput("page_preview", height = "720px"))
            )
          ),
          col_widths = c(4,8)
        )
    )
  ),
  nav_panel(
    "MICs extraídos",
    div(class = "container-fluid py-4",
        div(class = "section-title", "Base consolidada de MICs"),
        div(class = "section-sub", "Uma linha por MIC. Quando um mesmo item possui valores distintos em páginas de continuação, os valores são preservados e separados por |."),
        card(
          card_header(div(class="d-flex justify-content-between align-items-center",
                          span("MICs consolidados"),
                          div(
                            downloadButton("download_mics_csv", "CSV", class="btn btn-sm btn-outline-primary"),
                            actionButton("add_control", "Adicionar selecionados ao controle", class="btn btn-sm btn-primary", icon=icon("plus"))
                          ))),
          card_body(DTOutput("mics_table"))
        ),
        br(),
        card(
          card_header("Itens extraídos por página"),
          card_body(
            selectInput("detail_mic_filter", "Filtrar por MIC", choices = c("Todos" = ""), width = "320px"),
            DTOutput("detail_table")
          )
        )
    )
  ),
  nav_panel(
    "Revisão",
    div(class = "container-fluid py-4",
        div(class = "section-title", "Conferência assistida"),
        div(class = "section-sub", "Prioriza leituras marcadas como incertas pela IA. Você pode corrigir diretamente os valores e a base consolidada será atualizada."),
        layout_columns(
          card(
            card_header("Fila de revisão"),
            card_body(
              radioButtons("review_level", "Mostrar", choices = c("Itens incertos"="baixa", "Todos"="todos"), selected = "baixa", inline = TRUE),
              DTOutput("review_table")
            )
          ),
          card(
            card_header("Página associada"),
            card_body(div(class="preview-wrap", imageOutput("review_preview", height="720px")))
          ),
          col_widths = c(7,5)
        )
    )
  ),
  nav_panel(
    "Auditoria",
    div(class = "container-fluid py-4",
        div(class = "section-title", "Auditoria do processamento"),
        div(class = "section-sub", "Veja quais páginas foram processadas, modelo utilizado, MIC identificado e eventuais erros. Útil para lotes grandes."),
        card(
          card_header("Log do lote"),
          card_body(DTOutput("audit_table"))
        )
    )
  ),
  nav_panel(
    "Controle de embarques",
    div(class = "container-fluid py-4",
        div(class = "section-title", "Controle de embarques"),
        div(class = "section-sub", "Uma linha por MIC, seguindo exatamente as colunas A:M da aba CONTROLE. Os campos reconhecíveis são correlacionados automaticamente com os itens do MIC."),
        layout_columns(
          card(
            card_header("Importar base existente"),
            card_body(
              fileInput("control_file", "Planilha de controle", accept = c(".xlsx", ".xlsm", ".xls", ".csv"), buttonLabel = "Selecionar planilha"),
              actionButton("load_control", "Carregar planilha", class="btn-outline-primary w-100"),
              div(class="help-note mt-2", "Se existir uma aba chamada CONTROLE, ela será usada automaticamente. A exportação gera um novo .xlsx e não preserva macros do .xlsm original.")
            )
          ),
          card(
            card_header("Exportação"),
            card_body(
              downloadButton("download_control_xlsx", "Baixar controle completo (.xlsx)", class="btn-primary w-100"),
              br(), br(),
              downloadButton("download_control_csv", "Baixar apenas controle (.csv)", class="btn-outline-primary w-100")
            )
          ),
          col_widths = c(6,6)
        ),
        br(),
        card(
          card_header("Mapeamento MIC → CONTROLE"),
          card_body(
            div(class="help-note mb-2", "O preenchimento é determinístico depois da leitura da IA: a IA lê o MIC e o R aplica o mapeamento abaixo. Isso evita que o modelo invente a coluna de destino."),
            DTOutput("mapping_table")
          )
        ),
        br(),
        card(
          card_header(div(class="d-flex justify-content-between align-items-center",
                          span("Base de embarques"),
                          actionButton("add_blank_control", "Novo embarque", class="btn btn-sm btn-outline-primary", icon=icon("plus")))),
          card_body(DTOutput("control_table"))
        )
    )
  ),
  nav_panel(
    "Configurações",
    div(class = "container-fluid py-4",
        div(class = "section-title", "Configurações da IA"),
        layout_columns(
          card(
            card_header("OpenAI"),
            card_body(
              passwordInput("api_key", "OPENAI API Key", placeholder = "sk-...", value = ""),
              div(class="help-note mb-3", "A chave digitada fica apenas na sessão do aplicativo. Em produção, prefira configurar OPENAI_API_KEY nas variáveis de ambiente do shinyapps.io."),
              selectInput("processing_profile", "Perfil de processamento", choices = c(
                "Rápido - recomendado para lotes" = "fast",
                "Equilibrado" = "balanced",
                "Máxima precisão" = "precision"
              ), selected = "fast"),
              selectInput("model", "Modelo principal", choices = c(
                "GPT-5 mini - rápido" = "gpt-5-mini",
                "GPT-5 - máxima precisão" = "gpt-5",
                "GPT-4.1 - alternativa" = "gpt-4.1"
              ), selected = "gpt-5-mini"),
              numericInput("parallel_calls", "Chamadas simultâneas", value = 3, min = 1, max = 4, step = 1),
              checkboxInput("smart_fallback", "Reprocessar automaticamente páginas críticas com GPT-5", value = TRUE),
              selectInput("image_detail", "Detalhe da imagem", choices = c("Alto"="high", "Automático"="auto", "Baixo"="low"), selected="high"),
              div(class="ai-badge", icon("bolt"), "JSON compacto + processamento paralelo + fallback seletivo")
            )
          ),
          card(
            card_header("Como o aplicativo trabalha"),
            card_body(
              tags$ol(
                tags$li("Converte cada página do PDF escaneado em imagem."),
                tags$li("Envia a página para um modelo multimodal com instruções específicas para MIC/DTA."),
                tags$li("Recebe 47 campos em JSON estruturado, com nível de confiança por item."),
                tags$li("Agrupa páginas pelo item 4 e pelo item 5, sem descartar valores de páginas de continuação."),
                tags$li("Sugere o preenchimento da base de controle e deixa campos não presentes no MIC para conferência manual.")
              )
            )
          ),
          col_widths = c(6,6)
        )
    )
  )
)

server <- function(input, output, session) {
  initial_control <- tryCatch(
    ensure_control_columns(read.csv("data/controle_inicial.csv", check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")),
    error = function(e) as.data.frame(setNames(replicate(length(control_columns), character(0), simplify=FALSE), control_columns), check.names=FALSE)
  )

  rv <- reactiveValues(
    detail = NULL,
    mics = NULL,
    control = initial_control,
    page_files = character(0),
    pdf_name = NULL,
    review_page = NULL,
    audit = data.frame(),
    last_processing = NULL
  )

  current_api_key <- reactive({
    typed <- input$api_key
    if (is.null(typed) || length(typed) == 0) typed <- ""
    typed <- trimws(as.character(typed)[1])
    env_key <- trimws(as.character(Sys.getenv("OPENAI_API_KEY", unset = ""))[1])
    if (isTRUE(nzchar(typed))) typed else env_key
  })

  output$pdf_info_ui <- renderUI({
    req(input$mic_pdf)
    info <- tryCatch(pdftools::pdf_info(input$mic_pdf$datapath), error=function(e) NULL)
    if (is.null(info)) return(div(class="text-danger", "Não foi possível ler o PDF."))
    div(class="ai-badge", icon("file-pdf"), paste(info$pages, "páginas"))
  })

  output$page_range_ui <- renderUI({
    req(input$mic_pdf)
    info <- pdftools::pdf_info(input$mic_pdf$datapath)
    sliderInput("page_range", "Intervalo para processamento", min=1, max=info$pages, value=c(1, min(info$pages, 3)), step=1, sep="")
  })

  observeEvent(input$process_all, {
    if (isTRUE(input$process_all) && !is.null(input$mic_pdf)) {
      info <- pdftools::pdf_info(input$mic_pdf$datapath)
      updateSliderInput(session, "page_range", value=c(1, info$pages))
    }
  })

  observeEvent(input$mic_pdf, {
    rv$page_files <- character(0)
    rv$pdf_name <- input$mic_pdf$name
    rv$detail <- NULL
    rv$mics <- NULL
  })

  observeEvent(input$run_ai, {
    req(input$mic_pdf)
    key <- current_api_key()
    if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
      showNotification(
        "Informe sua OpenAI API Key na aba Configurações ou configure OPENAI_API_KEY no ambiente.",
        type = "error", duration = 10
      )
      return()
    }

    info <- pdftools::pdf_info(input$mic_pdf$datapath)
    pages <- if (isTRUE(input$process_all)) seq_len(info$pages) else seq.int(input$page_range[1], input$page_range[2])
    if (!length(pages)) return()

    profile <- input$processing_profile %||% "fast"
    if (profile == "fast") {
      dpi <- 165
      primary_model <- "gpt-5-mini"
      detail <- "high"
      max_active <- max(1L, min(as.integer(input$parallel_calls %||% 3), 4L))
    } else if (profile == "balanced") {
      dpi <- 190
      primary_model <- input$model %||% "gpt-5-mini"
      detail <- input$image_detail %||% "high"
      max_active <- max(1L, min(as.integer(input$parallel_calls %||% 2), 3L))
    } else {
      dpi <- 220
      primary_model <- input$model %||% "gpt-5"
      detail <- input$image_detail %||% "high"
      max_active <- 1L
    }

    outdir <- file.path(tempdir(), paste0("meridian_", as.integer(Sys.time())))
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    jpg_files <- file.path(outdir, sprintf("page_%03d.jpg", pages))
    t_start <- Sys.time()

    withProgress(message = "Preparando lote...", value = 0, {
      incProgress(0.05, detail = paste("Convertendo", length(pages), "página(s) para JPEG otimizado"))
      pdftools::pdf_convert(
        input$mic_pdf$datapath, format = "jpeg", pages = pages,
        filenames = jpg_files, dpi = dpi, verbose = FALSE
      )
      rv$page_files <- setNames(jpg_files, pages)

      incProgress(0.12, detail = paste("Lendo em paralelo com", primary_model))
      results <- call_openai_vision_parallel(
        jpg_files, key, model = primary_model, detail = detail, max_active = max_active
      )

      # Reprocessa somente páginas com campos críticos ausentes/duvidosos.
      if (isTRUE(input$smart_fallback) && primary_model != "gpt-5") {
        fallback_idx <- which(vapply(results, function(x) {
          !inherits(x, "mic_error") && isTRUE(critical_items_missing(x))
        }, logical(1)))
        if (length(fallback_idx)) {
          incProgress(0.68, detail = paste("Revisando", length(fallback_idx), "página(s) crítica(s) com GPT-5"))
          fallback <- call_openai_vision_parallel(
            jpg_files[fallback_idx], key, model = "gpt-5", detail = "high",
            max_active = min(2L, max_active)
          )
          for (k in seq_along(fallback_idx)) {
            if (!inherits(fallback[[k]], "mic_error")) results[[fallback_idx[k]]] <- fallback[[k]]
          }
        }
      }

      all_results <- list()
      audit_rows <- list()
      for (j in seq_along(pages)) {
        p <- pages[j]
        res <- results[[j]]
        model_used <- if (!inherits(res, "mic_error")) as.character(attr(res, "response_model") %||% primary_model) else primary_model
        if (inherits(res, "mic_error")) {
          audit_rows[[length(audit_rows)+1]] <- data.frame(
            pagina=p, status="Erro", modelo=model_used, tempo_s=NA_real_, mic=NA_character_, detalhe=res$error,
            stringsAsFactors=FALSE
          )
          showNotification(paste("Página", p, "não processada:", res$error), type="error", duration=12)
          next
        }
        norm <- tryCatch(normalize_page_result(res, p, input$mic_pdf$name, model_used), error=function(e) NULL)
        if (!is.null(norm)) {
          all_results[[length(all_results)+1]] <- norm
          audit_rows[[length(audit_rows)+1]] <- data.frame(
            pagina=p, status="OK", modelo=model_used,
            tempo_s=as.numeric(attr(res, "elapsed_sec") %||% NA_real_),
            mic=as.character(res$mic_id %||% NA_character_), detalhe=NA_character_, stringsAsFactors=FALSE
          )
        }
      }

      incProgress(0.86, detail = "Consolidando MICs e preenchendo o controle")
      if (length(all_results)) {
        rv$detail <- do.call(rbind, all_results)
        rv$detail$row_id <- seq_len(nrow(rv$detail))
        rv$mics <- consolidate_mics(rv$detail)
        rv$audit <- if (length(audit_rows)) do.call(rbind, audit_rows) else data.frame()
        updateSelectInput(session, "detail_mic_filter", choices = c("Todos"="", setNames(rv$mics$mic_id, rv$mics$mic_id)))

        auto_added <- 0L
        auto_updated <- 0L
        if (isTRUE(input$auto_control)) {
          suggested <- suggest_control_rows(rv$mics, rv$pdf_name)
          synced <- append_control_rows(rv$control, suggested)
          rv$control <- synced$control
          auto_added <- length(synced$added)
          auto_updated <- length(synced$skipped)
        }

        total_sec <- as.numeric(difftime(Sys.time(), t_start, units="secs"))
        rv$last_processing <- list(
          pages=length(pages), seconds=total_sec, model=primary_model,
          parallel=max_active, control_added=auto_added, control_updated=auto_updated
        )
        incProgress(1, detail = "Concluído")
        showNotification(
          paste0(nrow(rv$mics), " MIC(s) consolidados. ", auto_added,
                 " nova(s) linha(s) incluída(s) automaticamente no CONTROLE."),
          type="message", duration=8
        )
      } else {
        rv$audit <- if (length(audit_rows)) do.call(rbind, audit_rows) else data.frame()
        showNotification("Nenhuma página foi processada com sucesso.", type="error")
      }
    })
  })

  output$processing_summary_ui <- renderUI({
    x <- rv$last_processing
    if (is.null(x)) return(NULL)
    avg <- if (x$pages > 0) x$seconds / x$pages else NA_real_
    div(class="ai-badge mt-3",
        icon("gauge-high"),
        paste0(x$pages, " página(s) em ", round(x$seconds, 1), " s | média aparente ", round(avg, 1),
               " s/página | ", x$parallel, " chamada(s) simultânea(s)"))
  })

  output$audit_table <- renderDT({
    d <- rv$audit
    if (is.null(d) || !nrow(d)) return(datatable(data.frame(Status="Nenhum lote processado nesta sessão"), rownames=FALSE, options=list(dom="t")))
    datatable(d, rownames=FALSE, filter="top", options=list(scrollX=TRUE, pageLength=20))
  })

  output$mapping_table <- renderDT({
    map <- data.frame(
      `Coluna CONTROLE` = control_columns,
      `Origem` = c(
        "Item 24 - Aduana de destino (URF inferida)",
        "Item 34 - Destinatário",
        "Item 33 - Remetente",
        "Item 38 - Descrição das mercadorias",
        "Item 36 - Documentos anexos (nº após FACTURA DE EXPORTACION)",
        "Não disponível de forma confiável no MIC",
        "Não disponível de forma confiável no MIC",
        "Item 23 - Nº carta de porte / CRT",
        "Item 1 - Transportador",
        "Preenchimento manual / base importada",
        "Preenchimento manual / base importada",
        "Nome do PDF processado",
        "Data/hora da sincronização"
      ),
      `Regra` = c(
        "FOZ, GUAIRA, SANTA HELENA, MUNDO NOVO etc. conforme texto do item 24",
        "Remove apenas prefixos como NOMBRE:/NOME:",
        "Remove apenas prefixos como NOMBRE:/NOME:",
        "Preserva a descrição do documento",
        "Regex preservando zeros à esquerda, ex.: 001-002-0000255",
        "Mantém vazio",
        "Mantém vazio",
        "Preserva exatamente o código, ex.: PY290200084",
        "Preserva o nome do transportador",
        "Mantém valor existente",
        "Mantém valor existente",
        "Automático",
        "Automático"
      ),
      check.names=FALSE,
      stringsAsFactors=FALSE
    )
    datatable(map, rownames=FALSE, options=list(dom="t", scrollX=TRUE, pageLength=20))
  })

  output$preview_selector_ui <- renderUI({
    if (!length(rv$page_files)) return(div(class="help-note", "A pré-visualização aparecerá após iniciar a extração."))
    selectInput("preview_page", "Página", choices = names(rv$page_files), selected = names(rv$page_files)[1], width="160px")
  })

  output$page_preview <- renderImage({
    req(input$preview_page, length(rv$page_files))
    f <- rv$page_files[[as.character(input$preview_page)]]
    req(file.exists(f))
    list(src = f, contentType = "image/png", alt = paste("Página", input$preview_page))
  }, deleteFile = FALSE)

  output$mics_table <- renderDT({
    req(rv$mics)
    cols <- c("mic_id", "paginas", "total_folhas", "status_revisao", sprintf("item_%02d", 1:47))
    datatable(rv$mics[, intersect(cols, names(rv$mics)), drop=FALSE],
              rownames=FALSE, filter="top", selection="multiple",
              options=list(scrollX=TRUE, pageLength=10, fixedColumns=list(leftColumns=4)))
  })

  output$detail_table <- renderDT({
    req(rv$detail)
    d <- rv$detail
    if (nzchar(input$detail_mic_filter %||% "")) d <- d[d$mic_id == input$detail_mic_filter, , drop=FALSE]
    datatable(d[, c("pagina_pdf","mic_id","folha","total_folhas","item","rotulo","valor","confianca","observacao")],
              rownames=FALSE, filter="top", options=list(scrollX=TRUE, pageLength=25))
  })

  review_data <- reactive({
    req(rv$detail)
    d <- rv$detail
    if (input$review_level == "baixa") d <- d[d$confianca == "baixa", , drop=FALSE]
    d[, c("row_id","pagina_pdf","mic_id","item","rotulo","valor","confianca","observacao"), drop=FALSE]
  })

  output$review_table <- renderDT({
    d <- review_data()
    datatable(d, rownames=FALSE, selection="single", editable=list(target="cell", disable=list(columns=c(0,1,2,3,4))),
              options=list(pageLength=20, scrollX=TRUE, columnDefs=list(list(targets=0, visible=FALSE))))
  })

  observeEvent(input$review_table_rows_selected, {
    sel <- input$review_table_rows_selected
    if (length(sel)) rv$review_page <- review_data()$pagina_pdf[sel]
  })

  output$review_preview <- renderImage({
    req(rv$review_page, length(rv$page_files))
    f <- rv$page_files[[as.character(rv$review_page)]]
    req(file.exists(f))
    list(src=f, contentType="image/png", alt=paste("Página", rv$review_page))
  }, deleteFile=FALSE)

  observeEvent(input$review_table_cell_edit, {
    info <- input$review_table_cell_edit
    dshow <- review_data()
    req(nrow(dshow) >= info$row)
    colname <- names(dshow)[info$col + 1]
    if (!colname %in% c("valor", "confianca", "observacao")) return()
    rid <- dshow$row_id[info$row]
    idx <- which(rv$detail$row_id == rid)
    if (!length(idx)) return()
    val <- DT::coerceValue(info$value, rv$detail[idx, colname])
    if (colname == "confianca" && !val %in% c("alta","media","baixa")) {
      showNotification("Confiança deve ser alta, media ou baixa.", type="warning")
      return()
    }
    rv$detail[idx, colname] <- val
    rv$mics <- consolidate_mics(rv$detail)
  })

  observeEvent(input$add_control, {
    req(rv$mics)
    sel <- input$mics_table_rows_selected
    if (!length(sel)) {
      showNotification("Selecione um ou mais MICs na tabela.", type="warning")
      return()
    }
    chosen <- rv$mics[sel, , drop=FALSE]
    add <- suggest_control_rows(chosen, rv$pdf_name)
    synced <- append_control_rows(rv$control, add)
    rv$control <- synced$control
    showNotification(paste(length(synced$added), "novo(s) registro(s) incluído(s);", length(synced$skipped), "registro(s) já existente(s) atualizado(s)/mantido(s)."), type="message")
  })

  observeEvent(input$load_control, {
    req(input$control_file)
    x <- tryCatch(read_control_file(input$control_file$datapath), error=function(e) e)
    if (inherits(x, "error")) {
      showNotification(conditionMessage(x), type="error", duration=10)
      return()
    }
    rv$control <- ensure_control_columns(x)
    showNotification(paste(nrow(rv$control), "registro(s) carregados."), type="message")
  })

  observeEvent(input$add_blank_control, {
    x <- rv$control
    if (ncol(x) == 0) x <- as.data.frame(setNames(replicate(length(control_columns), character(0), simplify=FALSE), control_columns), check.names=FALSE)
    blank <- as.data.frame(as.list(setNames(rep(NA_character_, ncol(x)), names(x))), check.names=FALSE, stringsAsFactors=FALSE)
    rv$control <- rbind(x, blank)
  })

  output$control_table <- renderDT({
    req(rv$control)
    datatable(rv$control, rownames=FALSE, filter="top", editable=TRUE,
              options=list(scrollX=TRUE, pageLength=20, fixedColumns=list(leftColumns=2)))
  })

  observeEvent(input$control_table_cell_edit, {
    info <- input$control_table_cell_edit
    req(nrow(rv$control) >= info$row)
    colname <- names(rv$control)[info$col + 1]
    rv$control[info$row, colname] <- DT::coerceValue(info$value, rv$control[info$row, colname])
  })

  output$download_mics_csv <- downloadHandler(
    filename = function() paste0("MICs_extraidos_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$mics, file, row.names=FALSE, na="", fileEncoding="UTF-8")
  )

  output$download_control_csv <- downloadHandler(
    filename = function() paste0("CONTROLE_EMBARQUES_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$control, file, row.names=FALSE, na="", fileEncoding="UTF-8")
  )

  output$download_control_xlsx <- downloadHandler(
    filename = function() paste0("CONTROLE_EMBARQUES_MERIDIAN_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wbx <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wbx, "CONTROLE")
      openxlsx::writeData(wbx, "CONTROLE", rv$control, withFilter=TRUE)
      hs <- openxlsx::createStyle(fgFill=brand_blue, fontColour="#FFFFFF", textDecoration="bold", halign="center")
      openxlsx::addStyle(wbx, "CONTROLE", hs, rows=1, cols=seq_len(ncol(rv$control)), gridExpand=TRUE)
      openxlsx::freezePane(wbx, "CONTROLE", firstRow=TRUE)
      openxlsx::setColWidths(wbx, "CONTROLE", cols=seq_len(ncol(rv$control)), widths="auto")
      if (!is.null(rv$mics) && nrow(rv$mics)) {
        openxlsx::addWorksheet(wbx, "MIC_CONSOLIDADO")
        openxlsx::writeData(wbx, "MIC_CONSOLIDADO", rv$mics, withFilter=TRUE)
        openxlsx::addStyle(wbx, "MIC_CONSOLIDADO", hs, rows=1, cols=seq_len(ncol(rv$mics)), gridExpand=TRUE)
        openxlsx::freezePane(wbx, "MIC_CONSOLIDADO", firstRow=TRUE)
      }
      if (!is.null(rv$detail) && nrow(rv$detail)) {
        openxlsx::addWorksheet(wbx, "MIC_ITENS")
        openxlsx::writeData(wbx, "MIC_ITENS", rv$detail, withFilter=TRUE)
        openxlsx::addStyle(wbx, "MIC_ITENS", hs, rows=1, cols=seq_len(ncol(rv$detail)), gridExpand=TRUE)
        openxlsx::freezePane(wbx, "MIC_ITENS", firstRow=TRUE)
      }
      if (!is.null(rv$audit) && nrow(rv$audit)) {
        openxlsx::addWorksheet(wbx, "AUDITORIA")
        openxlsx::writeData(wbx, "AUDITORIA", rv$audit, withFilter=TRUE)
        openxlsx::addStyle(wbx, "AUDITORIA", hs, rows=1, cols=seq_len(ncol(rv$audit)), gridExpand=TRUE)
        openxlsx::freezePane(wbx, "AUDITORIA", firstRow=TRUE)
      }
      mapa <- data.frame(
        COLUNA = control_columns,
        ORIGEM = c("Item 24", "Item 34", "Item 33", "Item 38", "Item 36", "Manual/base", "Manual/base", "Item 23", "Item 1", "Manual/base", "Manual/base", "PDF", "Sistema"),
        stringsAsFactors=FALSE
      )
      openxlsx::addWorksheet(wbx, "MAPEAMENTO")
      openxlsx::writeData(wbx, "MAPEAMENTO", mapa, withFilter=TRUE)
      openxlsx::addStyle(wbx, "MAPEAMENTO", hs, rows=1, cols=1:ncol(mapa), gridExpand=TRUE)
      openxlsx::saveWorkbook(wbx, file, overwrite=TRUE)
    }
  )

  kpis <- reactive(summary_kpis(rv$detail, rv$mics, rv$control))
  output$kpi_paginas <- renderText(kpis()$paginas)
  output$kpi_mics <- renderText(kpis()$mics)
  output$kpi_revisar <- renderText(kpis()$revisar)
  output$kpi_embarques <- renderText(kpis()$embarques)

  output$dashboard_mics <- renderDT({
    if (is.null(rv$mics) || !nrow(rv$mics)) return(datatable(data.frame(Status="Nenhum MIC processado nesta sessão"), rownames=FALSE, options=list(dom="t")))
    d <- head(rv$mics[, c("mic_id","paginas","status_revisao","item_06","item_23")], 8)
    names(d) <- c("MIC","Páginas","Status","Emissão MIC","CRT/Carta de porte")
    datatable(d, rownames=FALSE, options=list(dom="t", scrollX=TRUE))
  })

  output$dashboard_controle <- renderDT({
    d <- rv$control
    if (is.null(d) || !nrow(d)) return(datatable(data.frame(Status="Controle vazio"), rownames=FALSE, options=list(dom="t")))
    keep <- intersect(c("URF","IMPORTADOR","EXPORTADOR","FATURA","CRT","TRANSPORTADORA","DATA REGISTRO"), names(d))
    datatable(tail(d[, keep, drop=FALSE], 8), rownames=FALSE, options=list(dom="t", scrollX=TRUE))
  })
}

shinyApp(ui, server)
