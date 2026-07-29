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
source("R/management.R")
source("R/adapter_seara.R")
source("R/db.R")

brand_blue <- "#2F5FAA"; navy <- "#173B78"; brand_orange <- "#F28A32"
app_theme <- bs_theme(version=5,bg="#F6F8FC",fg="#172033",primary=brand_blue,secondary=brand_orange,base_font=font_google("Inter"),heading_font=font_google("Inter"))

metric_card <- function(label, output_id, icon_name="circle", tone="blue") div(class=paste("metric-card",paste0("tone-",tone)),div(class="metric-icon",icon(icon_name)),div(div(class="metric-label",label),div(class="metric-value",textOutput(output_id,inline=TRUE))))
sidebar_link <- function(id,label,icon_name) actionLink(id,tagList(icon(icon_name),span(label)),class="side-link")
section_head <- function(kicker,title,subtitle=NULL,action=NULL) div(class="page-head",div(div(class="page-kicker",kicker),h1(title),if(!is.null(subtitle)) p(subtitle)),action)
empty_hint <- function(txt) div(class="empty-hint",icon("circle-info"),span(txt))

login_ui <- function(error = NULL) {
  div(class="login-shell",
    div(class="login-orbit login-orbit-one"),
    div(class="login-orbit login-orbit-two"),
    div(class="login-panel",
      div(class="login-brand",
        tags$img(src="logo_meridian_comex.svg", class="login-logo", alt="MeridIAn Comex"),
        div(class="login-eyebrow","SISTEMA DE INTELIGÊNCIA OPERACIONAL"),
        h1("Bem-vindo ao MeridIAn Comex"),
        p("Acesse o ambiente de gestão de processos, cargas e inteligência documental da Meridian.")
      ),
      div(class="login-form",
        div(class="login-form-head",h2("Acessar plataforma"),p("Informe suas credenciais para continuar.")),
        textInput("login_user","Usuário",placeholder="Digite seu usuário",width="100%"),
        passwordInput("login_password","Senha",placeholder="Digite sua senha",width="100%"),
        if (!is.null(error)) div(class="login-error",icon("circle-exclamation"),span(error)),
        actionButton("login_btn","Entrar",icon=icon("arrow-right"),class="btn btn-primary login-submit"),
        div(class="login-security",icon("shield-halved"),span("Acesso restrito • Sessão protegida"))
      )
    ),
    div(class="login-footer",span("Meridian Comissária de Despachos Aduaneiros"),span("MeridIAn Comex • v4.3"))
  )
}

app_shell_ui <- function() {
  div(class="app-shell",
    tags$aside(class="app-sidebar",
      div(class="brand-zone",tags$img(src="logo_meridian_comex.svg",class="brand-logo",alt="MeridIAn Comex")),
      div(class="side-section",div(class="side-label","OPERAÇÃO"),
        sidebar_link("nav_dashboard","Dashboard","table-cells-large"), sidebar_link("nav_movements","Movimentações","arrow-right-arrow-left"),
        sidebar_link("nav_processes","Processos","folder-open"), sidebar_link("nav_loads","Cargas","truck-fast"), sidebar_link("nav_pending","Pendências","triangle-exclamation")
      ),
      div(class="side-section",div(class="side-label","ENTRADAS & IA"),
        sidebar_link("nav_import","Importações","cloud-arrow-up"), sidebar_link("nav_documents","Inteligência documental","wand-magic-sparkles")
      ),
      div(class="side-section",div(class="side-label","GESTÃO"),
        sidebar_link("nav_reports","Relatórios","chart-line"), sidebar_link("nav_search","Pesquisa","magnifying-glass"), sidebar_link("nav_audit","Auditoria","clock-rotate-left")
      ),
      div(class="side-section",div(class="side-label","SISTEMA"), sidebar_link("nav_master","Cadastros","building"), sidebar_link("nav_settings","Configurações","sliders")),
      div(class="sidebar-foot",div(class="product-chip",span(class="dot-live"),"Connect Cloud"),tags$small("MeridIAn Comex • v4.3"))
    ),
    tags$main(class="app-main",
      div(class="topbar",
        div(class="topbar-search",icon("magnifying-glass"),textInput("quick_query",NULL,placeholder="Buscar processo, DI, fatura, CRT ou MIC...")),
        div(class="topbar-actions",
          uiOutput("db_status"),
          div(class="last-update",textOutput("top_last_update",inline=TRUE)),
          div(class="user-pill",div(class="user-avatar","M"),div(strong("meridian"),tags$small("MeridIAn Comex"))),
          actionButton("logout_btn", NULL, icon=icon("right-from-bracket"), class="logout-btn", title="Sair")
        )
      ),
      div(class="workspace",
        navset_hidden(id="workspace_nav", selected="Dashboard",
          nav_panel("Dashboard",
            section_head("VISÃO EXECUTIVA","Operação em um só lugar","Processos, cargas, pendências e inteligência documental conectados em uma única visão.",actionButton("go_import","Nova importação",icon=icon("plus"),class="btn btn-primary btn-hero")),
            div(class="metrics-grid",metric_card("Processos ativos","dash_active","folder-open"),metric_card("Processos parciais","dash_partial","clock","orange"),metric_card("Finalizados","dash_final","circle-check","green"),metric_card("Cargas / MICs","dash_loads","truck-fast"),metric_card("Pendências","dash_pending","triangle-exclamation","red"),metric_card("Clientes","dash_clients","building")),
            div(class="content-grid two-thirds",
              card(class="product-card",card_header(div(h4("Processos recentes"),span(class="subtle","Estado operacional atual"))),card_body(DTOutput("dashboard_controle"))),
              card(class="product-card",card_header(div(h4("Inteligência documental"),span(class="subtle","MICs processados na sessão"))),card_body(DTOutput("dashboard_mics")))
            ),
            div(class="content-grid thirds",
              card(class="product-card action-card",card_body(div(class="action-icon",icon("cloud-arrow-up")),h4("Importar relatórios"),p("Receba planilhas operacionais e consolide processos e cargas."),actionButton("dash_import","Abrir importações",class="btn btn-outline-primary"))),
              card(class="product-card action-card",card_body(div(class="action-icon ai",icon("wand-magic-sparkles")),h4("Ler documentos com IA"),p("Extraia MIC/DTA escaneados com visão multimodal e revisão assistida."),actionButton("dash_ai","Abrir IA documental",class="btn btn-outline-primary"))),
              card(class="product-card action-card",card_body(div(class="action-icon",icon("file-export")),h4("Gerar relatório"),p("Exporte uma visão consolidada de processos, cargas e pendências."),downloadButton("download_management_xlsx","Gerar Excel",class="btn btn-outline-primary")))
            )
          ),
          nav_panel("Movimentações",
            section_head("HISTÓRICO OPERACIONAL","Movimentações","Consulte o que mudou, quando mudou e em quais processos."),
            div(class="filter-strip",dateRangeInput("movement_period","Período",start=Sys.Date()-30,end=Sys.Date(),format="dd/mm/yyyy"),selectInput("movement_date_type","Critério",choices=c("Alterações no sistema","Data da operação","Data da importação"),selected="Alterações no sistema")),
            div(class="metrics-grid compact",metric_card("Registros no período","movement_count","arrow-right-arrow-left"),metric_card("Clientes movimentados","movement_clients","building")),
            card(class="product-card",card_header(h4("Linha operacional")),card_body(DTOutput("movements_table")))
          ),
          nav_panel("Processos",
            section_head("GESTÃO OPERACIONAL","Processos","Acompanhe o estado atual e pesquise por cliente, processo, fatura, DI, CRT, produto ou URF."),
            card(class="product-card",card_body(DTOutput("processes_table")))
          ),
          nav_panel("Cargas",section_head("EMBARQUES","Cargas","Cada MIC/carga é tratado como um registro independente e rastreável."),card(class="product-card",card_body(DTOutput("loads_table")))),
          nav_panel("Pendências",section_head("FILA DE TRABALHO","Pendências","Pontos que exigem complemento, associação ou revisão operacional."),card(class="product-card",card_body(DTOutput("pending_table")))),
          nav_panel("Importações",
            section_head("ENTRADA OPERACIONAL","Importar relatórios","Upload em lote com validação prévia. A primeira configuração é o layout SEARA."),
            div(class="content-grid split",
              card(class="product-card",card_header(h4("Relatórios SEARA")),card_body(fileInput("seara_files","Arquivos .xls, .xlsx ou .xlsm",multiple=TRUE,accept=c(".xls",".xlsx",".xlsm")),p(class="help-note","Os arquivos são lidos somente para extração. O original nunca é alterado."),actionButton("process_seara","Processar arquivos validados",class="btn btn-primary w-100",icon=icon("gears")))),
              card(class="product-card",card_header(h4("Validação antes da importação")),card_body(DTOutput("seara_preview_table")))
            ),
            card(class="product-card",card_header(h4("Log desta sessão")),card_body(DTOutput("import_log_table")))
          ),
          nav_panel("Inteligência documental",
            section_head("IA DOCUMENTAL","Leitura inteligente de MIC/DTA","A API multimodal continua preservada, agora integrada ao fluxo operacional do MeridIAn Comex."),
            div(class="content-grid split",
              card(class="product-card",card_header(h4("Documento")),card_body(fileInput("mic_pdf","PDF com MIC/DTA",accept=".pdf",buttonLabel="Selecionar PDF"),uiOutput("pdf_info_ui"),uiOutput("page_range_ui"),checkboxInput("process_all","Processar todas as páginas",TRUE),checkboxInput("auto_control","Sincronizar automaticamente com o controle",TRUE),actionButton("run_ai","Extrair com IA",class="btn btn-primary w-100",icon=icon("wand-magic-sparkles")),uiOutput("processing_summary_ui"))),
              card(class="product-card",card_header(h4("Pré-visualização")),card_body(uiOutput("preview_selector_ui"),div(class="preview-wrap",imageOutput("page_preview",height="650px"))))
            ),
            navset_card_tab(
              nav_panel("MICs consolidados",DTOutput("mics_table")),
              nav_panel("Itens extraídos",selectInput("detail_mic_filter","Filtrar MIC",choices=c("Todos"=""),width="320px"),DTOutput("detail_table")),
              nav_panel("Revisão",radioButtons("review_level","Mostrar",choices=c("Itens incertos"="baixa","Todos"="todos"),selected="baixa",inline=TRUE),div(class="content-grid split",DTOutput("review_table"),div(class="preview-wrap",imageOutput("review_preview",height="620px")))),
              nav_panel("Controle",div(class="toolbar-row",actionButton("add_control","Adicionar MICs selecionados",icon=icon("plus"),class="btn btn-outline-primary"),downloadButton("download_control_xlsx","Exportar controle",class="btn btn-primary")),DTOutput("control_table"))
            )
          ),
          nav_panel("Relatórios",section_head("ANÁLISE & SAÍDA","Relatórios","Gere bases consolidadas para operação e gestão."),div(class="content-grid thirds",card(class="product-card action-card",card_body(icon("file-excel"),h4("Relatório gerencial"),p("Processos, cargas, movimentações, pendências e controle."),downloadButton("download_management_xlsx","Baixar Excel",class="btn btn-primary"))),card(class="product-card action-card",card_body(icon("table"),h4("Controle operacional"),p("Exporta a estrutura A:M de controle de embarques."),downloadButton("download_control_xlsx","Baixar controle",class="btn btn-outline-primary"))),card(class="product-card action-card",card_body(icon("file-csv"),h4("MICs extraídos"),p("Base consolidada de documentos processados por IA."),downloadButton("download_mics_csv","Baixar CSV",class="btn btn-outline-primary"))))),
          nav_panel("Pesquisa",section_head("CONSULTA RÁPIDA","Pesquisa global","Localize processos e cargas sem precisar saber em qual módulo a informação está."),textInput("global_query",NULL,placeholder="Digite processo, DI, fatura, CRT, MIC, cliente, produto ou URF...",width="100%"),card(class="product-card",card_body(DTOutput("search_table")))),
          nav_panel("Auditoria",section_head("RASTREABILIDADE","Auditoria","Histórico técnico do processamento documental e das importações desta versão."),div(class="content-grid split",card(class="product-card",card_header(h4("IA documental")),card_body(DTOutput("audit_table"))),card(class="product-card",card_header(h4("Importações")),card_body(DTOutput("import_log_table"))))),
          nav_panel("Cadastros",section_head("ESTRUTURA MESTRE","Cadastros","Fundação preparada para clientes, unidades, aliases, produtos, NCMs, transportadoras, URFs e layouts."),div(class="notice-panel",icon("database"),div(h4("Camada PostgreSQL preparada"),p("A V4 inclui migrations e variáveis DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD e DB_SSLMODE. Enquanto o banco externo não estiver configurado, o aplicativo mantém o modo sessão para preservar o funcionamento atual."))),card(class="product-card",card_header(h4("Mapeamento MIC → Controle")),card_body(DTOutput("mapping_table")))),
          nav_panel("Configurações",section_head("ADMINISTRAÇÃO","Configurações","IA, desempenho e infraestrutura."),div(class="content-grid split",card(class="product-card",card_header(h4("OpenAI")),card_body(passwordInput("api_key","OPENAI API Key",placeholder="Usa OPENAI_API_KEY do Connect Cloud quando vazio"),selectInput("processing_profile","Perfil",choices=c("Rápido"="fast","Equilibrado"="balanced","Máxima precisão"="precision"),selected="fast"),selectInput("model","Modelo",choices=c("GPT-5 mini"="gpt-5-mini","GPT-5"="gpt-5","GPT-4.1"="gpt-4.1"),selected="gpt-5-mini"),numericInput("parallel_calls","Chamadas simultâneas",3,min=1,max=4),checkboxInput("smart_fallback","Fallback seletivo com GPT-5",TRUE),selectInput("image_detail","Detalhe da imagem",choices=c("Alto"="high","Automático"="auto","Baixo"="low"),selected="high"))),card(class="product-card",card_header(h4("Infraestrutura")),card_body(uiOutput("db_status"),tags$hr(),p(class="help-note","Para produção multiusuário e histórico persistente, configure PostgreSQL externo e os secrets no Connect Cloud. O schema inicial está em db/001_initial_schema.sql.")))) )
        )
      )
    )
  )
}

ui <- page_fillable(
  theme=app_theme,
  tags$head(
    tags$link(rel="stylesheet",href="styles.css"),
    tags$meta(name="viewport",content="width=device-width, initial-scale=1"),
    tags$title("MeridIAn Comex")
  ),
  uiOutput("root_ui")
)

server <- function(input, output, session) {
  auth <- reactiveValues(logged_in = FALSE, login_error = NULL)

  output$root_ui <- renderUI({
    if (isTRUE(auth$logged_in)) app_shell_ui() else login_ui(auth$login_error)
  })

  observeEvent(input$login_btn, {
    user <- trimws(as.character(input$login_user %||% ""))
    pass <- as.character(input$login_password %||% "")

    # Em produção, os secrets podem substituir as credenciais padrão via Connect Cloud.
    expected_user <- Sys.getenv("MERIDIAN_APP_USER", unset = "meridian")
    env_pass <- Sys.getenv("MERIDIAN_APP_PASSWORD", unset = "")
    default_pass_hash <- "7914a51b174f2af3c060ac57e174f0d343a256d6a024b9987aa059a2a34ea6ae"
    password_ok <- if (nzchar(env_pass)) {
      identical(pass, env_pass)
    } else {
      identical(digest::digest(pass, algo = "sha256", serialize = FALSE), default_pass_hash)
    }

    if (identical(user, expected_user) && isTRUE(password_ok)) {
      auth$logged_in <- TRUE
      auth$login_error <- NULL
    } else {
      auth$logged_in <- FALSE
      auth$login_error <- "Usuário ou senha inválidos."
      updateTextInput(session, "login_password", value = "")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$logout_btn, {
    auth$logged_in <- FALSE
    auth$login_error <- NULL
  }, ignoreInit = TRUE)

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
    last_processing = NULL,
    report_preview = data.frame(),
    report_loads = data.frame(),
    import_log = data.frame()
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
    list(src = f, contentType = "image/jpeg", alt = paste("Página", input$preview_page))
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
    list(src=f, contentType="image/jpeg", alt=paste("Página", rv$review_page))
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

  # --- Navegação do produto -------------------------------------------------
  nav_map <- c(
    nav_dashboard="Dashboard", nav_movements="Movimentações", nav_processes="Processos",
    nav_loads="Cargas", nav_pending="Pendências", nav_import="Importações",
    nav_documents="Inteligência documental", nav_reports="Relatórios", nav_search="Pesquisa",
    nav_audit="Auditoria", nav_master="Cadastros", nav_settings="Configurações"
  )
  lapply(names(nav_map), function(id) {
    observeEvent(input[[id]], { bslib::nav_select("workspace_nav", selected = unname(nav_map[[id]]), session = session) }, ignoreInit=TRUE)
  })

  # --- Dados operacionais derivados -----------------------------------------
  processes_data <- reactive(processes_from_control(rv$control))
  loads_data <- reactive(loads_from_mics(rv$mics))
  pending_data <- reactive(pending_from_control(rv$control))
  movements_data <- reactive(movement_summary(rv$control))

  output$top_last_update <- renderText({
    if (is.null(rv$control) || !nrow(rv$control)) return("Sem atualizações")
    x <- suppressWarnings(as.POSIXct(rv$control$ATUALIZAÇÃO))
    x <- x[!is.na(x)]
    if (!length(x)) return("Sem atualizações")
    paste("Atualizado", format(max(x), "%d/%m/%Y %H:%M"))
  })
  output$db_status <- renderUI({
    if (meridian_db_ready()) div(class="status-dot status-ok", span(), "PostgreSQL configurado")
    else div(class="status-dot status-demo", span(), "Modo sessão")
  })

  output$dash_active <- renderText({ fmt_int(sum(processes_data()$STATUS != "FINALIZADO", na.rm=TRUE)) })
  output$dash_partial <- renderText({ fmt_int(sum(processes_data()$STATUS == "PARCIAL", na.rm=TRUE)) })
  output$dash_final <- renderText({ fmt_int(sum(processes_data()$STATUS == "FINALIZADO", na.rm=TRUE)) })
  output$dash_loads <- renderText({ fmt_int(if (is.null(rv$mics)) 0 else nrow(rv$mics)) })
  output$dash_pending <- renderText({ fmt_int(nrow(pending_data())) })
  output$dash_clients <- renderText({
    d <- processes_data(); fmt_int(if (!nrow(d)) 0 else length(unique(d$CLIENTE[!is.na(d$CLIENTE)])))
  })

  output$processes_table <- renderDT({
    d <- processes_data()
    if (!nrow(d)) return(datatable(data.frame(Status="Nenhum processo disponível"), rownames=FALSE, options=list(dom="t")))
    datatable(d, rownames=FALSE, filter="top", selection="single", options=list(scrollX=TRUE, pageLength=15, autoWidth=TRUE))
  })
  output$loads_table <- renderDT({
    d <- loads_data()
    if (!nrow(d)) return(datatable(data.frame(Status="Nenhuma carga/MIC processada nesta sessão"), rownames=FALSE, options=list(dom="t")))
    datatable(d, rownames=FALSE, filter="top", options=list(scrollX=TRUE, pageLength=20))
  })
  output$pending_table <- renderDT({
    d <- pending_data()
    if (!nrow(d)) return(datatable(data.frame(Status="Nenhuma pendência identificada"), rownames=FALSE, options=list(dom="t")))
    datatable(d, rownames=FALSE, filter="top", options=list(scrollX=TRUE, pageLength=20))
  })
  output$movements_table <- renderDT({
    d <- movements_data()
    if (!nrow(d)) return(datatable(data.frame(Status="Nenhuma movimentação disponível"), rownames=FALSE, options=list(dom="t")))
    rng <- input$movement_period
    if (!is.null(rng) && length(rng)==2) d <- d[d$DATA >= rng[1] & d$DATA <= rng[2],,drop=FALSE]
    datatable(d, rownames=FALSE, filter="top", options=list(scrollX=TRUE, pageLength=20))
  })
  output$movement_count <- renderText({
    d <- movements_data(); if (!nrow(d)) return("0")
    rng <- input$movement_period; if (!is.null(rng) && length(rng)==2) d <- d[d$DATA >= rng[1] & d$DATA <= rng[2],,drop=FALSE]
    fmt_int(nrow(d))
  })
  output$movement_clients <- renderText({
    d <- movements_data(); if (!nrow(d)) return("0")
    rng <- input$movement_period; if (!is.null(rng) && length(rng)==2) d <- d[d$DATA >= rng[1] & d$DATA <= rng[2],,drop=FALSE]
    fmt_int(length(unique(d$CLIENTE)))
  })

  # Pesquisa global
  search_results <- reactive({
    q <- trimws(toupper(input$global_query %||% ""))
    if (!nzchar(q)) return(data.frame())
    p <- processes_data(); l <- loads_data(); out <- list()
    if (nrow(p)) {
      hit <- apply(p,1,function(z) any(grepl(q, toupper(as.character(z)), fixed=TRUE), na.rm=TRUE))
      if (any(hit)) { z<-p[hit,,drop=FALSE]; z$TIPO <- "Processo"; out[[length(out)+1]]<-z[,c("TIPO","PROCESSO","CLIENTE","FATURA","DI","CRT","URF","STATUS")] }
    }
    if (nrow(l)) {
      hit <- apply(l,1,function(z) any(grepl(q, toupper(as.character(z)), fixed=TRUE), na.rm=TRUE))
      if (any(hit)) { z<-l[hit,,drop=FALSE]; out[[length(out)+1]]<-data.frame(TIPO="Carga",PROCESSO=NA,CLIENTE=NA,FATURA=z$FATURA,DI=NA,CRT=z$CRT,URF=z$URF,STATUS=z$STATUS_REVISAO,stringsAsFactors=FALSE) }
    }
    if (!length(out)) return(data.frame())
    do.call(rbind,out)
  })
  output$search_table <- renderDT({
    d <- search_results()
    if (!nrow(d)) return(datatable(data.frame(Resultado="Digite processo, DI, fatura, CRT, MIC, cliente, produto, placa ou URF"), rownames=FALSE, options=list(dom="t")))
    datatable(d, rownames=FALSE, options=list(scrollX=TRUE, pageLength=20))
  })

  # --- Importador de relatórios SEARA ----------------------------------------
  observeEvent(input$seara_files, {
    req(input$seara_files)
    previews <- list(); loads <- list(); logs <- list()
    for (i in seq_len(nrow(input$seara_files))) {
      f <- input$seara_files[i,]
      parsed <- tryCatch(parse_seara_report(f$datapath, f$name), error=function(e) e)
      if (inherits(parsed,"error")) {
        logs[[length(logs)+1]] <- data.frame(ARQUIVO=f$name, STATUS="Erro", DETALHE=conditionMessage(parsed), stringsAsFactors=FALSE)
      } else {
        m <- parsed$meta; m$CARGAS_ENCONTRADAS <- nrow(parsed$loads); m$STATUS_VALIDACAO <- ifelse(is.na(m$FATURA) || is.na(m$PROCESSO), "Revisar", "Pronto")
        previews[[length(previews)+1]] <- m
        if (nrow(parsed$loads)) { z<-parsed$loads; z$ARQUIVO<-f$name; loads[[length(loads)+1]]<-z }
        logs[[length(logs)+1]] <- data.frame(ARQUIVO=f$name, STATUS="Validado", DETALHE=paste(nrow(parsed$loads),"carga(s)"), stringsAsFactors=FALSE)
      }
    }
    rv$report_preview <- if (length(previews)) do.call(rbind,previews) else data.frame()
    rv$report_loads <- if (length(loads)) do.call(rbind,loads) else data.frame()
    rv$import_log <- if (length(logs)) do.call(rbind,logs) else data.frame()
  })
  output$seara_preview_table <- renderDT({
    d <- rv$report_preview
    if (is.null(d) || !nrow(d)) return(datatable(data.frame(Status="Selecione relatórios .xls/.xlsx/.xlsm para validar"),rownames=FALSE,options=list(dom="t")))
    datatable(d,rownames=FALSE,options=list(scrollX=TRUE,pageLength=12))
  })
  output$import_log_table <- renderDT({
    d <- rv$import_log
    if (is.null(d) || !nrow(d)) return(datatable(data.frame(Status="Nenhuma importação nesta sessão"),rownames=FALSE,options=list(dom="t")))
    datatable(d,rownames=FALSE,options=list(dom="t",scrollX=TRUE))
  })
  observeEvent(input$process_seara, {
    req(input$seara_files)
    if (is.null(rv$report_preview) || !nrow(rv$report_preview)) { showNotification("Valide os arquivos antes de processar.",type="warning"); return() }
    added <- 0L; updated <- 0L
    for (i in seq_len(nrow(input$seara_files))) {
      parsed <- tryCatch(parse_seara_report(input$seara_files$datapath[i], input$seara_files$name[i]), error=function(e) NULL)
      if (is.null(parsed)) next
      sync <- append_control_rows(rv$control, seara_to_control(parsed)); rv$control <- sync$control
      added <- added + length(sync$added); updated <- updated + length(sync$skipped)
    }
    showNotification(paste(added,"processo(s) incluído(s) e",updated,"registro(s) reconciliado(s)."),type="message",duration=8)
  })

  # Relatório gerencial em Excel
  output$download_management_xlsx <- downloadHandler(
    filename=function() paste0("MeridIAn_Comex_Relatorio_",Sys.Date(),".xlsx"),
    content=function(file){
      wb<-openxlsx::createWorkbook(); hs<-openxlsx::createStyle(fgFill="#173B78",fontColour="#FFFFFF",textDecoration="bold")
      tabs<-list(Processos=processes_data(),Cargas=loads_data(),Movimentacoes=movements_data(),Pendencias=pending_data(),Controle=rv$control)
      for(nm in names(tabs)){ openxlsx::addWorksheet(wb,nm); openxlsx::writeData(wb,nm,tabs[[nm]],withFilter=TRUE); if(ncol(tabs[[nm]])>0) openxlsx::addStyle(wb,nm,hs,rows=1,cols=seq_len(ncol(tabs[[nm]])),gridExpand=TRUE); openxlsx::freezePane(wb,nm,firstRow=TRUE); openxlsx::setColWidths(wb,nm,cols=seq_len(max(1,ncol(tabs[[nm]]))),widths="auto") }
      openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
    }
  )

  observeEvent(input$go_import, { bslib::nav_select("workspace_nav", selected="Importações", session=session) })
  observeEvent(input$dash_import, { bslib::nav_select("workspace_nav", selected="Importações", session=session) })
  observeEvent(input$dash_ai, { bslib::nav_select("workspace_nav", selected="Inteligência documental", session=session) })
  observeEvent(input$quick_query, {
    if (!is.null(input$quick_query) && nzchar(trimws(input$quick_query))) { updateTextInput(session,"global_query",value=input$quick_query); bslib::nav_select("workspace_nav",selected="Pesquisa",session=session) }
  }, ignoreInit=TRUE)

}

shinyApp(ui, server)
