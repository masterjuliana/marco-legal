# app.R

# --- 1. Carregar Pacotes ---
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr) # <--- ESSENCIAL PARA 'replace_na'

# --- 2. Carregar e Preparar Dados ---
csv_file_path <- "W:/R/Base Regulação/Documentos_ANA_Saneamento_Basico/metadados_normativos_saneamento_ana.csv"
csv_file_path <- normalizePath(csv_file_path, winslash = "/", mustWork = FALSE)

# Inicializa df como NULL para lidar com o escopo dentro do tryCatch
df <- NULL

tryCatch({
  # Tentar com codificação UTF-8 explicitamente
  df <- read_csv(csv_file_path,
                 col_types = cols(
                   id_documento = col_character(),
                   titulo = col_character(),
                   ementa = col_character(),
                   data_publicacao = col_date(), # Tipo correto
                   orgao_emissor = col_character(),
                   tipo_documento = col_character(),
                   numero_documento = col_character(),
                   ano_documento = col_character(),
                   status_vigencia = col_character(),
                   abrangencia_geografica = col_character(),
                   estado = col_character(),
                   link_original = col_character(),
                   modulo_saneamento = col_character(), # <--- Tipo explícito para esta coluna
                   area_tematica = col_character(), # <--- Tipo explícito
                   caminho_local = col_character()
                 ),
                 locale = locale(encoding = "UTF-8"))
  message(paste("Dados carregados com sucesso de:", csv_file_path, "com UTF-8."))
}, error = function(e) {
  message(paste("Falha ao carregar com UTF-8. Tentando com Latin1...", e$message))
  tryCatch({
    df <<- read_csv(csv_file_path, # Usar <<- para atribuir no escopo pai
                    col_types = cols(
                      id_documento = col_character(),
                      titulo = col_character(),
                      ementa = col_character(),
                      data_publicacao = col_date(),
                      orgao_emissor = col_character(),
                      tipo_documento = col_character(),
                      numero_documento = col_character(),
                      ano_documento = col_character(),
                      status_vigencia = col_character(),
                      abrangencia_geografica = col_character(),
                      estado = col_character(),
                      link_original = col_character(),
                      modulo_saneamento = col_character(),
                      area_tematica = col_character(),
                      caminho_local = col_character()
                    ),
                    locale = locale(encoding = "latin1"))
    message(paste("Dados carregados com sucesso de:", csv_file_path, "com Latin1."))
  }, error = function(e2) {
    stop(paste("ERRO FATAL: Falha ao carregar o arquivo CSV com UTF-8 ou Latin1. Verifique o caminho e o formato:", csv_file_path,
               "\nDetalhes UTF-8:", e$message,
               "\nDetalhes Latin1:", e2$message,
               "\nCausa provável: Problema de codificação ou arquivo corrompido.",
               "\nPor favor, execute o script 'scrape_ana_saneamento.R' novamente para gerar um CSV novo e tente.",
               "\nSe o problema persistir, verifique a codificação padrão do seu sistema ou tente abrir e salvar o CSV manualmente em um editor de texto (ex: Notepad++) como 'UTF-8 sem BOM'."))
  })
})

# --- VERIFICAÇÃO CRÍTICA APÓS O CARREGAMENTO ---
# Garante que df foi carregado e contém todas as colunas esperadas
required_cols <- c("data_publicacao", "ementa", "area_tematica", "status_vigencia",
                   "abrangencia_geografica", "estado", "modulo_saneamento",
                   "link_original", "caminho_local", "ano_documento")

missing_cols <- setdiff(required_cols, colnames(df))
if (is.null(df) || length(missing_cols) > 0) {
  stop(paste("ERRO FATAL: O DataFrame 'df' não foi carregado corretamente ou faltam colunas essenciais:",
             paste(missing_cols, collapse = ", "),
             "\nPor favor, verifique o CSV e o script de raspagem para garantir que todas as colunas sejam geradas corretamente."))
}


# Garante que 'data_publicacao' é um tipo de data e preenche NAs para outras colunas
df <- df %>%
  mutate(
    data_publicacao = as.Date(data_publicacao), # Reafirma como Date, lida com NAs
    ementa = replace_na(ementa, "Não informada."),
    area_tematica = replace_na(area_tematica, "Não Classificado"),
    status_vigencia = replace_na(status_vigencia, "Desconhecido"),
    abrangencia_geografica = replace_na(abrangencia_geografica, "Desconhecida"),
    estado = replace_na(estado, ""),
    modulo_saneamento = replace_na(modulo_saneamento, "Outros Normativos"), # Esta é a linha problemática
    link_original = replace_na(link_original, ""),
    caminho_local = replace_na(caminho_local, ""),
    # Assegura que ano_documento seja de um tipo consistente para gráficos
    ano_documento = as.character(replace_na(ano_documento, "Desconhecido"))
  )

# Criar opções dinâmicas para os filtros, garantindo que "Todos" seja a primeira opção
orgao_choices <- c("Todos", sort(unique(df$orgao_emissor)))
tipo_choices <- c("Todos", sort(unique(df$tipo_documento)))
area_choices <- c("Todos", sort(unique(df$area_tematica)))
status_choices <- c("Todos", sort(unique(df$status_vigencia)))
abrangencia_choices <- c("Todos", sort(unique(df$abrangencia_geografica)))
estado_choices <- c("Todos", sort(unique(df$estado[df$abrangencia_geografica == "Estadual" & df$estado != ""])))
modulo_choices <- c("Todos", sort(unique(df$modulo_saneamento)))

# Definir a data inicial e final para o dateRangeInput
min_date_data <- if(nrow(df) > 0 && !all(is.na(df$data_publicacao))) min(df$data_publicacao, na.rm = TRUE) else Sys.Date() - years(5)
max_date_data <- if(nrow(df) > 0 && !all(is.na(df$data_publicacao))) max(df$data_publicacao, na.rm = TRUE) else Sys.Date()


# --- 3. UI (User Interface) ---
ui <- dashboardPage(
  dashboardHeader(title = "Normativos ANA: Saneamento Básico"),
  dashboardSidebar(
    sidebarMenu(id = "sidebarMenu", # Adiciona um ID para controlar a seleção da aba
                menuItem("Visão Geral", tabName = "overview", icon = icon("dashboard")),
                menuItem("Tabela de Documentos", tabName = "table", icon = icon("table")),
                menuItem("Detalhes do Documento", tabName = "details", icon = icon("info-circle")),
                menuItem("Estatísticas", tabName = "stats", icon = icon("chart-bar"))
    ),
    
    # Filtros na barra lateral
    h4("Filtros:"),
    selectInput("modulo_filter", "Módulo Saneamento:", choices = modulo_choices), # Novo filtro por módulo
    selectInput("orgao_filter", "Órgão Emissor:", choices = orgao_choices),
    selectInput("tipo_filter", "Tipo de Documento:", choices = tipo_choices),
    selectInput("area_filter", "Área Temática:", choices = area_choices),
    selectInput("status_filter", "Status de Vigência:", choices = status_choices),
    selectInput("abrangencia_filter", "Abrangência Geográfica:", choices = abrangencia_choices),
    conditionalPanel(
      condition = "input.abrangencia_filter == 'Estadual'",
      selectInput("estado_filter", "Estado:", choices = estado_choices)
    ),
    dateRangeInput("date_filter", "Período de Publicação:",
                   start = min_date_data,
                   end = max_date_data,
                   format = "dd/mm/yyyy",
                   language = "pt-BR")
  ),
  
  # --- 4. Body (Content) ---
  dashboardBody(
    tabItems(
      # Visão Geral Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Boas-Vindas!", width = 12,
                    p("Bem-vindo ao painel de Normativos da ANA para o Saneamento Básico."),
                    p("Explore os documentos regulatórios utilizando os filtros na barra lateral e as abas de navegação.")
                )
              ),
              fluidRow(
                valueBoxOutput("totalDocsBox"),
                valueBoxOutput("anaDocsBox"),
                valueBoxOutput("vigenteDocsBox")
              ),
              fluidRow(
                box(title = "Últimos 5 Documentos Publicados", width = 12,
                    DT::dataTableOutput("recentDocsTable"))
              )
      ),
      
      # Tabela de Documentos Tab
      tabItem(tabName = "table",
              h2("Documentos Regulatórios Detalhados"),
              DT::dataTableOutput("regulationTable")
      ),
      
      # Detalhes do Documento Tab
      tabItem(tabName = "details",
              h2("Detalhes do Documento Selecionado"),
              fluidRow(
                box(width = 12,
                    uiOutput("selectedDocDetails")
                )
              )
      ),
      
      # Estatísticas Tab
      tabItem(tabName = "stats",
              h2("Estatísticas dos Normativos"),
              fluidRow(
                box(title = "Documentos por Órgão Emissor", width = 6,
                    plotOutput("orgaoPlot")),
                box(title = "Documentos por Módulo de Saneamento", width = 6,
                    plotOutput("moduloPlot")) # Novo gráfico por módulo
              ),
              fluidRow(
                box(title = "Documentos por Tipo", width = 6,
                    plotOutput("tipoPlot")),
                box(title = "Documentos por Área Temática", width = 6,
                    plotOutput("areaPlot"))
              ),
              fluidRow(
                box(title = "Publicações ao Longo do Tempo", width = 12,
                    plotOutput("timePlot"))
              )
      )
    )
  )
)

# --- 5. Server Logic ---
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- df
    
    if (input$modulo_filter != "Todos") {
      data <- data %>% filter(modulo_saneamento == input$modulo_filter)
    }
    if (input$orgao_filter != "Todos") {
      data <- data %>% filter(orgao_emissor == input$orgao_filter)
    }
    if (input$tipo_filter != "Todos") {
      data <- data %>% filter(tipo_documento == input$tipo_filter)
    }
    if (input$area_filter != "Todos") {
      data <- data %>% filter(area_tematica == input$area_filter)
    }
    if (input$status_filter != "Todos") {
      data <- data %>% filter(status_vigencia == input$status_filter)
    }
    if (input$abrangencia_filter != "Todos") {
      data <- data %>% filter(abrangencia_geografica == input$abrangencia_filter)
    }
    if (input$abrangencia_filter == "Estadual" && input$estado_filter != "Todos") {
      data <- data %>% filter(estado == input$estado_filter)
    }
    
    # Filtragem de data: garantir que a data não seja NA
    # Assegura que o filtro de data só seja aplicado se a coluna for do tipo Date
    if ("data_publicacao" %in% names(data) && !inherits(data$data_publicacao, "Date")) {
      data$data_publicacao <- as.Date(data$data_publicacao) # Tenta converter novamente se necessário
    }
    
    data <- data %>% filter(
      !is.na(data_publicacao) &
        data_publicacao >= input$date_filter[1] &
        data_publicacao <= input$date_filter[2]
    )
    
    data
  })
  
  # --- Outputs para a aba 'Visão Geral' ---
  output$totalDocsBox <- renderValueBox({
    valueBox(
      nrow(filtered_data()), "Total de Documentos Encontrados", icon = icon("file-alt"),
      color = "blue"
    )
  })
  
  output$anaDocsBox <- renderValueBox({
    ana_docs <- filtered_data() %>% filter(orgao_emissor == "ANA") %>% nrow()
    valueBox(
      ana_docs, "Documentos da ANA", icon = icon("water"),
      color = "teal"
    )
  })
  
  output$vigenteDocsBox <- renderValueBox({
    vigente_docs <- filtered_data() %>% filter(status_vigencia == "Vigente (Presumido)") %>% nrow()
    valueBox(
      vigente_docs, "Documentos Vigentes (Presumidos)", icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$recentDocsTable <- DT::renderDataTable({
    filtered_data() %>%
      arrange(desc(data_publicacao)) %>%
      head(5) %>% # Mostrar os 5 documentos mais recentes
      select(data_publicacao, titulo, orgao_emissor, modulo_saneamento) %>%
      DT::datatable(
        options = list(dom = 't', # Apenas a tabela, sem search/pagination
                       ordering = FALSE,
                       searching = FALSE,
                       paging = FALSE),
        rownames = FALSE,
        colnames = c("Data", "Título", "Órgão Emissor", "Módulo")
      )
  })
  
  
  # --- Outputs para a aba 'Tabela de Documentos' ---
  output$regulationTable <- DT::renderDataTable({
    data_for_table <- filtered_data() %>%
      # Cria a coluna 'Abrir Documento' formatada como hiperlink
      mutate(
        # target='_blank' abre o link em uma nova aba
        'Abrir Documento' = ifelse(link_original != "",
                                   paste0('<a href="', link_original, '" target="_blank">Abrir Online</a>'),
                                   "N/A")
      ) %>%
      # Reordena e seleciona as colunas para a exibição na tabela
      select(
        titulo,
        modulo_saneamento,
        data_publicacao,
        orgao_emissor,
        tipo_documento,
        numero_documento,
        area_tematica,
        status_vigencia,
        `Abrir Documento`, # A coluna que contém o link clicável
        ementa # Será uma coluna oculta usada para detalhes
      )
    
    DT::datatable(
      data_for_table,
      options = list(
        pageLength = 10,
        scrollX = TRUE, # Permite scroll horizontal se a tabela for muito larga
        # Definir largura de colunas para melhor visualização
        columnDefs = list(
          list(width = '250px', targets = 0), # Título
          list(width = '150px', targets = 1), # Módulo
          list(width = '100px', targets = 2), # Data
          list(width = '80px', targets = c(3,4,5,7,8)), # Órgão, Tipo, Número, Status, Abrir Doc
          list(width = '120px', targets = 6), # Área Temática
          list(visible = FALSE, targets = ncol(data_for_table) - 1) # Esconder a coluna 'ementa'
        )
      ),
      rownames = FALSE,
      escape = FALSE, # IMPORTANTE: Permite que o HTML (o link) seja renderizado
      selection = 'single', # Permite selecionar uma única linha para detalhes
      colnames = c("Título", "Módulo Saneamento", "Publicação", "Órgão", "Tipo", "Número",
                   "Área Temática", "Status", "Abrir Documento", "Ementa (hidden)") # Renomear colunas
    )
  })
  
  # --- Lógica para exibir detalhes do documento selecionado na aba 'Detalhes' ---
  observeEvent(input$regulationTable_rows_selected, {
    selected_row_index <- input$regulationTable_rows_selected
    req(selected_row_index) # Garante que uma linha foi selecionada
    
    selected_doc <- filtered_data()[selected_row_index, ]
    
    output$selectedDocDetails <- renderUI({
      div(
        h3(selected_doc$titulo),
        hr(), # Linha separadora
        p(strong("ID do Documento:"), selected_doc$id_documento),
        p(strong("Módulo de Saneamento:"), selected_doc$modulo_saneamento),
        p(strong("Órgão Emissor:"), selected_doc$orgao_emissor),
        p(strong("Tipo de Documento:"), selected_doc$tipo_documento),
        p(strong("Número do Documento:"), selected_doc$numero_documento),
        p(strong("Ano do Documento:"), selected_doc$ano_documento),
        p(strong("Data de Publicação:"), format(selected_doc$data_publicacao, "%d/%m/%Y")),
        p(strong("Status de Vigência:"), selected_doc$status_vigencia),
        p(strong("Área(s) Temática(s):"), selected_doc$area_tematica),
        p(strong("Abrangência Geográfica:"), selected_doc$abrangencia_geografica,
          ifelse(selected_doc$abrangencia_geografica == "Estadual" && selected_doc$estado != "", paste0(" (", selected_doc$estado, ")"), "")),
        br(), # Quebra de linha
        p(strong("Ementa/Descrição:")),
        tags$blockquote(selected_doc$ementa), # Bloco de citação para a ementa
        br(),
        p(strong("Link Online:"),
          if (selected_doc$link_original != "") {
            tags$a(href = selected_doc$link_original, target = "_blank", "Acessar Documento Online")
          } else {
            "Link não disponível"
          }
        ),
        p(strong("Caminho Local do Arquivo:"),
          if (!is.na(selected_doc$caminho_local) && selected_doc$caminho_local != "" && file.exists(selected_doc$caminho_local)) {
            # O 'file:///' permite que alguns navegadores abram arquivos locais
            # No Windows, 'normalizePath' com 'winslash = "/"' ajuda na formatação da URL
            tags$a(href = paste0("file:///", normalizePath(selected_doc$caminho_local, winslash = "/")),
                   target = "_blank",
                   title = "Pode não funcionar diretamente do navegador por segurança (abra manualmente)",
                   normalizePath(selected_doc$caminho_local)) # Exibe o caminho
          } else {
            "Arquivo não baixado ou caminho desconhecido/inválido."
          }
        )
      )
    })
    
    # Mudar para a aba de detalhes automaticamente ao selecionar uma linha
    updateTabItems(session, "sidebarMenu", "details")
  })
  
  
  # --- Outputs para a aba 'Estatísticas' ---
  output$orgaoPlot <- renderPlot({
    # Garante que há dados para plotar
    req(nrow(filtered_data()) > 0)
    data_plot <- filtered_data() %>% count(orgao_emissor)
    ggplot(data_plot, aes(x = reorder(orgao_emissor, n), y = n, fill = orgao_emissor)) +
      geom_col() +
      coord_flip() +
      labs(title = "Documentos por Órgão Emissor", x = "Órgão Emissor", y = "Número de Documentos") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$moduloPlot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    data_plot <- filtered_data() %>% count(modulo_saneamento)
    ggplot(data_plot, aes(x = reorder(modulo_saneamento, n), y = n, fill = modulo_saneamento)) +
      geom_col() +
      coord_flip() +
      labs(title = "Documentos por Módulo de Saneamento", x = "Módulo", y = "Número de Documentos") +
      theme_minimal() +
      theme(legend.position = "none") +
      # Ajusta margens se os nomes dos módulos forem muito longos
      theme(axis.text.y = element_text(size = 9))
  })
  
  output$tipoPlot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    data_plot <- filtered_data() %>% count(tipo_documento)
    ggplot(data_plot, aes(x = reorder(tipo_documento, n), y = n, fill = tipo_documento)) +
      geom_col() +
      coord_flip() +
      labs(title = "Documentos por Tipo", x = "Tipo de Documento", y = "Número de Documentos") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$areaPlot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    data_plot <- filtered_data() %>% count(area_tematica)
    ggplot(data_plot, aes(x = reorder(area_tematica, n), y = n, fill = area_tematica)) +
      geom_col() +
      coord_flip() +
      labs(title = "Documentos por Área Temática", x = "Área Temática", y = "Número de Documentos") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$timePlot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    data_plot <- filtered_data() %>%
      mutate(ano_publicacao_int = as.integer(ano_documento)) %>%
      filter(!is.na(ano_publicacao_int)) %>%
      count(ano_publicacao_int)
    
    ggplot(data_plot, aes(x = ano_publicacao_int, y = n)) +
      geom_line(color = "darkblue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(title = "Número de Publicações por Ano", x = "Ano", y = "Número de Documentos") +
      theme_minimal() +
      scale_x_continuous(breaks = unique(data_plot$ano_publicacao_int)) # Garante todos os anos no eixo X
  })
}

# Rodar a aplicação
shinyApp(ui = ui, server = server)