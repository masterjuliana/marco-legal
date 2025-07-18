# app.R

# --- 1. Carregar Pacotes ---
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(lubridate) # Para manipulação de datas
library(ggplot2)   # Para gráficos

# --- 2. Carregar e Preparar Dados ---
# Ajuste o caminho do arquivo se ele não estiver no mesmo diretório
# Se estiver usando Excel, use readxl::read_excel("regulacoes_saneamento.xlsx")
tryCatch({
  df <- read_csv("regulacoes_saneamento.csv", col_types = cols(data_publicacao = col_date(format = "%Y-%m-%d")))
}, error = function(e) {
  stop("Erro ao carregar 'regulacoes_saneamento.csv'. Certifique-se de que o arquivo existe e está no formato CSV correto. Detalhes: ", e$message)
})


# Garantir que as colunas essenciais existem e estão no formato correto
df <- df %>%
  mutate(
    data_publicacao = as.Date(data_publicacao),
    # Preencher NAs para evitar problemas com filtros
    ementa = replace_na(ementa, ""),
    area_tematica = replace_na(area_tematica, "Não Classificado"),
    status_vigencia = replace_na(status_vigencia, "Desconhecido"),
    abrangencia_geografica = replace_na(abrangencia_geografica, "Desconhecida"),
    estado = replace_na(estado, ""),
    link_original = replace_na(link_original, "")
  )

# Criar opções dinâmicas para os filtros
orgao_choices <- c("Todos", sort(unique(df$orgao_emissor)))
tipo_choices <- c("Todos", sort(unique(df$tipo_documento)))
area_choices <- c("Todos", sort(unique(df$area_tematica)))
status_choices <- c("Todos", sort(unique(df$status_vigencia)))
abrangencia_choices <- c("Todos", sort(unique(df$abrangencia_geografica)))
estado_choices <- c("Todos", sort(unique(df$estado[df$abrangencia_geografica == "Estadual" & df$estado != ""])))


# --- 3. UI (User Interface) ---
ui <- dashboardPage(
  dashboardHeader(title = "Regulação de Saneamento no Brasil"),
  dashboardSidebar(
    sidebarMenu(id = "sidebarMenu", # Adiciona um ID para controlar a seleção da aba
                menuItem("Visão Geral", tabName = "overview", icon = icon("dashboard")),
                menuItem("Tabela de Documentos", tabName = "table", icon = icon("table")),
                menuItem("Detalhes do Documento", tabName = "details", icon = icon("info-circle")), # Nova aba para detalhes
                menuItem("Estatísticas", tabName = "stats", icon = icon("chart-bar"))
    ),
    
    # Filters in the sidebar
    h4("Filtros:"),
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
                   start = min(df$data_publicacao, na.rm = TRUE),
                   end = max(df$data_publicacao, na.rm = TRUE),
                   format = "dd/mm/yyyy",
                   language = "pt-BR")
  ),
  
  # --- 4. Body (Content) ---
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Boas-Vindas!", width = 12,
                    p("Bem-vindo ao painel de Regulação de Saneamento Básico. Utilize os filtros na barra lateral para explorar os documentos."),
                    p("Este painel oferece uma visão organizada e interativa das principais regulamentações do setor de saneamento no Brasil, publicadas pela ANA e outras agências reguladoras infranacionais.")
                )
              ),
              fluidRow(
                valueBoxOutput("totalDocsBox"),
                valueBoxOutput("anaDocsBox"),
                valueBoxOutput("vigenteDocsBox")
              ),
              fluidRow(
                box(title = "Publicações Recentes", width = 12,
                    DT::dataTableOutput("recentDocsTable"))
              )
      ),
      
      # Table Tab
      tabItem(tabName = "table",
              h2("Documentos Regulatórios"),
              DT::dataTableOutput("regulationTable")
      ),
      
      # Details Tab (Nova aba para exibir detalhes do documento selecionado)
      tabItem(tabName = "details",
              h2("Detalhes do Documento Selecionado"),
              fluidRow(
                box(width = 12,
                    uiOutput("selectedDocDetails")
                )
              )
      ),
      
      # Statistics Tab
      tabItem(tabName = "stats",
              h2("Estatísticas"),
              fluidRow(
                box(title = "Documentos por Órgão Emissor", width = 6,
                    plotOutput("orgaoPlot")),
                box(title = "Documentos por Tipo", width = 6,
                    plotOutput("tipoPlot"))
              ),
              fluidRow(
                box(title = "Documentos por Área Temática", width = 6,
                    plotOutput("areaPlot")),
                box(title = "Publicações ao Longo do Tempo", width = 6,
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
    
    # Date filtering - check for NA in date_publicacao before filtering
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
    vigente_docs <- filtered_data() %>% filter(status_vigencia == "Vigente") %>% nrow()
    valueBox(
      vigente_docs, "Documentos Vigentes", icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$recentDocsTable <- DT::renderDataTable({
    filtered_data() %>%
      arrange(desc(data_publicacao)) %>%
      head(5) %>% # Mostrar os 5 documentos mais recentes
      select(data_publicacao, titulo, orgao_emissor) %>%
      DT::datatable(
        options = list(dom = 't', # Apenas a tabela, sem search/pagination
                       ordering = FALSE),
        rownames = FALSE,
        colnames = c("Data", "Título", "Órgão Emissor")
      )
  })
  
  
  # --- Outputs para a aba 'Tabela de Documentos' ---
  output$regulationTable <- DT::renderDataTable({
    data_for_table <- filtered_data() %>%
      # Cria a coluna 'Link' formatada como hiperlink
      mutate(
        # target='_blank' abre o link em uma nova aba
        link = ifelse(link_original != "",
                      paste0('<a href="', link_original, '" target="_blank">Abrir</a>'),
                      "N/A")
      ) %>%
      # Seleciona e reordena as colunas para a exibição na tabela
      select(id_documento, titulo, data_publicacao, orgao_emissor, tipo_documento,
             area_tematica, status_vigencia, abrangencia_geografica, link, ementa)
    
    DT::datatable(
      data_for_table,
      options = list(
        pageLength = 10,
        scrollX = TRUE, # Permite scroll horizontal se a tabela for muito larga
        columnDefs = list(
          list(width = '100px', targets = c(0,3,4,5,6,7,8)), # Ajustar largura para algumas colunas
          list(width = '300px', targets = c(1)), # Largura para título
          list(visible = FALSE, targets = ncol(data_for_table) - 0) # Esconder a coluna 'ementa'
        )
      ),
      rownames = FALSE,
      escape = FALSE, # IMPORTANTE: Permite que o HTML (o link) seja renderizado
      selection = 'single', # Permite selecionar uma única linha
      colnames = c("ID", "Título", "Publicação", "Órgão", "Tipo", "Área Temática",
                   "Status", "Abrangência", "Link", "Ementa (hidden)") # Renomear colunas
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
        p(strong("ID do Documento:"), selected_doc$id_documento),
        p(strong("Órgão Emissor:"), selected_doc$orgao_emissor),
        p(strong("Tipo de Documento:"), selected_doc$tipo_documento),
        p(strong("Data de Publicação:"), format(selected_doc$data_publicacao, "%d/%m/%Y")),
        p(strong("Status de Vigência:"), selected_doc$status_vigencia),
        p(strong("Área(s) Temática(s):"), selected_doc$area_tematica), # Ajustar se for multi-valor
        p(strong("Abrangência Geográfica:"), selected_doc$abrangencia_geografica,
          ifelse(selected_doc$abrangencia_geografica == "Estadual" && selected_doc$estado != "", paste0(" (", selected_doc$estado, ")"), "")),
        br(), # Quebra de linha
        p(strong("Ementa:")),
        tags$blockquote(selected_doc$ementa), # Bloco de citação para a ementa
        br(),
        p(strong("Link Original:"),
          if (selected_doc$link_original != "") {
            tags$a(href = selected_doc$link_original, target = "_blank", "Acessar Documento Original")
          } else {
            "Link não disponível"
          }
        )
      )
    })
    
    # Mudar para a aba de detalhes automaticamente ao selecionar uma linha
    updateTabItems(session, "sidebarMenu", "details")
  })
  
  
  # --- Outputs para a aba 'Estatísticas' ---
  output$orgaoPlot <- renderPlot({
    filtered_data() %>%
      count(orgao_emissor) %>%
      ggplot(aes(x = reorder(orgao_emissor, n), y = n, fill = orgao_emissor)) +
      geom_col() +
      coord_flip() +
      labs(title = "Documentos por Órgão Emissor", x = "Órgão Emissor", y = "Número de Documentos") +
      theme_minimal() +
      theme(legend.position = "none") # Remove a legenda se a cor for apenas para o órgão
  })
  
  output$tipoPlot <- renderPlot({
    filtered_data() %>%
      count(tipo_documento) %>%
      ggplot(aes(x = reorder(tipo_documento, n), y = n, fill = tipo_documento)) +
      geom_col() +
      coord_flip() +
      labs(title = "Documentos por Tipo", x = "Tipo de Documento", y = "Número de Documentos") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$areaPlot <- renderPlot({
    filtered_data() %>%
      count(area_tematica) %>%
      ggplot(aes(x = reorder(area_tematica, n), y = n, fill = area_tematica)) +
      geom_col() +
      coord_flip() +
      labs(title = "Documentos por Área Temática", x = "Área Temática", y = "Número de Documentos") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$timePlot <- renderPlot({
    filtered_data() %>%
      mutate(ano_publicacao = year(data_publicacao)) %>%
      filter(!is.na(ano_publicacao)) %>%
      count(ano_publicacao) %>%
      ggplot(aes(x = ano_publicacao, y = n)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue", size = 2) +
      labs(title = "Número de Publicações por Ano", x = "Ano", y = "Número de Documentos") +
      theme_minimal() +
      scale_x_continuous(breaks = unique(year(df$data_publicacao)),
                         labels = unique(year(df$data_publicacao))) # Garante todos os anos no eixo X
  })
}

# Run the application
shinyApp(ui = ui, server = server)