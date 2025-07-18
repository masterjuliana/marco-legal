# app.R
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr) # Or readxl if using Excel

# --- 1. Sample Data (replace with your actual data loading) ---
# In a real scenario, you'd load from CSV, Excel, or a database
# Example: df <- read_csv("your_regulations_data.csv")
df <- tibble::tribble(
  ~id_documento, ~titulo, ~data_publicacao, ~orgao_emissor, ~tipo_documento, ~area_tematica, ~status_vigencia,
  "ANA_RES_177_2024", "Resolução ANA nº 177, de 12 de janeiro de 2024", "2024-01-12", "ANA", "Resolução", "Governança", "Vigente",
  "ANA_NR_04_2024", "Norma de Referência nº 4/2024", "2024-01-12", "ANA", "Norma de Referência", "Governança", "Vigente",
  "AGENERSA_DEL_01_2023", "Deliberação AGENERSA nº 01, de 15 de março de 2023", "2023-03-15", "AGENERSA", "Deliberação", "Tarifas", "Vigente",
  "ARSESP_RES_05_2023", "Resolução ARSESP nº 05, de 20 de maio de 2023", "2023-05-20", "ARSESP", "Resolução", "Qualidade dos Serviços", "Vigente"
) %>%
  mutate(data_publicacao = as.Date(data_publicacao)) # Ensure date format

# --- 2. UI (User Interface) ---
ui <- dashboardPage(
  dashboardHeader(title = "Regulação de Saneamento no Brasil"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "overview", icon = icon("dashboard")),
      menuItem("Tabela de Documentos", tabName = "table", icon = icon("table")),
      menuItem("Estatísticas", tabName = "stats", icon = icon("chart-bar"))
    ),
    
    # Filters in the sidebar
    selectInput("orgao_filter", "Órgão Emissor:",
                choices = c("Todos", unique(df$orgao_emissor))),
    selectInput("tipo_filter", "Tipo de Documento:",
                choices = c("Todos", unique(df$tipo_documento))),
    selectInput("area_filter", "Área Temática:",
                choices = c("Todos", unique(df$area_tematica))),
    dateRangeInput("date_filter", "Período de Publicação:",
                   start = min(df$data_publicacao),
                   end = max(df$data_publicacao),
                   format = "dd/mm/yyyy")
  ),
  
  # --- 3. Body (Content) ---
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Boas-Vindas!", width = 12,
                    "Explore o banco de dados de regulamentações de saneamento básico.")
              ),
              fluidRow(
                valueBoxOutput("totalDocsBox"),
                valueBoxOutput("anaDocsBox")
              )
      ),
      
      # Table Tab
      tabItem(tabName = "table",
              h2("Documentos Regulatórios"),
              DT::dataTableOutput("regulationTable")
      ),
      
      # Statistics Tab
      tabItem(tabName = "stats",
              h2("Estatísticas"),
              fluidRow(
                box(title = "Documentos por Órgão Emissor", width = 6,
                    plotOutput("orgaoPlot")),
                box(title = "Documentos por Área Temática", width = 6,
                    plotOutput("areaPlot"))
              )
      )
    )
  )
)

# --- 4. Server Logic ---
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
    
    # Date filtering
    data <- data %>% filter(data_publicacao >= input$date_filter[1] & 
                              data_publicacao <= input$date_filter[2])
    
    data
  })
  
  # Value Boxes
  output$totalDocsBox <- renderValueBox({
    valueBox(
      nrow(filtered_data()), "Total de Documentos", icon = icon("file-alt"),
      color = "blue"
    )
  })
  
  output$anaDocsBox <- renderValueBox({
    ana_docs <- filtered_data() %>% filter(orgao_emissor == "ANA") %>% nrow()
    valueBox(
      ana_docs, "Documentos da ANA", icon = icon("water"),
      color = "green"
    )
  })
  
  # Render the interactive table
  output$regulationTable <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  selection = 'single') # Allows selecting a row for more details if needed
  })
  
  # Render plots
  output$orgaoPlot <- renderPlot({
    filtered_data() %>%
      count(orgao_emissor) %>%
      ggplot(aes(x = reorder(orgao_emissor, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Contagem por Órgão Emissor", x = "Órgão Emissor", y = "Número de Documentos") +
      theme_minimal()
  })
  
  output$areaPlot <- renderPlot({
    filtered_data() %>%
      count(area_tematica) %>%
      ggplot(aes(x = reorder(area_tematica, n), y = n)) +
      geom_col(fill = "lightcoral") +
      coord_flip() +
      labs(title = "Contagem por Área Temática", x = "Área Temática", y = "Número de Documentos") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
