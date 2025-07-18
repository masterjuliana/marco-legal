library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("🔎 Painel de Normas Regulatórias"),
  sidebarLayout(
    sidebarPanel(
      actionButton("atualizar", "🔄 Atualizar Normas da ARSAE-MG"),
      br(), br(),
      downloadButton("download", "📥 Baixar CSV")
    ),
    mainPanel(
      dataTableOutput("tabela")
    )
  )
)

server <- function(input, output, session) {
  dados <- reactiveVal({
    if (file.exists("data/ARSAE-MG/normas.csv")) {
      readr::read_csv("data/ARSAE-MG/normas.csv", show_col_types = FALSE)
    } else {
      tibble::tibble(Entidade = character(), Pagina = character(), Titulo = character(), Link = character(),
                     Data = as.Date(character()), Tema = character())
    }
  })
  
  observeEvent(input$atualizar, {
    source("entidades/arsae_mg.R", local = TRUE)
    novos_dados <- scrape_arsae_mg()
    dados(novos_dados)
  })
  
  output$tabela <- renderDataTable({
    DT::datatable(dados(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$download <- downloadHandler(
    filename = function() { "normas_arsae_mg.csv" },
    content = function(file) {
      readr::write_csv(dados(), file)
    }
  )
}

shinyApp(ui, server)
