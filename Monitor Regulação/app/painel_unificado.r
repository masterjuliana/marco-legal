library(shiny)
library(tidyverse)
library(DT)

# Carregar os dados de forma segura
read_seguro <- function(path) {
  if (file.exists(path)) {
    read_csv(path, locale = locale(encoding = "Latin1"))
  } else {
    warning(paste("❌ Arquivo não encontrado:", path))
    # Retorna um tibble vazio com as colunas esperadas para evitar erros no Shiny
    # Se 'docs' ou 'reg' puderem estar vazios, o Shiny ainda tenta renderizar
    # o que pode causar erros se as colunas não existirem.
    # Para este exemplo, estou assumindo que eles sempre existirão ou
    # que o filtro de 'unique' lidará com tibbles vazios.
    return(tibble())
  }
}

# --- Preparação dos Dados ---

# Dados de regionalização (usado para 'reg')
reg <- tribble(
  ~Estado, ~`Lei de Regionalização`, ~Ano, ~Modelo, ~Componentes, ~Situação, ~`Link Oficial`,
  "SP", "Sim", 2022, "Blocos Regionais", "AESD", "Aprovada", "AL-SP",
  "RJ", "Sim", 2021, "Concessão em Bloco", "AES", "Aprovada", "ALERJ",
  "MG", "Em tramitação", NA, "Bloco de Referência", "AESD", "Em discussão", "ALMG",
  "RS", "Sim", 2023, "Microrregião", "AESD", "Em implantação", "AL-RS",
  "BA", "Sim", 2020, "Microrregião", "AESD", "Aprovada", "AL-BA",
  "AL", "Sim", 2021, "URSB", "AESD", "Aprovada", "AL-AL",
  "ES", "Sim", 2021, "Microrregião", "AES", "Aprovada", "AL-ES",
  "GO", "Em tramitação", NA, "Microrregião", "AESD", "Em discussão", "ALEGO",
  "PE", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "ARPE",
  "CE", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "ARCE",
  "DF", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "ADASA",
  "PR", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "AGEPAR",
  "MA", "Sim", 2022, "Microrregião", "AESD", "Aprovada", "AL-MA",
  "PB", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "AL-PB",
  "PI", "Sim", 2022, "Microrregião", "AESD", "Aprovada", "AL-PI",
  "RN", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "ARSEP",
  "RO", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "AL-RO",
  "MT", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "AGER-MT",
  "MS", "Parcial", NA, "Concessão Regional", "AESD", "Parcialmente regionalizado", "AGEMS",
  "TO", "Não", NA, NA, NA, "Sem proposta", NA,
  "PA", "Não", NA, NA, NA, "Sem proposta", NA,
  "AM", "Não", NA, NA, NA, "Sem proposta", NA,
  "AC", "Não", NA, NA, NA, "Sem proposta", NA,
  "RR", "Não", NA, NA, NA, "Sem proposta", NA,
  "AP", "Parcial", NA, "Concessão Federal", "AES", "Exceção legal", NA,
  "SC", "Sim", 2021, "URSB", "AESD", "Aprovada", "ARIS-SC",
  "SE", "Sim", 2021, "URSB", "AESD", "Aprovada", "AGRESE"
)

# Salvar os dados de regionalização em um arquivo CSV
# Certifique-se de que o diretório 'W:/R/Monitor Regulação/data/' exista ou ajuste o caminho.
# Para fins de demonstração, vou salvar em um diretório temporário ou local,
# mas mantenha seu caminho se ele for válido e você quiser persistir o arquivo.
# Para rodar o código, você pode comentar a linha abaixo ou criar a pasta.
# write_csv(reg, "W:/R/Monitor Regulação/data/regionalizacao.csv")

# Carregar os dados de regionalização (lendo do caminho onde você salva)
# reg <- read_seguro("W:/R/Monitor Regulação/data/regionalizacao.csv")

# Dados de entidades (usado para 'docs')
# Para simular 'entidades_saneamento.csv', vou criar um tibble de exemplo.
# Você pode ter que ajustar as colunas para corresponder ao seu arquivo real.
docs <- tribble(
  ~`Tipo de Documento`, ~`Nome do Documento`, ~Data, ~Link,
  "Relatório Anual", "RA 2023", "2024-03-15", "link_ra23",
  "Parecer Técnico", "PT 001/2024", "2024-01-20", "link_pt001",
  "Lei", "Lei da Regionalização SP", "2022-07-01", "link_lei_sp",
  "Decreto", "Decreto de Regulamentação RJ", "2021-11-10", "link_decreto_rj",
  "Relatório Anual", "RA 2022", "2023-03-20", "link_ra22",
  "Parecer Técnico", "PT 002/2024", "2024-02-05", "link_pt002",
  "Lei", "Lei da Regionalização RS", "2023-05-20", "link_lei_rs"
)
# Carregar as entidades de saneamento
# docs <- read_seguro("W:/R/Monitor Regulação/data/entidades_saneamento.csv")


# --- Interface do Usuário (UI) ---
ui <- fluidPage(
  titlePanel("🧭 Monitor Institucional do Marco Legal do Saneamento"),
  tabsetPanel(
    tabPanel("📘 Documentos",
             selectInput("tipo_doc", "Tipo de Documento:", choices = unique(docs$`Tipo de Documento`), multiple = TRUE),
             dataTableOutput("doc_table")
    ),
    tabPanel("📍 Regionalização",
             selectInput("situacao_reg", "Situação:", choices = unique(reg$Situação), multiple = TRUE),
             dataTableOutput("reg_table")
    ),
    tabPanel("📊 Dashboards",
             plotOutput("grafico_tipo"),
             plotOutput("grafico_modelos")
    )
  )
)

# --- Lógica do Servidor ---
server <- function(input, output) {
  
  # Tabela de Documentos
  output$doc_table <- renderDataTable({
    # Filtra os documentos com base na seleção do usuário
    # Se input$tipo_doc for NULL (nada selecionado), exibe todos os documentos
    if (is.null(input$tipo_doc)) {
      docs
    } else {
      docs %>%
        filter(`Tipo de Documento` %in% input$tipo_doc)
    }
  }, options = list(pageLength = 10)) # Exemplo: 10 linhas por página
  
  # Tabela de Regionalização
  output$reg_table <- renderDataTable({
    # Filtra a regionalização com base na seleção do usuário
    # Se input$situacao_reg for NULL (nada selecionado), exibe todos os dados de regionalização
    if (is.null(input$situacao_reg)) {
      reg
    } else {
      reg %>%
        filter(Situação %in% input$situacao_reg)
    }
  }, options = list(pageLength = 10)) # Exemplo: 10 linhas por página
  
  # Gráfico de Distribuição de Tipos de Documentos
  output$grafico_tipo <- renderPlot({
    # Garante que 'docs' não esteja vazio antes de tentar plotar
    if (nrow(docs) > 0) {
      docs %>%
        count(`Tipo de Documento`) %>%
        ggplot(aes(x = reorder(`Tipo de Documento`, -n), y = n)) +
        geom_col(fill = "#2C7BB6") +
        coord_flip() +
        labs(title = "Distribuição de Tipos de Documentos", x = "", y = "Quantidade") +
        theme_minimal() # Adiciona um tema para melhor visualização
    } else {
      # Mensagem de erro ou plot vazio se os dados estiverem vazios
      ggplot() +
        labs(title = "Nenhum dado de documento disponível para o gráfico.") +
        theme_void()
    }
  })
  
  # Gráfico de Modelos Adotados na Regionalização
  output$grafico_modelos <- renderPlot({
    # Garante que 'reg' não esteja vazio e que haja dados de 'Modelo' antes de tentar plotar
    if (nrow(reg) > 0 && any(!is.na(reg$Modelo))) {
      reg %>%
        filter(!is.na(Modelo)) %>%
        count(Modelo) %>%
        ggplot(aes(x = reorder(Modelo, -n), y = n)) +
        geom_col(fill = "#FDAE61") +
        coord_flip() +
        labs(title = "Modelos Adotados na Regionalização", x = "", y = "Estados") +
        theme_minimal() # Adiciona um tema para melhor visualização
    } else {
      # Mensagem de erro ou plot vazio se os dados estiverem vazioslibrary(shiny)
library(tidyverse)
library(DT)

# Carregar os dados de forma segura
read_seguro <- function(path) {
  if (file.exists(path)) {
    read_csv(path, locale = locale(encoding = "Latin1"))
  } else {
    warning(paste("❌ Arquivo não encontrado:", path))
    return(tibble())
  }
}

# --- Preparação dos Dados ---

# Dados de regionalização (usado para 'reg')
reg <- tribble(
  ~Estado, ~`Lei de Regionalização`, ~Ano, ~Modelo, ~Componentes, ~Situação, ~`Link Oficial`,
  "SP", "Sim", 2022, "Blocos Regionais", "AESD", "Aprovada", "AL-SP",
  "RJ", "Sim", 2021, "Concessão em Bloco", "AES", "Aprovada", "ALERJ",
  "MG", "Em tramitação", NA, "Bloco de Referência", "AESD", "Em discussão", "ALMG",
  "RS", "Sim", 2023, "Microrregião", "AESD", "Em implantação", "AL-RS",
  "BA", "Sim", 2020, "Microrregião", "AESD", "Aprovada", "AL-BA",
  "AL", "Sim", 2021, "URSB", "AESD", "Aprovada", "AL-AL",
  "ES", "Sim", 2021, "Microrregião", "AES", "Aprovada", "AL-ES",
  "GO", "Em tramitação", NA, "Microrregião", "AESD", "Em discussão", "ALEGO",
  "PE", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "ARPE",
  "CE", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "ARCE",
  "DF", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "ADASA",
  "PR", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "AGEPAR",
  "MA", "Sim", 2022, "Microrregião", "AESD", "Aprovada", "AL-MA",
  "PB", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "AL-PB",
  "PI", "Sim", 2022, "Microrregião", "AESD", "Aprovada", "AL-PI",
  "RN", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "ARSEP",
  "RO", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "AL-RO",
  "MT", "Sim", 2021, "Microrregião", "AESD", "Aprovada", "AGER-MT",
  "MS", "Parcial", NA, "Concessão Regional", "AESD", "Parcialmente regionalizado", "AGEMS",
  "TO", "Não", NA, NA, NA, "Sem proposta", NA,
  "PA", "Não", NA, NA, NA, "Sem proposta", NA,
  "AM", "Não", NA, NA, NA, "Sem proposta", NA,
  "AC", "Não", NA, NA, NA, "Sem proposta", NA,
  "RR", "Não", NA, NA, NA, "Sem proposta", NA,
  "AP", "Parcial", NA, "Concessão Federal", "AES", "Exceção legal", NA,
  "SC", "Sim", 2021, "URSB", "AESD", "Aprovada", "ARIS-SC",
  "SE", "Sim", 2021, "URSB", "AESD", "Aprovada", "AGRESE"
)

# Salvar os dados de regionalização em um arquivo CSV (comentado para evitar criar arquivos repetidos em testes)
# write_csv(reg, "W:/R/Monitor Regulação/data/regionalizacao.csv")
# reg <- read_seguro("W:/R/Monitor Regulação/data/regionalizacao.csv")

# Dados de entidades (usado para 'docs') - AGORA COM MAIS COLUNAS PARA FILTRO
docs <- tribble(
  ~`Tipo de Documento`, ~`Nome do Documento`, ~Data, ~Link, ~Entidade, ~Ano,
  "Relatório Anual", "RA 2023 ANA", "2024-03-15", "link_ra23_ana", "ANA", 2023,
  "Parecer Técnico", "PT 001/2024 ARSESP", "2024-01-20", "link_pt001_arsp", "ARSESP", 2024,
  "Lei", "Lei da Regionalização SP", "2022-07-01", "link_lei_sp", "AL-SP", 2022,
  "Decreto", "Decreto de Regulamentação RJ", "2021-11-10", "link_decreto_rj", "AGENERSA", 2021,
  "Relatório Anual", "RA 2022 ABAR", "2023-03-20", "link_ra22_abar", "ABAR", 2022,
  "Parecer Técnico", "PT 002/2024 ARSAE", "2024-02-05", "link_pt002_arsae", "ARSAE", 2024,
  "Lei", "Lei da Regionalização RS", "2023-05-20", "link_lei_rs", "AL-RS", 2023,
  "Consulta Pública", "CP Saneamento Básico", "2023-09-01", "link_cp_ana", "ANA", 2023,
  "Resolução", "Resolução Tarifa 001", "2022-01-10", "link_res_001", "ADASA", 2022,
  "Agenda Regulatória", "AR 2024-2025", "2024-01-01", "link_ar_abas", "ABES", 2024
)
# docs <- read_seguro("W:/R/Monitor Regulação/data/entidades_saneamento.csv") # Se usar arquivo real

# --- Interface do Usuário (UI) ---
ui <- fluidPage(
  titlePanel("🧭 Monitor Institucional de Regulação do Saneamento"),
  tabsetPanel(
    tabPanel("📘 Documentos",
      fluidRow( # Usamos fluidRow para organizar os inputs lado a lado
        column(4, selectInput("tipo_doc", "Tipo de Documento:", choices = unique(docs$`Tipo de Documento`), multiple = TRUE)),
        column(4, selectInput("entidade_doc", "Entidade:", choices = unique(docs$Entidade), multiple = TRUE)),
        column(4, selectInput("ano_doc", "Ano:", choices = sort(unique(docs$Ano), decreasing = TRUE), multiple = TRUE))
      ),
      dataTableOutput("doc_table")
    ),
    tabPanel("📍 Regionalização",
      fluidRow( # Usamos fluidRow para organizar os inputs lado a lado
        column(4, selectInput("situacao_reg", "Situação:", choices = unique(reg$Situação), multiple = TRUE)),
        column(4, selectInput("modelo_reg", "Modelo:", choices = unique(reg$Modelo), multiple = TRUE)),
        column(4, selectInput("ano_reg", "Ano da Lei:", choices = sort(unique(reg$Ano), decreasing = TRUE), multiple = TRUE))
      ),
      dataTableOutput("reg_table")
    ),
    tabPanel("📊 Dashboards",
      h3("Gráficos de Documentos"), # Um subtítulo para organizar
      plotOutput("grafico_tipo"),
      plotOutput("grafico_entidade_doc"), # Novo gráfico: Documentos por Entidade

      hr(), # Uma linha divisória para separar visualmente
      h3("Gráficos de Regionalização"), # Outro subtítulo
      plotOutput("grafico_modelos"),
      plotOutput("grafico_evolucao_regionalizacao") # Novo gráfico: Evolução da Regionalização
    )
  )
)

# --- Lógica do Servidor ---
server <- function(input, output) {

  # Tabela de Documentos
  output$doc_table <- renderDataTable({
    dados_filtrados <- docs

    # Aplica filtro por Tipo de Documento
    if (!is.null(input$tipo_doc)) {
      dados_filtrados <- dados_filtrados %>%
        filter(`Tipo de Documento` %in% input$tipo_doc)
    }

    # Aplica filtro por Entidade
    if (!is.null(input$entidade_doc)) {
      dados_filtrados <- dados_filtrados %>%
        filter(Entidade %in% input$entidade_doc)
    }

    # Aplica filtro por Ano
    if (!is.null(input$ano_doc)) {
      dados_filtrados <- dados_filtrados %>%
        filter(Ano %in% input$ano_doc)
    }

    dados_filtrados
  }, options = list(pageLength = 10))

  # Tabela de Regionalização
  output$reg_table <- renderDataTable({
    dados_filtrados_reg <- reg

    # Aplica filtro por Situação
    if (!is.null(input$situacao_reg)) {
      dados_filtrados_reg <- dados_filtrados_reg %>%
        filter(Situação %in% input$situacao_reg)
    }

    # Aplica filtro por Modelo
    if (!is.null(input$modelo_reg)) {
      dados_filtrados_reg <- dados_filtrados_reg %>%
        filter(Modelo %in% input$modelo_reg)
    }

    # Aplica filtro por Ano da Lei
    if (!is.null(input$ano_reg)) {
      dados_filtrados_reg <- dados_filtrados_reg %>%
        filter(Ano %in% input$ano_reg)
    }

    dados_filtrados_reg
  }, options = list(pageLength = 10))

  # Gráfico de Distribuição de Tipos de Documentos
  output$grafico_tipo <- renderPlot({
    if (nrow(docs) > 0) {
      docs %>%
        count(`Tipo de Documento`) %>%
        ggplot(aes(x = reorder(`Tipo de Documento`, -n), y = n)) +
        geom_col(fill = "#2C7BB6") +
        coord_flip() +
        labs(title = "Distribuição de Tipos de Documentos", x = "", y = "Quantidade") +
        theme_minimal()
    } else {
      ggplot() + labs(title = "Nenhum dado de documento disponível para o gráfico.") + theme_void()
    }
  })

  # NOVO GRÁFICO: Distribuição de Documentos por Entidade
  output$grafico_entidade_doc <- renderPlot({
    if (nrow(docs) > 0 && any(!is.na(docs$Entidade))) {
      docs %>%
        count(Entidade) %>%
        ggplot(aes(x = reorder(Entidade, -n), y = n)) +
        geom_col(fill = "#FF7F00") + # Uma cor diferente
        coord_flip() +
        labs(title = "Distribuição de Documentos por Entidade", x = "", y = "Quantidade") +
        theme_minimal()
    } else {
      ggplot() + labs(title = "Nenhum dado de entidade disponível para o gráfico.") + theme_void()
    }
  })

  # Gráfico de Modelos Adotados na Regionalização
  output$grafico_modelos <- renderPlot({
    if (nrow(reg) > 0 && any(!is.na(reg$Modelo))) {
      reg %>%
        filter(!is.na(Modelo)) %>%
        count(Modelo) %>%
        ggplot(aes(x = reorder(Modelo, -n), y = n)) +
        geom_col(fill = "#FDAE61") +
        coord_flip() +
        labs(title = "Modelos Adotados na Regionalização", x = "", y = "Estados") +
        theme_minimal()
    } else {
      ggplot() + labs(title = "Nenhum dado de modelo de regionalização disponível para o gráfico.") + theme_void()
    }
  })

  # NOVO GRÁFICO: Evolução das Leis de Regionalização por Ano
  output$grafico_evolucao_regionalizacao <- renderPlot({
    if (nrow(reg) > 0 && any(!is.na(reg$Ano))) {
      reg %>%
        filter(!is.na(Ano), `Lei de Regionalização` == "Sim") %>%
        count(Ano) %>%
        ggplot(aes(x = as.factor(Ano), y = n, group = 1)) +
        geom_line(color = "#4CAF50", size = 1.2) +
        geom_point(color = "#4CAF50", size = 3) +
        labs(title = "Evolução Anual das Leis de Regionalização Aprovadas", x = "Ano", y = "Número de Leis Aprovadas") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot() + labs(title = "Nenhum dado de ano de regionalização disponível para o gráfico.") + theme_void()
    }
  })
}

# Executar o aplicativo Shiny
shinyApp(ui, server)
      ggplot() +
        labs(title = "Nenhum dado de modelo de regionalização disponível para o gráfico.") +
        theme_void()
    }
  })
}

# Executar o aplicativo Shiny
shinyApp(ui, server)