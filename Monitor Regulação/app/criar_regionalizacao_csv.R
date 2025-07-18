# Criar pasta data/ se não existir
if (!dir.exists("data")) dir.create("data")

# Caminho do arquivo
arquivo <- "data/regionalizacao.csv"

# Verifica se o arquivo já existe
if (file.exists(arquivo)) {
  message("✅ O arquivo regionalizacao.csv já existe em /data.")
} else {
  message("🛠️ Criando o arquivo regionalizacao.csv com dados reais...")
  
  # Dados de regionalização
  dados_regionalizacao <- tribble(
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
  
  # Salvar o CSV com codificação segura
  write_csv(dados_regionalizacao, arquivo)
  message("✅ Arquivo regionalizacao.csv criado com sucesso!")
}

