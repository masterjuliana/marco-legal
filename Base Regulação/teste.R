# app.R (APENAS PARA TESTE INICIAL DE CARREGAMENTO)
library(readr)
# Caminho do seu CSV
csv_file_path <- "W:/R/Base Regulação/Documentos_ANA_Saneamento_Basico/metadados_normativos_saneamento_ana.csv"
csv_file_path <- normalizePath(csv_file_path, winslash = "/", mustWork = FALSE)

test_df <- read.csv(csv_file_path, stringsAsFactors = FALSE) # Use read.csv base R para teste
# OU
# test_df <- read_csv(csv_file_path, show_col_types = FALSE) # Sem col_types

print(colnames(test_df))
print(head(test_df$data_publicacao))
print(class(test_df$data_publicacao))
