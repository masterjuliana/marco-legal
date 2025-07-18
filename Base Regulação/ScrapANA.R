# scrape_ana_saneamento.R

# --- Instalar e Carregar Pacotes ---
# Instale estes pacotes se ainda não os tiver
# install.packages(c("rvest", "dplyr", "stringr", "httr", "lubridate", "readr", "tibble", "purrr"))

library(rvest)
library(dplyr)
library(stringr)
library(httr)
library(lubridate)
library(readr)
library(tibble)
library(purrr) # Necessário para purrr::discard

# --- Configurações ---
base_url_saneamento_ana <- "https://www.gov.br/ana/pt-br/assuntos/saneamento-basico/Normativos-publicados-pela-ANA"
download_dir <- file.path("W:", "R", "Base Regulação", "Documentos_ANA_Saneamento_Basico") # Caminho absoluto para evitar problemas

# Criar o diretório de download se não existir
if (!dir.exists(download_dir)) {
  dir.create(download_dir, recursive = TRUE) # recursive = TRUE para criar pastas pai se não existirem
  message(paste("Diretório criado:", normalizePath(download_dir)))
} else {
  message(paste("Diretório já existe:", normalizePath(download_dir)))
}

message(paste("\nIniciando raspagem da URL:", base_url_saneamento_ana))

# --- Raspar a página principal ---
page <- tryCatch({
  read_html(base_url_saneamento_ana)
}, error = function(e) {
  stop(paste("ERRO FATAL: Erro ao ler a URL principal:", base_url_saneamento_ana, "-", e$message))
})

# Inicializar lista para armazenar os dados de todas as resoluções
all_resolutions_data <- list()

# --- NOVA ESTRATÉGIA DE SELEÇÃO DE MÓDULOS ---
# Procurar por títulos de seção (h2 ou h3) e, a partir deles, encontrar os links subsequentes.
# Esta abordagem é mais robusta a pequenas mudanças de layout de containers.
section_titles_nodes <- page %>% html_nodes("h2, h3")

if (length(section_titles_nodes) == 0) {
  stop("ERRO FATAL: Nenhum título de seção (h2 ou h3) encontrado. A estrutura da página pode ter mudado drasticamente.")
}

message(paste("Encontrados", length(section_titles_nodes), "possíveis títulos de módulo."))

current_module_title <- "Outros Normativos" # Título padrão para documentos sem módulo claro

for (title_node in section_titles_nodes) {
  title_text <- html_text(title_node, trim = TRUE)
  
  # Considerar apenas títulos que parecem ser de módulos de documentos
  # (ex: "Normas de Referência", "Resoluções", "Portarias")
  if (str_detect(tolower(title_text), "normas de referência|resoluções|portarias|instruções normativas|orientações normativas|decretos|leis")) {
    current_module_title <- title_text
    message(paste("\nProcessando Módulo:", current_module_title))
    
    # Agora, encontrar os links que vêm *depois* deste título
    # Usamos XPath 'following-sibling' para encontrar irmãos seguintes
    # Procurar por links 'a' que são descendentes de qualquer elemento irmão subsequente
    # OU links 'a' que são irmãos diretos
    doc_links_nodes <- title_node %>%
      html_nodes(xpath = "following-sibling::*//a | following-sibling::a")
    
    if (length(doc_links_nodes) == 0) {
      message(paste("Nenhum link de documento encontrado diretamente após o módulo:", current_module_title))
      next # Pula para o próximo título de módulo se não houver links associados
    }
    
    for (doc_link_node in doc_links_nodes) {
      title_text_doc <- doc_link_node %>% html_text(trim = TRUE)
      link_url <- doc_link_node %>% html_attr("href")
      
      # --- Validações e Limpezas (AGORA MAIS ROBUSTAS E COM MENSAGENS) ---
      if (is.na(link_url) || link_url == "#" || str_detect(link_url, "^javascript:")) {
        message(paste("  AVISO: Pulando link (inválido/javascript):", title_text_doc, "URL:", link_url))
        next
      }
      
      # Adiciona o prefixo 'https://www.gov.br' se o link for relativo
      if (!str_starts(link_url, "http")) {
        original_link_url <- link_url # Guarda o original para depuração
        link_url <- paste0("https://www.gov.br", link_url)
        # message(paste("  INFO: Link corrigido de relativo para absoluto:", original_link_url, "->", link_url))
      }
      
      # Verifica se o link é um PDF ou HTML APÓS a correção, se houver
      if (!str_detect(link_url, "\\.pdf$|\\.htm$|\\.html$", negate = FALSE, ignore.case = TRUE)) {
        message(paste("  AVISO: Pulando link (não-PDF/HTML após correção):", title_text_doc, "URL:", link_url))
        next
      }
      
      message(paste("  Processando documento:", title_text_doc, "URL:", link_url)) # Esta mensagem agora só aparece se o link passar todas as validações
      
      # --- Extração de Metadados ---
      tipo_documento <- str_extract(title_text_doc, "^(RESOLUÇÃO|NORMA DE REFERÊNCIA|PORTARIA|INSTRUÇÃO NORMATIVA|ORIENTAÇÃO NORMATIVA)", ignore.case = TRUE)
      if (is.na(tipo_documento)) {
        tipo_documento <- "Documento Geral"
      } else {
        tipo_documento <- toupper(tipo_documento)
      }
      
      numero <- str_extract(title_text_doc, "Nº\\s*\\d+(\\.\\d+)?(?:\\/\\d{4})?")
      if (!is.na(numero)) {
        numero <- str_remove(numero, "Nº\\s*") %>% str_trim()
      } else {
        numero <- NA_character_
      }
      
      date_match <- str_extract(title_text_doc, "\\d{1,2}\\s+DE\\s+(?:JANEIRO|FEVEREIRO|MARÇO|ABRIL|MAIO|JUNHO|JULHO|AGOSTO|SETEMBRO|OUTUBRO|NOVEMBRO|DEZEMBRO|AGÔSTO|SETÊMBRO|OUTÚBRO)\\s+DE\\s+\\d{4}", ignore.case = TRUE)
      
      data_publicacao <- NA_Date_
      ano_documento <- NA_character_
      
      if (!is.na(date_match)) {
        month_map <- c(
          "JANEIRO" = "01", "FEVEREIRO" = "02", "MARÇO" = "03", "ABRIL" = "04",
          "MAIO" = "05", "JUNHO" = "06", "JULHO" = "07", "AGOSTO" = "08",
          "SETEMBRO" = "09", "OUTUBRO" = "10", "NOVEMBRO" = "11", "DEZEMBRO" = "12",
          "AGÔSTO" = "08", "SETÊMBRO" = "09", "OUTÚBRO" = "10"
        )
        date_match_num <- str_replace_all(toupper(date_match), month_map)
        data_publicacao <- suppressWarnings(as.Date(date_match_num, format = "%d DE %m DE %Y"))
        if (!is.na(data_publicacao)) {
          ano_documento <- year(data_publicacao) %>% as.character()
        }
      }
      
      if (is.na(ano_documento)) {
        ano_documento_link <- str_extract(link_url, "/(\\d{4})/([^/]+\\.pdf|\\.html)", group = 1)
        if (!is.na(ano_documento_link)) {
          ano_documento <- ano_documento_link
        } else {
          ano_documento_title <- str_extract(title_text_doc, "\\d{4}$")
          if (!is.na(ano_documento_title)) ano_documento <- ano_documento_title
        }
      }
      if (is.na(ano_documento)) ano_documento <- "Desconhecido"
      
      ementa_text <- title_text_doc
      
      id_documento_gen <- paste0("ANA_SANEAMENTO_",
                                 gsub(" ", "_", toupper(tipo_documento)), "_",
                                 ifelse(is.na(numero), "SN", numero), "_",
                                 ifelse(is.na(ano_documento), "SA", ano_documento)) %>%
        str_replace_all("[^a-zA-Z0-9_.-]", "")
      
      # Adicionar dados à lista
      all_resolutions_data[[length(all_resolutions_data) + 1]] <- tibble(
        id_documento = id_documento_gen,
        titulo = title_text_doc,
        ementa = ementa_text,
        data_publicacao = data_publicacao,
        orgao_emissor = "ANA",
        tipo_documento = tipo_documento,
        numero_documento = numero,
        ano_documento = ano_documento,
        status_vigencia = "Vigente (Presumido)",
        abrangencia_geografica = "Nacional",
        estado = NA_character_,
        link_original = link_url,
        modulo_saneamento = current_module_title, # USA o título do módulo que estamos processando
        area_tematica = NA_character_,
        caminho_local = NA_character_
      )
    }
  }
}

# Adicione esta verificação (Já estava incluída, mas para reforçar)
if (length(all_resolutions_data) == 0) {
  stop("ERRO FATAL: NENHUM documento foi processado ou adicionado à lista 'all_resolutions_data'. Verifique as condições de filtragem de links.")
}

# Unir todos os dados raspados e remover duplicatas
all_resolutions_df <- bind_rows(all_resolutions_data) %>%
  distinct(link_original, .keep_all = TRUE)

message(paste("\nTotal de", nrow(all_resolutions_df), "documentos únicos encontrados para download no setor de saneamento básico."))

# --- Adicionando depuração antes de salvar o CSV ---
message("\n--- Inspecionando o DataFrame antes de salvar o CSV ---")
print(head(all_resolutions_df))
print(colnames(all_resolutions_df)) # VERIFIQUE AQUI SE 'modulo_saneamento' ESTÁ PRESENTE
print(glimpse(all_resolutions_df))
message("--------------------------------------------------")

# --- Download dos Documentos e Atualização do DataFrame ---

for (i in 1:nrow(all_resolutions_df)) {
  doc_row <- all_resolutions_df[i, ]
  doc_url <- doc_row$link_original
  
  if (is.na(doc_url) || doc_url == "") {
    message(paste("AVISO: Link de download ausente ou inválido para:", doc_row$titulo, ". Pulando."))
    next
  }
  
  file_extension <- str_extract(doc_url, "\\.(pdf|html?)$", ignore.case = TRUE)
  if (is.na(file_extension)) {
    all_resolutions_df$caminho_local[i] <- NA_character_
    next
  }
  
  doc_year_folder <- ifelse(is.na(doc_row$ano_documento) || doc_row$ano_documento == "Desconhecido",
                            "Ano_Desconhecido",
                            as.character(doc_row$ano_documento))
  
  year_dir <- file.path(download_dir, doc_year_folder)
  if (!dir.exists(year_dir)) {
    dir.create(year_dir)
  }
  
  # Cria um nome de arquivo seguro usando id_documento
  safe_filename_part <- doc_row$id_documento %>%
    str_replace_all("[^a-zA-Z0-9_.-]", "") %>%
    str_trunc(80, side = "right", ellipsis = "")
  
  file_name <- paste0(safe_filename_part, file_extension)
  file_path <- file.path(year_dir, file_name)
  
  if (file.exists(file_path)) {
    all_resolutions_df$caminho_local[i] <- file_path
    next
  }
  
  message(paste("Baixando:", doc_url, "para", file_path))
  
  response <- tryCatch({
    GET(doc_url, write_disk(file_path, overwrite = TRUE), timeout = 60)
  }, error = function(e) {
    message(paste("ERRO AO BAIXAR:", doc_url, "-", e$message))
    return(NULL)
  })
  
  if (!is.null(response) && http_error(response) == FALSE) {
    all_resolutions_df$caminho_local[i] <- file_path
    message("Download concluído com sucesso.")
  } else {
    message("FALHA NO DOWNLOAD.")
    all_resolutions_df$caminho_local[i] <- NA_character_
  }
  
  Sys.sleep(0.5)
}

# --- Salvar Metadados em CSV ---
output_csv_path <- file.path(download_dir, "metadados_normativos_saneamento_ana.csv")
write_csv(all_resolutions_df, output_csv_path)

message(paste("\nProcesso de raspagem e download concluído."))
message(paste("Metadados salvos em:", normalizePath(output_csv_path)))
message(paste("Documentos baixados em:", normalizePath(download_dir)))