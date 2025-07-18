library(rvest)
library(tidyverse)
library(lubridate)
library(httr)
library(pdftools)

# Keywords para identificar conteúdos normativos
keywords <- c("resolu", "norma", "decreto", "legisla", "ato", "delibera", "regulação")

# Função para encontrar links relevantes na página
find_links <- function(page, base) {
  page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    map_chr(~ ifelse(str_starts(.x, "http"), .x, url_absolute(.x, base))) %>%
    unique() %>%
    keep(~ {
      x <- tolower(.x)
      # Evitar NA e garantir TRUE/FALSE único
      if (is.na(x)) return(FALSE)
      str_detect(x, paste(keywords, collapse = "|"))
    })
}

# Função para extrair documentos e datas da página
collect_docs <- function(page_url, entity) {
  page <- read_html(page_url)
  els <- page %>% html_elements("a")
  titulos <- els %>% html_text(trim = TRUE)
  hrefs  <- els %>% html_attr("href")
  
  tibble(Titulo = titulos, Link = hrefs) %>%
    filter(str_detect(tolower(Titulo), paste(keywords, collapse = "|"))) %>%
    mutate(
      Link = if_else(str_starts(Link, "http"), Link, url_absolute(Link, page_url)),
      Data = str_extract(Titulo, "\\d{1,2}/\\d{1,2}/\\d{4}") %>% dmy(),
      pdf = str_detect(tolower(Link), "\\.pdf$"),
      Entidade = entity,
      Pagina = page_url
    )
}

# Baixar e extrair texto de PDFs
download_and_extract_pdf <- function(link, dest_dir = "pdfs") {
  dir.create(dest_dir, showWarnings = FALSE)
  nome <- basename(link)
  dest <- file.path(dest_dir, nome)
  tryCatch({
    download.file(link, dest, mode = "wb", quiet = TRUE)
    text <- pdf_text(dest)
    tibble(pdf = link, texto = paste(text, collapse = "\n"))
  }, error = function(e) {
    warning(paste("Erro ao baixar ou extrair PDF:", link))
    tibble(pdf = link, texto = NA_character_)
  })
}

# Classificação temática aprimorada
classifica_tema <- function(texto) {
  if (is.na(texto) || texto == "" || length(texto) == 0) {
    return("Não Classificado")
  }
  
  texto <- tolower(texto)
  
  temas <- c()
  if (str_detect(texto, "governança|governanca|gestão|gestao|controle|monitoramento|planejamento")) {
    temas <- c(temas, "Governança")
  }
  if (str_detect(texto, "serviço|servico|prestação|operacional|atendimento|qualidade")) {
    temas <- c(temas, "Serviço")
  }
  if (str_detect(texto, "água|agua|captação|abastecimento|manancial|poço|rio|bacia")) {
    temas <- c(temas, "Água")
  }
  if (str_detect(texto, "esgoto|resíduo|residuo|tratamento|lodo|separação|fossa|drenagem")) {
    temas <- c(temas, "Esgoto/Resíduos")
  }
  if (str_detect(texto, "drenagem|chuva|inundação|inundacao|escoamento|pluvial")) {
    temas <- c(temas, "Drenagem")
  }
  if (str_detect(texto, "regionalização|regionalizacao|consórcio|intermunicipal|cooperação|regional")) {
    temas <- c(temas, "Regionalização")
  }
  if (str_detect(texto, "tarifa|preço|preco|custo|taxa|valor|faturamento|cobrança")) {
    temas <- c(temas, "Tarifa")
  }
  if (length(temas) == 0) temas <- "Outros"
  
  paste(temas, collapse = "; ")
}

# Função principal para raspagem ARSAE-MG (exemplo)
scrape_arsae_mg <- function(base_url = "https://www.arsae.mg.gov.br/", entity = "ARSAE-MG") {
  cat("Base:", base_url, "\n")
  page0 <- read_html(base_url)
  subs <- find_links(page0, base_url)
  cat("Encontrou", length(subs), "links relevantes\n")
  
  docs <- map_dfr(subs, ~ {
    cat("→ Visitando:", .x, "\n")
    tryCatch(collect_docs(.x, entity), 
             error = function(e) tibble(Entidade = entity, Pagina = .x, Titulo = NA_character_, Link = NA_character_, Data = as.Date(NA), pdf = FALSE))
  })
  
  # Baixa e extrai textos dos PDFs
  pdf_docs <- docs %>% filter(pdf)
  pdf_texts <- map_dfr(pdf_docs$Link, download_and_extract_pdf)
  
  if (nrow(pdf_texts) > 0) {
    final <- left_join(docs, pdf_texts, by = c("Link" = "pdf"))
  } else {
    final <- docs %>% mutate(texto = NA_character_)
  }
  
  # Classifica o tema dos textos
  final <- final %>%
    mutate(Tema = map_chr(texto, classifica_tema))
  
  return(final)
}

# Executa a raspagem
resultado <- scrape_arsae_mg()

# Seleciona colunas importantes
resultados_limpos <- resultado %>%
  select(Entidade, Pagina, Titulo, Link, Data, Tema, texto)

# Salva CSV com encoding Latin1
caminho_arquivo <- "W:/R/Monitor Regulação/data/arsae_mg_normas_completa.csv"

write.csv2(resultados_limpos, file = caminho_arquivo, row.names = FALSE, fileEncoding = "Latin1")

cat("Arquivo salvo em:", caminho_arquivo, "\n")

