source("funcoes/raspagem_links.R")
source("funcoes/extrator_pdfs.R")
source("funcoes/classificador_tema.R")

scrape_entidade <- function(entity, base_url, keywords) {
  links <- mod_scrape_links(base_url, keywords)
  resultado <- map_dfr(links, ~ {
    page <- read_html(.x)
    nodes <- page %>% html_elements("a")
    tibble(
      Entidade = entity,
      Pagina = .x,
      Titulo = nodes %>% html_text(trim = TRUE),
      Link = nodes %>% html_attr("href") %>% map_chr(~ url_absolute(.x, .x)),
      Data = Sys.Date()
    )
  }) %>% distinct(Link, .keep_all = TRUE)
  
  pdfs <- resultado %>% filter(str_detect(Link, "\\.pdf$"))
  textos <- map_dfr(pdfs$Link, download_and_extract_pdf)
  
  resultado <- left_join(resultado, textos, by = c("Link" = "pdf")) %>%
    mutate(Tema = map_chr(texto, classifica_tema))
  
  dir.create(file.path("data", entity), showWarnings = FALSE)
  write_csv2(resultado, file.path("data", entity, "normas.csv"))
}
