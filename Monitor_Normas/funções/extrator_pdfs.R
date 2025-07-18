download_and_extract_pdf <- function(link, dest_dir = "pdfs") {
  dir.create(dest_dir, showWarnings = FALSE)
  destino <- file.path(dest_dir, basename(link))
  
  if (!file.exists(destino)) {
    try(download.file(link, destino, mode = "wb", quiet = TRUE), silent = TRUE)
  }
  
  tryCatch({
    texto <- pdftools::pdf_text(destino)
    tibble::tibble(pdf = link, texto = paste(texto, collapse = "\n"))
  }, error = function(e) {
    warning(paste("Erro ao processar:", link))
    tibble::tibble(pdf = link, texto = NA)
  })
}

