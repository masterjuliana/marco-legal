download_and_extract_pdf - function(link, dest_dir = pdfs) {
  dir.create(dest_dir, showWarnings = FALSE)
  destino - file.path(dest_dir, basename(link))
  if (!file.exists(destino)) {
    try(download.file(link, destino, mode = wb, quiet = TRUE), silent = TRUE)
  }
  tryCatch({
    tibble(pdf = link, texto = paste(pdf_text(destino), collapse = n))
  }, error = function(e) {
    tibble(pdf = link, texto = NA_character_)
  })
}
