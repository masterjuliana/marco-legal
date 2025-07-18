classifica_tema <- function(texto) {
  if (is.na(texto)) return("Não Classificado")
  texto <- tolower(texto)
  temas <- c()
  if (str_detect(texto, "governança|gestão|monitoramento")) temas <- c(temas, "Governança")
  if (str_detect(texto, "água|manancial")) temas <- c(temas, "Água")
  if (str_detect(texto, "esgoto|tratamento")) temas <- c(temas, "Esgoto")
  if (str_detect(texto, "regionalização")) temas <- c(temas, "Regionalização")
  if (str_detect(texto, "tarifa|taxa")) temas <- c(temas, "Tarifa")
  if (length(temas) == 0) temas <- "Outros"
  paste(temas, collapse = "; ")
}
