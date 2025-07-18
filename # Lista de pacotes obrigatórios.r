# Lista de pacotes obrigatórios
pacotes <- c("shiny", "tidyverse", "DT")

# Verifica e instala se necessário
instalar <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("📦 Instalando pacote:", pkg))
    install.packages(pkg)
  } else {
    message(paste("✅ Pacote já instalado:", pkg))
  }
}

# Rodar instalação
invisible(lapply(pacotes, instalar))

# (Opcional) Executar o painel após instalar
if (file.exists("app/painel_unificado.R")) {
  message("🚀 Iniciando painel...")
  source("app/painel_unificado.R", echo = TRUE)
} else {
  message("⚠️ Arquivo 'painel_unificado.R' não encontrado em /app.")
}
