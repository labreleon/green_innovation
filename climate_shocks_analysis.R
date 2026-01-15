# Carregar os pacotes necessários
if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
if (!requireNamespace("future", quietly = TRUE)) install.packages("future")

library(fixest)
library(data.table)
library(future)

# Configurar o plano de execução paralelo
plan(multisession, workers = 4)

# Definir o diretório de trabalho
setwd("D:/Users/leonl/Desktop/Projeto/temp/Direction of inovation/Climate shocks and innovation")

# Carregar os dados
weather_rais <- fread("./Output/Final_base/weather_rais_total.csv")
weather_patent <- fread("./Output/Final_base/weather_patent_total.csv")
weather_quadro_societario <- fread("./Output/Final_base/weather_quadro_societario_total.csv")

# Função para preparar e analisar os dados
prepare_and_analyze <- function(data, dependent_variable, file_prefix, additional_grouping = NULL) {
  models_list <- list()
  vars_to_analyze <- list(
    c("ma_shock_temp_10yr", "ma_shock_precip_10yr"),
    c("cont_shock_temp", "cont_shock_precip"),
    c("lag5_cont_shock_temp", "lag5_cont_shock_precip"),
    c("lag10_cont_shock_temp", "lag10_cont_shock_precip")
  )
  
  # Criar a variável estado_ano
  data[, estado_ano := interaction(code_state, year)]
  
  # Determinar a parte dos efeitos fixos do modelo
  fixed_effects <- "mun_code + year + estado_ano"
  if (!is.null(additional_grouping)) {
    fixed_effects <- paste(fixed_effects, "+", additional_grouping)
  }
  
  for (vars in vars_to_analyze) {
    # Regressões de temperatura e chuva separados
    for (var in vars) {
      formula <- as.formula(paste0(dependent_variable, " ~ ", var, " | ", fixed_effects))
      fe_model <- feols(formula, data = data, cluster = "mun_code")
      models_list[[var]] <- fe_model
    }
    
    # Regressões de temperatura e chuva juntos
    independent_vars <- paste(vars, collapse = " + ")
    formula <- as.formula(paste0(dependent_variable, " ~ ", independent_vars, " | ", fixed_effects))
    fe_model <- feols(formula, data = data, cluster = "mun_code")
    models_list[[paste(vars, collapse = "_")]] <- fe_model
  }
  
  # Definir o caminho do arquivo
  file_path <- paste0("tables/", file_prefix, "_models.tex")
  
  # Remover o arquivo existente, se houver
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
  
  # Usar etable para exportar os resultados em formato LaTeX
  etable(models_list, file = file_path)
}

# Exemplo de uso da função
prepare_and_analyze(weather_quadro_societario, "prop_verde", "climate_shocks")
