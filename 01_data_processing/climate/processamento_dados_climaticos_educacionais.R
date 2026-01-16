# Carregar as bibliotecas necessárias
library(data.table)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Carregar os dados
weather <- fread("./output/weather data/final_weather_data_1940_2020.csv")
censo_escolar <- fread("./output/Censo_da_educacao_basica.csv")
censo_ensino_superior <- fread("./output/Censo_do_ensino_superior.csv")


# Realizar o merge com os censos escolares
merged_data <- merge(weather, censo_escolar, by = c("municipality_code", "year"), all.x = TRUE)
merged_data <- merge(merged_data, censo_ensino_superior, by = c("municipality_code", "year"), all.x = TRUE)

# Função para criar variáveis de interação
create_interaction_vars <- function(df, var1, var2_list, suffix) {
  for (var2 in var2_list) {
    new_col_name <- paste0(var2, "_", suffix)
    df[, (new_col_name) := get(var1) * get(var2)]
  }
  return(df)
}

# Listas de variáveis e lags
precip_vars <- c("dummy_precipitation", "lagged_5_dummy_precipitation", "lagged_10_dummy_precipitation", "lagged_15_dummy_precipitation")
temp_vars <- c("dummy_temperature", "lagged_5_dummy_temperature", "lagged_10_dummy_temperature", "lagged_15_dummy_temperature")
shock_precip_vars <- c("cont_shock_precipitation", "lagged_5_cont_shock_precipitation", "lagged_10_cont_shock_precipitation", "lagged_15_cont_shock_precipitation")
shock_temp_vars <- c("cont_shock_temperature", "lagged_5_cont_shock_temperature", "lagged_10_cont_shock_temperature", "lagged_15_cont_shock_temperature")
moving_avg_vars <- c("moving_average_shock_temperature_ten_year", "moving_average_shock_precipitation_ten_year")

# Variáveis de matrícula e seus respectivos sufixos
matricula_vars <- list(
  "total_matriculas_per_capita" = "matricula_superior",
  "total_quantidade_matricula_medio_per_capita" = "matricula_medio",
  "total_matriculas_fundamental_anos_finais_per_capita" = "matricula_fundamental_final",
  "total_matricula_fundamental_anos_iniciais_per_capita" = "matricula_fundamental_inicial"
)

# Criando variáveis de interação para precipitação e temperatura
for (var in names(matricula_vars)) {
  suffix <- matricula_vars[[var]]
  merged_data <- create_interaction_vars(merged_data, var, precip_vars, paste0("preci_", suffix))
  merged_data <- create_interaction_vars(merged_data, var, temp_vars, paste0("temp_", suffix))
}

# Criando variáveis de interação para choques contínuos de precipitação e temperatura
for (var in names(matricula_vars)) {
  suffix <- matricula_vars[[var]]
  merged_data <- create_interaction_vars(merged_data, var, shock_precip_vars, paste0("cont_preci_", suffix))
  merged_data <- create_interaction_vars(merged_data, var, shock_temp_vars, paste0("cont_temp_", suffix))
}

# Criando variáveis de interação para médias móveis de choques de temperatura e precipitação
for (var in names(matricula_vars)) {
  suffix <- matricula_vars[[var]]
  merged_data <- create_interaction_vars(merged_data, var, moving_avg_vars, paste0("mov_avg_", suffix))
}

# Salvar o resultado final em CSV
fwrite(merged_data, "./output/weather data/merged_climate_education_data.csv")
