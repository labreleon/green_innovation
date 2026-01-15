library(data.table)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Função para calcular médias e deltas
calculate_deltas <- function(data, start_year1, end_year1, start_year2, end_year2, value_col, suffix) {
  period1 <- data[year %between% c(start_year1, end_year1), .(mean_value = mean(get(value_col), na.rm = TRUE)), by = .(municipality_code)]
  period2 <- data[year %between% c(start_year2, end_year2), .(mean_value = mean(get(value_col), na.rm = TRUE)), by = .(municipality_code)]
  results <- merge(period1, period2, by = "municipality_code", suffixes = c(paste0("_", start_year1, "_", end_year1), paste0("_", start_year2, "_", end_year2)))
  results[, delta := get(paste0("mean_value_", start_year2, "_", end_year2)) - get(paste0("mean_value_", start_year1, "_", end_year1))]
  results <- results[, .(municipality_code, delta)]
  setnames(results, "delta", paste0("delta_", suffix))
  return(results)
}

# Carregar os dados
weather <- fread("./Output/weather data/final_weather_data_1940_2020.csv")
rais <- fread("./Output/rais/data_rais_total_merge.csv")
patent <- fread("./Output/patent data/patent.csv")
quadro_societario <- fread("./Output/quadro_societario/quadro_societario.csv")

# Calcular deltas para dados climáticos
delta_temp <- calculate_deltas(weather, 1995, 2000, 2015, 2020, "cont_shock_temperature", "temp")
delta_preci <- calculate_deltas(weather, 1995, 2000, 2015, 2020, "cont_shock_precipitation", "preci")
weather_long <- merge(delta_temp, delta_preci, by = "municipality_code")

# Calcular deltas para dados RAIS
setnames(rais,c("id_municipio","ano"),c("municipality_code","year"))
delta_rais <- calculate_deltas(rais, 1995, 2000, 2015, 2020, "ProporcaoVerde", "rais")

# Calcular deltas para dados de patentes
setnames(patent,c("cd_ibge_cidade","DT_DEPOSITO"),c("municipality_code","year"))
delta_patent <- calculate_deltas(patent, 1997, 2000, 2017, 2020, "ProporcaoVerde", "patent")

# Calcular deltas para dados do quadro societário
delta_quadro_societario <- calculate_deltas(quadro_societario, 1995, 2000, 2015, 2020, "ProporcaoVerde", "quadro_societario")

# Combinar todos os resultados em uma única base de dados
final_data <- merge(weather_long, delta_rais, by = "municipality_code", all = TRUE)
final_data <- merge(final_data, delta_patent, by = "municipality_code", all = TRUE)
final_data <- merge(final_data, delta_quadro_societario, by = "municipality_code", all = TRUE)



# Salvar a base de dados final
fwrite(final_data, "./output/final_combined_data.csv")

final_data$municipality_code <- as.character(final_data$municipality_code)