# Carregar a biblioteca necessária
library(data.table)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Carregar os dados de temperatura e precipitação
dados_finais_temp <- fread("./output/weather data/final_temperature_data_1940_2020.csv")
dados_finais_preci <- fread("./output/weather data/final_precipitation_data_1940_2020.csv")

# Realizar o merge dos dados
dados_finais <- merge(dados_finais_temp, dados_finais_preci, by = c("municipality_code", "year"))

# Salvar os resultados combinados em CSV
fwrite(dados_finais, "./output/weather data/final_weather_data_1940_2020.csv")
