# Carregar a biblioteca necessária
library(data.table)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Carregar os dados de temperatura e precipitação
dados_finais_temp <- fread("./output/weather data/final_temperature_data_1940_2020_micro.csv")
dados_finais_preci <- fread("./output/weather data/final_precipitation_data_1940_2020_micro.csv")

setnames(dados_finais_temp,"code_muni","municipality_code")
setnames(dados_finais_preci,"code_muni","municipality_code")


dados_finais_preci <- dados_finais_preci[,1:13]
dados_finais_preci <- dados_finais_preci[,cod_rgi:=NULL]

# Realizar o merge dos dados
dados_finais <- merge(dados_finais_temp, dados_finais_preci, by = c("municipality_code", "year"))

# Salvar os resultados combinados em CSV
fwrite(dados_finais, "./output/weather data/final_weather_data_1940_2020_micro.csv")
