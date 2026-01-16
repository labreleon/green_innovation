# Carregar a biblioteca necessária
library(data.table)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/input/weather data/datazoom")

# Lista com os nomes dos arquivos
arquivos <- c("precip_00_20.csv", "precip_80_00.csv", "precip_60_80.csv", "precip_58_60.csv")

# Função para processar cada arquivo
processar_arquivo <- function(arquivo) {
  # Carregar os dados
  dados <- fread(arquivo)
  
  # Converter a coluna de data e extrair o ano
  dados[, date := as.IDate(date, "%Y-%m-%d")]
  dados[, year := year(date)]
  
  # Filtrar por anos de 1940 a 2020
  dados <- dados[year >= 1940 & year <= 2020]
  
  # Calcular a média anual da precipitação para cada município
  media_anual <- dados[, .(media_anual_precipitation = mean(precipitation, na.rm = TRUE)), by = .(municipality_code, year)]
  
  return(media_anual)
}

# Aplicar a função a cada arquivo e combinar os resultados
resultados <- rbindlist(lapply(arquivos, processar_arquivo))

# Calcular a média histórica e o desvio padrão da precipitação para cada município
media_historica_sd <- resultados[year < 2000, .(media_historica = mean(media_anual_precipitation, na.rm = TRUE), sd = sd(media_anual_precipitation, na.rm = TRUE)), by = municipality_code]

# Junção para calcular a variável dummy
dados_finais <- merge(resultados, media_historica_sd, by = "municipality_code", all.x = TRUE)
dados_finais[, dummy := ifelse((media_anual_precipitation - media_historica) / sd > 1, 1, 0)]


#mean
dados_finais[year > 1999 & year < 2011, mean_precipitation_00_10 := mean(media_anual_precipitation,),by = municipality_code]
dados_finais[year > 1999 & year < 2021, mean_precipitation_00_20 := mean(media_anual_precipitation,),by = municipality_code]




# Medida alternativa
dados_finais[, cont_shock_precipitation := (media_anual_precipitation - media_historica) / sd]
dados_finais[, cont_shock_precipitation_long_00_10 := (mean_precipitation_00_10  - media_historica) / sd]
dados_finais[, cont_shock_precipitation_long_00_20 := (mean_precipitation_00_20  - media_historica) / sd]



# Garantir que os dados estão ordenados por município e ano
setorder(dados_finais, municipality_code, year)

# Definir o período da média móvel (10 anos)
period <- 10

# Calcular a média móvel de choque de precipitação (10 anos)
dados_finais[, moving_average_shock_precipitation_ten_year := frollmean(cont_shock_precipitation, period, align = "right")]

# Renomear a coluna dummy para dummy_precipitation
setnames(dados_finais, "dummy", "dummy_precipitation")

# Selecionar as colunas finais
dados_finais <- dados_finais[, .(municipality_code, year, media_anual_precipitation, dummy_precipitation, cont_shock_precipitation, moving_average_shock_precipitation_ten_year,cont_shock_precipitation_long_00_10,cont_shock_precipitation_long_00_20  )]

# Remover duplicados
dados_finais <- unique(dados_finais)


# Criar variáveis defasadas
dados_finais <- dados_finais[, 
                             `:=`(
                               lagged_5_dummy_precipitation = shift(dummy_precipitation, 5, type = "lag"),
                               lagged_10_dummy_precipitation = shift(dummy_precipitation, 10, type = "lag"),
                               lagged_15_dummy_precipitation = shift(dummy_precipitation, 15, type = "lag"),
                               lagged_5_cont_shock_precipitation = shift(cont_shock_precipitation, 5, type = "lag"),
                               lagged_10_cont_shock_precipitation = shift(cont_shock_precipitation, 10, type = "lag"),
                               lagged_15_cont_shock_precipitation = shift(cont_shock_precipitation, 15, type = "lag")
                             ), by = municipality_code]
# Salvar a nova base de dados
fwrite(dados_finais, "C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/weather data/final_precipitation_data_1940_2020.csv")
