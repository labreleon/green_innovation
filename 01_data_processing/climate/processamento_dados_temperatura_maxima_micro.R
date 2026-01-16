# Carregar a biblioteca necessária
library(data.table)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/input/weather data/datazoom/")


#
mun <- fread("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/dummy_micro_year_state.csv")


# Lista com os nomes dos arquivos
arquivos <- c("max_temp_00_20.csv", "max_temp_80_00.csv", "max_temp_60_80.csv", "max_temp_58_60.csv")

# Função para processar cada arquivo
processar_arquivo <- function(arquivo) {
  # Carregar os dados
  dados <- fread(arquivo)
  
  # Converter a coluna de data e extrair o ano
  dados[, date := as.IDate(date, "%Y-%m-%d")]
  dados[, year := year(date)]
  
  # Filtrar por anos de 1940 a 2020
  dados <- dados[year >= 1940 & year <= 2020]
  
  #
  dados <- merge(dados,mun, by.x = c("municipality_code","year"),by.y = c("code_muni","year"))
  
  # Calcular a média anual da temperatura máxima para cada município
  media_anual <- dados[, .(media_anual_max_temp = mean(max_temperature, na.rm = TRUE)), by = .(cod_rgi, year)]
  
  return(media_anual)
}

# Aplicar a função a cada arquivo e combinar os resultados
resultados <- rbindlist(lapply(arquivos, processar_arquivo))

# Calcular a média histórica e o desvio padrão da temperatura máxima para cada município
media_historica_sd <- resultados[, .(media_historica = mean(media_anual_max_temp, na.rm = TRUE), sd = sd(media_anual_max_temp, na.rm = TRUE)), by = cod_rgi]

# Junção para calcular a variável dummy
dados_finais <- merge(resultados, media_historica_sd, by = "cod_rgi", all.x = TRUE)
dados_finais[, dummy := ifelse((media_anual_max_temp - media_historica) / sd > 1, 1, 0)]

# Variável contínua
dados_finais[, cont_shock_temperature := (media_anual_max_temp - media_historica) / sd]

# Garantir que os dados estão ordenados por município e ano
setorder(dados_finais, cod_rgi, year)

# Definir o período da média móvel (10 anos)
period <- 10

# Calcular a média móvel de choque de temperatura (10 anos)
dados_finais[, moving_average_shock_temperature_ten_year := frollmean(media_anual_max_temp, period, align = "right")]

# Renomear a coluna dummy para dummy_temperature
setnames(dados_finais, "dummy", "dummy_temperature")

# Selecionar as colunas finais
dados_finais <- dados_finais[, .(cod_rgi, year, media_anual_max_temp, dummy_temperature, cont_shock_temperature, moving_average_shock_temperature_ten_year)]

# Remover duplicados
dados_finais <- unique(dados_finais)


# Criar variáveis defasadas
dados_finais <- dados_finais[, 
                             `:=`(
                               lagged_5_dummy_temperature = shift(dummy_temperature, 5, type = "lag"),
                               lagged_10_dummy_temperature = shift(dummy_temperature, 10, type = "lag"),
                               lagged_15_dummy_temperature = shift(dummy_temperature, 15, type = "lag"),
                               lagged_5_cont_shock_temperature = shift(cont_shock_temperature, 5, type = "lag"),
                               lagged_10_cont_shock_temperature = shift(cont_shock_temperature, 10, type = "lag"),
                               lagged_15_cont_shock_temperature = shift(cont_shock_temperature, 15, type = "lag")
                             ), by = cod_rgi
]

dados_finais <- merge(dados_finais,mun,by = c("cod_rgi","year"),all.x=TRUE)

# Salvar a nova base de dados
fwrite(dados_finais, "C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/weather data/final_temperature_data_1940_2020_micro.csv")