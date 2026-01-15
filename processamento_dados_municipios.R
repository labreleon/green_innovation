# Carregar as bibliotecas necessárias
library(readxl)
library(data.table)
library(geobr)
library(sf)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Ler todos os municípios do país em um dado ano
mun <- read_municipality(code_muni = "all", year = 2020)

# Calcular o centroide do MULTIPOLYGON
centroide <- st_centroid(mun$geom)

# Extrair as coordenadas do centroide
coords_centroide <- st_coordinates(centroide)
coords_centroide <- data.table(coords_centroide)
setDT(mun)

mun <- mun[, .(code_muni, code_state)]
mun[, `:=` (latitude = coords_centroide$Y, longitude = coords_centroide$X)]
mun$code_muni <- as.character(mun$code_muni)

# Ler os dados das regiões geográficas
micro <- read_xlsx("./input/ibge/regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")
setDT(micro)

# Fazer o merge dos dados
mun <- merge(mun, micro, by.x = "code_muni", by.y = "CD_GEOCODI", all.x = TRUE)

# Selecionar as colunas relevantes
mun <- mun[, .(code_muni, code_state, cod_rgi, latitude, longitude)]

# Gerar uma sequência de anos entre 1990 e 2023
years <- 1990:2023

# Criar uma combinação de todos os municípios com todos os anos
mun <- mun[, .(year = years), by = .(code_muni, code_state, cod_rgi, latitude, longitude)]

# Criar variáveis dummy para cada ano e estado
#dummy_df <- dcast(mun, code_muni + cod_rgi + latitude + longitude + year ~ paste0("dummy_", year, "_", code_state), fun.aggregate = length)

# Salvar o resultado em CSV
fwrite(mun, "./output/dummy_municipalities_year_state.csv", row.names = FALSE)
