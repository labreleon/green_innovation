# Carregar as bibliotecas necessárias
library(readxl)
library(data.table)
library(geobr)
library(sf)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Ler todos os municípios do país em um dado ano
mun <- read_municipality(year = 2020, simplified = TRUE)
setDT(municipios)

# Calcular o centroide do MULTIPOLYGON
centroide <- st_centroid(mun$geom)

# Extrair as coordenadas do centroide
coords_centroide <- st_coordinates(centroide)
coords_centroide <- data.table(coords_centroide)
setDT(mun)


mun[, `:=` (latitude = coords_centroide$Y, longitude = coords_centroide$X)]


# Selecionar as colunas relevantes
mun <- mun[, .(code_muni, code_state, code_region, latitude, longitude)]

# Gerar uma sequência de anos entre 1990 e 2023
years <- 1990:2023

# Criar uma combinação de todos os municípios com todos os anos
mun <- mun[, .(year = years), by = .(code_muni, code_state, code_region, latitude, longitude)]

# Criar variáveis dummy para cada ano e estado
#dummy_df <- dcast(mun, code_muni + cod_rgi + latitude + longitude + year ~ paste0("dummy_", year, "_", code_state), fun.aggregate = length)

# Salvar o resultado em CSV
fwrite(mun, "./output/dummy_municipalities_year_state_mun.csv", row.names = FALSE)
