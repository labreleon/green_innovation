library(data.table)
library(lubridate)
library(ggplot2)
library(tidyr)
library(exactextractr)
library(sf)
library(terra)
library(geobr)
library(dplyr)


setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


############################################

# Carregue os municípios do Brasil (ano 2020, simplificado)
municipios <- read_municipality(year = 2020, simplified = TRUE)

# Defina o caminho base dos rasters
caminho_base <- "C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/input/solar_potential/Brazil_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/monthly/"

# Carregue um raster de exemplo para obter a projeção
raster_exemplo <- rast(paste0(caminho_base, "PVOUT_01.tif"))

# Reprojete o shapefile para a projeção do raster
municipios <- st_transform(municipios, crs(raster_exemplo))

# Loop para processar cada arquivo mensal
for (mes in 1:12) {
  # Defina o nome do arquivo
  nome_arquivo <- paste0("PVOUT_", sprintf("%02d", mes), ".tif")
  caminho_raster <- paste0(caminho_base, nome_arquivo)
  
  # Carregue o raster
  raster_data <- rast(caminho_raster)
  
  # Extraia os valores médios para os municípios
  valores_municipios <- exact_extract(raster_data, municipios, fun = "mean", force_df = TRUE)
  
  # Adicione os valores ao dataframe dos municípios
  coluna_nome <- paste0("PVOUT_", sprintf("%02d", mes))
  municipios[[coluna_nome]] <- valores_municipios$mean
}

# Visualize os primeiros registros
head(municipios)


# Calcular a média das variáveis PVOUT_01 a PVOUT_12
municipios <- municipios %>% 
  rowwise() %>% 
  mutate(solar_mean = mean(c_across(starts_with("PVOUT_")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-starts_with("PVOUT_"))


# Carregue o raster de velocidade do vento
caminho_wind <- "C:/Users/User/Downloads/BRA_wind-speed_100m.tif"
raster_wind <- rast(caminho_wind)

# Extraia os valores médios de velocidade do vento para os municípios
valores_wind <- exact_extract(raster_wind, municipios, fun = "mean", force_df = TRUE)

# Adicione os valores de velocidade do vento ao dataframe dos municípios
municipios$wind_speed_mean <- valores_wind$mean

rm(raster_data,raster_exemplo,raster_wind,valores_municipios,valores_wind)

setDT(municipios)


##########################################################################

dt <- fread("./input/aneel/siga-empreendimentos-geracao.csv", encoding = "Latin-1")


dt[, DatEntradaOperacao := as.Date(DatEntradaOperacao, format = "%Y-%m-%d")]

dt[, ano := format(DatEntradaOperacao, "%Y")]

dt[, ano := year(DatEntradaOperacao)]
# Ordenar por Ano (opcional)
setorder(dt, ano)

# Separando municípios e estados
dt <- dt %>%
  mutate(municipios_estados = strsplit(DscMuninicpios, ", ")) %>%
  unnest(municipios_estados)

dt$Municipio <- sub(" - .*", "", dt$municipios_estados)
dt$Estado <- sub(".*- ", "", dt$municipios_estados)


setDT(dt)
# Agregar os dados somando a quantidade de projetos
dt_agreg <- dt[, .(Quantidade_Projetos = .N), by = .(ano, DscOrigemCombustivel, Municipio,Estado)]



dt_agreg <- dt_agreg[(DscOrigemCombustivel == "Solar" |	DscOrigemCombustivel == "Eólica") & ano > 1999,]

# Função para encontrar o nome mais próximo considerando município e estado
match_municipios <- function(muni, estado, municipios) {
  municipios_filtrados <- municipios %>% filter(abbrev_state == estado)
  
  if(nrow(municipios_filtrados) == 0) {
    return(data.frame(Municipio_Match = NA, Distancia = NA))
  }
  
  distancias <- stringdist::stringdist(muni, municipios_filtrados$name_muni, method = "jw")
  idx_min <- which.min(distancias)
  return(data.frame(Municipio_Match = municipios_filtrados$name_muni[idx_min], Distancia = distancias[idx_min]))
}

# Aplicando o matching considerando município e estado
dt_match <- do.call(rbind, mapply(match_municipios, dt_agreg$Municipio, dt_agreg$Estado, MoreArgs = list(municipios = municipios), SIMPLIFY = FALSE))
dt_agreg <- cbind(dt_agreg, dt_match)

dt_agreg  <- dt_agreg[ano < 2020]

setDT(municipios)
municipios <- municipios[,.(code_muni,name_muni,code_state,abbrev_state,name_state,solar_mean,wind_speed_mean)]


dt_final <- merge(municipios,dt_agreg,by.x = c("name_muni","abbrev_state"),by.y = c("Municipio_Match","Estado"),all.x = TRUE)

dt_final <- dt_final[,.(code_muni,Municipio,ano,abbrev_state,code_state,DscOrigemCombustivel,Quantidade_Projetos,solar_mean,wind_speed_mean)]



# Criar uma sequência de anos de 2000 a 2019
anos <- 2000:2019
DscOrigemCombustivel <- c("Eólica","Solar") 

dt_expandido <- dt_final[
  , {
    # Repetir cada ano duas vezes (uma para Solar, outra para Eólica)
    ano_expandido <- rep(anos, each = 2)
    DscOrigemCombustivel_expandido <- rep(c("Solar", "Eólica"), times = length(anos))
    
    .(
      ano = ano_expandido,
      DscOrigemCombustivel = DscOrigemCombustivel_expandido
    )
  }, 
  by = .(code_muni, Municipio, abbrev_state, code_state, solar_mean, wind_speed_mean)
]
# Ordenar por code_muni e ano
dt_expandido <- dt_expandido[order(code_muni, ano)]

dt_final <- dt_final[,.(code_muni,ano,DscOrigemCombustivel,Quantidade_Projetos)]

dt_final_completo <- merge(
  dt_expandido,
  dt_final,
  by = c("code_muni", "ano", "DscOrigemCombustivel"),
  all.x = TRUE
)


dt_final_completo <- dt_final_completo[is.na(Quantidade_Projetos),Quantidade_Projetos:=0]




fwrite(dt_final_completo,"./output/annel/solar_eolico_projetos.csv",bom = TRUE)