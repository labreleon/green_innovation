library(data.table)
library(haven)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


# Carregar os dados
weather <- fread("./output/weather data/merged_climate_education_data_micro.csv")
rais <- fread("./output/rais/data_rais_total_merge.csv")
patent <- fread("./output/patent data/patent.csv")
quadro_societario <- fread("./output/quadro_societario/quadro_societario.csv")
mun <- fread("./output/dummy_micro_year_state.csv")
sol_eol <- fread("./output/annel/solar_eolico_projetos.csv", encoding = "Latin-1")



#Limpeza

weather <- weather[,1:25]
setnames(rais,c("id_municipio","ano"),c("municipality_code","year"))
setnames(patent,c("cd_ibge_cidade","DT_DEPOSITO"),c("municipality_code","year"))
setnames(mun,c("code_muni"),c("municipality_code"))
setnames(sol_eol,c("code_muni","ano"),c("municipality_code","year"))


rais <- merge(rais,mun, by = c("municipality_code","year"),all.x= TRUE)
patent <- merge(patent,mun, by = c("municipality_code","year"),all.x= TRUE)
quadro_societario <- merge(quadro_societario,mun, by = c("municipality_code","year"),all.x= TRUE)
sol_eol <- merge(sol_eol,mun,by = c("municipality_code","year"),all.x = TRUE)

# Agregar os dados por Ano e RGI
rais <- rais[, .(
  TotalVinculosAtivos = sum(TotalVinculosAtivos, na.rm = TRUE),
  TotalVinculosVerdes = sum(TotalVinculosVerdes, na.rm = TRUE)
), by = .(year, cod_rgi)]

rais <- rais[,ProporcaoVerde:=TotalVinculosVerdes/TotalVinculosAtivos]



# Agregar os dados por Ano e RGI
patent <- patent[, .(
  Quantidade_Patentes = sum(Quantidade_Patentes, na.rm = TRUE),
  Quantidade_Patentes_verdes = sum(Quantidade_Patentes_verdes, na.rm = TRUE),
  Quantidade_Patentes_5_anos = sum(Quantidade_Patentes_5_anos, na.rm = TRUE),
  Quantidade_Patentes_verdes_5_anos = sum(Quantidade_Patentes_verdes_5_anos, na.rm = TRUE)
), by = .(year, cod_rgi)]

patent <- patent[,ProporcaoVerde:=Quantidade_Patentes_verdes/Quantidade_Patentes]
patent <- patent[,ProporcaoVerde_5_anos:=Quantidade_Patentes_5_anos/Quantidade_Patentes_verdes_5_anos]


# Agregar os dados por Ano e RGI
quadro_societario <- quadro_societario[, .(
  firmas_ativas = sum(firmas_ativas, na.rm = TRUE),
  firmas_ativas_verde = sum(firmas_ativas_verde, na.rm = TRUE)
), by = .(year, cod_rgi)]

quadro_societario <- quadro_societario[,ProporcaoVerde:=firmas_ativas_verde/firmas_ativas]

# Agregar os dados por Ano e RGI
sol_eol <- sol_eol[, .(
  Quantidade_Projetos = sum(Quantidade_Projetos, na.rm = TRUE),
  solar_mean = mean(solar_mean),
  wind_speed_mean = mean(wind_speed_mean)
), by = .(year, cod_rgi,DscOrigemCombustivel)]





# Merge

weather_rais <- merge(rais,weather,by = c("cod_rgi","year"))
weather_patent <- merge(patent,weather,by = c("cod_rgi","year"))
weather_quadro_societario <- merge(quadro_societario,weather,by = c("cod_rgi","year"))

sol <- sol_eol[DscOrigemCombustivel == "Solar",]
eol <- sol_eol[DscOrigemCombustivel == "EÃ³lica",]

sol <- merge(sol,weather,by = c("cod_rgi","year"))

sol <- sol[!duplicated(sol, by = c("cod_rgi", "year"))]

eol <- merge(eol,weather,by = c("cod_rgi","year"))

eol <- eol[!duplicated(eol, by = c("cod_rgi", "year"))]


weather_sol_eol <- rbind(sol,eol)


#

weather_patent <- weather_patent[!duplicated(weather_patent, by = c("cod_rgi", "year"))]
weather_quadro_societario <- weather_quadro_societario[!duplicated(weather_quadro_societario, by = c("cod_rgi", "year"))]
weather_rais <- weather_rais[!duplicated(weather_rais, by = c("cod_rgi", "year"))]



colnames(weather_rais) <- c(
  "cod_rgi",         # cod_rgi
  "year",             # year
  "total_vinc",       # TotalVinculosAtivos
  "total_vinc_verde", # TotalVinculosVerdes
  "prop_verde",       # ProporcaoVerde
  "ma_max_temp",      # media_anual_max_temp
  "dum_temp",         # dummy_temperature
  "cont_shock_temp",  # cont_shock_temperature
  "ma_shock_temp_10yr", # moving_average_shock_temperature_ten_year
  "lag5_dum_temp",    # lagged_5_dummy_temperature
  "lag10_dum_temp",   # lagged_10_dummy_temperature
  "lag15_dum_temp",   # lagged_15_dummy_temperature
  "lag5_cont_shock_temp",  # lagged_5_cont_shock_temperature
  "lag10_cont_shock_temp", # lagged_10_cont_shock_temperature
  "lag15_cont_shock_temp", # lagged_15_cont_shock_temperature
  "code_state",
  "lat",              # latitude
  "lon",               # longitude
  "ma_precip",        # media_anual_precipitation
  "dum_precip",       # dummy_precipitation
  "cont_shock_precip", # cont_shock_precipitation
  "ma_shock_precip_10yr", # moving_average_shock_precipitation_ten_year
  "lag5_dum_precip",  # lagged_5_dummy_precipitation
  "lag10_dum_precip", # lagged_10_dummy_precipitation
  "lag15_dum_precip", # lagged_15_dummy_precipitation
  "lag5_cont_shock_precip",  # lagged_5_cont_shock_precipitation
  "lag10_cont_shock_precip", # lagged_10_cont_shock_precipitation
  "lag15_cont_shock_precip" # lagged_15_cont_shock_precipitation
)

colnames(weather_patent) <- c(
  "cod_rgi",         # municipality_code
  "year",            # year
  "qtd_pat",         # Quantidade_Patentes
  "qtd_pat_verde",   # Quantidade_Patentes_verdes
  "prop_verde",      # ProporcaoVerde
  "qtd_pat_5yr",     # Quantidade_Patentes_5_anos
  "qtd_pat_verde_5yr",# Quantidade_Patentes_verdes_5_anos
  "prop_verde_5yr",   # ProporcaoVerde_5_anos
  "ma_max_temp",     # media_anual_max_temp
  "dum_temp",        # dummy_temperature
  "cont_shock_temp", # cont_shock_temperature
  "ma_shock_temp_10yr", # moving_average_shock_temperature_ten_year
  "lag5_dum_temp",   # lagged_5_dummy_temperature
  "lag10_dum_temp",  # lagged_10_dummy_temperature
  "lag15_dum_temp",  # lagged_15_dummy_temperature
  "lag5_cont_shock_temp",  # lagged_5_cont_shock_temperature
  "lag10_cont_shock_temp", # lagged_10_cont_shock_temperature
  "lag15_cont_shock_temp", # lagged_15_cont_shock_temperature
  "code_state",
  "lat",             # latitude
  "lon",             # longitude
  "ma_precip",       # media_anual_precipitation
  "dum_precip",      # dummy_precipitation
  "cont_shock_precip", # cont_shock_precipitation
  "ma_shock_precip_10yr", # moving_average_shock_precipitation_ten_year
  "lag5_dum_precip", # lagged_5_dummy_precipitation
  "lag10_dum_precip",# lagged_10_dummy_precipitation
  "lag15_dum_precip",# lagged_15_dummy_precipitation
  "lag5_cont_shock_precip",  # lagged_5_cont_shock_precipitation
  "lag10_cont_shock_precip", # lagged_10_cont_shock_precipitation
  "lag15_cont_shock_precip"  # lagged_15_cont_shock_precipitation
)


colnames(weather_quadro_societario) <- c(
  "cod_rgi",               # [1] cod_rgi
  "year",                  # [2] year
  "firmas_ativas",         # [3] firmas_ativas
  "firmas_verde",          # [4] firmas_ativas_verde
  "prop_verde",            # [5] ProporcaoVerde
  "ma_max_temp",           # [6] media_anual_max_temp
  "dum_temp",              # [7] dummy_temperature
  "cont_shock_temp",       # [8] cont_shock_temperature
  "ma_shock_temp_10yr",    # [9] moving_average_shock_temperature_ten_year
  "lag5_dum_temp",         # [10] lagged_5_dummy_temperature
  "lag10_dum_temp",        # [11] lagged_10_dummy_temperature
  "lag15_dum_temp",        # [12] lagged_15_dummy_temperature
  "lag5_cont_shock_temp",  # [13] lagged_5_cont_shock_temperature
  "lag10_cont_shock_temp", # [14] lagged_10_cont_shock_temperature
  "lag15_cont_shock_temp", # [15] lagged_15_cont_shock_temperature
  "code_state",            # [16] code_state
  "lat",                   # [17] latitude
  "lon",                   # [18] longitude
  "ma_precip",             # [19] media_anual_precipitation
  "dum_precip",            # [20] dummy_precipitation
  "cont_shock_precip",     # [21] cont_shock_precipitation
  "ma_shock_precip_10yr",  # [22] moving_average_shock_precipitation_ten_year
  "lag5_dum_precip",       # [23] lagged_5_dummy_precipitation
  "lag10_dum_precip",      # [24] lagged_10_dummy_precipitation
  "lag15_dum_precip",      # [25] lagged_15_dummy_precipitation
  "lag5_cont_shock_precip",# [26] lagged_5_cont_shock_precipitation
  "lag10_cont_shock_precip",# [27] lagged_10_cont_shock_precipitation
  "lag15_cont_shock_precip" # [28] lagged_15_cont_shock_precipitation
)




colnames(weather_sol_eol) <- c(
  "cod_rgi",             # municipality_code
  "year",                 # year
  "code_state",           # code_state
  "dsc_combustivel",      # DscOrigemCombustivel
  "qtd_proj",             # Quantidade_Projetos
  "solar_mean",           # solar_mean
  "wind_speed_mean",      # wind_speed_mean
  "ma_max_temp",          # media_anual_max_temp
  "dum_temp",             # dummy_temperature
  "cont_shock_temp",      # cont_shock_temperature
  "ma_shock_temp_10yr",   # moving_average_shock_temperature_ten_year
  "lag5_dum_temp",        # lagged_5_dummy_temperature
  "lag10_dum_temp",       # lagged_10_dummy_temperature
  "lag15_dum_temp",       # lagged_15_dummy_temperature
  "lag5_cont_shock_temp", # lagged_5_cont_shock_temperature
  "lag10_cont_shock_temp",# lagged_10_cont_shock_temperature
  "lag15_cont_shock_temp",# lagged_15_cont_shock_temperature
  "lat",                  # latitude
  "lon" ,                  # longitude
  "ma_precip",            # media_anual_precipitation
  "dum_precip",           # dummy_precipitation
  "cont_shock_precip",    # cont_shock_precipitation
  "ma_shock_precip_10yr", # moving_average_shock_precipitation_ten_year
  "lag5_dum_precip",      # lagged_5_dummy_precipitation
  "lag10_dum_precip",     # lagged_10_dummy_precipitation
  "lag15_dum_precip",     # lagged_15_dummy_precipitation
  "lag5_cont_shock_precip",# lagged_5_cont_shock_precipitation
  "lag10_cont_shock_precip",# lagged_10_cont_shock_precipitation
  "lag15_cont_shock_precip"# lagged_15_cont_shock_precipitation
)



# Contando a quantidade de NAs por coluna
na_por_coluna <- colSums(is.na(weather_patent))
print(na_por_coluna)

# Contando a quantidade de NAs por coluna
na_por_coluna <- colSums(is.na(weather_rais))
print(na_por_coluna)

# Contando a quantidade de NAs por coluna
na_por_coluna <- colSums(is.na(weather_quadro_societario))
print(na_por_coluna)



#
dt$cod_rgi <- as.double(dt$cod_rgi)

#
weather_patent <- weather_patent[!is.na(cont_shock_precip),]
weather_rais <- weather_rais[!is.na(cont_shock_precip),]
weather_rais <- weather_rais[is.na(prop_verde),prop_verde:=0]
weather_quadro_societario <- weather_quadro_societario[!is.na(cont_shock_precip),]

#


weather_patent <- merge(weather_patent,dt,by = c("cod_rgi","year"),all.x = TRUE)

weather_patent <- weather_patent[is.na(T),T:=0]

weather_patent <- unique(weather_patent)

weather_patent [, T_lag5 := shift(T, 5), by = "cod_rgi"]
weather_patent [, T_lag10 := shift(T, 10), by = "cod_rgi"]


weather_rais <- merge(weather_rais,dt,by = c("cod_rgi","year"),all.x = TRUE)
weather_rais <- weather_rais[is.na(T),T:=0]
weather_rais <- unique(weather_rais)

weather_rais[, T_lag5 := shift(T, 5), by = "cod_rgi"]
weather_rais[, T_lag10 := shift(T, 10), by = "cod_rgi"]


weather_quadro_societario <- merge(weather_quadro_societario,dt,by = c("cod_rgi","year"),all.x = TRUE)
weather_quadro_societario <- weather_quadro_societario[is.na(T),T:=0]
weather_quadro_societario <- unique(weather_quadro_societario)

weather_quadro_societario[, T_lag5 := shift(T, 5), by = "cod_rgi"]
weather_quadro_societario[, T_lag10 := shift(T, 10), by = "cod_rgi"]






# Salvar a base de dados final
fwrite(weather_patent, "./output/final_base/weather_patent_micro.csv")
fwrite(weather_quadro_societario, "./output/final_base/weather_quadro_societario_micro.csv")
fwrite(weather_rais, "./output/final_base/weather_rais_micro.csv")
fwrite(weather_sol_eol, "./output/final_base/weather_sol_eol_micro.csv")
# Salvar as bases em formato .dta
write_dta(weather_patent, "./output/final_base/weather_patent_micro.dta")
write_dta(weather_quadro_societario, "./output/final_base/weather_quadro_societario_micro.dta")
write_dta(weather_rais, "./output/final_base/weather_rais_micro.dta")
write_dta(weather_sol_eol, "./output/final_base/weather_sol_eol_micro.dta")
