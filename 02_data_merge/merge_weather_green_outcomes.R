library(data.table)
library(haven)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


# Carregar os dados
weather <- fread("./output/weather data/merged_climate_education_data.csv")
rais <- fread("./output/rais/data_rais_total_merge.csv")
patent <- fread("./output/patent data/patent.csv")
quadro_societario <- fread("./output/quadro_societario/quadro_societario.csv")
mun <- fread("./output/dummy_municipalities_year_state.csv")
sol_eol <- fread("./output/annel/solar_eolico_projetos.csv", encoding = "Latin-1")
controles <- fread("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/controles/controle.csv")

controles <-controles[,.(code_muni,population)]


#Limpeza

weather <- weather[,1:26]
setnames(rais,c("id_municipio","ano"),c("municipality_code","year"))
setnames(patent,c("cd_ibge_cidade","DT_DEPOSITO"),c("municipality_code","year"))
setnames(mun,c("code_muni"),c("municipality_code"))
setnames(sol_eol,c("code_muni","ano"),c("municipality_code","year"))



weather[, .N, by=.(municipality_code, year)][N>1]
weather <- unique(weather, by = c("municipality_code", "year"))




# Merge

weather_rais <- merge(rais,weather,by = c("municipality_code","year"))
weather_patent <- merge(patent,weather,by = c("municipality_code","year"))
weather_quadro_societario <- merge(quadro_societario,weather,by = c("municipality_code","year"))
weather_sol_eol <- merge(sol_eol,weather,by = c("municipality_code","year"),all.x = TRUE)



weather_rais <- merge(weather_rais,mun,by = c("municipality_code","year"))
weather_patent <- merge(weather_patent,mun,by = c("municipality_code","year"))
weather_quadro_societario <- merge(weather_quadro_societario,mun,by = c("municipality_code","year"))
weather_sol_eol <- merge(weather_sol_eol,mun,by = c("municipality_code","year","code_state"))

#

weather_patent <- weather_patent[!duplicated(weather_patent, by = c("municipality_code", "year"))]
weather_quadro_societario <- weather_quadro_societario[!duplicated(weather_quadro_societario, by = c("municipality_code", "year"))]
weather_rais <- weather_rais[!duplicated(weather_rais, by = c("municipality_code", "year"))]
weather_sol_eol <- weather_sol_eol[!duplicated(weather_sol_eol, by = c("municipality_code", "year","DscOrigemCombustivel"))]


######

weather_rais <- weather_rais[,cont_shock_precipitation_long_00_10:= NULL]
weather_rais <- weather_rais[,cont_shock_precipitation_long_00_20:= NULL]
weather_rais <- weather_rais[,cont_shock_temperature_long_00_10:= NULL]
weather_rais <- weather_rais[,cont_shock_temperature_long_00_20:= NULL]


weather_rais <- merge(weather_rais, controles, 
              by.x = "municipality_code", by.y = "code_muni", all.x = TRUE)


weather_rais[, TotalVinculosAtivos := TotalVinculosAtivos / population * 1000]
weather_rais[, TotalVinculosVerdes := TotalVinculosVerdes / population * 1000]

weather_rais[, TotalVinculosAtivos_b := TotalVinculosAtivos_b / population * 1000]
weather_rais[, TotalVinculosVerdes_b := TotalVinculosVerdes_b / population * 1000]



weather_rais <- weather_rais[!is.na(TotalVinculosVerdes),]
weather_rais <- weather_rais[!is.na(cont_shock_temperature),]


colnames(weather_rais) <- c(
  "mun_code",         # municipality_code
  "year",             # year
  "total_vinc",       # TotalVinculosAtivos
  "total_vinc_verde", # TotalVinculosVerdes
  "prop_verde",       # ProporcaoVerde
  "total_vinc_b",       # TotalVinculosAtivos
  "total_vinc_verde_b", # TotalVinculosVerdes
  "prop_verde_b",       # ProporcaoVerde
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
  "ma_precip",        # media_anual_precipitation
  "dum_precip",       # dummy_precipitation
  "cont_shock_precip", # cont_shock_precipitation
  "ma_shock_precip_10yr", # moving_average_shock_precipitation_ten_year
  "lag5_dum_precip",  # lagged_5_dummy_precipitation
  "lag10_dum_precip", # lagged_10_dummy_precipitation
  "lag15_dum_precip", # lagged_15_dummy_precipitation
  "lag5_cont_shock_precip",  # lagged_5_cont_shock_precipitation
  "lag10_cont_shock_precip", # lagged_10_cont_shock_precipitation
  "lag15_cont_shock_precip", # lagged_15_cont_shock_precipitation
  "code_state",
  "cod_rgi",          # cod_rgi
  "lat",              # latitude
  "lon",
  "population"
  )


weather_patent <- weather_patent[,cont_shock_precipitation_long_00_10:= NULL]
weather_patent <- weather_patent[,cont_shock_precipitation_long_00_20:= NULL]
weather_patent <- weather_patent[,cont_shock_temperature_long_00_10:= NULL]
weather_patent <- weather_patent[,cont_shock_temperature_long_00_20:= NULL]



weather_patent <- merge(weather_patent, controles, 
                by.x = "municipality_code", by.y = "code_muni", all.x = TRUE)


weather_patent[, Quantidade_Patentes_5_anos  := Quantidade_Patentes_5_anos / population*1000]
weather_patent[, Quantidade_Patentes_verdes_5_anos := Quantidade_Patentes_verdes_5_anos / population * 1000]

weather_patent <- weather_patent[
  , if (sum(Quantidade_Patentes) > 0) .SD, 
  by = municipality_code
]

weather_patent <- weather_patent[!is.na(Quantidade_Patentes_5_anos),]
weather_patent <- weather_patent[!is.na(cont_shock_temperature),]
weather_patent <- weather_patent[year > 2000]

weather_patent <- weather_patent[Quantidade_Patentes_5_anos == 0 & Quantidade_Patentes_verdes_5_anos== 0,
                                 ProporcaoVerde_5_anos:= 0]




colnames(weather_patent) <- c(
  "mun_code",         # municipality_code
  "year",             # year
  "qtd_pat",          # Quantidade_Patentes
  "qtd_pat_verde",    # Quantidade_Patentes_verdes
  "prop_verde",       # ProporcaoVerde
  "qtd_pat_5yr",      # Quantidade_Patentes_5_anos
  "qtd_pat_verde_5yr",# Quantidade_Patentes_verdes_5_anos
  "prop_verde_5yr",   # ProporcaoVerde_5_anos
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
  "ma_precip",        # media_anual_precipitation
  "dum_precip",       # dummy_precipitation
  "cont_shock_precip",# cont_shock_precipitation
  "ma_shock_precip_10yr", # moving_average_shock_precipitation_ten_year
  "lag5_dum_precip",  # lagged_5_dummy_precipitation
  "lag10_dum_precip", # lagged_10_dummy_precipitation
  "lag15_dum_precip", # lagged_15_dummy_precipitation
  "lag5_cont_shock_precip",  # lagged_5_cont_shock_precipitation
  "lag10_cont_shock_precip", # lagged_10_cont_shock_precipitation
  "lag15_cont_shock_precip", # lagged_15_cont_shock_precipitation
  "code_state",
  "cod_rgi",          # cod_rgi
  "lat",              # latitude
  "lon",              # longitude
  "population"
)


weather_quadro_societario <- weather_quadro_societario[,cont_shock_precipitation_long_00_10:= NULL]
weather_quadro_societario <- weather_quadro_societario[,cont_shock_precipitation_long_00_20:= NULL]
weather_quadro_societario <- weather_quadro_societario[,cont_shock_temperature_long_00_10:= NULL]
weather_quadro_societario <- weather_quadro_societario[,cont_shock_temperature_long_00_20:= NULL]


weather_quadro_societario <- merge(weather_quadro_societario, controles, 
                           by.x = "municipality_code", by.y = "code_muni", all.x = TRUE)


weather_quadro_societario[, firmas_ativas := firmas_ativas / population*1000]
weather_quadro_societario[, firmas_ativas_verde := firmas_ativas_verde / population * 1000]

weather_quadro_societario[, firmas_ativas_b := firmas_ativas_b / population*1000]
weather_quadro_societario[, firmas_ativas_verde_b := firmas_ativas_verde_b / population * 1000]



weather_quadro_societario <- weather_quadro_societario[!is.na(firmas_ativas),]
weather_quadro_societario <- weather_quadro_societario[!is.na(cont_shock_temperature),]



colnames(weather_quadro_societario) <- c(
  "mun_code",         # municipality_code
  "year",             # year
  "firmas_ativas",    # firmas_ativas
  "firmas_verde",     # firmas_ativas_verde
  "prop_verde",       # ProporcaoVerde
  "firmas_ativas_b",    # firmas_ativas
  "firmas_verde_b",     # firmas_ativas_verde
  "prop_verde_b",       # ProporcaoVerde
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
  "ma_precip",        # media_anual_precipitation
  "dum_precip",       # dummy_precipitation
  "cont_shock_precip",# cont_shock_precipitation
  "ma_shock_precip_10yr", # moving_average_shock_precipitation_ten_year
  "lag5_dum_precip",  # lagged_5_dummy_precipitation
  "lag10_dum_precip", # lagged_10_dummy_precipitation
  "lag15_dum_precip", # lagged_15_dummy_precipitation
  "lag5_cont_shock_precip",  # lagged_5_cont_shock_precipitation
  "lag10_cont_shock_precip", # lagged_10_cont_shock_precipitation
  "lag15_cont_shock_precip", # lagged_15_cont_shock_precipitation
  "code_state",
  "cod_rgi",          # cod_rgi
  "lat",              # latitude
  "lon",              # longitude
  "population"
)


weather_sol_eol <- weather_sol_eol[,cont_shock_precipitation_long_00_10:= NULL]
weather_sol_eol <- weather_sol_eol[,cont_shock_precipitation_long_00_20:= NULL]
weather_sol_eol <- weather_sol_eol[,cont_shock_temperature_long_00_10:= NULL]
weather_sol_eol <- weather_sol_eol[,cont_shock_temperature_long_00_20:= NULL]


colnames(weather_sol_eol) <- c(
  "mun_code",             # municipality_code
  "year",                 # year
  "code_state",           # code_state
  "dsc_combustivel",      # DscOrigemCombustivel
  "mun_name",             # Municipio
  "state_abbr",           # abbrev_state
  "solar_mean",           # solar_mean
  "wind_speed_mean",      # wind_speed_mean
  "qtd_proj",             # Quantidade_Projetos
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
  "ma_precip",            # media_anual_precipitation
  "dum_precip",           # dummy_precipitation
  "cont_shock_precip",    # cont_shock_precipitation
  "ma_shock_precip_10yr", # moving_average_shock_precipitation_ten_year
  "lag5_dum_precip",      # lagged_5_dummy_precipitation
  "lag10_dum_precip",     # lagged_10_dummy_precipitation
  "lag15_dum_precip",     # lagged_15_dummy_precipitation
  "lag5_cont_shock_precip",# lagged_5_cont_shock_precipitation
  "lag10_cont_shock_precip",# lagged_10_cont_shock_precipitation
  "lag15_cont_shock_precip",# lagged_15_cont_shock_precipitation
  "cod_rgi",              # cod_rgi
  "lat",                  # latitude
  "lon"                   # longitude
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

# Contando a quantidade de NAs por coluna
na_por_coluna <- colSums(is.na(weather_sol_eol))
print(na_por_coluna)

#
weather_patent <- weather_patent[!is.na(cont_shock_precip),]
weather_rais <- weather_rais[!is.na(cont_shock_precip),]
weather_rais <- weather_rais[is.na(prop_verde),prop_verde:=0]
weather_quadro_societario <- weather_quadro_societario[!is.na(cont_shock_precip),]
weather_sol_eol <- weather_sol_eol[!is.na(cont_shock_precip),]

#

#controles <- fread("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/controles/controle.csv")

#weather_patent <- merge(weather_patent,controles,by.x = "mun_code",by.y = "code_muni",all.x = TRUE)
#weather_rais <- merge(weather_rais,controles,by.x = "mun_code",by.y = "code_muni",all.x = TRUE)
#weather_quadro_societario <- merge(weather_quadro_societario,controles,by.x = "mun_code",by.y = "code_muni",all.x = TRUE)
#weather_sol_eol <- merge(weather_sol_eol,controles,by.x = "mun_code",by.y = "code_muni",all.x = TRUE)



# Salvar a base de dados final
fwrite(weather_patent, "./output/final_base/weather_patent.csv")
fwrite(weather_quadro_societario, "./output/final_base/weather_quadro_societario.csv")
fwrite(weather_rais, "./output/final_base/weather_rais.csv")
fwrite(weather_sol_eol, "./output/final_base/weather_sol_eol.csv")

# Salvar as bases em formato .dta
write_dta(weather_patent, "./output/final_base/weather_patent.dta")
write_dta(weather_quadro_societario, "./output/final_base/weather_quadro_societario.dta")
write_dta(weather_rais, "./output/final_base/weather_rais.dta")
write_dta(weather_sol_eol, "./output/final_base/weather_sol_eol.dta")
