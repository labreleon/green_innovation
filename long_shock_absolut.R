library(data.table)
library(geobr)
library(conleyreg)
library(haven)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


# Função para calcular médias e deltas
calculate_deltas <- function(data, start_year1, end_year1, start_year2, end_year2, value_col, suffix) {
  # Agregação dos dados para o primeiro período (t-k)
  period1 <- data[year %between% c(start_year1, end_year1), 
                  .(mean_value = mean(get(value_col), na.rm = TRUE)), 
                  by = .(municipality_code)]
  
  # Agregação dos dados para o segundo período (t)
  period2 <- data[year %between% c(start_year2, end_year2), 
                  .(mean_value = mean(get(value_col), na.rm = TRUE)), 
                  by = .(municipality_code)]
  
  # Merge das agregações utilizando o código do município
  results <- merge(period1, period2, 
                   by = "municipality_code", 
                   suffixes = c(paste0("_", start_year1, "_", end_year1),
                                paste0("_", start_year2, "_", end_year2)))
  
  # Cálculo de \tilde{Y} conforme a fórmula: 2*(Y_t - Y_{t-k}) / (Y_t + Y_{t-k})
  results[, tilde_Y := 2 * (get(paste0("mean_value_", start_year2, "_", end_year2)) - 
                              get(paste0("mean_value_", start_year1, "_", end_year1))) /
            (get(paste0("mean_value_", start_year2, "_", end_year2)) + 
               get(paste0("mean_value_", start_year1, "_", end_year1)))]
  
  # Seleção apenas do código do município e da variável calculada
  results <- results[, .(municipality_code, tilde_Y)]
  
  # Renomeando a coluna do resultado com o sufixo fornecido
  setnames(results, "tilde_Y", paste0("tilde_Y_", suffix))
  
  return(results)
}

# Carregar os dados
weather <- fread("./Output/weather data/final_weather_data_1940_2020.csv")
rais <- fread("./Output/rais/data_rais_total_merge.csv")
patent <- fread("./Output/patent data/patent.csv")
quadro_societario <- fread("./Output/quadro_societario/quadro_societario.csv")
sol_eol <- fread("./output/annel/solar_eolico_projetos.csv", encoding = "Latin-1")



weather <- weather[year > 1999,]
weather <- weather[,.(municipality_code,year,cont_shock_temperature_long_00_10,cont_shock_temperature_long_00_20,cont_shock_precipitation_long_00_10,cont_shock_precipitation_long_00_20)]

weather_00_10 <- weather[,.(municipality_code,cont_shock_temperature_long_00_10,cont_shock_precipitation_long_00_10)]
weather_00_10 <- unique(weather_00_10)
weather_00_10 <- weather_00_10[!is.na(cont_shock_temperature_long_00_10)]

weather_00_20 <- weather[,.(municipality_code,cont_shock_temperature_long_00_20,cont_shock_precipitation_long_00_20)]
weather_00_20 <- unique(weather_00_20)
weather_00_20 <- weather_00_20[!is.na(cont_shock_temperature_long_00_20)]

weather <- merge(weather_00_10,weather_00_20,by = "municipality_code")

controles <- fread("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/controles/controle.csv")


##################
setnames(rais,c("id_municipio","ano"),c("municipality_code","year"))
#rais <- rais[,.(municipality_code,year,ProporcaoVerde)]
rais <- rais[year > 1999,]
rais <- rais[!is.na(municipality_code)]




rais_00 <- rais[ year == 2000, ] 
rais_00 <- rais_00[,year:= NULL]
setnames(rais_00,c("ProporcaoVerde"),c("ProporcaoVerde_00"))
setnames(rais_00,c("TotalVinculosAtivos"),c("TotalVinculosAtivos_00"))
setnames(rais_00,c("TotalVinculosVerdes"),c("TotalVinculosVerdes_00"))


rais_10 <- rais[ year == 2010, ] 
rais_10 <- rais_10[,year:= NULL]
setnames(rais_10,c("ProporcaoVerde"),c("ProporcaoVerde_10"))
setnames(rais_10,c("TotalVinculosAtivos"),c("TotalVinculosAtivos_10"))
setnames(rais_10,c("TotalVinculosVerdes"),c("TotalVinculosVerdes_10"))



rais_20 <- rais[ year == 2020, ] 
rais_20 <- rais_20[,year:= NULL]
setnames(rais_20,c("ProporcaoVerde"),c("ProporcaoVerde_20"))
setnames(rais_20,c("TotalVinculosAtivos"),c("TotalVinculosAtivos_20"))
setnames(rais_20,c("TotalVinculosVerdes"),c("TotalVinculosVerdes_20"))



rais <- merge(rais_00,rais_10,by = "municipality_code")
rais <- merge(rais,rais_20,by = "municipality_code")


rais <- rais[,delta_00_10_rais_vinculo:= TotalVinculosAtivos_10 -TotalVinculosAtivos_00 ]
rais <- rais[,delta_00_20_rais_vinculo:= TotalVinculosAtivos_20 -TotalVinculosAtivos_00 ]



rais <- rais[,delta_00_10_rais_vinculo_verde:= TotalVinculosVerdes_10 -TotalVinculosVerdes_00 ]
rais <- rais[,delta_00_20_rais_vinculo_verde:= TotalVinculosVerdes_20 -TotalVinculosVerdes_00 ]

rais <- rais[,delta_00_10_rais_prop_verde:= ProporcaoVerde_10 -ProporcaoVerde_00 ]
rais <- rais[,delta_00_20_rais_prop_verde:= ProporcaoVerde_20 -ProporcaoVerde_00 ]

rais <- rais[,delta_00_20_rais_prop_verde:= ProporcaoVerde_20 -ProporcaoVerde_00 ]


rais <- rais[,.(municipality_code,delta_00_10_rais_vinculo,delta_00_20_rais_vinculo,delta_00_10_rais_vinculo_verde,
                delta_00_20_rais_vinculo_verde,delta_00_10_rais_prop_verde,delta_00_20_rais_prop_verde)]


##################


#quadro_societario <- quadro_societario[,.(municipality_code,year,ProporcaoVerde)]
quadro_societario <- quadro_societario[year > 1999,]
quadro_societario <- quadro_societario[!is.na(municipality_code)]


quadro_societario_00 <- quadro_societario[ year == 2000, ] 
quadro_societario_00 <- quadro_societario_00[,year:= NULL]
setnames(quadro_societario_00,c("ProporcaoVerde"),c("ProporcaoVerde_00"))
setnames(quadro_societario_00,c("firmas_ativas_verde"),c("firmas_ativas_verde_00"))
setnames(quadro_societario_00,c("firmas_ativas"),c("firmas_ativas_00"))


quadro_societario_10 <- quadro_societario[ year == 2010, ] 
quadro_societario_10 <- quadro_societario_10[,year:= NULL]
setnames(quadro_societario_10,c("ProporcaoVerde"),c("ProporcaoVerde_10"))
setnames(quadro_societario_10,c("firmas_ativas_verde"),c("firmas_ativas_verde_10"))
setnames(quadro_societario_10,c("firmas_ativas"),c("firmas_ativas_10"))


quadro_societario_20 <- quadro_societario[ year == 2019, ] 
quadro_societario_20 <- quadro_societario_20[,year:= NULL]
setnames(quadro_societario_20,c("ProporcaoVerde"),c("ProporcaoVerde_20"))
setnames(quadro_societario_20,c("firmas_ativas_verde"),c("firmas_ativas_verde_20"))
setnames(quadro_societario_20,c("firmas_ativas"),c("firmas_ativas_20"))



quadro_societario <- merge(quadro_societario_00,quadro_societario_10,by = "municipality_code")
quadro_societario <- merge(quadro_societario,quadro_societario_20,by = "municipality_code")


quadro_societario <- quadro_societario[,delta_00_10_quadro_societario_firma:= firmas_ativas_10 -firmas_ativas_00 ]
quadro_societario <- quadro_societario[,delta_00_20_quadro_societario_firma:= firmas_ativas_20 -firmas_ativas_00 ]

quadro_societario <- quadro_societario[,delta_00_10_quadro_societario_firma_verde:= firmas_ativas_verde_10 -firmas_ativas_verde_00 ]
quadro_societario <- quadro_societario[,delta_00_20_quadro_societario_firma_verde:= firmas_ativas_verde_20 -firmas_ativas_verde_00 ]

quadro_societario <- quadro_societario[,delta_00_10_quadro_societario_prop_verde:= ProporcaoVerde_10 -ProporcaoVerde_00 ]
quadro_societario <- quadro_societario[,delta_00_20_quadro_societario_prop_verde:= ProporcaoVerde_20 -ProporcaoVerde_00 ]





quadro_societario <- quadro_societario[,.(municipality_code,delta_00_10_quadro_societario_firma,
                                          delta_00_20_quadro_societario_firma,delta_00_10_quadro_societario_firma_verde,
                                          delta_00_20_quadro_societario_firma_verde,delta_00_10_quadro_societario_prop_verde,
                                          delta_00_20_quadro_societario_prop_verde)]

##################


patent <- patent[
  , if (sum(Quantidade_Patentes) > 0) .SD, 
  by = cd_ibge_cidade
]




setnames(patent,c("cd_ibge_cidade","DT_DEPOSITO"),c("municipality_code","year"))
patent <- patent[!is.na(municipality_code)]



delta_patent_a <- calculate_deltas(patent, 1997, 2000, 2010, 2013, "ProporcaoVerde", "patent")
delta_patent_b <- calculate_deltas(patent, 1997, 2000, 2017, 2020, "ProporcaoVerde", "patent")

delta_patent_c <- calculate_deltas(patent, 1997, 2000, 2010, 2013, "Quantidade_Patentes", "patent")
delta_patent_d <- calculate_deltas(patent, 1997, 2000, 2017, 2020, "Quantidade_Patentes", "patent")


delta_patent_e <- calculate_deltas(patent, 1997, 2000, 2010, 2013, "Quantidade_Patentes_verdes", "patent")
delta_patent_f <- calculate_deltas(patent, 1997, 2000, 2017, 2020, "Quantidade_Patentes_verdes", "patent")



setnames(delta_patent_a,"tilde_Y_patent","delta_00_10_patent_prop_verde")
setnames(delta_patent_b,"tilde_Y_patent","delta_00_20_patent_prop_verde")

setnames(delta_patent_c,"tilde_Y_patent","delta_00_10_patent_total")
setnames(delta_patent_d,"tilde_Y_patent","delta_00_20_patent_total")


setnames(delta_patent_e,"tilde_Y_patent","delta_00_10_patent_total_verde")
setnames(delta_patent_f,"tilde_Y_patent","delta_00_20_patent_total_verde")


patent <- merge(delta_patent_a,delta_patent_b,by = "municipality_code",all.x = TRUE)
patent <- merge(patent,delta_patent_c,by = "municipality_code",all.x = TRUE)
patent <- merge(patent,delta_patent_d,by = "municipality_code",all.x = TRUE)
patent <- merge(patent,delta_patent_e,by = "municipality_code",all.x = TRUE)
patent <- merge(patent,delta_patent_f,by = "municipality_code",all.x = TRUE)

patent <- patent[is.na(delta_00_10_patent_prop_verde),delta_00_10_patent_prop_verde:= 0]
patent <- patent[is.na(delta_00_20_patent_prop_verde),delta_00_20_patent_prop_verde:= 0]

patent <- patent[is.na(delta_00_10_patent_total),delta_00_10_patent_total:= 0]
patent <- patent[is.na(delta_00_20_patent_total),delta_00_20_patent_total:= 0]

patent <- patent[is.na(delta_00_10_patent_total_verde),delta_00_10_patent_total_verde:= 0]
patent <- patent[is.na(delta_00_10_patent_total_verde),delta_00_10_patent_total_verde:= 0]

##################



final_data <- merge(weather,rais,by = "municipality_code",all.x = TRUE)
final_data <- merge(final_data,quadro_societario,by = "municipality_code",all.x = TRUE)
final_data <- merge(final_data,patent,by = "municipality_code",all.x = TRUE)


municipios <- fread("./output/dummy_municipalities_year_state_mun.csv")
setDT(municipios)

municipios <- municipios[,.(code_muni,code_state,latitude,longitude)]
municipios$code_muni<- as.character(municipios$code_muni)
final_data$municipality_code<- as.character(final_data$municipality_code)

final_data <- merge(final_data,municipios,by.x = "municipality_code",by.y = "code_muni")
final_data$code_state<- as.character(final_data$code_state)
final_data$code_region<- as.character(final_data$code_region)

final_data <- unique(final_data)

controles <- fread("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/controles/controle.csv")

controles$code_muni <- as.character(controles$code_muni)

final_data <- merge(final_data,controles,by.x = "municipality_code",by.y = "code_muni",all.x = TRUE)

final_data <- unique(final_data)


#final_data <- final_data[!(municipality_code == "1506351" & urban == "0.7119326"),]
#final_data <- final_data[!(municipality_code == "2410306" & urban == "0.3430180"),]
#final_data <- final_data[!(municipality_code == "2410306" & urban == "0.2877446"),]
#final_data <- final_data[!(municipality_code == "2513653" & is.na(urban)),]
#final_data <- final_data[!(municipality_code == "2513802" & urban == "0.1555305"),]



fwrite(final_data, "./output/final_combined_data.csv")

final_data <- final_data[,code_region := NULL]

final_data <- final_data[,population:= NULL]
# Renomear todas as colunas de forma abreviada
colnames(final_data) <- c(
  "mun_code",                   # municipality_code
  "cont_shock_temp_00_10",      # cont_shock_temperature_long_00_10
  "cont_shock_precip_00_10",    # cont_shock_precipitation_long_00_10
  "cont_shock_temp_00_20",      # cont_shock_temperature_long_00_20
  "cont_shock_precip_00_20",    # cont_shock_precipitation_long_00_20
  "delta_00_10_rais",           # delta_00_10_rais_vinculo
  "delta_00_20_rais",           # delta_00_20_rais_vinculo
  "delta_00_10_rais_verde",     # delta_00_10_rais_vinculo_verde
  "delta_00_20_rais_verde",     # delta_00_20_rais_vinculo_verde
  "delta_00_10_rais_prop",      # delta_00_10_rais_prop_verde
  "delta_00_20_rais_prop",      # delta_00_20_rais_prop_verde
  "delta_qsoc_00_10",           # delta_00_10_quadro_societario_firma
  "delta_qsoc_00_20",           # delta_00_20_quadro_societario_firma
  "delta_qsoc_verde_00_10",     # delta_00_10_quadro_societario_firma_verde
  "delta_qsoc_verde_00_20",     # delta_00_20_quadro_societario_firma_verde
  "delta_qsoc_prop_00_10",      # delta_00_10_quadro_societario_prop_verde
  "delta_qsoc_prop_00_20",      # delta_00_20_quadro_societario_prop_verde
  "delta_patent_prop_00_10",    # delta_00_10_patent_prop_verde
  "delta_patent_prop_00_20",    # delta_00_20_patent_prop_verde
  "delta_patent_00_10",         # delta_00_10_patent_total
  "delta_patent_00_20",         # delta_00_20_patent_total
  "delta_patent_tot_verde_00_10",# delta_00_10_patent_total_verde
  "delta_patent_tot_verde_00_20",# delta_00_20_patent_total_verde
  "state_code",                 # code_state
  "lat",                        # latitude
  "lon",                        # longitude
  "urban",                      # urban
  "log_income",                 # log_renda
  "alphabet_rate"               # alphabetization_rate
)

final_data <- unique(final_data)
#write_dta(final_data, "./output/final_base/final_combined_data.dta")


#Rais


######################################

conleyreg(delta_00_10_rais~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")



modelo <- lm(delta_00_10_rais ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)



#######################################


conleyreg(delta_00_10_rais_verde~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_00_10_rais_verde ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)


###################################

conleyreg(delta_00_10_rais_prop~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_00_10_rais_prop ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)




###################################

conleyreg(delta_00_20_rais~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_00_20_rais ~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)

###################################

conleyreg(delta_00_20_rais_verde~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_00_20_rais_verde ~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)

###################################

conleyreg(delta_00_20_rais_prop~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_00_20_rais_prop ~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)




#Patent


######################################

conleyreg(delta_patent_00_10~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_patent_00_10 ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)



#######################################


conleyreg(delta_patent_tot_verde_00_10~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_patent_tot_verde_00_10 ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)


###################################
conleyreg(delta_patent_prop_00_10~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_patent_prop_00_10 ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)



###################################

conleyreg(delta_patent_00_20~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_patent_00_20 ~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)

###################################

conleyreg(delta_patent_tot_verde_00_20~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_patent_tot_verde_00_20 ~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)

###################################

conleyreg(delta_patent_prop_00_20~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_patent_prop_00_20 ~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)






#firma


######################################

conleyreg(delta_qsoc_00_10~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_qsoc_00_10~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)



#######################################


conleyreg(delta_qsoc_verde_00_10~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_qsoc_verde_00_10 ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)


###################################

conleyreg(delta_qsoc_prop_00_10~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_qsoc_prop_00_10 ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)




###################################

conleyreg(delta_qsoc_00_20~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_qsoc_00_20~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)

###################################

conleyreg(delta_qsoc_verde_00_20~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_qsoc_verde_00_20 ~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)

###################################

conleyreg(delta_qsoc_prop_00_20~ cont_shock_temp_00_20+cont_shock_precip_00_20+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_qsoc_prop_00_20 ~ 
               cont_shock_temp_00_20 +
               cont_shock_precip_00_20 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)




