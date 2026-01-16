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

controles <-controles[,.(code_muni,population)]


##################
setnames(rais,c("id_municipio","ano"),c("municipality_code","year"))
rais <- rais[!is.na(municipality_code)]

rais <- merge(rais, controles, 
              by.x = "municipality_code", by.y = "code_muni", all.x = TRUE)


rais[, TotalVinculosAtivos := TotalVinculosAtivos / population * 1000]
rais[, TotalVinculosVerdes := TotalVinculosVerdes / population * 1000]


# filtra e garante anos 1995–2015
rais <- rais[year %between% c(1995, 2015) & !is.na(municipality_code)]

# calcula deltas
delta_rais_vinculos       <- calculate_deltas(rais, 2000, 2000, 2010, 2010, "TotalVinculosAtivos",     "rais_vinculos")
delta_rais_vinculos_verde <- calculate_deltas(rais, 1995, 2000, 2010, 2015, "TotalVinculosVerdes",    "rais_vinculos_verde")
delta_rais_prop_verde     <- calculate_deltas(rais, 1995, 2000, 2010, 2015, "ProporcaoVerde",         "rais_prop_verde")


# junta tudo num só data.table
rais_deltas <- Reduce(function(x,y) merge(x,y, by="municipality_code"), 
                      list(delta_rais_vinculos,
                           delta_rais_vinculos_verde,
                           delta_rais_prop_verde))


# --- 2) Quadro societário -----------------------------------------

quadro_societario <- quadro_societario[!is.na(municipality_code)]

quadro_societario <- merge(quadro_societario, controles, 
              by.x = "municipality_code", by.y = "code_muni", all.x = TRUE)


quadro_societario[, firmas_ativas := firmas_ativas / population*1000]
quadro_societario[, firmas_ativas_verde := firmas_ativas_verde / population * 1000]





quadro_societario <- quadro_societario[year %between% c(1995, 2015) & !is.na(municipality_code)]

delta_qsoc_firmas       <- calculate_deltas(quadro_societario, 1995, 2000, 2010, 2015, "firmas_ativas",           "qsoc_firmas")
delta_qsoc_firmas_verde <- calculate_deltas(quadro_societario, 1995, 2000, 2010, 2015, "firmas_ativas_verde",    "qsoc_firmas_verde")
delta_qsoc_prop_verde   <- calculate_deltas(quadro_societario, 1995, 2000, 2010, 2015, "ProporcaoVerde",         "qsoc_prop_verde")

qsoc_deltas <- Reduce(function(x,y) merge(x,y, by="municipality_code"),
                      list(delta_qsoc_firmas,
                           delta_qsoc_firmas_verde,
                           delta_qsoc_prop_verde))



# --- 3) Patent -----------------------------------------------------





patent <- patent[
  , if (sum(Quantidade_Patentes) > 0) .SD, 
  by = cd_ibge_cidade
]


setnames(patent,c("cd_ibge_cidade","DT_DEPOSITO"),c("municipality_code","year"))
patent <- patent[!is.na(municipality_code)]


patent <- merge(patent, controles, 
                           by.x = "municipality_code", by.y = "code_muni", all.x = TRUE)


patent[, Quantidade_Patentes  := Quantidade_Patentes / population*1000]
patent[, Quantidade_Patentes_verdes := Quantidade_Patentes_verdes / population * 1000]




patent <- patent[year %between% c(1995, 2015) & !is.na(municipality_code)]

# ajustando nomes se necessário; aqui assumo que você tem colunas
# Quantidade_Patentes_5_anos, Quantidade_Patentes_verdes_5_anos, ProporcaoVerde_5_anos

delta_patent_total       <- calculate_deltas(patent, 1997, 2000, 2010, 2013, "Quantidade_Patentes",        "patent_total")
delta_patent_verdes      <- calculate_deltas(patent, 1997, 2000, 2010, 2013, "Quantidade_Patentes_verdes", "patent_total_verde")
delta_patent_prop_verde  <- calculate_deltas(patent, 1997, 2000, 2010, 2013, "ProporcaoVerde",              "patent_prop_verde")

patent_deltas <- Reduce(function(x,y) merge(x,y, by="municipality_code"),
                        list(delta_patent_total,
                             delta_patent_verdes,
                             delta_patent_prop_verde))


# --- 4) Montagem do final_data ------------------------------------

final_data <- merge(weather,        rais_deltas,   by = "municipality_code", all.x = TRUE)
final_data <- merge(final_data,     qsoc_deltas,   by = "municipality_code", all.x = TRUE)
final_data <- merge(final_data,     patent_deltas, by = "municipality_code", all.x = TRUE)





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

#Typos
final_data <- final_data[-259,]
final_data <- final_data[-1188,]
final_data <- final_data[-1189,]
final_data <- final_data[-1415,]
final_data <- final_data[-1418,]



fwrite(final_data, "./output/final_combined_data.csv")

final_data <- final_data[,code_region := NULL]

final_data <- final_data[,population:= NULL]

names(final_data) <- c(
  "mun_code",                # municipality_code
  "cont_shock_temp_00_10",   # cont_shock_temperature_long_00_10
  "cont_shock_precip_00_10", # cont_shock_precipitation_long_00_10
  "cont_shock_temp_00_20",   # cont_shock_temperature_long_00_20
  "cont_shock_precip_00_20", # cont_shock_precipitation_long_00_20
  "delta_rais_vinculos",     # tilde_Y_rais_vinculos
  "delta_rais_verde",        # tilde_Y_rais_vinculos_verde
  "delta_rais_prop_verde",   # tilde_Y_rais_prop_verde
  "delta_qsoc_firmas",       # tilde_Y_qsoc_firmas
  "delta_qsoc_firmas_verde", # tilde_Y_qsoc_firmas_verde
  "delta_qsoc_prop_verde",   # tilde_Y_qsoc_prop_verde
  "delta_patent_total",      # tilde_Y_patent_total
  "delta_patent_tot_verde",  # tilde_Y_patent_total_verde
  "delta_patent_prop",       # tilde_Y_patent_prop_verde
  "state_code",              # code_state
  "lat",                     # latitude
  "lon",                     # longitude
  "urban",                   # urban
  "log_income",              # log_renda
  "alphabet_rate"            # alphabetization_rate
)



# 1) Identifique as colunas de outcome
outcomes <- grep("^delta_", names(final_data), value = TRUE)

# 2) Substitua NA por 0 em todas elas
final_data[, (outcomes) := lapply(.SD, function(x) replace(x, is.na(x), 0)), 
           .SDcols = outcomes]

#Rais


######################################

conleyreg(delta_rais_vinculos~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")



modelo <- lm(delta_rais_vinculos ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)



#######################################


conleyreg(delta_rais_verde~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


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

conleyreg(delta_rais_prop_verde~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_00_10_rais_prop ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)




#Patent


######################################

conleyreg(delta_patent_total~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


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


conleyreg(delta_patent_tot_verde~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


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

conleyreg(delta_patent_prop~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_qsoc_prop_00_10 ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)




#firma


######################################

conleyreg(delta_qsoc_firmas~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


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


conleyreg(delta_qsoc_firmas_verde~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


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

conleyreg(delta_qsoc_prop_verde~ cont_shock_temp_00_10+cont_shock_precip_00_10+urban+log_income+urban+alphabet_rate+state_code,250,data = final_data, lat = "lat", lon = "lon",unit = "mun_code")


modelo <- lm(delta_qsoc_prop_00_10 ~ 
               cont_shock_temp_00_10 +
               cont_shock_precip_00_10 +
               urban +
               log_income +
               state_code+alphabet_rate,
             data = final_data)

summary(modelo)
nobs(modelo)


