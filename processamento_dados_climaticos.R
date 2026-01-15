library(datazoom.amazonia)
library(data.table)
library(lubridate)
library(readxl)
library(geobr)
library(furrr)
library(sf)

setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/input/weather data/datazoom")

# Configuring parallel processing with 4 workers to improve efficiency in data handling
plan(multisession, workers=4)

######### Datazoom #########
cols_interessantes <- c("municipality_code", "date", "max_temperature")

max_temp <- load_climate(dataset = "max_temperature", time_period = 2000:2020)
max_temp <- max_temp[, cols_interessantes, with = FALSE]
max_temp <- st_set_geometry(max_temp, NULL)
fwrite(max_temp,"./max_temp_00_20.csv", row.names = FALSE)
rm(max_temp)
gc()



max_temp <- load_climate(dataset = "max_temperature", time_period = 1980:2000)
max_temp <- max_temp[, cols_interessantes, with = FALSE]
max_temp <- st_set_geometry(max_temp, NULL)
fwrite(max_temp,"./max_temp_80_00.csv", row.names = FALSE)
rm(max_temp)
gc()



max_temp <- load_climate(dataset = "max_temperature", time_period = 1960:1980)
max_temp <- max_temp[, cols_interessantes, with = FALSE]
max_temp <- st_set_geometry(max_temp, NULL)
fwrite(max_temp,"./max_temp_60_80.csv", row.names = FALSE)
rm(max_temp)
gc()


max_temp <- load_climate(dataset = "max_temperature", time_period = 1958:1960)
max_temp <- max_temp[, cols_interessantes, with = FALSE]
max_temp <- st_set_geometry(max_temp, NULL)
fwrite(max_temp,"./max_temp_58_60.csv", row.names = FALSE)
rm(max_temp)
gc()




##################################
cols_interessantes <- c("municipality_code", "date", "precipitation")



precipitation <- load_climate(dataset = "precipitation", time_period = 2000:2020)
precipitation<- precipitation[, cols_interessantes, with = FALSE]
precipitation <- st_set_geometry(precipitation, NULL)
fwrite(precipitation,"./precip_00_20.csv", row.names = FALSE)
rm(precipitation)
gc()



precipitation <- load_climate(dataset = "precipitation", time_period = 1980:2000)
precipitation<- precipitation[, cols_interessantes, with = FALSE]
precipitation <- st_set_geometry(precipitation, NULL)
fwrite(precipitation,"./precip_80_00.csv", row.names = FALSE)
rm(precipitation)
gc()



precipitation <- load_climate(dataset = "precipitation", time_period = 1960:1980)
precipitation<- precipitation[, cols_interessantes, with = FALSE]
precipitation <- st_set_geometry(precipitation, NULL)
fwrite(precipitation,"./precip_60_80.csv", row.names = FALSE)
rm(precipitation)
gc()



precipitation <- load_climate(dataset = "precipitation", time_period = 1958:1960)
precipitation<- precipitation[, cols_interessantes, with = FALSE]
precipitation <- st_set_geometry(precipitation, NULL)
fwrite(precipitation,"./precip_58_60.csv", row.names = FALSE)
rm(precipitation)
gc()
