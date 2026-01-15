library(data.table)
library(haven)
library(readxl)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


####Abrir base de dados


dt <- read_xlsx("./input/S2id/BD_Atlas_1991_2023_v1.0_2024.04.29.xlsx",sheet = 3,range ="A1:BQ67231")
setDT(dt)

dt <- dt[Status == "Reconhecido" | Status == "Registro" ,]

dt <- dt[,.(Cod_IBGE_Mun,Nome_Municipio,Sigla_UF,Data_Evento,descricao_tipologia)]
dt <- dt[!(descricao_tipologia == "Doenças infecciosas" | descricao_tipologia == "Rompimento/Colapso de barragens"
         |descricao_tipologia == "Outros"|descricao_tipologia == "Movimento de Massa")]
dt$ano <- substr(dt$Data_Evento, 1, 4)
dt <- dt[,T:=1]
dt <- dt[,.(Cod_IBGE_Mun,ano, T)]

setnames(dt,c("Cod_IBGE_Mun","ano"),c("mun_code","year"))
dt$year <- as.double(dt$year)

######

# Ler os dados das regiões geográficas
micro <- read_xlsx("./input/ibge/regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")
setDT(micro)

#######################

micro$CD_GEOCODI <- as.double(micro$CD_GEOCODI)

dt <- merge(dt,micro,by.x = c("mun_code"),by.y = c("CD_GEOCODI"),all.x = TRUE)

# Agregar os dados por Ano e RGI
dt <- dt[, .(
  T_total = sum(T, na.rm = TRUE)  # Somando a variável "T" por ano e RGI
), by = .(year, cod_rgi)]

setorder(dt,cod_rgi,year)

dt <- dt[T_total > 0,T:= 1]

dt <- dt[,.(cod_rgi,year,T)]

dt$cod_rgi <- as.double(dt$cod_rgi)
