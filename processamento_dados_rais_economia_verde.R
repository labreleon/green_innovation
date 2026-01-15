# Carregar as bibliotecas necessárias
library(data.table)
library(dplyr)

#
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


# Definir caminhos dos arquivos
rais_file <- "./input/rais/data_rais_total.csv"
legenda_file <- "./input/legenda_rais.csv"
green_file <- "./input/Febrabran Green Taxonomy/FEBRABAN - Versão Final da Taxonomia_20210217.csv"
output_file <- "./output/rais/data_rais_total_merge.csv"

# Carregar os dados
rais <- fread(rais_file, colClasses = "character")
legenda <- fread(legenda_file, colClasses = "character")
green <- fread(green_file, colClasses = "character")
setDT(rais)
setDT(legenda)
setDT(green)


#green

# Ajustar os valores de 'Economia verde'
green[`Economia verde` == "", `Economia verde` := 0]
green[`Economia verde` %in% c(
  "Alta contribuição [Social + Ambiental]",
  " Alta contribuição [Social+Ambiental]",  
  "Alta contribuição [Ambiental]",
  "Moderada contribuição [Social + Ambiental]",
  "Moderada contribuição [Ambiental]"
), `Economia verde` := 1]
#green <- green[cnae_2_subclasse < 0141501, `Economia verde` := 0]
#green <- green[cnae_2_subclasse > 0142300 &  cnae_2_subclasse < 0159801 , `Economia verde` := 0]
#green <- green[cnae_2_subclasse > 3500000 &  cnae_2_subclasse < 3700000 , `Economia verde` := 0]
green[`Economia verde` != 1, `Economia verde` := 0]
green[is.na(`Economia verde`), `Economia verde` := 0]
green[, `Economia verde` := as.double(`Economia verde`)]




#
legenda[, cnae_1 := gsub("[^0-9]", "", cnae_1)]
legenda <- legenda[,.(cnae_1,cnae_2_subclasse)]


#
legenda <- merge(legenda,green, by = "cnae_2_subclasse",all.x = TRUE)

legenda <- legenda[,.(cnae_1,cnae_2_subclasse,`Economia verde`)]
legenda <- unique(legenda)

legenda <- legenda[cnae_1 == "20109",`Economia verde`:= 0]
legenda <- legenda[cnae_1 == "50415",`Economia verde`:= 0]
legenda <- legenda[cnae_1 == "52159",`Economia verde`:= 0]
legenda <- legenda[cnae_1 == "55212",`Economia verde`:= 0]
legenda <- legenda[cnae_1 == "55220",`Economia verde`:= 0]
legenda <- legenda[cnae_1 == "99007",`Economia verde`:= 0]

legenda <- legenda[cnae_1 %in% legenda[, .N, by = cnae_1][N == 1]$cnae_1]



# Realizar a junção dos dados usando cnae_1
data <- merge(rais, legenda, by = "cnae_1")

data$total_vinculos_ativos <- as.integer(data$total_vinculos_ativos)

# Calcular os totais e a proporção de vínculos verdes
resultado <- data[, .(
  TotalVinculosAtivos = sum(as.numeric(total_vinculos_ativos), na.rm = TRUE) %>% as.double(),
  TotalVinculosVerdes = sum(ifelse(`Economia verde` == 1, as.numeric(total_vinculos_ativos), 0), na.rm = TRUE) %>% as.double()
), by = .(id_municipio, ano)][, ProporcaoVerde := TotalVinculosVerdes / TotalVinculosAtivos]


# Calcular os totais e a proporção de vínculos verdes
resultado_b <- data[(cnae_2_subclasse >=  0500000 & cnae_2_subclasse <= 3500000) |cnae_2_subclasse >= 3700000 , .(
  TotalVinculosAtivos_b = sum(as.numeric(total_vinculos_ativos), na.rm = TRUE) %>% as.double(),
  TotalVinculosVerdes_b = sum(ifelse(`Economia verde` == 1, as.numeric(total_vinculos_ativos), 0), na.rm = TRUE) %>% as.double()
), by = .(id_municipio, ano)][, ProporcaoVerde_b := TotalVinculosVerdes_b / TotalVinculosAtivos_b]


resultado <- merge(resultado,resultado_b,by = c("id_municipio", "ano"),all.x = TRUE)


resultado  <- resultado[ano > 1999 & ano < 2021,]
resultado  <- resultado [!(id_municipio == ""),]


# 1) grade completa de municipality_code × anos 2000–2020
grade <- CJ(
  id_municipio = unique(resultado$id_municipio),
  ano              = 2000:2020
)

resultado$ano <- as.double(resultado$ano)
# 2) faz um left-join de 'a' sobre a grade
a_full <- merge(
  grade, resultado ,
  by = c("id_municipio","ano"),
  all.x = TRUE
)

# 3) preenche todos os NAs (que correspondem às colunas faltantes) com zero
# método simples: (municipality_code e year não têm NA, então ficam intactos)
a_full[is.na(a_full)] <- 0

# 4) (opcional) reordena
setkey(a_full, id_municipio, ano)

# 5) se quiser sobrescrever o objeto original:
resultado  <- a_full


# Escrever o resultado para um arquivo CSV
fwrite(resultado, output_file, row.names = FALSE)






