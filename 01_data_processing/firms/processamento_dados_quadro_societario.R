# Carregar as bibliotecas necessárias
library(data.table)
library(readxl)

setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


# Carregar os dados dos estabelecimentos
estabelecimentos <- fread("./output/quadro_societario/quadro_societario_raw.csv", colClasses = "character")

# Filtrar os dados para municípios válidos
estabelecimentos <- estabelecimentos[municipality_code > 0,]

# Carregar dados da Taxonomia Verde FEBRABAN
green <- read_xlsx("./input/febrabran green taxonomy/FEBRABAN - Versão Final da Taxonomia_20210217.xlsx", sheet = 2, range = "B4:S2401")
setDT(green)
green <- green[, .(Subclasse, Denominação, `Classificação Febraban...6`, `Classificação Febraban...10`, `Classificação FEBRABAN`)]
green <- green[!is.na(Subclasse),]
green$Subclasse <- gsub("[-/]", "", green$Subclasse)
setnames(green, c("Subclasse", "Classificação Febraban...6", "Classificação Febraban...10", "Classificação FEBRABAN"), c("cnae_2_subclasse", "Exposição a Mudanças Climáticas", "Economia verde", "Exposição ao Risco Ambiental"))
green <- green[, .(cnae_2_subclasse, `Economia verde`)]
green[, cnae_2_subclasse := sprintf("%07d", as.integer(green$cnae_2_subclasse))]

# Realizar merge dos dados
data <- merge(estabelecimentos, green, by = "cnae_2_subclasse", all.x = TRUE)
setDT(data)

# Ajustar os valores de 'Economia verde'
data[`Economia verde` == "", `Economia verde` := 0]
data[`Economia verde` %in% c(
  "Alta contribuição [Social + Ambiental]",
  " Alta contribuição [Social+Ambiental]",  
  "Alta contribuição [Ambiental]",
  "Moderada contribuição [Ambiental]",
  "Moderada contribuição [Social + Ambiental]"
), `Economia verde` := 1]
#data <- data[cnae_2_subclasse < 0141501, `Economia verde` := 0]
#data <- data[cnae_2_subclasse > 0142300 &  cnae_2_subclasse < 0159801 , `Economia verde` := 0]
#data <- data[cnae_2_subclasse > 3500000 &  cnae_2_subclasse < 3700000 , `Economia verde` := 0]

#data <- data[!(cnae_2_subclasse < 0141501), ]
#data <- data[!(cnae_2_subclasse > 0142300 &  cnae_2_subclasse < 0159801) , ]
#data <- data[!(cnae_2_subclasse > 3500000 &  cnae_2_subclasse < 3700000) , ]



data[`Economia verde` != 1, `Economia verde` := 0]
data[is.na(`Economia verde`), `Economia verde` := 0]
data[, `Economia verde` := as.double(`Economia verde`)]



# Calcular os resultados finais
resultado <- data[, .(
  firmas_ativas = sum(as.numeric(quantidade_observacoes), na.rm = TRUE),
  firmas_ativas_verde = sum(ifelse(`Economia verde` == 1, as.numeric(quantidade_observacoes), 0), na.rm = TRUE)
), by = .(municipality_code, year)][, ProporcaoVerde := as.double(firmas_ativas_verde) / as.double(firmas_ativas)]




# Calcular os resultados finais
resultado_b <- data[(cnae_2_subclasse >=  0500000 & cnae_2_subclasse <= 3500000) |cnae_2_subclasse >= 3700000  , .(
  firmas_ativas_b = sum(as.numeric(quantidade_observacoes), na.rm = TRUE),
  firmas_ativas_verde_b = sum(ifelse(`Economia verde` == 1, as.numeric(quantidade_observacoes), 0), na.rm = TRUE)
), by = .(municipality_code, year)][, ProporcaoVerde_b := as.double(firmas_ativas_verde_b) / as.double(firmas_ativas_b)]


resultado <- merge(resultado,resultado_b,by = c("municipality_code", "year"),all.x = TRUE)

# Ordenar resultado por município e ano
setorder(resultado, municipality_code, year)

# Filtrar para anos maiores que 1994
resultado <- resultado[year > 1999,]

#

resultado$year <- as.double(resultado$year)


# Criar uma tabela com todos os municípios e todos os anos entre 1995 e 2023
years <- 2000:2020
municipalities <- unique(resultado$municipality_code)
municipality_years <- CJ(municipality_code = municipalities, year = years)
municipality_years$year <- as.double(municipality_years$year)

# Mesclar a tabela de anos com o dataframe original
resultado <- merge(municipality_years, resultado, by = c("municipality_code", "year"), all.x = TRUE)

# Preencher os valores NA com 0
resultado[is.na(resultado)] <- 0

# Salvar os resultados em CSV
fwrite(resultado, "./output/quadro_societario/quadro_societario.csv", row.names = FALSE)
