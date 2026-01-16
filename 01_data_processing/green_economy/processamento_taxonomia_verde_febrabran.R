# Carregue as bibliotecas necessárias
library(readxl)
library(data.table)
library(stringr)

#
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


# Defina o caminho do arquivo de entrada e o caminho do arquivo de saída
input_file <- "./input/Febrabran Green Taxonomy/FEBRABAN - Versão Final da Taxonomia_20210217.xlsx"
output_file <- "./input/Febrabran Green Taxonomy/FEBRABAN - Versão Final da Taxonomia_20210217.csv"

# Leia a planilha específica do arquivo Excel
green <- read_xlsx(input_file, sheet = 2, range = "B4:S2401")

# Converta o data frame para data.table
setDT(green)

# Selecione e renomeie as colunas relevantes
green <- green[, .(Subclasse, Denominação, `Classificação Febraban...6`, `Classificação Febraban...10`, `Classificação FEBRABAN`)]
green <- green[!is.na(Subclasse), ]
green$Subclasse <- gsub("[-/]", "", green$Subclasse)
setnames(green, 
         c("Subclasse", "Classificação Febraban...6", "Classificação Febraban...10", "Classificação FEBRABAN"), 
         c("cnae_2_subclasse", "Exposição a Mudanças Climáticas", "Economia verde", "Exposição ao Risco Ambiental"))

# Selecione apenas as colunas necessárias para o output final
green <- green[, .(cnae_2_subclasse, `Economia verde`)]

green <- green[, cnae_2_subclasse := str_pad(cnae_2_subclasse, width = 7, side = "left", pad = "0")]


# Escreva o data.table para um arquivo CSV
fwrite(green, output_file)
