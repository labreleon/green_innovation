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
  "Moderada contribuição [Social + Ambiental]",
  "Alta contribuição [Ambiental]",
  "Moderada contribuição [Ambiental]"
), `Economia verde` := 1]
green[`Economia verde` != 1, `Economia verde` := 0]
green[is.na(`Economia verde`), `Economia verde` := 0]
green[, `Economia verde` := as.double(`Economia verde`)]




#
legenda[, cnae_1 := gsub("[^0-9]", "", cnae_1)]
legenda <- legenda[,.(cnae_1,cnae_2_subclasse)]


#
legenda <- merge(legenda,green, by = "cnae_2_subclasse",all.x = TRUE)

legenda <- legenda[,.(cnae_1,`Economia verde`)]
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

query_pop <- "SELECT * FROM `basedosdados.br_ibge_populacao.municipio`"
pop <- read_sql(query_pop)
setDT(pop)

pop <- pop[,.(id_municipio,sigla_uf)]
pop <- unique(pop)

resultado <- merge(resultado,pop,by  = "id_municipio",all.x = TRUE)

setDT(ativos_por_estado)


# Agregar os dados por estado para TotalVinculosAtivos
ativos_por_estado <- resultado %>%
  group_by(sigla_uf) %>%
  summarise(total_ativos = sum(TotalVinculosAtivos, na.rm = TRUE)) %>%
  ungroup()

# Gerar o gráfico de ranking para TotalVinculosAtivos
ggplot(ativos_por_estado[!is.na(sigla_uf),], aes(x = reorder(sigla_uf, total_ativos), y = total_ativos)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +  # Inverte os eixos para facilitar a visualização dos estados
  labs(title = "Ranking dos Estados por Total de Vinculos Ativos",
       x = "Estado",
       y = "Total de Vinculos Ativos") +
  theme_minimal()




setDT(verdes_por_estado)

# Agregar os dados por estado para TotalVinculosVerdes
verdes_por_estado <- resultado %>%
  group_by(sigla_uf) %>%
  summarise(total_verdes = sum(TotalVinculosVerdes, na.rm = TRUE)) %>%
  ungroup()

# Gerar o gráfico de ranking para TotalVinculosVerdes
ggplot(verdes_por_estado[!is.na(sigla_uf),], aes(x = reorder(sigla_uf, total_verdes), y = total_verdes)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +  # Inverte os eixos para facilitar a visualização dos estados
  labs(title = "Ranking dos Estados por Total de Vinculos Verdes",
       x = "Estado",
       y = "Total de Vinculos Verdes") +
  theme_minimal()


setDT(proporcao_verde_estado)


# 1. Agregar os dados por estado e calcular a nova definição de ProporcaoVerde
# Nova ProporcaoVerde = soma de TotalVinculosVerdes / soma de TotalVinculosAtivos por estado
proporcao_verde_estado <- resultado %>%
  group_by(sigla_uf) %>%
  summarise(total_ativos = sum(TotalVinculosAtivos, na.rm = TRUE),
            total_verdes = sum(TotalVinculosVerdes, na.rm = TRUE)) %>%
  mutate(nova_proporcao_verde = total_verdes / total_ativos) %>%
  ungroup()

# 2. Gerar o gráfico de ranking com a nova ProporcaoVerde
ggplot(proporcao_verde_estado[!is.na(sigla_uf),], aes(x = reorder(sigla_uf, nova_proporcao_verde), y = nova_proporcao_verde)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +  # Inverte os eixos para facilitar a visualização dos estados
  labs(title = "Ranking dos Estados por  Proporcao Verde de Vinculos",
       x = "Estado",
       y = "Proporcao Verde") +
  theme_minimal()