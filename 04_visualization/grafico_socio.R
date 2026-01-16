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
  "Moderada contribuição [Social + Ambiental]",
  "Alta contribuição [Ambiental]",
  "Moderada contribuição [Ambiental]"
), `Economia verde` := 1]
data[`Economia verde` != 1, `Economia verde` := 0]
data[is.na(`Economia verde`), `Economia verde` := 0]
data[, `Economia verde` := as.double(`Economia verde`)]

# Calcular os resultados finais
resultado <- data[, .(
  firmas_ativas = sum(as.numeric(quantidade_observacoes), na.rm = TRUE),
  firmas_ativas_verde = sum(ifelse(`Economia verde` == 1, as.numeric(quantidade_observacoes), 0), na.rm = TRUE)
), by = .(municipality_code, year)][, ProporcaoVerde := as.double(firmas_ativas_verde) / as.double(firmas_ativas)]

# Ordenar resultado por município e ano
setorder(resultado, municipality_code, year)

# Filtrar para anos maiores que 1994
resultado <- resultado[year > 1994,]

#

resultado$year <- as.double(resultado$year)


# Criar uma tabela com todos os municípios e todos os anos entre 1995 e 2023
years <- 1995:2023
municipalities <- unique(resultado$municipality_code)
municipality_years <- CJ(municipality_code = municipalities, year = years)
municipality_years$year <- as.double(municipality_years$year)

# Mesclar a tabela de anos com o dataframe original
resultado <- merge(municipality_years, resultado, by = c("municipality_code", "year"), all.x = TRUE)


# Preencher os valores NA com 0
resultado[is.na(resultado)] <- 0

query_pop <- "SELECT * FROM `basedosdados.br_ibge_populacao.municipio`"
pop <- read_sql(query_pop)
setDT(pop)
pop <- pop[,.(id_municipio,sigla_uf)]
pop <- unique(pop)

setnames(pop,"id_municipio","municipality_code")

resultado <- merge(resultado,pop,by  = "municipality_code",all.x = TRUE)





#### 1. Gráfico: Ranking dos Estados por Nova Proporção Verde ####

# Agregar os dados por estado e calcular a nova proporção verde:
# Nova ProporcaoVerde = soma de firmas_ativas_verde / soma de firmas_ativas
prop_verde_estado <- resultado %>%
  group_by(sigla_uf) %>%
  summarise(total_ativas = sum(firmas_ativas, na.rm = TRUE),
            total_verdes = sum(firmas_ativas_verde, na.rm = TRUE)) %>%
  mutate(nova_proporcao_verde = total_verdes / total_ativas) %>%
  ungroup()

# Gerar o gráfico de ranking
ggplot(prop_verde_estado, aes(x = reorder(sigla_uf, nova_proporcao_verde), y = nova_proporcao_verde)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +  # Inverte os eixos para facilitar a visualização dos estados
  labs(title = "Ranking dos Estados Proporção Verde",
       x = "Estado",
       y = "Proporção Verde") +
  theme_minimal()

#### 2. Gráfico: Ranking dos Estados por Total de Firmas Ativas ####

# Agregar os dados por estado para Total de Firmas Ativas
ativas_por_estado <- resultado %>%
  group_by(sigla_uf) %>%
  summarise(total_ativas = sum(firmas_ativas, na.rm = TRUE)) %>%
  ungroup()

# Gerar o gráfico de ranking
ggplot(ativas_por_estado, aes(x = reorder(sigla_uf, total_ativas), y = total_ativas)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Ranking dos Estados por Total de Firmas Ativas",
       x = "Estado",
       y = "Total de Firmas Ativas") +
  theme_minimal()

#### 3. Gráfico: Ranking dos Estados por Total de Firmas Ativas Verdes ####

# Agregar os dados por estado para Total de Firmas Ativas Verdes
verdes_por_estado <- resultado %>%
  group_by(sigla_uf) %>%
  summarise(total_verdes = sum(firmas_ativas_verde, na.rm = TRUE)) %>%
  ungroup()

# Gerar o gráfico de ranking
ggplot(verdes_por_estado, aes(x = reorder(sigla_uf, total_verdes), y = total_verdes)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Ranking dos Estados por Total de Firmas Ativas Verdes",
       x = "Estado",
       y = "Total de Firmas Ativas Verdes") +
  theme_minimal()