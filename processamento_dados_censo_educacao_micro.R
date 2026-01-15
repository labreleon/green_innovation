# Carregar as bibliotecas necessárias
library(basedosdados)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)

setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Definir o projeto no Google Cloud
set_billing_id("projeto-333523")

# Consulta ao Censo do Ensino Superior
query_superior <- "
SELECT 
    ano,
    id_municipio,
    SUM(quantidade_ingressantes) AS total_ingressantes,
    SUM(quantidade_matriculas) AS total_matriculas,
    SUM(quantidade_concluintes) AS total_concluintes
FROM 
    `basedosdados.br_inep_censo_educacao_superior.curso`
GROUP BY 
    ano,
    id_municipio;"
censo_ensino_superior <- read_sql(query_superior)
setDT(censo_ensino_superior)

# Consulta ao Censo da Educação Básica
query_escolar <- "
SELECT 
    ano,
    id_municipio,
    SUM(quantidade_matricula_fundamental_anos_iniciais) AS total_matricula_fundamental_anos_iniciais,
    SUM(quantidade_matricula_fundamental_anos_finais) AS total_matriculas_fundamental_anos_finais,
    SUM(quantidade_matricula_medio) AS total_quantidade_matricula_medio
FROM 
    `basedosdados.br_inep_censo_escolar.escola`
GROUP BY 
    ano,
    id_municipio;"
censo_escolar <- read_sql(query_escolar)
setDT(censo_escolar)

# Consulta à População
query_pop <- "SELECT * FROM `basedosdados.br_ibge_populacao.municipio`"
pop <- read_sql(query_pop)
setDT(pop)

# Ler os dados das regiões geográficas
micro <- read_xlsx("./input/ibge/regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")
setDT(micro)

pop <- merge(pop,micro, by.x = c("id_municipio"), by.y = c("CD_GEOCODI"),all.x = TRUE)


# Processar dados do Censo da Educação Básica
censo_escolar <- merge(censo_escolar, pop, by = c("id_municipio", "ano"))


# Agregar os dados por Ano e RGI
censo_aggregado <- censo_escolar[, .(
  total_matricula_anos_iniciais = sum(total_matricula_fundamental_anos_iniciais, na.rm = TRUE),
  total_matricula_anos_finais = sum(total_matriculas_fundamental_anos_finais, na.rm = TRUE),
  total_matricula_medio = sum(total_quantidade_matricula_medio, na.rm = TRUE),
  populacao = sum(populacao, na.rm = TRUE)
), by = .(ano, cod_rgi, nome_rgi)]


censo_escolar <- censo_escolar[, 
                               `:=`(
                                 total_matricula_fundamental_anos_iniciais_per_capita = (total_matricula_fundamental_anos_iniciais / populacao) * 1000,
                                 total_matriculas_fundamental_anos_finais_per_capita = (total_matriculas_fundamental_anos_finais / populacao) * 1000,
                                 total_quantidade_matricula_medio_per_capita = (total_quantidade_matricula_medio / populacao) * 1000
                               )]
censo_escolar <- censo_escolar[, .(
  cod_rgi, ano, 
  total_matricula_fundamental_anos_iniciais_per_capita,
  total_matriculas_fundamental_anos_finais_per_capita,
  total_quantidade_matricula_medio_per_capita
)]

# Processar dados do Censo do Ensino Superior
censo_ensino_superior <- merge(censo_ensino_superior, pop, by = c("id_municipio", "ano"))
setDT(censo_ensino_superior)


censo_ensino_superior <- censo_ensino_superior[, .(
  total_ingressantes = sum(total_ingressantes, na.rm = TRUE),
  total_matriculas = sum(total_matriculas, na.rm = TRUE),
  total_concluintes = sum(total_concluintes, na.rm = TRUE),
  populacao = sum(populacao, na.rm = TRUE)
), by = .(ano, cod_rgi, nome_rgi)]


censo_ensino_superior <- censo_ensino_superior[, 
                                               `:=`(
                                                 total_ingressantes_per_capita = (total_ingressantes / populacao) * 1000,
                                                 total_matriculas_per_capita = (total_matriculas / populacao) * 1000,
                                                 total_concluintes_per_capita = (total_concluintes / populacao) * 1000
                                               )]
censo_ensino_superior <- censo_ensino_superior[, .(
  cod_rgi, ano, 
  total_ingressantes_per_capita,
  total_matriculas_per_capita,
  total_concluintes_per_capita
)]

# Renomear colunas para padronização
setnames(censo_escolar, c("ano"), c( "year"))
setnames(censo_ensino_superior, c("ano"), c("year"))

# Salvar dados processados em arquivos CSV
fwrite(censo_escolar, "./output/Censo_da_educacao_basica_micro.csv", row.names = FALSE)
fwrite(censo_ensino_superior, "./output/Censo_do_ensino_superior_micro.csv", row.names = FALSE)
