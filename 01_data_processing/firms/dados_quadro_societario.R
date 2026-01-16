# Carregar as bibliotecas necessárias
library(basedosdados)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)


setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


# Definir o projeto no Google Cloud
set_billing_id("projeto-333523")

# Consulta para Estabelecimentos
query_estabelecimentos <- "
SELECT
    id_municipio,
    cnae_fiscal_principal,
    EXTRACT(YEAR FROM data_inicio_atividade) AS ano_inicio_atividade,
    COUNT(*) AS quantidade_observacoes
FROM
    `basedosdados.br_me_cnpj.estabelecimentos`
GROUP BY
    id_municipio,
    cnae_fiscal_principal,
    ano_inicio_atividade
ORDER BY
    id_municipio,
    cnae_fiscal_principal,
    ano_inicio_atividade
"
estabelecimentos <- read_sql(query_estabelecimentos)
setDT(estabelecimentos)
setnames(estabelecimentos, c("id_municipio", "ano_inicio_atividade", "cnae_fiscal_principal"), c("municipality_code", "year", "cnae_2_subclasse"))


# Salvar os resultados em CSV
fwrite(estabelecimentos, "./output/quadro_societario/quadro_societario_raw.csv", row.names = FALSE)
