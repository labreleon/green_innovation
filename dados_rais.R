# Carregue as bibliotecas necessárias
library("basedosdados")
library(data.table)

#
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")


# Defina o projeto de faturamento no Google Cloud
set_billing_id("projeto-333523")

# Função para executar consultas SQL e retornar os dados
execute_query <- function(query) {
  tryCatch({
    data <- read_sql(query)
    return(data)
  }, error = function(e) {
    cat("Erro ao executar a consulta:", e$message, "\n")
    return(NULL)
  })
}


# Puxa os dados somados por município, subclasse CNAE e ano
query_dados_somados <- "
SELECT 
  id_municipio, 
  cnae_1,
  ano,
  SUM(quantidade_vinculos_ativos) AS total_vinculos_ativos
FROM 
  `basedosdados.br_me_rais.microdados_estabelecimentos`
WHERE
  ano > 1994
GROUP BY 
  id_municipio, 
  cnae_1,
  ano
"

data_somados <- execute_query(query_dados_somados)
if (!is.null(data_somados)) {
  fwrite(data_somados, "./input/rais/data_rais_total.csv", row.names = FALSE)
}
