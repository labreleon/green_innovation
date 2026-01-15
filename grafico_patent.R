# Carregar as bibliotecas necessárias
library(data.table)
library(readxl)

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Carregar as tabelas de 2022
ptn_ipc_campo_tec <- fread("./input/patent data/badepiv9_ptn_ipc_campo_tec.csv")
ptn_depositante_v2 <- fread("./input/patent data/badepiv9_ptn_depositante_v2.csv")
ptn_deposito <- fread("./input/patent data/badepiv9_ptn_deposito.csv")

# Realizar o merge das tabelas
merged_data <- merge(ptn_depositante_v2, ptn_deposito, by.x = "no_pedido", by.y = "NO_PEDIDO")
merged_data <- merge(merged_data, ptn_ipc_campo_tec, by.x = "no_pedido", by.y = "NO_PEDIDO")


merge_priority <- merged_data
# Filtrar por prioridade
#merge_priority <- merged_data[no_ordem == 1,]

# Converter DT_DEPOSITO para apenas ano
merge_priority[, DT_DEPOSITO := as.IDate(substr(DT_DEPOSITO, 1, 10), format = "%d/%m/%Y")]
merge_priority[, DT_DEPOSITO := year(DT_DEPOSITO)]

# Agregar os dados para somar as patentes por CD_IBGE_CIDADE, DT_DEPOSITO e CD_CLASSIF
resultado <- merge_priority[, .(Quantidade_Patentes = uniqueN(no_pedido)), by = .(cd_ibge_cidade, DT_DEPOSITO, CD_CLASSIF,nm_cidade_pfpj,cd_uf_pfpj)]
resultado <- resultado[!is.na(cd_ibge_cidade),]

# Carregar dados de IPC verde
green <- read_xlsx("./input/green ipc/green ipc.xlsx")
setDT(green)
green[, green := 1]
green <- unique(green[, .(CD_CLASSIF, green)])

# Padronizar CD_CLASSIF
resultado[, CD_CLASSIF := toupper(trimws(CD_CLASSIF))]
green[, CD_CLASSIF := toupper(trimws(CD_CLASSIF))]

# Realizar o merge com dados verdes
resultado <- merge(resultado, green, by = "CD_CLASSIF", all.x = TRUE)
resultado[is.na(green), green := 0]

# Agregar os dados para somar as patentes por CD_IBGE_CIDADE, DT_DEPOSITO e CD_CLASSIF
resultado <- resultado[, .(Quantidade_Patentes = sum(Quantidade_Patentes, na.rm = TRUE)), by = .(cd_ibge_cidade, DT_DEPOSITO, green,nm_cidade_pfpj,cd_uf_pfpj)]

# Calcular proporção de patentes verdes
resultado <- resultado[, .(
  Quantidade_Patentes = sum(as.numeric(Quantidade_Patentes), na.rm = TRUE),
  Quantidade_Patentes_verdes = sum(ifelse(green == 1, as.numeric(Quantidade_Patentes), 0), na.rm = TRUE)
), by = .(cd_ibge_cidade, DT_DEPOSITO,nm_cidade_pfpj,cd_uf_pfpj)][, ProporcaoVerde := Quantidade_Patentes_verdes / Quantidade_Patentes]

# Ordenar resultado por cidade e ano
setorder(resultado, cd_ibge_cidade, DT_DEPOSITO)

# Filtrar para anos maiores que 1996
resultado <- resultado[DT_DEPOSITO > 1996,]


# 1. Agregar os dados por estado: calcular a média de ProporcaoVerde para todos os anos
media_por_estado <- resultado %>%
  group_by(cd_uf_pfpj) %>%
  summarise(media_ProporcaoVerde = mean(ProporcaoVerde, na.rm = TRUE)) %>%
  ungroup()

# 2. (Opcional) Caso deseje selecionar os top estados, por exemplo, os 10 com maior média:
# top10_estados <- media_por_estado %>%
#   arrange(desc(media_ProporcaoVerde)) %>%
#   head(10)

# 3. Gerar o gráfico de ranking para todos os estados
ggplot(media_por_estado, aes(x = reorder(cd_uf_pfpj, media_ProporcaoVerde), y = media_ProporcaoVerde)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Inverte os eixos para facilitar a visualização dos estados
  labs(title = "Ranking dos Estados com Maior Média de Proporção de patentes Verdes",
       x = "Estado",
       y = "Média de ProporcaoVerde") +
  theme_minimal()




# Agregando os dados por estado para calcular a soma da Quantidade_Patentes
patentes_por_estado <- resultado %>%
  group_by(cd_uf_pfpj) %>%
  summarise(total_patentes = sum(Quantidade_Patentes, na.rm = TRUE)) %>%
  ungroup()

ggplot(proporcao_verde_estado, aes(x = reorder(cd_uf_pfpj, nova_proporcao_verde), y = nova_proporcao_verde)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +  # Inverte os eixos para facilitar a visualização dos estados
  labs(title = "Ranking dos Estados por Proporção verde de Patentes",
       x = "Estado",
       y = "Proporção verde") +
  theme_minimal()