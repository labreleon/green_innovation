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
resultado <- merge_priority[, .(Quantidade_Patentes = uniqueN(no_pedido)), by = .(cd_ibge_cidade, DT_DEPOSITO, CD_CLASSIF)]
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
resultado <- resultado[, .(Quantidade_Patentes = sum(Quantidade_Patentes, na.rm = TRUE)), by = .(cd_ibge_cidade, DT_DEPOSITO, green,CD_CLASSIF)]

resultado <- resultado[DT_DEPOSITO > 1996,]

# 1) Filtrar apenas patentes verdes (green == 1)
dt_verdes <- resultado[green == 1]

# 2) Agregar somando a quantidade por classe
resumo_classes <- dt_verdes[
  , .(Quantidade_Total = sum(Quantidade_Patentes, na.rm = TRUE))
  , by = CD_CLASSIF
]

# 3) Ordenar do maior para o menor
resumo_classes <- resumo_classes[order(-Quantidade_Total)]

# 4) Verificar as top categorias
head(resumo_classes, 10)





# Agregar os dados para somar as patentes por CD_IBGE_CIDADE, DT_DEPOSITO e CD_CLASSIF
resultado <- merge_priority[, .(Quantidade_Patentes = uniqueN(no_pedido)), by = .(cd_ibge_cidade, DT_DEPOSITO, CD_CLASSIF)]
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
resultado <- resultado[, .(Quantidade_Patentes = sum(Quantidade_Patentes, na.rm = TRUE)), by = .(cd_ibge_cidade, DT_DEPOSITO, green)]

# Calcular proporção de patentes verdes
resultado <- resultado[, .(
  Quantidade_Patentes = sum(as.numeric(Quantidade_Patentes), na.rm = TRUE),
  Quantidade_Patentes_verdes = sum(ifelse(green == 1, as.numeric(Quantidade_Patentes), 0), na.rm = TRUE)
), by = .(cd_ibge_cidade, DT_DEPOSITO)][, ProporcaoVerde := Quantidade_Patentes_verdes / Quantidade_Patentes]

# Ordenar resultado por cidade e ano
setorder(resultado, cd_ibge_cidade, DT_DEPOSITO)

# Filtrar para anos maiores que 1996
resultado <- resultado[DT_DEPOSITO > 1996,]

# Deixar o painel balanceado
# Gerar uma sequência de anos entre 1997 e 2021
years <- 1997:2021

# Expandir o dataframe para incluir todos os anos entre 1997 e 2021 para cada cidade
resultado <- resultado[CJ(cd_ibge_cidade = unique(resultado$cd_ibge_cidade), DT_DEPOSITO = years, unique = TRUE), on = .(cd_ibge_cidade, DT_DEPOSITO)]

# Substituir NAs por 0
resultado[is.na(Quantidade_Patentes), Quantidade_Patentes := 0]
resultado[is.na(Quantidade_Patentes_verdes), Quantidade_Patentes_verdes := 0]
resultado[is.na(ProporcaoVerde), ProporcaoVerde := 0]

setorder(resultado,cd_ibge_cidade,DT_DEPOSITO)

# Calcular somas móveis de 5 anos
resultado[, Quantidade_Patentes_5_anos := frollsum(Quantidade_Patentes, n = 5, align = "right", fill = NA)]
resultado[, Quantidade_Patentes_verdes_5_anos := frollsum(Quantidade_Patentes_verdes, n = 5, align = "right", fill = NA)]
resultado[, ProporcaoVerde_5_anos := Quantidade_Patentes_verdes_5_anos / Quantidade_Patentes_5_anos]





# 1. National Trend of Green Patents (1997-2022)
national_trend <- resultado[, .(TotalPatents = sum(Quantidade_Patentes, na.rm = TRUE),
                                GreenPatents = sum(Quantidade_Patentes_verdes, na.rm = TRUE)),
                            by = DT_DEPOSITO][, GreenShare := GreenPatents/TotalPatents]

fig1 <- ggplot(national_trend, aes(x = DT_DEPOSITO, y = GreenShare)) +
  geom_line(color = "#238b45", size = 1.2) +
  geom_point(color = "#238b45", size = 2) +
  labs(title = "Proportion of Green Patents in Brazil (1997-2022)",
       x = "Year", y = "Proportion of Green Patents") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(fig1)


# 2. Spatial Distribution of Green Patents (2015-2022 average)
mun_shapes <- read_municipality(code_muni = "all", year = 2020)

spatial_data <- resultado[DT_DEPOSITO >= 2015, 
                          .(AvgGreenShare = mean(ProporcaoVerde, na.rm = TRUE)),
                          by = cd_ibge_cidade]

spatial_data <- merge(mun_shapes, spatial_data, 
                      by.x = "code_muni", by.y = "cd_ibge_cidade")

fig2 <- ggplot() +
  geom_sf(data = spatial_data, aes(fill = AvgGreenShare), color = NA) +
  scale_fill_viridis(name = "Green Patent Share",
                     option = "D", 
                     labels = percent_format(),
                     na.value = "grey90") +
  labs(title = "Geographical Distribution of Green Patents (2015-2022 average)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

print(fig2)


