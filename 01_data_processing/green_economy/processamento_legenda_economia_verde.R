#https://concla.ibge.gov.br/classificacoes/correspondencias/atividades-economicas



# Carregar as bibliotecas necessárias
library(data.table)
library(readxl)
library(basedosdados)

# Definir caminhos dos arquivos
legenda_file <- "./Input/legenda_rais.csv"
green_file <- "./Input/Febrabran Green Taxonomy/FEBRABAN - Versão Final da Taxonomia_20210217.csv"
complemento_file <- "D:/Users/leonl/Desktop/Projeto/temp/Direction of inovation/Climate shocks and innovation/Input/Rais/Rais_cnae.xlsx"

# Carregar os dados
legenda <- fread(legenda_file, colClasses = "character")
green <- fread(green_file,colClasses = "character")
setDT(legenda)
setDT(green)

# Verificar correspondência entre cnae_1 e cnae_2
contagem_cnae <- legenda[, .(contagem = uniqueN(cnae_2)), by = cnae_1]
cnae_multiplos <- contagem_cnae[contagem > 1]
cnae_unico <- contagem_cnae[contagem == 1]

# Realizar a junção das duas tabelas usando cnae_2_subclasse
dados_combinados <- merge(legenda, green, by = "cnae_2_subclasse", all.x = TRUE)

# Identificar cnae_1 cujo cnae_2_subclasse correspondente tenha todos os valores de Economia verde iguais a NA
cnae_excluir_na <- dados_combinados[
  cnae_1 %in% cnae_multiplos$cnae_1,
  .(todos_na = all(is.na(`Economia verde`))),
  by = cnae_1
][todos_na == TRUE, cnae_1]

# Excluir esses cnae_1 da tabela original
dados_filtrados_na <- dados_combinados[!cnae_1 %in% cnae_excluir_na]

# Identificar os valores de "Economia verde" que devem ser considerados para exclusão
valores_exclusao <- c(
  "Alta contribuição [Social + Ambiental]",
  " Alta contribuição [Social+Ambiental]",  
  "Moderada contribuição [Social + Ambiental]",
  "Alta contribuição [Ambiental]",
  "Moderada contribuição [Ambiental]"
)

# Excluir cnae_1 cujo cnae_2_subclasse correspondente tenha apenas os valores especificados em "Economia verde"
cnae_excluir_valores <- dados_filtrados_na[
  cnae_1 %in% cnae_multiplos$cnae_1,
  .(todos_valores = all(`Economia verde` %in% valores_exclusao)),
  by = cnae_1
][todos_valores == TRUE, cnae_1]

# Excluir esses cnae_1 da tabela filtrada
dados_filtrados_1 <- dados_filtrados_na[!cnae_1 %in% cnae_excluir_valores]

# Identificar os valores de "Economia verde" que devem ser considerados para exclusão
valores_exclusao_2 <- c(
  "Alta contribuição [Social]",
  "Moderada contribuição [Social]" 
)

# Excluir cnae_1 cujo cnae_2_subclasse correspondente tenha apenas os valores especificados em "Economia verde"
cnae_excluir_valores_2 <- dados_filtrados_1[
  cnae_1 %in% cnae_multiplos$cnae_1,
  .(todos_valores = all(is.na(`Economia verde`) | `Economia verde` %in% valores_exclusao_2)),
  by = cnae_1
][todos_valores == TRUE, cnae_1]

# Excluir esses cnae_1 da tabela filtrada
dados_filtrados_2 <- dados_filtrados_1[!cnae_1 %in% cnae_excluir_valores_2]

# Atualizar cnae_multiplos excluindo cnae_1 indesejados
cnae_multiplos <- cnae_multiplos[!cnae_1 %in% cnae_excluir_na]
cnae_multiplos <- cnae_multiplos[!cnae_1 %in% cnae_excluir_valores]
cnae_multiplos <- cnae_multiplos[!cnae_1 %in% cnae_excluir_valores_2]
cnae_multiplos <- cnae_multiplos[, .(cnae_1)]
cnae_multiplos[, `Economia verde` := NA]

# Preparar tabelas de exclusão com valores de "Economia verde"
Economia_verde_na <- rep(0, length(cnae_excluir_na))
cnae_excluir_na <- data.table(cnae_1 = cnae_excluir_na, `Economia verde` = Economia_verde_na)

Economia_verde_1 <- rep(1, length(cnae_excluir_valores))
cnae_excluir_valores <- data.table(cnae_1 = cnae_excluir_valores, `Economia verde` = Economia_verde_1)

Economia_verde_2 <- rep(0, length(cnae_excluir_valores_2))
cnae_excluir_valores_2 <- data.table(cnae_1 = cnae_excluir_valores_2, `Economia verde` = Economia_verde_2)

# Preparar cnae_unico
cnae_unico <- cnae_unico[, .(cnae_1)]
cnae_unico <- legenda[cnae_1 %in% cnae_unico$cnae_1]
cnae_unico <- cnae_unico[, .(cnae_1, cnae_2_subclasse)]

# Realizar a junção das duas tabelas usando cnae_2_subclasse
cnae_unico <- merge(cnae_unico, green, by = "cnae_2_subclasse", all.x = TRUE)

# Atualizar valores de "Economia verde"
cnae_unico[`Economia verde` %in% valores_exclusao, `Economia verde` := 1]
cnae_unico[is.na(`Economia verde`), `Economia verde` := 0]
cnae_unico[, `Economia verde` := as.double(`Economia verde`)]
cnae_unico <- unique(cnae_unico[, .(cnae_1, `Economia verde`)])

# Excluir cnae_1 duplicados com valores conflitantes de "Economia verde"
duplicados <- cnae_unico[duplicated(cnae_1) | duplicated(cnae_1, fromLast = TRUE)]
duplicados <- unique(duplicados$cnae_1)
cnae_unico <- cnae_unico[!cnae_1 %in% duplicados]

# Combinar todos os resultados
legenda_final <- rbind(cnae_excluir_na, cnae_excluir_valores, cnae_excluir_valores_2, cnae_unico)

# Carregar complemento e adicionar ao resultado final
complemento <- read_xlsx(complemento_file, sheet = 1, range = "A1:B59")
legenda_final <- rbind(legenda_final, complemento)

# Escrever o resultado final para um arquivo CSV
fwrite(legenda_final, "D:/Users/leonl/Desktop/Projeto/temp/Direction of inovation/Climate shocks and innovation/Input/legenda_final.csv", row.names = FALSE)
