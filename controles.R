library(readxl)
library(fuzzyjoin)
library(stringdist)
library(geobr)

controles <- read_xlsx("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/input/controles/data.xlsx")
setDT(controles)


controles[, municipio := ifelse(grepl("\\(", Territorialidades),
                         trimws(sub("\\(.*", "", Territorialidades)),
                         Territorialidades)]

# Cria a coluna 'estado'
controles[, estado := ifelse(grepl("\\(", Territorialidades),
                      trimws(gsub(".*\\(|\\)", "", Territorialidades)),
                      NA)]

municipios <- read_municipality(year = 2020, simplified = TRUE)
setDT(municipios)


name_controles <- controles[,.(municipio,estado)]
name_municipios <- municipios[,.(code_muni,name_muni,abbrev_state)]

merged_dt <- merge(controles, municipios, 
                   by.x = c("municipio", "estado"), 
                   by.y = c("name_muni", "abbrev_state"), 
                   all.x = TRUE)

# Função para limpar e padronizar os nomes
limpa_nome <- function(nome) {
  nome %>% 
    tolower() %>%                                  # converte para minúsculas
    stringi::stri_trans_general("Latin-ASCII") %>% # remove acentuação
    gsub("[[:punct:]]", "", .) %>%                  # remove pontuação
    gsub("\\s+", " ", .) %>%                        # reduz múltiplos espaços para um único
    trimws()                                       # remove espaços em branco no início e fim
}

name_controles[, municipio := sapply(municipio, limpa_nome)]
name_municipios[, name_muni := sapply(name_muni, limpa_nome)]


resultado <- fuzzy_left_join(
  name_controles, name_municipios,
  by = c("municipio" = "name_muni", "estado" = "abbrev_state"),
  match_fun = list(
    function(x, y) stringdist(x, y, method = "lv") <= 0.9,  # fuzzy matching para município
    `==`                                                # comparação exata para estado
  )
)

setDT(resultado)

resultado <- resultado[municipio == "amparo de sao francisco",code_muni:= "2800100"]
resultado <- resultado[municipio == "augusto severo",code_muni:= "2401305"]
resultado <- resultado[municipio == "biritibamirim",code_muni:= "3506607"]
resultado <- resultado[municipio == "brasopolis",code_muni:= "3108909"]
resultado <- resultado[municipio == "dona eusebia",code_muni:= "3122900"]
resultado <- resultado[municipio == "eldorado dos carajas",code_muni:= "1502954"]
resultado <- resultado[municipio == "embu",code_muni:= "3515004"]
resultado <- resultado[municipio == "florinia",code_muni:= "3516101"]
resultado <- resultado[municipio == "fortaleza do tabocao",code_muni:= "1708254"]
resultado <- resultado[municipio == "grao para",code_muni:= "4206108"]
resultado <- resultado[municipio == "iguaraci",code_muni:= "2606903"]
resultado <- resultado[municipio == "itapage",code_muni:= "2306306"]
resultado <- resultado[municipio == "muquem de sao francisco",code_muni:= "2922250"]
resultado <- resultado[municipio == "olhodagua do borges",code_muni:= "2408409"]
resultado <- resultado[municipio == "passavinte",code_muni:= "3147808"]
resultado <- resultado[municipio == "poxoreo",code_muni:= "5107008"]
resultado <- resultado[municipio == "presidente juscelino",code_muni:= "2410306"]
resultado <- resultado[municipio == "santa isabel do para",code_muni:= "1506351"]
resultado <- resultado[municipio == "santarem",code_muni:= "2513653"]
resultado <- resultado[municipio == "santa teresinha",code_muni:= "2513802"]
resultado <- resultado[municipio == "sao luis do paraitinga",code_muni:= "3550001"]
resultado <- resultado[municipio == "sao thome das letras",code_muni:= "3165206"]
resultado <- resultado[municipio == "serido",code_muni:= "2515401"]

resultado <- resultado[!is.na(code_muni),]
resultado <- resultado[,.(code_muni,municipio,estado)]


controles[, municipio := sapply(municipio, limpa_nome)]

controles <- merge(controles,resultado,by = c("municipio","estado"),all.x = TRUE)

controles <- controles[!is.na(code_muni),]

controles$code_muni<- as.character(controles$code_muni)

controles  <- controles[,urban:= `População urbana 1991`/`População total 1991`]
controles  <- controles[,log_renda:=log(`Renda per capita 1991`)]
controles  <- controles[,alphabetization_rate:=(100 - `Taxa de analfabetismo - 18 anos ou mais de idade 1991`)/100]
controles  <- controles[,population:= `População total 1991`]



controles <- controles[,.(code_muni,urban,log_renda,alphabetization_rate,population)]

controles<- controles[-4348,]
controles <- controles[-4017,]
controles <- controles[-4018,]
controles <- controles[-4517,]
controles <- controles[-4471,]

fwrite(controles,"C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/controles/controle.csv")
