library(data.table)
library(readxl)
library(basedosdados)
library(xtable)




# Definir caminhos dos arquivos
green_file <- "./Input/Febrabran Green Taxonomy/FEBRABAN - Versão Final da Taxonomia_20210217.csv"

# Carregar os dados
green <- fread(green_file,colClasses = "character")
setDT(green)

green  <- green[!(`Economia verde` == "" |`Economia verde` == "Alta contribuição [Social]"
                |`Economia verde` == "Moderada contribuição [Social]"),]

table(green$`Economia verde`)


legenda <- read_xls("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/input/cnae21_estrutura_detalhada.xls")
setDT(legenda)

legenda <- legenda[,.(`...5`,`...6`)]
legenda <- legenda[!is.na(`...5`),]
legenda <- legenda[!is.na(`...6`),]


setnames(legenda,
         old = c("...5",    "...6"),
         new = c("cnae",    "descricao"))

# Limpar a coluna 'codigo', mantendo apenas números e traços
legenda[, cnae := gsub("\\D+", "", cnae)]


green <- merge(green,legenda,by.x = "cnae_2_subclasse",by.y = "cnae")


# English translation for the column `descricao`
green$description_en <- c(
  "Rice cultivation",                                             # 1
  "Maize cultivation",                                            # 2
  "Wheat cultivation",                                            # 3
  "Cultivation of other cereals not elsewhere specified",         # 4
  "Herbaceous cotton cultivation",                                # 5
  "Jute cultivation",                                             # 6
  "Cultivation of other temporary‑crop fibres n.e.c.",            # 7
  "Sugar‑cane cultivation",                                       # 8
  "Soybean cultivation",                                          # 9
  "Peanut cultivation",                                           #10
  "Sunflower cultivation",                                        #11
  "Castor‑bean cultivation",                                      #12
  "Cultivation of other temporary oilseeds n.e.c.",               #13
  "Pineapple cultivation",                                        #14
  "Garlic cultivation",                                           #15
  "Potato cultivation",                                           #16
  "Onion cultivation",                                            #17
  "Bean cultivation",                                             #18
  "Cassava cultivation",                                          #19
  "Melon cultivation",                                            #20
  "Water‑melon cultivation",                                      #21
  "Processing‑tomato cultivation",                                #22
  "Cultivation of other temporary crops n.e.c.",                  #23
  "Horticulture, except strawberry",                              #24
  "Strawberry cultivation",                                       #25
  "Cultivation of flowers and ornamental plants",                 #26
  "Orange cultivation",                                           #27
  "Grape cultivation",                                            #28
  "Açaí cultivation",                                             #29
  "Banana cultivation",                                           #30
  "Cashew cultivation",                                           #31
  "Cultivation of citrus fruits except orange",                   #32
  "Coconut cultivation",                                          #33
  "Guaraná cultivation",                                          #34
  "Apple cultivation",                                            #35
  "Papaya cultivation",                                           #36
  "Passion‑fruit cultivation",                                    #37
  "Mango cultivation",                                            #38
  "Peach cultivation",                                            #39
  "Cultivation of other permanent fruit crops n.e.c.",            #40
  "Coffee cultivation",                                           #41
  "Cocoa cultivation",                                            #42
  "Tea cultivation",                                              #43
  "Yerba‑mate cultivation",                                       #44
  "Black‑pepper cultivation",                                     #45
  "Cultivation of spice plants except black pepper",              #46
  "Oil‑palm cultivation",                                         #47
  "Rubber‑tree cultivation",                                      #48
  "Cultivation of other permanent crops n.e.c.",                  #49
  "Production of certified seeds, except forage for pasture",     #50
  "Production of certified forage seeds for pasture",             #51
  "Production of certified seedlings and other propagules",       #52
  "Beef‑cattle raising",                                          #53
  "Dairy‑cattle raising",                                         #54
  "Cattle raising, other purposes",                               #55
  "Buffalo raising",                                              #56
  "Horse raising",                                                #57
  "Donkey and mule raising",                                      #58
  "Goat raising",                                                 #59
  "Sheep raising, incl. wool production",                         #60
  "Pig farming",                                                  #61
  "Broiler‑chicken raising",                                      #62
  "Production of day‑old chicks",                                 #63
  "Raising of other gallinaceous birds, except broilers",         #64
  "Raising of poultry, except gallinaceous",                      #65
  "Egg production",                                               #66
  "Beekeeping",                                                   #67
  "Eucalyptus cultivation",                                       #68
  "Black‑wattle cultivation",                                     #69
  "Pine cultivation",                                             #70
  "Teak cultivation",                                             #71
  "Cultivation of timber species n.e.c.",                         #72
  "Cultivation of seedlings in forest nurseries",                 #73
  "Logging in planted forests",                                   #74
  "Charcoal production – planted forests",                        #75
  "Black‑wattle bark production – planted forests",               #76
  "Production of non‑timber products n.e.c. – planted forests",   #77
  "Brazil‑nut gathering in native forests",                       #78
  "Latex gathering in native forests",                            #79
  "Conservation of native forests",                               #80
  "Gathering of non‑timber products n.e.c. – native forests",     #81
  "Support activities for forestry",                              #82
  "Preservation of fish, crustaceans and mollusks",               #83
  "Re‑refining of lubricating oils",                              #84
  "Alcohol (ethanol) manufacturing",                              #85
  "Biofuel manufacturing, except alcohol",                        #86
  "Manufacture of trucks and buses",                              #87
  "Manufacture of engines for trucks and buses",                  #88
  "Manufacture of bus bodies",                                    #89
  "Manufacture of locomotives, wagons and rolling stock",         #90
  "Manufacture of parts & accessories for railway vehicles",      #91
  "Manufacture of bicycles & non‑motorized tricycles, parts",     #92
  "Maintenance & repair of railway vehicles",                     #93
  "Electric power generation",                                    #94
  "Coordination & control of power generation & transmission",    #95
  "Electric power transmission",                                  #96
  "Electric power distribution",                                  #97
  "Water capture, treatment and supply",                          #98
  "Sewerage network management",                                  #99
  "Other sewerage‑related activities",                            #100
  "Collection of non‑hazardous waste",                            #101
  "Collection of hazardous waste",                                #102
  "Treatment & disposal of non‑hazardous waste",                  #103
  "Treatment & disposal of hazardous waste",                      #104
  "Recovery of aluminum scrap",                                   #105
  "Recovery of metal materials, except aluminum",                 #106
  "Recovery of plastic materials",                                #107
  "Composting plants",                                            #108
  "Recovery of materials n.e.c.",                                 #109
  "Decontamination & other waste‑management services",            #110
  "Highway and railway construction",                             #111
  "Construction of dams & reservoirs for power generation",       #112
  "Construction of power distribution stations & networks",       #113
  "Maintenance of power distribution networks",                   #114
  "Construction of water & sewerage networks (non‑irrigation)",   #115
  "Wholesale of bicycles, tricycles & recreational vehicles",     #116
  "Wholesale of vegetable‑based fuels, except fuel ethanol",      #117
  "Wholesale of paper & cardboard waste",                         #118
  "Wholesale of non‑metallic waste & scrap, except paper/cardboard", #119
  "Wholesale of metallic waste and scrap",                        #120
  "Retail of bicycles & tricycles; parts & accessories",          #121
  "Rail freight transport",                                       #122
  "Intermunicipal & interstate rail passenger transport",         #123
  "Municipal & metro‑area rail passenger transport",              #124
  "Metro transport",                                              #125
  "Municipal fixed‑route bus transport",                          #126
  "Metropolitan intermunicipal fixed‑route bus transport",        #127
  "Intermunicipal fixed‑route bus transport (non‑metro)",         #128
  "Interstate fixed‑route bus transport",                         #129
  "International fixed‑route bus transport",                      #130
  "School transport",                                             #131
  "Municipal charter bus transport",                              #132
  "Intermunicipal/interstate/international charter bus transport",#133
  "Tourist trains, cable cars and similar",                       #134
  "Highway, bridge & tunnel concessionaires",                     #135
  "Road and rail terminals",                                      #136
  "Restoration & conservation of historic sites and buildings",   #137
  "Botanical gardens, zoos, parks & protected‑area activities"    #138
)


# Recode 'Economia verde' into English using data.table's fcase
green[, green_economy_en := data.table::fcase(
  `Economia verde` == "Alta contribuição [Ambiental]", "High contribution [Environmental]",
  `Economia verde` %in% c("Alta contribuição [Social + Ambiental]", "Alta contribuição [Social+Ambiental]"),
  "High contribution [Social + Environmental]",
  `Economia verde` == "Moderada contribuição [Ambiental]", "Moderate contribution [Environmental]",
  `Economia verde` == "Moderada contribuição [Social + Ambiental]", "Moderate contribution [Social + Environmental]",
  default = NA_character_
)]



#green_pt <- green[,.(cnae_2_subclasse,`Economia verde`,descricao)]
green_en <- green[,.(cnae_2_subclasse,green_economy_en,description_en)]


# Split into chunks of ~30 rows so each table fits comfortably
chunk_size <- 50
n <- nrow(green_en)
parts <- split(
  green_en,
  rep(
    seq_len(ceiling(n / chunk_size)),
    each = chunk_size,
    length.out = n
  )
)

# Loop over each chunk and print a fully-manual LaTeX table
for (i in seq_along(parts)) {
  part <- parts[[i]]
  
  cat(
    "\\begin{table}[ht]\n",
    "\\centering\n",
    "\\resizebox{\\textwidth}{!}{%\n",
    "\\begin{threeparttable}\n",
    sprintf("\\caption{Classificação de Economia Verde por subclasse CNAE (Parte %d)}\n", i),
    sprintf("\\label{tab:green_economy_class_part%d}\n", i),
    "\\begin{tabular}{lll}\n",
    "\\toprule\n",
    "CNAE 2 Subclasse & Descrição & Economia Verde (PT) \\\\\n",
    "\\midrule\n",
    sep = ""
  )
  
  # Print each row
  for (j in seq_len(nrow(part))) {
    row <- part[j]
    cat(
      sprintf("%s & %s & %s \\\\\n",
              row[["CNAE 2 Subclasse"]],
              row[["Descrição"]],
              row[["Economia Verde (PT)"]]
      )
    )
  }
  
  cat(
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{threeparttable}}\n",
    "\\end{table}\n\n",
    sep = ""
  )
}




# Ensure `green_en` is a data.table and select/rename the needed columns
setDT(green_en)
full_tbl <- green_en[, .(
  `CNAE 2 Subclasse`     = cnae_2_subclasse,
  `Green Economy (EN)`   = green_economy_en,
  `Description (EN)`     = description_en
)]

# Split into chunks of ~30 rows so each table fits comfortably
chunk_size <- 50
n <- nrow(full_tbl)
parts <- split(
  full_tbl,
  rep(
    seq_len(ceiling(n / chunk_size)),
    each = chunk_size,
    length.out = n
  )
)

# Loop over each chunk and print a fully-manual LaTeX table
for (i in seq_along(parts)) {
  part <- parts[[i]]
  
  cat(
    "\\begin{table}[ht]\n",
    "\\centering\n",
    "\\resizebox{\\textwidth}{!}{%\n",
    "\\begin{threeparttable}\n",
    sprintf("\\caption{Green Economy Classification by CNAE Subclass (Part %d)}\n", i),
    sprintf("\\label{tab:green_economy_en_part%d}\n", i),
    "\\begin{tabular}{lll}\n",
    "\\toprule\n",
    "CNAE 2 Subclasse & Green Economy (EN) & Description (EN) \\\\\n",
    "\\midrule\n",
    sep = ""
  )
  
  # Print each row
  for (j in seq_len(nrow(part))) {
    row <- part[j]
    cat(
      sprintf("%s & %s & %s \\\\\n",
              row[["CNAE 2 Subclasse"]],
              row[["Green Economy (EN)"]],
              row[["Description (EN)"]]
      )
    )
  }
  
  cat(
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{threeparttable}}\n",
    "\\end{table}\n\n",
    sep = ""
  )
}