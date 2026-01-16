library(data.table)

# 1) Carregar e filtrar bases para anos > 1999 -------------------------------
weather <- fread("./Output/weather data/final_weather_data_1940_2020.csv",
                 select = c("municipality_code", "year",
                            "cont_shock_temperature", "cont_shock_precipitation"))[
                              year > 1999 & !is.na(cont_shock_temperature)
                            ]
weather[, .N, by=.(municipality_code, year)][N>1]
weather <- unique(weather, by = c("municipality_code", "year"))


rais <- fread("./output/final_base/weather_rais.csv",
              select = c("mun_code", "year",
                         "total_vinc", "total_vinc_verde", "prop_verde"))[
                           year > 1999
                         ]
setnames(rais, "mun_code", "municipality_code")

quadro <- fread("./output/final_base/weather_quadro_societario.csv",
                select = c("mun_code", "year",
                           "firmas_ativas", "firmas_verde", "prop_verde"))[
                             year > 1999
                           ]
setnames(quadro, "mun_code", "municipality_code")

patent <- fread("./output/final_base/weather_patent.csv",
                select = c("mun_code", "year",
                           "qtd_pat_5yr", "qtd_pat_verde_5yr", "prop_verde_5yr"))[
                             year > 1999
                           ]
setnames(patent, "mun_code", "municipality_code")

patent <- patent[qtd_pat_5yr == 0 & qtd_pat_verde_5yr == 0 ,prop_verde_5yr:=0]
patent <- patent[is.na(qtd_pat_5yr) & is.na(qtd_pat_verde_5yr) ,prop_verde_5yr:= NA]

# 2) Fazer merges ------------------------------------------------------------
wr <- merge(rais,   weather, by = c("municipality_code", "year"))
wq <- merge(quadro, weather, by = c("municipality_code", "year"))
wp <- merge(patent, weather, by = c("municipality_code", "year"))

wp <- wp[!is.na(qtd_pat_5yr)]

# 3) Função para estatísticas (ignora NAs) ------------------------------------
calc_stats <- function(x) {
  list(
    mean = mean(x,    na.rm = TRUE),
    sd   = sd(x,      na.rm = TRUE),
    min  = min(x,     na.rm = TRUE),
    p25  = quantile(x, 0.25, na.rm = TRUE),
    p75  = quantile(x, 0.75, na.rm = TRUE),
    max  = max(x,     na.rm = TRUE),
    n    = sum(!is.na(x))
  )
}

# 4) Definir painéis e variáveis ---------------------------------------------
panels <- list(
  Jobs = list(
    dt   = wr,
    vars = list(
      "$\\Delta T_{m,t}$"             = "cont_shock_temperature",
      "$\\Delta P_{m,t}$"             = "cont_shock_precipitation",
      "Total~Jobs$_{m,t}$"            = "total_vinc",
      "Green~Jobs$_{m,t}$"            = "total_vinc_verde",
      "Prop~Green~Jobs$_{m,t}$"       = "prop_verde"
    )
  ),
  Firms = list(
    dt   = wq,
    vars = list(
      "$\\Delta T_{m,t}$"             = "cont_shock_temperature",
      "$\\Delta P_{m,t}$"             = "cont_shock_precipitation",
      "Total~Firms$_{m,t}$"           = "firmas_ativas",
      "Green~Firms$_{m,t}$"           = "firmas_verde",
      "Prop~Green~Firms$_{m,t}$"      = "prop_verde"
    )
  ),
  Patents = list(
    dt   = wp,
    vars = list(
      "$\\Delta T_{m,t}$"             = "cont_shock_temperature",
      "$\\Delta P_{m,t}$"             = "cont_shock_precipitation",
      "Total~Patents$_{m,t}$"         = "qtd_pat_5yr",
      "Green~Patents$_{m,t}$"         = "qtd_pat_verde_5yr",
      "Prop~Green~Patents$_{m,t}$"    = "prop_verde_5yr"
    )
  )
)

# 5) Montar a tabela LaTeX ---------------------------------------------------
latex <- c(
  "\\begin{table}[ht]",
  "  \\centering",
  "  \\caption{Estatísticas Descritivas dos Municípios (2000--2010 e 2000--2020)\\label{tab:desc_stats_filtrada}}",
  "  \\resizebox{\\textwidth}{!}{%",
  "    \\small",
  "    \\begin{threeparttable}",
  "      \\begin{tabular}{",
  "        l",
  "        S[table-format=4.2]  % Mean",
  "        S[table-format=5.2]  % Std. Dev.",
  "        S[table-format=3.2]  % Min",
  "        S[table-format=4.2]  % Pctl(25)",
  "        S[table-format=4.2]  % Pctl(75)",
  "        @{\n        }      % espaço extra antes de Max",
  "        S[table-format=7.2]  % Max",
  "        S[table-format=6.0]  % Obs.",
  "      }",
  "        \\toprule",
  "        Variable & {Mean} & {Std. Dev.} & {Min} & {Pctl(25)} & {Pctl(75)} & {Max} & {Obs.} \\\\",
  "        \\midrule"
)

for (panel_name in names(panels)) {
  latex <- c(latex,
             sprintf("        \\multicolumn{8}{l}{\\textbf{%s}} \\\\", panel_name)
  )
  for (lbl in names(panels[[panel_name]]$vars)) {
    var  <- panels[[panel_name]]$vars[[lbl]]
    dt   <- panels[[panel_name]]$dt
    s    <- calc_stats(dt[[var]])
    latex <- c(latex,
               sprintf("        {%s} & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f & %s \\\\",
                       lbl,
                       s$mean, s$sd, s$min, s$p25, s$p75, s$max,
                       format(s$n, big.mark = ",", scientific = FALSE))
    )
  }
  latex <- c(latex, "        \\addlinespace")
}

latex <- c(
  latex,
  "        \\bottomrule",
  "      \\end{tabular}",
  "      \\begin{tablenotes}",
  "        \\footnotesize",
  "        \\item \\textit{Nota:} Pctl = percentil; Obs. = Observações.",
  "      \\end{tablenotes}",
  "    \\end{threeparttable}%",
  "  }",
  "\\end{table}"
)

# 6) Imprimir no console -----------------------------------------------------
cat(paste(latex, collapse = "\n"))

######################
# ---------------------------------------------------------------------------
# 1) Read the combined dataset
# ---------------------------------------------------------------------------
final_data <- fread("./output/final_combined_data.csv")
final_data <- final_data[!is.na(urban),]

# ---------------------------------------------------------------------------
# 2) Variable lists
# ---------------------------------------------------------------------------
weather_vars <- c("cont_shock_temperature_long_00_10",
                  "cont_shock_precipitation_long_00_10",
                  "cont_shock_temperature_long_00_20",
                  "cont_shock_precipitation_long_00_20")

control_vars <- c("urban", "log_renda", "alphabetization_rate")

jobs_outcome <- c("delta_00_10_rais_vinculo",
                  "delta_00_20_rais_vinculo",
                  "delta_00_10_rais_vinculo_verde",
                  "delta_00_20_rais_vinculo_verde",
                  "delta_00_10_rais_prop_verde",
                  "delta_00_20_rais_prop_verde")

firms_outcome <- c("delta_00_10_quadro_societario_firma",
                   "delta_00_20_quadro_societario_firma",
                   "delta_00_10_quadro_societario_firma_verde",
                   "delta_00_20_quadro_societario_firma_verde",
                   "delta_00_10_quadro_societario_prop_verde",
                   "delta_00_20_quadro_societario_prop_verde")

pat_outcome <- c("delta_00_10_patent_total",
                 "delta_00_20_patent_total",
                 "delta_00_10_patent_total_verde",
                 "delta_00_20_patent_total_verde",
                 "delta_00_10_patent_prop_verde",
                 "delta_00_20_patent_prop_verde")

# ---------------------------------------------------------------------------
# 3) Helper: descriptive-statistics function
# ---------------------------------------------------------------------------
calc_stats <- function(x) list(
  mean = mean(x, na.rm = TRUE),
  sd   = sd(  x, na.rm = TRUE),
  min  = min( x, na.rm = TRUE),
  p25  = quantile(x, .25, na.rm = TRUE),
  p75  = quantile(x, .75, na.rm = TRUE),
  max  = max( x, na.rm = TRUE),
  n    = sum(!is.na(x))
)

# ---------------------------------------------------------------------------
# 4) Helper: build stats for one panel (weather + outcomes + controls)
#    • Keeps municipalities with ≥1 non-missing outcome
# ---------------------------------------------------------------------------
make_panel_stats <- function(data, panel_name, outcome_vars) {
  keep <- data[, rowSums(!is.na(.SD)) > 0, .SDcols = outcome_vars]
  sub  <- data[keep, c(weather_vars, outcome_vars, control_vars), with = FALSE]
  
  rbindlist(lapply(names(sub), function(v) {
    s <- calc_stats(sub[[v]])
    data.table(Panel = panel_name, Variable = v,
               Mean = s$mean, SD = s$sd, Min = s$min,
               P25 = s$p25,  P75 = s$p75,
               Max = s$max,  Obs = s$n)
  }))
}

# ---------------------------------------------------------------------------
# 5) Build complete stats_dt
# ---------------------------------------------------------------------------
stats_dt <- rbindlist(list(
  make_panel_stats(final_data, "Jobs",    jobs_outcome),
  make_panel_stats(final_data, "Firms",   firms_outcome),
  make_panel_stats(final_data, "Patents", pat_outcome)
))

# ---------------------------------------------------------------------------
# 6) LaTeX labels
# ---------------------------------------------------------------------------
label_map <- list(
  # Weather
  cont_shock_temperature_long_00_10   = "$\\Delta T_{m,2000\\text{--}2010}$",
  cont_shock_precipitation_long_00_10 = "$\\Delta P_{m,2000\\text{--}2010}$",
  cont_shock_temperature_long_00_20   = "$\\Delta T_{m,2000\\text{--}2020}$",
  cont_shock_precipitation_long_00_20 = "$\\Delta P_{m,2000\\text{--}2020}$",
  # Jobs
  delta_00_10_rais_vinculo            = "$\\Delta\\,Total\\,Jobs_{m,2000\\text{--}2010}$",
  delta_00_20_rais_vinculo            = "$\\Delta\\,Total\\,Jobs_{m,2000\\text{--}2020}$",
  delta_00_10_rais_vinculo_verde      = "$\\Delta\\,Green\\,Jobs_{m,2000\\text{--}2010}$",
  delta_00_20_rais_vinculo_verde      = "$\\Delta\\,Green\\,Jobs_{m,2000\\text{--}2020}$",
  delta_00_10_rais_prop_verde         = "$\\Delta\\,Prop\\,Green\\,Jobs_{m,2000\\text{--}2010}$",
  delta_00_20_rais_prop_verde         = "$\\Delta\\,Prop\\,Green\\,Jobs_{m,2000\\text{--}2020}$",
  # Firms
  delta_00_10_quadro_societario_firma           = "$\\Delta\\,Total\\,Firms_{m,2000\\text{--}2010}$",
  delta_00_20_quadro_societario_firma           = "$\\Delta\\,Total\\,Firms_{m,2000\\text{--}2020}$",
  delta_00_10_quadro_societario_firma_verde     = "$\\Delta\\,Green\\,Firms_{m,2000\\text{--}2010}$",
  delta_00_20_quadro_societario_firma_verde     = "$\\Delta\\,Green\\,Firms_{m,2000\\text{--}2020}$",
  delta_00_10_quadro_societario_prop_verde      = "$\\Delta\\,Prop\\,Green\\,Firms_{m,2000\\text{--}2010}$",
  delta_00_20_quadro_societario_prop_verde      = "$\\Delta\\,Prop\\,Green\\,Firms_{m,2000\\text{--}2020}$",
  # Patents
  delta_00_10_patent_total            = "$\\Delta\\,Total\\,Patents_{m,2000\\text{--}2010}$",
  delta_00_20_patent_total            = "$\\Delta\\,Total\\,Patents_{m,2000\\text{--}2020}$",
  delta_00_10_patent_total_verde      = "$\\Delta\\,Green\\,Patents_{m,2000\\text{--}2010}$",
  delta_00_20_patent_total_verde      = "$\\Delta\\,Green\\,Patents_{m,2000\\text{--}2020}$",
  delta_00_10_patent_prop_verde       = "$\\Delta\\,Prop\\,Green\\,Patents_{m,2000\\text{--}2010}$",
  delta_00_20_patent_prop_verde       = "$\\Delta\\,Prop\\,Green\\,Patents_{m,2000\\text{--}2020}$",
  # Controls
  urban                   = "Urban Share",
  log_renda               = "Log Income",
  alphabetization_rate    = "Alphabetization Rate"
)

# ---------------------------------------------------------------------------
# 7) LaTeX rendering function (avoids atomic-vector issue)
# ---------------------------------------------------------------------------
render_table <- function(stats_dt, label_map) {
  cat(
    "\\begin{table}[ht]\n",
    "  \\centering\n",
    "  \\caption{Municipal Descriptive Statistics by Panel (Weather Shocks, Outcome Deltas, and Controls)}\n",
    "  \\resizebox{\\textwidth}{!}{%\n",
    "    \\small\n",
    "    \\begin{threeparttable}\n",
    "      \\begin{tabular}{lrrrrrrr}\n",
    "        \\toprule\n",
    "        Variable & Mean & Std. Dev. & Min & Pctl(25) & Pctl(75) & Max & Obs. \\\\\n",
    "        \\midrule\n",
    sep = "")
  
  for (panel in c("Jobs", "Firms", "Patents")) {
    cat(sprintf("        \\multicolumn{8}{l}{\\textbf{%s}} \\\\\n", panel))
    
    panel_dt <- stats_dt[Panel == panel]
    
    for (i in seq_len(nrow(panel_dt))) {
      r <- panel_dt[i]
      cat(sprintf(
        "        %s & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f & %s \\\\\n",
        label_map[[r$Variable]], r$Mean, r$SD, r$Min,
        r$P25, r$P75, r$Max,
        format(r$Obs, big.mark = ',', scientific = FALSE)))
    }
    cat("        \\addlinespace\n")
  }
  
  cat(
    "        \\bottomrule\n",
    "      \\end{tabular}\n",
    "      \\begin{tablenotes}\n",
    "        \\footnotesize\n",
    "        \\item \\textit{Note:} Weather shocks ($\\Delta T$, $\\Delta P$), outcome deltas, and census controls are calculated on the identical set of municipalities within each panel. Obs.\ denotes the number of municipalities.\n",
    "      \\end{tablenotes}\n",
    "    \\end{threeparttable}%\n",
    "  }\n",
    "\\end{table}\n",
    sep = "")
}

# ---------------------------------------------------------------------------
# 8) Generate LaTeX code
# ---------------------------------------------------------------------------
render_table(stats_dt, label_map)