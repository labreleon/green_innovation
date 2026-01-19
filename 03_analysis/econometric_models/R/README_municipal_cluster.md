# Regressões com Clustering Municipal e Efeitos Fixos Estado×Ano

Este diretório contém scripts para análise de painel com:
- **Clustering de erros padrão no nível do município** (sem Conley)
- **Efeitos fixos estado × ano**

## Scripts Disponíveis

### 1. `reg_employment_municipal_cluster.R`
Analisa efeitos de choques climáticos sobre **emprego** (total, verde, e proporção verde).

**Arquivo de entrada:** `./output/final_base/weather_rais.dta`

**Variáveis dependentes:**
1. Total de empregos (total_jobs)
2. Empregos verdes (green_jobs)
3. Proporção de empregos verdes (prop_verde)

### 2. `reg_patents_municipal_cluster.R`
Analisa efeitos de choques climáticos sobre **patentes** (total, verde, e proporção verde).

**Arquivo de entrada:** `./output/final_base/weather_patent.dta`

**Variáveis dependentes:**
1. Total de patentes (qtd_pat)
2. Patentes verdes (qtd_pat_verde)
3. Proporção de patentes verdes (prop_verde)

### 3. `reg_firms_municipal_cluster.R`
Analisa efeitos de choques climáticos sobre **firmas ativas** (total, verde, e proporção verde).

**Arquivo de entrada:** `./output/final_base/weather_quadro_societario.dta`

**Variáveis dependentes:**
1. Total de firmas ativas (firmas_ativas)
2. Firmas verdes (firmas_verde)
3. Proporção de firmas verdes (prop_verde)

### 4. `run_all_municipal_cluster.R`
Script master que executa todas as análises sequencialmente.

---

## Especificações Econométricas

**Modelo 1 - Básico (Município FE + Ano FE):**
```
Y_mt = β*T_mt + η*P_mt + α_m + γ_t + ε_mt
```
- α_m = efeito fixo município
- γ_t = efeito fixo ano

**Modelo 2 - Estado×Ano (Município FE + Estado×Ano FE):**
```
Y_mt = β*T_mt + η*P_mt + α_m + γ_st + ε_mt
```
- α_m = efeito fixo município
- γ_st = efeito fixo estado×ano

**Modelo 3 - Município×Ano FE + Tendências Estado-Ano (NOVO):**
```
Y_mt = β*T_mt + η*P_mt + δ*(year × state) + α_mt + ε_mt
```
- α_mt = efeito fixo município×ano
- year × state = tendência linear específica do estado
- Este é o modelo mais conservador, controlando por variação temporal dentro de cada município e por tendências estado-específicas

**Variáveis independentes (todas as análises):**
- `cont_shock_temp`: Choque de temperatura (desvios padrão)
- `cont_shock_precip`: Choque de precipitação (desvios padrão)
- `year_state_trend`: Tendência estado-ano (year × code_state) - apenas Modelo 3

**Erros padrão:**
- Clusterizados no nível do município
- **NÃO** usa correção espacial de Conley

## Como Executar

### Requisitos
```r
install.packages(c("haven", "dplyr", "fixest", "sandwich", "lmtest", "data.table"))
```

### Execução Individual
```bash
cd /home/user/green_innovation/03_analysis/econometric_models/R

# Rodar apenas emprego
Rscript reg_employment_municipal_cluster.R

# Rodar apenas patentes
Rscript reg_patents_municipal_cluster.R

# Rodar apenas firmas
Rscript reg_firms_municipal_cluster.R
```

### Execução de Todas as Análises (Recomendado)
```bash
cd /home/user/green_innovation/03_analysis/econometric_models/R
Rscript run_all_municipal_cluster.R
```

Ou no R:
```r
setwd("/home/user/green_innovation/03_analysis/econometric_models/R")
source("run_all_municipal_cluster.R")
```

## Outputs

### Emprego (reg_employment_municipal_cluster.R)
1. `table_employment_municipal_cluster_basic.tex` - Modelo 1: Município FE + Ano FE
2. `table_employment_municipal_cluster_state_year.tex` - Modelo 2: Município FE + Estado×Ano FE
3. `table_employment_municipal_cluster_comparison.tex` - Comparação Modelos 1 vs 2
4. `table_employment_municipal_cluster_mun_year_trend.tex` - Modelo 3: Município×Ano FE + Tendências Estado-Ano

### Patentes (reg_patents_municipal_cluster.R)
5. `table_patents_municipal_cluster_comparison.tex` - Comparação Modelos 1 vs 2
6. `table_patents_municipal_cluster_mun_year_trend.tex` - Modelo 3: Município×Ano FE + Tendências Estado-Ano

### Firmas (reg_firms_municipal_cluster.R)
7. `table_firms_municipal_cluster_comparison.tex` - Comparação Modelos 1 vs 2
8. `table_firms_municipal_cluster_mun_year_trend.tex` - Modelo 3: Município×Ano FE + Tendências Estado-Ano

## Estrutura dos Dados

**Período:** 2000-2020 (todos os arquivos)

**Arquivos de entrada:**

### 1. Emprego: `./output/final_base/weather_rais.dta`
- `mun_code`: Código do município
- `code_state`: Código do estado
- `year`: Ano
- `total_vinc` ou `total_jobs`: Total de empregos
- `total_vinc_verde` ou `green_jobs`: Empregos verdes
- `cont_shock_temp`: Choque de temperatura (desvios padrão)
- `cont_shock_precip`: Choque de precipitação (desvios padrão)

### 2. Patentes: `./output/final_base/weather_patent.dta`
- `mun_code`: Código do município
- `code_state`: Código do estado
- `year`: Ano
- `qtd_pat`: Total de patentes
- `qtd_pat_verde`: Patentes verdes
- `cont_shock_temp`: Choque de temperatura (desvios padrão)
- `cont_shock_precip`: Choque de precipitação (desvios padrão)

### 3. Firmas: `./output/final_base/weather_quadro_societario.dta`
- `mun_code`: Código do município
- `code_state`: Código do estado
- `year`: Ano
- `firmas_ativas`: Total de firmas ativas
- `firmas_verde`: Firmas verdes
- `cont_shock_temp`: Choque de temperatura (desvios padrão)
- `cont_shock_precip`: Choque de precipitação (desvios padrão)

## Diferenças em Relação aos Scripts Originais

### Scripts Originais (ex: `reg_employment_shortrun.R`)
- ✓ Usa Conley spatial HAC standard errors (250 km, 7 anos)
- ✓ Clustering municipal como fallback
- ✓ Município FE + Ano FE

### Novos Scripts (`reg_employment_municipal_cluster.R`)
- ✗ **NÃO** usa Conley spatial HAC
- ✓ **Apenas** clustering municipal (mais simples e rápido)
- ✓ Município FE + Ano FE (modelo básico)
- ✓ **Município FE + Estado×Ano FE** (modelo adicional)

## Interpretação dos Resultados

### Efeito Fixo Estado×Ano
- Controla por choques específicos do estado em cada ano
- Útil para capturar:
  - Políticas estaduais que variam no tempo
  - Ciclos econômicos específicos do estado
  - Choques regionais não capturados por FE de ano nacional

### Quando Usar Cada Especificação?

**Modelo 1 - Básico (Município + Ano FE):**
- Identifica o efeito usando variação dentro do município ao longo do tempo
- Controla por tendências nacionais (FE ano)
- Assume que choques temporais são comuns a todos os estados
- Menos conservador, mais poder estatístico

**Modelo 2 - Estado×Ano FE:**
- Identifica o efeito usando variação dentro do município, controlando por tendências estado-específicas
- Mais conservador que Modelo 1 (remove mais variação)
- Útil se há preocupação com choques específicos do estado
- Controla por políticas estaduais que variam no tempo

**Modelo 3 - Município×Ano FE + Tendências Estado-Ano (NOVO):**
- **Especificação mais conservadora** de todas
- Identifica o efeito usando variação residual após controlar por:
  - Todos os fatores município-ano específicos (α_mt)
  - Tendências lineares específicas de cada estado
- Útil para testes de robustez extrema
- Pode ter poder estatístico limitado devido ao alto nível de controles
- Recomendado quando há preocupação com:
  - Tendências estado-específicas não-lineares
  - Políticas estaduais com efeitos graduais ao longo do tempo
  - Choques econômicos estaduais correlacionados com clima

## Notas Metodológicas

1. **Clustering Municipal**: Corrige correlação serial dentro do município e heteroscedasticidade

2. **Por que não usar Conley?**
   - Computacionalmente mais eficiente
   - Mais simples de implementar
   - Adequado se correlação espacial não é a principal preocupação

3. **Limitações**:
   - Não corrige correlação espacial entre municípios
   - Pode subestimar erros padrão se há forte dependência espacial

## Extensões Possíveis

1. Adicionar variáveis de controle (renda, educação, urbanização)
2. Testar especificações com lags temporais
3. Analisar heterogeneidade por região
4. Incluir tendências específicas do estado
5. Adicionar interações temperatura × precipitação
