# Erros Padrão Conley com Tendências Estado-Ano (year_state_trend)

## Questão

Como calcular corretamente os erros padrão Conley quando incluímos `year_state_trend = year * code_state` no modelo?

## Resposta Curta

**SIM, você precisa demean a variável `year_state_trend` junto com todas as outras variáveis antes de calcular os erros Conley.**

## Explicação Detalhada

### 1. O Modelo Completo

Quando você adiciona `year_state_trend`, seu modelo fica:

```
Y_mt = β*T_mt + η*P_mt + δ*(year × state) + α_m + γ_t + ε_mt
```

Onde:
- `Y_mt` = variável dependente (total_jobs, green_jobs, prop_verde)
- `T_mt` = cont_shock_temp (choque de temperatura)
- `P_mt` = cont_shock_precip (choque de precipitação)
- `year × state` = year_state_trend (tendência linear específica por estado)
- `α_m` = efeitos fixos de município
- `γ_t` = efeitos fixos de ano
- `ε_mt` = termo de erro

### 2. Por que usar `year_state_trend`?

A variável `year_state_trend = year * code_state` captura **tendências temporais específicas por estado**. Isso permite que cada estado tenha uma tendência linear diferente ao longo do tempo.

**Exemplo:**
- Estado 1 (São Paulo, code_state = 35): year_state_trend = 2005 × 35 = 70,175
- Estado 2 (Rio de Janeiro, code_state = 33): year_state_trend = 2005 × 33 = 66,165

Isso significa que estamos controlando por diferentes taxas de crescimento entre estados ao longo do tempo.

### 3. O Problema com Conley SE

O pacote `conleyreg` em R **NÃO** tem parâmetros `id`, `time`, ou `fe` para lidar automaticamente com efeitos fixos.

**Código ERRADO (não funciona):**
```r
# Isso NÃO funciona - conleyreg não aceita esses parâmetros
conley_model <- conleyreg(
  formula = Y ~ T + P + year_state_trend,
  data = data,
  id = "mun_code",      # ❌ Não existe
  time = "year",         # ❌ Não existe
  fe = "both"            # ❌ Não existe
)
```

### 4. A Solução Correta: Within-Transformation (Demeaning)

Para incluir efeitos fixos de município E calcular Conley SE, usamos a **transformação within**:

#### Passo 1: Demean todas as variáveis por município

```r
# Variáveis a demean
vars_to_demean <- c(
  "total_jobs", "green_jobs", "prop_verde",
  "cont_shock_temp",
  "cont_shock_precip",
  "year_state_trend",  # ⭐ IMPORTANTE: incluir aqui
  "year_2001", "year_2002", ..., "year_2020"
)

# Demean por município
data_conley <- data %>%
  group_by(mun_code) %>%
  mutate(
    total_jobs_dm = total_jobs - mean(total_jobs, na.rm = TRUE),
    cont_shock_temp_dm = cont_shock_temp - mean(cont_shock_temp, na.rm = TRUE),
    cont_shock_precip_dm = cont_shock_precip - mean(cont_shock_precip, na.rm = TRUE),
    year_state_trend_dm = year_state_trend - mean(year_state_trend, na.rm = TRUE),
    # ... e assim por diante para todas as variáveis
  ) %>%
  ungroup()
```

#### Passo 2: Rodar Conley com variáveis demeaned

```r
# Fórmula CORRETA com variáveis demeaned
formula_conley <- total_jobs_dm ~ cont_shock_temp_dm +
                                   cont_shock_precip_dm +
                                   year_state_trend_dm +    # ⭐ Incluir a versão demeaned
                                   year_2001_dm + year_2002_dm + ...

conley_model <- conleyreg(
  formula = formula_conley,
  data = data_conley,
  lat = "lat",
  lon = "lon",
  dist_cutoff = 250,    # 250 km de correlação espacial
  lag_cutoff = 7        # 7 anos de correlação temporal
)
```

### 5. Por que demean `year_state_trend`?

Quando você faz o demeaning por município, está **absorvendo os efeitos fixos de município** (α_m).

**O que acontece com `year_state_trend` quando você demean:**

Para um município m no estado s:
```
year_state_trend_mt = year_t * code_state_s
```

Quando você demean por município:
```
year_state_trend_dm_mt = (year_t * code_state_s) - mean(year * code_state_s)
                       = code_state_s * (year_t - mean(year))
```

Isso **preserva** a variação temporal específica do estado, mas **remove** o nível médio.

### 6. Interpretação dos Resultados

Depois de calcular os erros Conley, você obtém:

```r
summary(conley_model)
#                           Estimate Std. Error t value Pr(>|t|)
# cont_shock_temp_dm         X.XXXX    X.XXXX    X.XX   X.XXXX
# cont_shock_precip_dm       X.XXXX    X.XXXX    X.XX   X.XXXX
# year_state_trend_dm        X.XXXX    X.XXXX    X.XX   X.XXXX  ⭐
```

Os erros padrão estão ajustados para:
- **Correlação espacial**: municípios próximos (até 250 km) podem ter erros correlacionados
- **Correlação temporal**: observações do mesmo município em anos próximos (até 7 anos) podem ter erros correlacionados

### 7. Diferença entre Abordagens

Existem **três** abordagens diferentes para controlar por tendências temporais:

#### Abordagem A: Apenas Efeitos Fixos de Ano
```
Y_mt = β*T_mt + η*P_mt + α_m + γ_t + ε_mt
```
- Mesma tendência temporal para todos os estados
- Mais simples, mas pode não capturar heterogeneidade entre estados

#### Abordagem B: Efeitos Fixos Estado×Ano (completos)
```
Y_mt = β*T_mt + η*P_mt + α_m + γ_st + ε_mt
```
- Tendência temporal completamente flexível para cada estado
- Absorve TODA a variação temporal dentro de cada estado
- **CUIDADO:** Pode remover variação que você quer estimar!

#### Abordagem C: Tendência Linear Estado-Ano
```
Y_mt = β*T_mt + η*P_mt + δ*(year × state) + α_m + γ_t + ε_mt
```
- Tendência linear específica por estado
- **Melhor compromisso**: controla por tendências estaduais sem remover toda variação temporal

### 8. Qual usar?

**Use a Abordagem C (com `year_state_trend`)** quando:
- Você quer controlar por tendências estaduais diferentes
- Você acredita que estados têm taxas de crescimento diferentes
- Você NÃO quer absorver toda a variação temporal (como na Abordagem B)

**Exemplo do código no Stata do arquivo:**
```stata
gen year_state_trend = year * code_state

reg2hdfespatial prop_verde cont_shock_temp cont_shock_precip year_state_trend, ///
    timevar(round) panelvar(id) lat(lat) lon(lon) ///
    distcutoff(250) lagcutoff(7)
```

Este comando Stata **automaticamente** faz o demeaning e calcula os erros Conley corretamente.

### 9. Código R Completo (Equivalente ao Stata)

O arquivo `reg_employment_shortrun_with_state_trend.R` implementa essa abordagem corretamente:

```r
# 1. Criar year_state_trend
data[, year_state_trend := year * code_state]

# 2. Demean TODAS as variáveis (incluindo year_state_trend)
vars_to_demean <- c(dep_vars, "cont_shock_temp", "cont_shock_precip",
                    "year_state_trend",  # ⭐ INCLUIR AQUI
                    paste0("year_", years))

data_conley[, paste0(vars_to_demean, "_dm") :=
  lapply(.SD, function(x) x - mean(x, na.rm = TRUE)),
  by = mun_code, .SDcols = vars_to_demean]

# 3. Fórmula Conley com year_state_trend_dm
formula_conley <- paste0(
  dv, "_dm ~ cont_shock_temp_dm + cont_shock_precip_dm + year_state_trend_dm + ",
  formula_years
)

# 4. Rodar Conley
conley_model <- conleyreg(
  formula = formula_conley,
  data = data_conley,
  lat = "lat", lon = "lon",
  dist_cutoff = 250, lag_cutoff = 7
)
```

## Checklist: Está correto?

✅ **SIM, está correto se:**
1. Você criou `year_state_trend = year * code_state`
2. Você fez o demeaning de `year_state_trend` por município
3. Você incluiu `year_state_trend_dm` na fórmula do conleyreg
4. Você também incluiu year dummies (demeaned)

❌ **NÃO está correto se:**
1. Você usou `year_state_trend` sem demean
2. Você não incluiu `year_state_trend` na lista de variáveis a demean
3. Você tentou usar parâmetros `id`, `time`, ou `fe` no conleyreg (não existem)

## Conclusão

**Resposta à sua dúvida:**

Sim, você deve ter cuidado ao calcular os erros Conley com `year_state_trend`. A variável **DEVE** ser demeaned por município junto com todas as outras variáveis antes de calcular os erros Conley.

O arquivo `reg_employment_shortrun_with_state_trend.R` implementa essa abordagem corretamente e pode ser usado como referência.

## Referências

- Conley, T. G. (1999). GMM estimation with cross sectional dependence. *Journal of Econometrics*, 92(1), 1-45.
- Newey, W. K., & West, K. D. (1987). A simple, positive semi-definite, heteroskedasticity and autocorrelation consistent covariance matrix. *Econometrica*, 55(3), 703-708.
- Hsiang, S. M. (2010). Temperatures and cyclones strongly associated with economic production in the Caribbean and Central America. *Proceedings of the National Academy of Sciences*, 107(35), 15367-15372.
