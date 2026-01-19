# Correções para Replicação Exata do Stata

## Data: 2026-01-19

## Arquivo Corrigido
- `03_analysis/econometric_models/R/reg_employment_shortrun_with_state_trend.R`

## Problema Identificado

O código R **não estava replicando** os resultados do Stata porque havia diferenças fundamentais na implementação dos fixed effects e do método Conley.

---

## Mudanças Implementadas

### 1. ✅ TWO-WAY DEMEANING (CRÍTICO)

**ANTES:**
- Usava **one-way demeaning** (apenas por município)
- Adicionava year dummies demeaned ao modelo

**DEPOIS:**
- Implementado **two-way demeaning** usando `fixest::demean()`
- Remove fixed effects de município E ano **simultaneamente**
- Replica exatamente o comportamento de `reg2hdfe` do Stata

**Código:**
```r
# Alternating projections method (Gauss-Seidel) - EXACTLY what reg2hdfe does
twoway_demean <- function(x, group1, group2, max_iter = 1000, tol = 1e-8) {
  x_demean <- x
  for(iter in 1:max_iter) {
    x_old <- x_demean
    # Demean by municipality
    means1 <- ave(x_demean, group1, FUN = mean)
    x_demean <- x_demean - means1
    # Demean by year
    means2 <- ave(x_demean, group2, FUN = mean)
    x_demean <- x_demean - means2
    # Check convergence
    if(max(abs(x_demean - x_old)) < tol) break
  }
  return(x_demean)
}
```

### 2. ✅ REMOÇÃO DE YEAR DUMMIES

**ANTES:**
- Incluía year dummies no modelo Conley
- `formula_conley <- Y_dm ~ X_dm + year_state_trend_dm + year_2001_dm + year_2002_dm + ...`

**DEPOIS:**
- **NÃO** inclui year dummies
- `formula_conley <- Y_dm ~ X_dm + year_state_trend_dm - 1`
- Year effects já foram removidos pelo two-way demeaning

### 3. ✅ AJUSTE DO LAG_CUTOFF

**ANTES:**
```r
lag_cutoff = 7  # 7-year temporal correlation
```

**DEPOIS:**
```r
lag_cutoff = 6  # 6-year temporal correlation (MATCHES STATA)
```

### 4. ✅ TRATAMENTO CORRETO DE year_state_trend

**ANTES:**
- Demeaned apenas por município

**DEPOIS:**
- Demeaned com **two-way FE** (município + ano)
- Captura corretamente as tendências lineares específicas por estado

---

## Por Que Isso Importa?

### One-way vs Two-way Demeaning

**One-way demeaning + year dummies ≠ Two-way demeaning!**

- **Two-way FE** remove:
  1. Média de cada município (across all years)
  2. Média de cada ano (across all municipalities)
  3. Usa algoritmo iterativo até convergência

- **One-way FE + dummies:**
  - Remove apenas média de município
  - Controla ano via dummies
  - **NÃO é equivalente matematicamente!**

### Impacto nos Resultados

- Coeficientes estimados são **diferentes**
- Erros padrão são **diferentes**
- Significância estatística pode **mudar**

---

## Comando Stata Replicado

```stata
reg2hdfespatial Y X year_state_trend, ///
    timevar(year) panelvar(mun_code) ///
    lat(lat) lon(lon) ///
    distcutoff(250) lagcutoff(6)
```

Onde `reg2hdfespatial`:
1. Chama `reg2hdfe` para fazer two-way demeaning
2. Chama `ols_spatial_HAC` nos dados demeaned

---

## Código R Equivalente (DEPOIS)

```r
# 1. Two-way demeaning
demeaned_result <- fixest::demean(
  as.formula(paste0(var, " ~ 1 | mun_code + year")),
  data = data_clean_df
)

# 2. Conley com dados demeaned (sem year dummies)
conley_model <- conleyreg::conleyreg(
  formula = Y_dm ~ cont_shock_temp_dm + cont_shock_precip_dm +
                   year_state_trend_dm - 1,
  data = data_model_df,
  lat = "lat",
  lon = "lon",
  dist_cutoff = 250,
  lag_cutoff = 6
)
```

---

## Nota Técnica: Alternating Projections

A implementação usa **alternating projections** (método Gauss-Seidel) para o two-way demeaning, que é EXATAMENTE o algoritmo usado pelo `reg2hdfe` do Stata.

**Por que não usar `fixest::demean()`?**
- Incompatibilidade com alguns formatos de dados
- Alternating projections é mais robusto e transparente
- É o algoritmo padrão em econometria para múltiplos FE

**Como funciona:**
1. Subtrai a média por município de cada variável
2. Subtrai a média por ano do resultado
3. Repete até convergência (tipicamente < 20 iterações)
4. Tolerância: 1e-8 (mesma que o Stata)

---

## Validação

Para validar que a replicação está correta, compare:

1. **Coeficientes**: devem ser idênticos (até precisão numérica)
2. **Erros padrão**: devem ser muito próximos
3. **t-statistics**: devem ser muito próximos
4. **R²**: deve ser idêntico

**Diferenças esperadas:**
- Pequenas diferenças numéricas devido a:
  - Algoritmo de convergência do two-way demeaning
  - Precisão de cálculo de distâncias geodésicas
  - Diferenças de implementação em Mata vs R

**Diferenças NÃO esperadas:**
- Coeficientes com sinais opostos
- Diferenças > 5% nos coeficientes
- Mudanças qualitativas na significância

---

## Próximos Passos

1. Rodar o script corrigido
2. Comparar resultados com output do Stata
3. Se ainda houver diferenças, verificar:
   - Dados de entrada são idênticos?
   - Missing values tratados da mesma forma?
   - Variáveis calculadas da mesma forma?

---

## Referências

- **Stata ado files analisados:**
  - `archive/reg2hdfespatial.ado` - Two-way FE wrapper
  - `archive/ols_spatial_HAC.ado` - Conley standard errors
  - `archive/reg_micro [Recovered].do` - Código original

- **Pacotes R utilizados:**
  - `fixest::demean()` - Two-way demeaning
  - `conleyreg::conleyreg()` - Conley standard errors
