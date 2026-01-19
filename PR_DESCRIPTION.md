# Fix Stata Replication: Implement Exact Two-Way Demeaning

## 🎯 Objetivo
Replicar **exatamente** o comportamento do comando Stata `reg2hdfespatial` no código R, corrigindo problemas de replicação identificados.

---

## ⚠️ Por que o PR #11 foi revertido?
O PR #11 anterior usava `fixest::demean()` que causava erro "Error: wrong type". Este PR corrige esse problema usando um método mais robusto.

---

## 🔧 Correções Implementadas

### 1. **Two-Way Demeaning com Alternating Projections**
- **Problema:** `fixest::demean()` tinha incompatibilidade com formato de dados
- **Solução:** Implementado método de **alternating projections** (Gauss-Seidel)
- **Motivo:** É EXATAMENTE o algoritmo usado pelo `reg2hdfe` do Stata
- **Convergência:** Tipicamente < 20 iterações, tolerância 1e-8

**Código:**
```r
twoway_demean <- function(x, group1, group2, max_iter = 1000, tol = 1e-8) {
  # Iterative demeaning (Gauss-Seidel method)
  for(iter in 1:max_iter) {
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

### 2. **Remoção de Year Dummies**
- Year fixed effects já removidos pelo two-way demeaning
- Formula Conley: `Y_dm ~ X_dm + year_state_trend_dm - 1`
- Replica comportamento exato do `ols_spatial_HAC` do Stata

### 3. **Ajuste de lag_cutoff**
- Mudado de **7** para **6** períodos
- Matches Stata: `lagcutoff(6)`
- Afeta correção Newey-West de correlação temporal

### 4. **Tratamento Correto de year_state_trend**
- Variável: `year * code_state`
- Demeaned com **two-way FE** (município + ano)
- Captura tendências lineares específicas por estado corretamente

---

## 📊 Diferenças Críticas: One-Way vs Two-Way Demeaning

**⚠️ IMPORTANTE:** One-way FE + year dummies ≠ Two-way FE

**Two-way demeaning:**
1. Remove média de cada município (across all years)
2. Remove média de cada ano (across all municipalities)
3. Usa algoritmo iterativo até convergência

**Impacto:**
- Afeta **TODOS** os coeficientes e erros padrão
- Necessário para replicação exata do Stata

---

## 🔬 Comando Stata Replicado

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

## 📁 Arquivos Modificados

1. **`03_analysis/econometric_models/R/reg_employment_shortrun_with_state_trend.R`**
   - Implementação de two-way demeaning com alternating projections
   - Remoção de year dummies do modelo Conley
   - lag_cutoff = 6
   - Documentação extensa

2. **`STATA_REPLICATION_FIXES.md`** (novo)
   - Documentação completa das correções
   - Explicação técnica do alternating projections
   - Guia de validação

---

## ✅ Validação

Para validar replicação, compare:
- **Coeficientes**: idênticos (até precisão numérica)
- **Erros padrão**: muito próximos
- **t-statistics**: muito próximos
- **R²**: idêntico

**Pequenas diferenças aceitáveis devido a:**
- Precisão numérica dos algoritmos
- Cálculo de distâncias geodésicas
- Implementação do Conley em diferentes linguagens

---

## 🚀 Como Testar

```r
source("03_analysis/econometric_models/R/reg_employment_shortrun_with_state_trend.R")
```

O script vai:
1. Carregar dados
2. Aplicar two-way demeaning (com mensagens de convergência)
3. Rodar regressões com Conley standard errors
4. Gerar tabela LaTeX
5. Mostrar resultados

---

## 📝 Commits Incluídos

- `8b81f59` Fix: Replace fixest::demean with alternating projections
- `77a4277` Fix Stata replication: implement two-way demeaning

---

## 🔗 Documentação

Ver `STATA_REPLICATION_FIXES.md` para detalhes técnicos completos.

---

## ⚠️ Nota
Este PR substitui o PR #11 revertido, corrigindo o erro técnico que causou o revert.
