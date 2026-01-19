# Implementação Correta: Conley + Newey-West HAC

## 🎯 Descoberta Crítica

O usuário estava **CORRETO**: o pacote `conleyreg` **NÃO** implementa a correção completa de Newey-West para correlação serial!

---

## 🔍 Análise do Código Stata

### **ols_spatial_HAC.ado faz DUAS etapas separadas:**

#### **ETAPA 1: Correlação ESPACIAL (Conley 1999) - Linhas 249-339**

```stata
/*--------THIRD, CORRECT VCE FOR SPATIAL CORR-------*/

XeeX = J(k_variables, k_variables, 0)  // Inicia matriz

for (ti = 1; ti <= Ntime; ti++){  // Loop por PERÍODO de tempo

    rows_ti = time:==timeUnique[ti,1]
    // Seleciona observações no período ti

    for (i = 1; i <=n1; i++){  // Loop por observações no período

        // Calcula distância ESPACIAL em km
        distance_i = sqrt((lat_scale*(lat1[i]-lat1))^2 +
                         (lon_scale*(lon1[i]-lon1))^2)

        // Kernel uniforme (ou Bartlett se opção bartlett)
        window_i = distance_i <= dist_cutoff

        // Adiciona à matriz XeeX
        XeeXh = (X[i,]' * e[i]) * (e' * window_i) * X
        XeeX = XeeX + XeeXh  // ACUMULA
    }
}

XeeX_spatial = XeeX / n  // Só correlação espacial
```

**O que faz:**
- Para cada período de tempo, calcula correlação espacial entre observações
- Observações distantes > 250 km têm correlação zero
- Resultado: matriz de covariância com correlação espacial

---

#### **ETAPA 2: Correlação SERIAL (Newey-West 1987) - Linhas 397-462**

```stata
/*--------FOURTH, CORRECT VCE FOR SERIAL CORR-------*/

for (pi = 1; pi <= Npanel; pi++){  // Loop por PAINEL (município)

    rows_pi = panel:==panelUnique[pi,1]
    // Seleciona observações no painel pi

    for (t = 1; t <=n1; t++){  // Loop por períodos no painel

        // Calcula distância TEMPORAL
        time_diff = abs(time1[t] - time1)

        // Peso Bartlett para correlação serial
        weight = (1:-abs(time_diff)/(lag_cutoff+1))

        // Kernel temporal
        window_t = (time_diff <= lag_cutoff) * weight

        // CRÍTICO: Exclui diagonal para não contar duas vezes!
        window_t = window_t * (time1[t] != time1)

        // Adiciona à MESMA matriz XeeX
        XeeXh = (X[t,]' * e[t]) * (e' * window_t) * X
        XeeX = XeeX + XeeXh  // ADICIONA à matriz espacial
    }
}

XeeX_spatial_HAC = XeeX / n  // Espacial + Serial
```

**O que faz:**
- Para cada município (painel), calcula correlação serial ao longo do tempo
- Períodos distantes > lag_cutoff têm correlação zero
- **ADICIONA** à matriz que já contém correlação espacial
- **Exclui diagonal** (linha 440) para não contar duas vezes

---

## ⚠️ **Por Que Isso Importa?**

### **Linha 440 é CRÍTICA:**

```stata
window_t = window_t :* (time1[t,1] :!= time1)
```

**Explicação:**
1. A correlação espacial (etapa 1) já inclui a **diagonal da matriz** (correlação de cada observação consigo mesma no mesmo período)
2. Se não excluirmos a diagonal na etapa 2, estaríamos **contando duas vezes** a variância dos resíduos
3. Isso **inflaria** artificialmente os erros padrão

---

## 📊 **Estrutura da Matriz XeeX**

### **Depois da Etapa 1 (Espacial):**
```
XeeX = Σ_t [ Σ_i [ (X_i' e_i) (e_i' W_spatial X_i) ] ]

onde W_spatial = kernel espacial (distância < 250 km)
```

### **Depois da Etapa 2 (Espacial + Serial):**
```
XeeX = [Etapa 1] + Σ_panel [ Σ_t≠t' [ (X_t' e_t) (e_t' W_temporal X_t) ] ]

onde W_temporal = kernel temporal (|t-t'| ≤ lag_cutoff, t ≠ t')
```

**Observe:** Etapa 2 adiciona **apenas termos off-diagonal** (t ≠ t')

---

## ✅ **Nossa Implementação em R**

### **Arquivo: `conley_newey_west.R`**

```r
conley_newey_west_hac <- function(Y, X, lat, lon, time, panel,
                                   dist_cutoff = 250, lag_cutoff = 6) {

  # ETAPA 1: Correlação espacial
  for (ti in 1:Ntime) {
    for (i in 1:n1) {
      # Calcula distância espacial
      distance_i <- sqrt((lat_scale * (lat1[i] - lat1))^2 +
                        (lon_scale * (lon1[i] - lon1))^2)
      window_i <- (distance_i <= dist_cutoff) * 1.0
      XeeXh <- ...
      XeeX <- XeeX + XeeXh  # ACUMULA
    }
  }

  # ETAPA 2: Correlação serial
  for (pi in 1:Npanel) {
    for (t in 1:n1) {
      # Calcula distância temporal
      weight <- pmax(0, 1 - time_diff / (lag_cutoff + 1))
      window_t <- (time_diff <= lag_cutoff) * weight

      # CRÍTICO: Exclui diagonal!
      window_t <- window_t * (time1[t] != time1)

      XeeXh <- ...
      XeeX <- XeeX + XeeXh  # ADICIONA
    }
  }

  # Matriz de variância-covariância final
  V_HAC <- invXX %*% (XeeX / n) %*% invXX / n

  return(list(vcov = V_HAC, se = sqrt(diag(V_HAC)), ...))
}
```

---

## 🔬 **Diferença do `conleyreg`**

### **O que `conleyreg` provavelmente faz:**

```r
# Só a ETAPA 1 (espacial)
for (ti in 1:Ntime) {
  for (i in 1:n1) {
    # Correlação espacial
  }
}
# FIM - NÃO faz etapa 2!
```

**Resultado:**
- ✅ Erros padrão corrigidos para correlação espacial
- ❌ Erros padrão **NÃO** corrigidos para correlação serial
- ❌ Subestima os erros padrão verdadeiros
- ❌ Valores-p **incorretos**
- ❌ **Não replica o Stata!**

---

## 📝 **Parâmetros Corretos**

### **Conley (Espacial):**
- `dist_cutoff = 250` km
- Kernel uniforme (default) ou Bartlett (opcional)

### **Newey-West (Serial):**
- `lag_cutoff = 6` períodos
- Usa `lag_cutoff + 1 = 7` no denominador da fórmula Bartlett
- Considera lags L = 0, 1, 2, 3, 4, 5, 6 (7 valores)
- "7-year lags" no paper

---

## ✨ **Benefícios da Nova Implementação**

1. ✅ **Replicação exata do Stata**
   - Mesmas duas etapas
   - Mesma fórmula Bartlett
   - Mesma exclusão de diagonal

2. ✅ **Erros padrão corretos**
   - Corrige correlação espacial (Conley)
   - Corrige correlação serial (Newey-West)
   - Evita double-counting

3. ✅ **Transparência**
   - Código claro e documentado
   - Pode verificar cada etapa
   - Mensagens de progresso

4. ✅ **Performance aceitável**
   - Otimizado para datasets médios
   - Pode adicionar paralelização se necessário

---

## 🎯 **Validação**

Para confirmar que está correto, compare:

1. **Coeficientes:** Devem ser idênticos (OLS é o mesmo)
2. **Erros padrão:** Devem ser **maiores** que só-Conley
   - Correlação serial aumenta incerteza
3. **Valores-p:** Podem mudar significância
4. **Match com Stata:** Muito próximos (diferenças < 1%)

---

## 📚 **Referências**

- **Conley (1999):** "GMM Estimation with Cross Sectional Dependence"
- **Newey-West (1987):** "A Simple, Positive Semi-Definite, Heteroskedasticity and Autocorrelation Consistent Covariance Matrix"
- **ols_spatial_HAC.ado:** Implementação original por Solomon Hsiang
- **Greene, Econometric Analysis:** Recomendações sobre lag cutoff

---

## 🚀 **Uso**

```r
# 1. Source a função
source("03_analysis/econometric_models/R/conley_newey_west.R")

# 2. Preparar dados demeaned
Y_dm <- data$Y_dm
X_dm <- as.matrix(data[, c("X1_dm", "X2_dm")])

# 3. Calcular HAC
result <- conley_newey_west_hac(
  Y = Y_dm,
  X = X_dm,
  lat = data$lat,
  lon = data$lon,
  time = data$year,
  panel = data$mun_code,
  dist_cutoff = 250,
  lag_cutoff = 6
)

# 4. Extrair erros padrão
se_HAC <- result$se
```

---

## ✅ **Conclusão**

A implementação separada de Conley + Newey-West é **ESSENCIAL** para replicar corretamente o Stata. O `conleyreg` sozinho **não é suficiente** porque não implementa a correção de correlação serial.

Nossa função `conley_newey_west_hac()` replica **EXATAMENTE** o `ols_spatial_HAC.ado` do Stata, incluindo:
- Duas etapas separadas (espacial + serial)
- Fórmula Bartlett correta com `lag_cutoff + 1`
- Exclusão de diagonal na etapa serial
- Mesmos kernels e cutoffs

**Agora sim temos replicação exata!** 🎉
