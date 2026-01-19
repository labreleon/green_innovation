# Guia: Regressões Conley com Tendências Estado-Ano

## Arquivos Disponíveis

### 1. `reg_employment_shortrun.R`
**Especificação:** Município FE + Ano FE

**Modelo:**
```
Y_mt = β*T_mt + η*P_mt + α_m + γ_t + ε_mt
```

**Quando usar:**
- Modelo baseline sem tendências específicas por estado
- Assume que todos os estados têm a mesma tendência temporal

**Comando para rodar:**
```r
source("03_analysis/econometric_models/R/reg_employment_shortrun.R")
```

### 2. `reg_employment_shortrun_with_state_trend.R` ⭐ NOVO
**Especificação:** Município FE + Ano FE + Tendência Linear Estado-Ano

**Modelo:**
```
Y_mt = β*T_mt + η*P_mt + δ*(year × state) + α_m + γ_t + ε_mt
```

**Quando usar:**
- Quando você acredita que estados têm taxas de crescimento diferentes
- Controla por tendências lineares específicas por estado
- **Recomendado** para análises de robustez

**Comando para rodar:**
```r
source("03_analysis/econometric_models/R/reg_employment_shortrun_with_state_trend.R")
```

### 3. `reg_employment_municipal_cluster.R`
**Especificação:** Várias especificações SEM Conley SE

**Modelos:**
1. Município FE + Ano FE (clustering municipal)
2. Município FE + Estado×Ano FE completos (clustering municipal)
3. Município FE + Ano FE + Tendência Estado-Ano (clustering municipal)

**Quando usar:**
- Para comparar diferentes especificações rapidamente
- **NÃO** usa erros Conley (apenas clustering municipal)
- Útil para análises exploratórias antes de rodar Conley (que é mais lento)

**Comando para rodar:**
```r
source("03_analysis/econometric_models/R/reg_employment_municipal_cluster.R")
```

## Comparação das Abordagens

| Abordagem | Município FE | Ano FE | Estado×Ano | Tendência Linear | Conley SE | Tempo |
|-----------|--------------|--------|------------|------------------|-----------|-------|
| **shortrun** | ✅ | ✅ | ❌ | ❌ | ✅ | ~5-10 min |
| **shortrun_with_state_trend** | ✅ | ✅ | ❌ | ✅ | ✅ | ~5-10 min |
| **municipal_cluster** (Modelo 1) | ✅ | ✅ | ❌ | ❌ | ❌ | ~30 seg |
| **municipal_cluster** (Modelo 2) | ✅ | ❌ | ✅ | ❌ | ❌ | ~30 seg |
| **municipal_cluster** (Modelo 3) | ✅ | ✅ | ❌ | ✅ | ❌ | ~30 seg |

## Fluxo de Trabalho Recomendado

### Passo 1: Análise Exploratória (Rápida)
```r
# Rodar primeiro para ver se os resultados fazem sentido
source("03_analysis/econometric_models/R/reg_employment_municipal_cluster.R")
```

**Vantagens:**
- Muito rápido (~30 segundos)
- Mostra 3 especificações diferentes
- Ajuda a identificar problemas nos dados

**Desvantagens:**
- NÃO usa erros Conley (pode subestimar os erros padrão)
- Não é adequado para publicação

### Passo 2: Análise Principal com Conley (Baseline)
```r
# Modelo baseline para o paper
source("03_analysis/econometric_models/R/reg_employment_shortrun.R")
```

**Vantagens:**
- Usa erros Conley (adequado para dados espaciais)
- Especificação mais simples e conservadora
- Adequado para tabela principal do paper

**Desvantagens:**
- Não controla por tendências específicas por estado
- Pode haver confounding se estados têm taxas de crescimento diferentes

### Passo 3: Análise de Robustez com Tendências Estado-Ano
```r
# Modelo com controle adicional para robustez
source("03_analysis/econometric_models/R/reg_employment_shortrun_with_state_trend.R")
```

**Vantagens:**
- Controla por tendências lineares específicas por estado
- Mais robusto a confounders temporais entre estados
- Adequado para apêndice ou análise de robustez

**Desvantagens:**
- Remove mais variação (pode reduzir poder estatístico)
- Assume tendências lineares (pode não ser adequado se tendências são não-lineares)

## Interpretação dos Resultados

### Se os resultados SÃO similares entre as abordagens:
✅ **Ótimo!** Significa que seus resultados são robustos a diferentes especificações de tendências temporais.

**O que reportar no paper:**
- Tabela principal: Resultados do `reg_employment_shortrun.R`
- Apêndice: Resultados do `reg_employment_shortrun_with_state_trend.R` como teste de robustez

### Se os resultados SÃO diferentes entre as abordagens:
⚠️ **Cuidado!** Isso pode indicar:
1. Tendências estaduais importantes que devem ser controladas
2. Confounding entre choques climáticos e tendências estaduais

**O que fazer:**
1. Investigar por que há diferença
2. Plotar tendências temporais por estado
3. Verificar se choques climáticos estão correlacionados com tendências estaduais
4. **Usar a especificação com tendências estado-ano como principal** (mais conservadora)

## Exemplo de Código para Comparar Resultados

```r
# Rodar ambas as especificações e comparar
source("03_analysis/econometric_models/R/reg_employment_shortrun.R")
results_baseline <- results_list  # Salvar resultados

source("03_analysis/econometric_models/R/reg_employment_shortrun_with_state_trend.R")
results_with_trend <- results_list  # Salvar resultados

# Comparar coeficientes
cat("\n=== COMPARAÇÃO: Baseline vs. Com Tendências Estado-Ano ===\n\n")

for(i in 1:3) {
  cat(dep_labels[i], ":\n")
  cat("  Temperatura (baseline):     ", sprintf("%.4f", results_baseline[[i]]$coef_temp),
      " (SE: ", sprintf("%.4f", results_baseline[[i]]$se_temp), ")\n", sep = "")
  cat("  Temperatura (com trend):    ", sprintf("%.4f", results_with_trend[[i]]$coef_temp),
      " (SE: ", sprintf("%.4f", results_with_trend[[i]]$se_temp), ")\n", sep = "")

  cat("  Precipitação (baseline):    ", sprintf("%.4f", results_baseline[[i]]$coef_precip),
      " (SE: ", sprintf("%.4f", results_baseline[[i]]$se_precip), ")\n", sep = "")
  cat("  Precipitação (com trend):   ", sprintf("%.4f", results_with_trend[[i]]$coef_precip),
      " (SE: ", sprintf("%.4f", results_with_trend[[i]]$se_precip), ")\n\n", sep = "")
}
```

## Perguntas Frequentes

### Q1: Qual especificação devo usar no meu paper?

**R:** Depende do seu argumento:
- Se você quer mostrar que os resultados são robustos: use **baseline** na tabela principal e **com tendências** no apêndice
- Se há evidência de tendências estaduais importantes: use **com tendências** como principal

### Q2: Por que incluir `year_state_trend` em vez de efeitos fixos Estado×Ano completos?

**R:** Efeitos fixos Estado×Ano completos (`state_year`) removem TODA a variação temporal dentro de cada estado. Isso pode absorver a variação que você quer estimar (choques climáticos). `year_state_trend` é mais conservador - controla apenas por tendências **lineares**.

### Q3: year_state_trend deve ser demeaned?

**R:** **SIM!** Quando você usa conleyreg sem parâmetros `id` e `fe`, você precisa fazer o demeaning manualmente. Isso inclui demean `year_state_trend` junto com todas as outras variáveis.

### Q4: Como interpretar o coeficiente de year_state_trend?

**R:** O coeficiente δ de `year_state_trend` mostra quanto o resultado aumenta por unidade de `year × code_state`. Como `code_state` é fixo para cada município, isso essencialmente captura a interação entre o ano e o estado.

**Interpretação prática:** Se δ > 0 para um estado com `code_state = 35`, significa que esse estado tem uma tendência de crescimento positiva ao longo do tempo, controlando por efeitos de ano.

### Q5: O que fazer se Conley falhar?

**R:** O código tem um fallback automático para erros clustered (por município). Isso garante que você sempre obtém resultados, mesmo que o cálculo Conley falhe.

## Solução de Problemas

### Erro: "argumentos não utilizados (id = 'mun_code', fe = 'both')"

**Causa:** Você está tentando usar parâmetros que não existem no `conleyreg`.

**Solução:** Use a transformação within (demeaning) manual. Veja `reg_employment_shortrun_with_state_trend.R` para o código correto.

### Conley muito lento (>30 minutos)

**Soluções:**
1. Reduza `dist_cutoff` de 250 para 150 km
2. Reduza `lag_cutoff` de 7 para 5 anos
3. Use `reg_employment_municipal_cluster.R` primeiro para testar

### Resultados muito diferentes entre especificações

**Investigação:**
1. Verifique se há tendências temporais fortes em estados específicos
2. Plote séries temporais por estado
3. Teste correlação entre choques climáticos e tendências estaduais

## Recursos Adicionais

- `CONLEY_SE_FIX.md` - Documentação sobre como corrigir erros Conley
- `CONLEY_STATE_TREND_EXPLANATION.md` - Explicação detalhada sobre year_state_trend e demeaning
- `CONLEY_OPTIMIZATION_README.md` - Dicas para otimizar performance

## Contato

Para dúvidas sobre as especificações ou implementação, consulte os documentos acima ou revise o código fonte dos scripts.
