v1 <- c(87.2, 94.4, 70.1, 76.2)
v2 <- c(77.7, 82.7, 64.5, 78.4)
n1 <- c(98.9, 107, 78.6, 80.4)
n2 <- c(67.5, 70.2, 62.7, 68.5)


# Exibindo o planejamento replicado
print(plan_replicado)

teste_wilcoxon <- wilcox.test(v1, n1, paired = TRUE) #são iguais p-value = 0.125.
teste_wilcoxon <- wilcox.test(v2, n2, paired = TRUE) #são iguais p-value = 0.125.

---
  ## Anova
  
Para avaliar a variabilidade nos dados, foi aplicada a análise de variância (ANOVA).

Ao utilizar um delineamento fatorial, a ANOVA permite identificar não apenas os efeitos principais de cada fator, mas também suas interações. Isso é fundamental para entender como diferentes combinações de bico, velocidade e pressão afetam a pulverização, auxiliando na tomada de decisões para otimização do processo.

A ANOVA aplicada neste estudo responde às seguintes perguntas principais:
  
  - *Os diferentes tipos de bico, velocidades e pressões impactam significativamente a perda de xarope?*
  - *Existe interação entre esses fatores? Ou seja, o efeito de um fator depende dos níveis dos outros fatores?*
  - *O modelo ajustado atende aos pressupostos necessários para uma análise estatística confiável?*
  
  ---
  ## Pressupostos
  
  #### 1. **Variação Aleatória**
  -   Os tratamentos são atribuídos aleatoriamente em linhas e colunas, mas de forma balanceada.
-   $\sum_{i=1}^{n} \tau_i = 0$ (soma dos efeitos dos tratamentos é zero).
#### 2. **Independência dos Erros**
-   Os erros $\epsilon_{ijk}$ devem ser independentes entre si.
-   $\text{Cov}(\epsilon_{ijk}, \epsilon_{i'j'k'}) = 0$ para $(i,j,k) \neq (i',j',k')$.
#### 3. **Normalidade dos Erros**
-   Os erros são normalmente distribuídos com média zero e variância constante.
-   $\epsilon_{ijk} \sim N(0, \sigma^2)$, onde $\sigma^2$ é a variância.
#### 4. **Homogeneidade de Variâncias (Homoscedasticidade)**
-   A variância dos erros é constante em todos os tratamentos, linhas e colunas.
-   $\text{Var}(\epsilon_{ijk}) = \sigma^2$ para todos os $i, j, k$.
Adapte esse texto para o contexto do experimento

---
  ## Analise Descritiva
  
  Incluir mais analises

```{r boxplot, warning=FALSE, message=FALSE}

p1 <- ggplot(df, aes(NozzleType, SyrupLoss)) +
  geom_boxplot() +
  labs(x = "Tipo de Bico", y = "Perda de Xarope")

p2 <- ggplot(df, aes(Speed, SyrupLoss)) +
  geom_boxplot() +
  labs(x = "Velocidade", y = "")

p3 <- ggplot(df, aes(Pressure, SyrupLoss)) +
  geom_boxplot() +
  labs(x = "Pressão", y = "")

grid.arrange(p1, p2, p3, ncol = 3)

```

---
  ## Analise Descritiva: Interção
  
  ```{r graph interação1, warning=FALSE, message=FALSE}
interaction.plot(df$NozzleType, df$Speed, df$SyrupLoss, col=1:4, lwd=2)
```

---
  ## Analise Descritiva: Interção
  
  ```{r graph interação2, warning=FALSE, message=FALSE}
interaction.plot(df$Speed, df$Pressure, df$SyrupLoss, col=1:4, lwd=2)
```

---
  
  ## Analise Descritiva: Interção
  
  ```{r graph interação3, warning=FALSE, message=FALSE}
interaction.plot(df$NozzleType, df$Pressure, df$SyrupLoss, col=1:4, lwd=2)
```

---
  
  ## Modelos com interação x Modelo sem interação
  
  ```{r anova, warning=FALSE, message=FALSE}

modelo_sem_interacao <- aov(SyrupLoss ~ NozzleType + Speed + Pressure,
                            data = df)
modelo_com_interacao <- aov(SyrupLoss ~ NozzleType * Speed * Pressure,
                            data = df)

summary(modelo_sem_interacao)

summary(modelo_com_interacao)

```

---
  ## Modelos com interação x Modelo sem interação
  
  ```{r anova modelos, warning=FALSE, message=FALSE }
anova(modelo_sem_interacao, modelo_com_interacao)

modelo <- modelo_com_interacao
```

---
  
  ## Verificação de pressupostos: Gráficos
  
  ```{r graph residuos, warning=FALSE, message=FALSE}

par(mfrow = c(2, 2))
plot(modelo)

```

---
  ## Verificação de pressupostos: Normalidade
  
  ```{r norm, warning=FALSE, message=FALSE}
shapiro.test(residuals(modelo))
```

---
  ## Verificação de pressupostos: Homocedasticidade
  
  ```{r homocedasticidade1}
leveneTest(SyrupLoss ~ NozzleType * Speed * Pressure, data = df)
```

---
  ## Verificação de pressupostos: Homocedasticidade
  
  ```{r homocedasticidade2, warning=FALSE, message=FALSE}

bartlett.test(SyrupLoss ~ interaction(NozzleType, Speed, Pressure), data = df)
```