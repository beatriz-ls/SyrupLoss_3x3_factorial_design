library(MontgomeryDAE) # pacotes com df do livro do montgomery
library(ggplot2)
library(car)
library(gridExtra)
library(dplyr)

### df ----------------------------------------------------------------------

df <- data.frame(
  'NozzleType'=factor(c(rep(1, 18), rep(2, 18), rep(3, 18))),
  'Speed'=rep(c(rep(100, 6), rep(120, 6), rep(140, 6)), 3),
  'Pressure'=rep(c(10, 10, 15, 15, 20, 20), 9),
  'SyrupLoss'=c(-35, -25, 110, 75, 4, 5,
                -45, -60, -10, 30, -40, -30,
                -40, 15, 80, 54, 31, 36,
                17, 24, 55, 120, -23, -5,
                -65, -58, -55, -44, -64, -62,
                20, 4, 110, 44, -20, -31,
                -39, -35, 90, 113, -30, -55,
                -55, -67, -28, -26, -61, -52,
                15, -30, 110, 135, 54, 4)
)

df <- df %>%
  mutate(
    NozzleType = factor(NozzleType),
    Speed = factor(Speed),
    Pressure = factor(Pressure)
)

# boxplot

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

# Análise de variância (ANOVA) -------------------------------------------------
modelo_sem_interacao <- aov(SyrupLoss ~ NozzleType + Speed + Pressure, data = df)

modelo_com_interacao <- aov(SyrupLoss ~ NozzleType * Speed * Pressure, data = df)

summary(modelo_sem_interacao)

summary(modelo_com_interacao)

anova(modelo_sem_interacao, modelo_com_interacao) #obs: modelo com interação selecionado

modelo <- modelo_com_interacao

# Verificação dos pressupostos -------------------------------------------------

par(mfrow = c(2, 2))
plot(modelo)  # Gráficos de resíduos
par(mfrow = c(1, 1))

# normalidade dos resíduos
shapiro.test(residuals(modelo))

# homocedasticidade

bartlett.test(SyrupLoss ~ interaction(NozzleType, Speed, Pressure), data = df)

leveneTest(SyrupLoss ~ NozzleType * Speed * Pressure, data = df)

# obs: considerar o teste de bartlett

# gráfico de interação
interaction.plot(df$NozzleType, df$Speed, df$SyrupLoss, col=1:4, lwd=2)
interaction.plot(df$Speed, df$Pressure, df$SyrupLoss, col=1:4, lwd=2)
interaction.plot(df$NozzleType, df$Pressure, df$SyrupLoss, col=1:4, lwd=2)

### análise de contrastes ------------------------------------------------------

# comparando os níveis de NozzleType, Speed e Pressure
emm_nozzle <- emmeans(modelo, ~ NozzleType)
contrast(emm_nozzle, method = "pairwise", adjust = "bonferroni")

emm_speed <- emmeans(modelo, ~ Speed)
contrast(emm_speed, method = "pairwise", adjust = "bonferroni")  

emm_pressure <- emmeans(modelo, ~ Pressure)
contrast(emm_pressure, method = "pairwise", adjust = "bonferroni")

# Se quiser fazer contrastes entre combinações específicas, como NozzleType e Speed:
emm_nozzle_speed <- emmeans(modelo, ~ NozzleType * Speed)
contrast(emm_nozzle_speed, method = "pairwise", adjust = "bonferroni")

# Se quiser contrastar interações específicas
emm_interacao <- emmeans(modelo, ~ NozzleType * Speed * Pressure)
contrast(emm_interacao, method = "pairwise", adjust = "bonferroni")
