#' ---
#' Título: Analises de dados de solos
#' Autor: Jessica Chamusca
#' Data: 06-09-2023
#' --- 

getwd()
library(dplyr)
library(tidyverse)
library(here)
library(car)
library(ggpubr)
library(ggforce)
library(lsmeans)
library(lmtest)
library(sjPlot)
library(nlme)
library(vegan)
library(rdist)
library(GGally)
dadoscampo <- read.csv2(file = "dados/DadoscampoV1.csv",header = TRUE, sep = ";", quote = "\"")
head(dadoscampo)
dim(dadoscampo)
dadosprof1 <- dadoscampo %>%
  dplyr::filter(Prof %in% c("0-20"))
dadosprof2 <- dadoscampo %>%
  dplyr::filter(Prof %in% c("20-40"))
dadosprof3 <- dadoscampo %>%
  dplyr::filter(Prof %in% c("40-60"))
head(dadosprof1)
summary(dadosprof1)
# ANOVA
a<- aov(D ~ Area, data = dadosprof1)
b<- aov(D ~ Area, data = dadosprof2)
c<- aov(D ~ Area, data = dadosprof3)
summary(a)
#Teste de tukey:
lsmeans(c, pairwise~Area, adjust="tukey")

#Gráficos:
boxplotprof1 <- ggplot(data = dadosprof1, aes(x = Area, y = D, color = Area)) + 
  geom_boxplot(fill = c("darkorange", "cyan4", "darkgreen"), width = 0.5, 
               color = "black", outlier.shape = NA, alpha = 0.7) +
  geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
  scale_color_manual(values = c("darkorange", "cyan4", "darkgreen")) +
  labs(x = "Área", 
       y = expression(paste("Densidade Aparente para a profundidade 0-20 cm")), size = 15) +
  theme(legend.position = "none")
plot(boxplotprof1)

#Residuos
plot(resid(c)~fitted(c))
resid(c)
#teste de shapiro wilk:
shapiro.test(resid(c))
#homogeneidade
bartlett.test(D ~ Area, data = dadosprof3)


