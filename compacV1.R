#' ---
#' Título: compactação
#' Autor: Jessica Chamusca
#' Data: 06-02-2024
#' --- 

getwd()
library(AlgDesign)
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
library(rstatix)
library(ggplot2)
library(multcomp)
library(agricolae)
library(cowplot)
library(forcats)
library(RColorBrewer)
library(svglite) 
library(extrafont)

##ANALISE estatistica dos dados de compactação
dados <- read_csv2(file = "dados/dadosR.csv")
head(dados)
dim(dados)
dados1 <- dados %>%
  dplyr::filter(H %in% c("0-20"))
dados2 <- dados %>%
  dplyr::filter(H %in% c("20-40"))
dados3 <- dados %>%
  dplyr::filter(H %in% c("40-60"))
dados4 <- dados %>%
  dplyr::filter(H %in% c("60-80"))
dados5 <- dados %>%
  dplyr::filter(H %in% c("80-100"))
dadosMS <- dados %>%
  dplyr::filter(H %in% c("MS"))
head(dados1)
summary(dados1)
# ANOVA
aa<- aov(D ~ A, data = dados1)
bb<- aov(D ~ A, data = dados2)
cc<- aov(D ~ A, data = dados3)
dd<- aov(D ~ A, data = dados4)
ee<- aov(D ~ A, data = dados5)

a<- aov(P ~ A, data = dadosprof1)
b<- aov(P ~ A, data = dadosprof2)
c<- aov(P ~ A, data = dadosprof3)
d<- aov(P ~ A, data = dadosprof4)
e<- aov(P ~ A, data = dadosprof5)

#Residuos
resid(f)
plot(resid(f)~fitted(f))

#teste de shapiro wilk:
shapiro.test(resid(f))
#homogeneidade
bartlett.test(D ~ A, data = dadosprof2)

#Teste de tukey:
lsmeans(aa, pairwise~A, adjust="tukey")

#Analises de do peso seco serrapilheiras

s<- aov(Ps ~ A, data = dados)

summary(s)

####### para colocar as letrinhas
## ANOVA
dados_sMS <- dados %>%
  filter(H != "MS")
dados_sMS$A <- factor(dados_sMS$A, levels = c("NC", "FSP", "CSP"))

summary(modelo_anova)

# Teste de Tukey
# Resultados do Teste de Tukey 1
modelo_anova <- aov(D ~ A, data = dados_sMS%>%
                      filter(H == "0-20"))
rt1 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(D ~ A, data = dados_sMS%>%
                      filter(H == "20-40"))
rt2 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(D ~ A, data = dados_sMS%>%
                      filter(H == "40-60"))
rt3 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(D ~ A, data = dados_sMS%>%
                      filter(H == "60-80"))
rt4 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(D ~ A, data = dados_sMS%>%
                      filter(H == "80-100"))
rt5 <- HSD.test(modelo_anova, "A", group = TRUE)
head(rt1)
# Convertendo resultados de Tukey em dataframes
rt1t <- as.data.frame(rt1$groups)
rt1t$A <- rownames(rt1t)
rt2t <- as.data.frame(rt2$groups)
rt2t$A <- rownames(rt2t)
rt3t <- as.data.frame(rt3$groups)
rt3t$A <- rownames(rt3t)
rt4t <- as.data.frame(rt4$groups)
rt4t$A <- rownames(rt4t)
rt5t <- as.data.frame(rt5$groups)
rt5t$A <- rownames(rt5t)
head(rt1t)


tukey <- rbind(rt1t, rt2t, rt3t, rt4t, rt5t)
head(tukey)

tukey$xx <- c(0.7,1,1.3,1.7,2,2.3,2.7,3,3.3,3.7,4,4.3,4.7,5,5.3)
tukey$yy <- seq(from = (min(tukey$D, na.rm = TRUE)+0.1), to = (max(tukey$D, na.rm = TRUE)*0.95), by=0.1)
tukey$yy <- c(0.88,0.92,1.05,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.8,0.95,0.8)

###gráfico tudo junto da porosidade

####### para colocar as letrinhas
## ANOVA
dados_sMS$A <- factor(dados_sMS$A, levels = c("NC", "FSP", "CSP"))
modelo_anova <- aov(P ~ A, data = dados_sMS%>%
                      filter(H == "0-20"))
rt1 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(P ~ A, data = dados_sMS%>%
                      filter(H == "20-40"))
rt2 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(P ~ A, data = dados_sMS%>%
                      filter(H == "40-60"))
rt3 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(P ~ A, data = dados_sMS%>%
                      filter(H == "60-80"))
rt4 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(P ~ A, data = dados_sMS%>%
                      filter(H == "80-100"))
rt5 <- HSD.test(modelo_anova, "A", group = TRUE)
summary(modelo_anova)

# Teste de Tukey
# Resultados do Teste de Tukey 1

# Convertendo resultados de Tukey em dataframes
p1t <- as.data.frame(rt1$groups)
p1t$A <- rownames(p1t)
p2t <- as.data.frame(rt2$groups)
p2t$A <- rownames(p2t)
p3t <- as.data.frame(rt3$groups)
p3t$A <- rownames(p3t)
p4t <- as.data.frame(rt4$groups)
p4t$A <- rownames(p4t)
p5t <- as.data.frame(rt5$groups)
p5t$A <- rownames(p5t)

tukeyp <- rbind(p1t, p2t, p3t, p4t, p5t)
head(tukeyp)

tukeyp$xx <- c(0.7,1,1.3,1.7,2,2.3,2.7,3,3.3,3.7,4,4.3,4.7,5,5.3)
tukeyp$yy <- c(0.65,0.62,0.58,0.62,0.62,0.62,0.62,0.62,0.62,0.62,0.62,0.62,0.68,0.63,0.69)

# Redefinindo os níveis do fator
dados_sMS <- dados %>%
 filter(H != "MS")
dados_sMS$A <- factor(dados_sMS$A, levels = c("NC", "FSP", "CSP"))

fig5a<- ggplot(dados_sMS, aes(x = H, y = D, fill = A)) + 
  geom_boxplot() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x=("Soil depths"),y=expression("Bulk density (g/cm³)"), fill = " ") +
  geom_text(data = tukey, aes(x = tukey$xx, y = tukey$yy, label = groups, vjust = -0.5))+  
  scale_fill_manual(values = c("darkgreen", "cyan4", "darkorange")) +
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 12))+ #adicionando o tipo de fonte e tamanho
  theme(legend.position = "top") #posição da legenda (opções "top", "bottom") e tem mais coisas q dá pra fazer aqui

plot(fig5a)

fig5b<- ggplot(dados_sMS, aes(x = H, y = P, fill = A)) + 
  geom_boxplot() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x=("Soil depths"),y=expression("Porosity (%)")) +
  geom_text(data = tukeyp, aes(x = tukeyp$xx, y = tukeyp$yy, label = groups), vjust = -0.5)+
  scale_fill_manual(values = c("darkgreen", "cyan4", "darkorange")) +
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 12))+ #adicionando o tipo de fonte e tamanho
  theme(legend.position = "none") #posição da legenda (opções "top", "bottom") e tem mais coisas q dá pra fazer aqui


plot(fig5b)

fig5_A <- ggdraw() + draw_plot(fig5a) + draw_label("(A)", x = 0.95, y = 0.95, size = 12, fontface = "bold") #colocando a letrinha da legenda

fig5_B <- ggdraw() + draw_plot(fig5b) + draw_label("(B)", x = 0.95, y = 0.95, size = 12, fontface = "bold") #colocando a letrinha da legenda

graffig5<- plot_grid( fig5_A, fig5_B,ncol = 1) #criando a figura do grafico com as 3 figuras, ncol=1 (apenas 1 coluna)- um embaixo do outro

plot(graffig5) 

ggsave("graffig5.png",graffig5, width = 16, height = 10, units = "cm") ##ggsave salva a figura direto no diretório q vc está trabalhando, aí vc nomea a figura, coloca a extensão (pdf, svg, jpeg) e coloca as dimensões

