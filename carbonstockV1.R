#' ---
#' Título: Analises de dados carbono
#' Autor: Jessica Chamusca
#' Data: 06-02-2024

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
library(rstatix)
library(ggplot2)
library(multcomp)
library(agricolae)

### cOMO DEIXAR O GRAFICO BONITO:
library(cowplot)
library(forcats)
library(RColorBrewer)
library(svglite)   

dados <- read_csv2(file = "dados/dadosR.csv")
dados$A <- factor(dados$A, levels = c("NC", "FSP", "CSP"))

tps <- data.frame( XX = c(0.8,1.7,2.8),
  yy = c(800, 800, 800),
  groups = c("a","b","b"),
  A = c("NC","FSP",'CSP')
)
  
fig6 <- ggplot(data = dados, aes(x = A, y = Ps, fill = A)) +
  geom_violin(width = .5, show.legend = FALSE) +
  geom_jitter(alpha = .4, show.legend = FALSE, 
              position = position_jitter(width = .15, seed = 0)) +
  geom_boxplot(width = 0.07)+
  scale_fill_manual(values = c("darkgreen", "cyan4","darkorange" )) +
  stat_summary(fun = mean, geom = "point", shape = 3, size = 3, color = "black")+
  geom_text(data = tps, aes(x = XX, y = yy, label = groups), vjust = -0.5)+
  labs(x = "LUS", y = "Dry Mass (g/m²)", fill=" ") +
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 12))+ #adicionando o tipo de fonte e tamanho
  theme(legend.position = "top") 

plot(fig6)

graf6<- plot_grid(fig6, ncol = 1) 

plot(graf6) 

ggsave("graf6.png",graf6, width = 18, height = 10, units = "cm") 

dadosMS <- dados %>%
  dplyr::filter(H %in% c("MS"))

dadosprof1 <- dados %>%
  dplyr::filter(H %in% c("0-20"))

dadosprof2 <- dados %>%
  dplyr::filter(H %in% c("20-40"))

dadosprof3 <- dados %>%
  dplyr::filter(H %in% c("40-60"))

dadosprof4 <- dados %>%
  dplyr::filter(H %in% c("60-80"))

dadosprof5 <- dados %>%
  dplyr::filter(H %in% c("80-100"))
head(dadosMS)
summary(dadosprof5)

fig6b <- ggplot(data = dadosprof5, aes(x = A, y = EC, fill = A)) +
  geom_violin(width = .5, show.legend = FALSE) +
  geom_jitter(alpha = .4, show.legend = FALSE, 
              position = position_jitter(width = .15, seed = 0)) +
  geom_boxplot(width = 0.01)+
  scale_fill_manual(values = c("darkgreen","cyan4","darkorange")) +
  labs(x = "LUS", y = "Dry mass Carbon Stock (Mg/ha)", fill = "A") +
  stat_summary(fun = mean, geom = "point", shape = 3, size = 3, color = "black")+
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 14))+ #adicionando o tipo de fonte e tamanho
  theme(legend.position = "none") 
plot(fig6b)

graf6<- plot_grid(fig6, ncol = 1) #criando a figura do grafico com as 3 figuras, ncol=1 (apenas 1 coluna)- um embaixo do outro

plot(grafD) 

ggsave("grafD.png",grafPS, width = 18, height = 10, units = "cm") ##ggsave salva a figura direto no diretório q vc está trabalhando, aí vc nomea a figura, coloca a extensão (pdf, svg, jpeg) e coloca as dimensões



### EC total por area das 10 trincheiras
dadosestoque <- read.csv2(file = "dados/ectotal.csv",header = TRUE, sep = ";", quote = "\"")


ec_t<- aov(ss ~ A, data = dadosestoque)

summary(ec_t)

#Residuos

plot(resid(ec_t)~fitted(ec_t))
resid(ec_t)
#Teste de shapiro wilk ( normalidade): deve ser >0,05)
shapiro.test(resid(ec_t))

#homogeneidade (deve ser > 0,05, se n não pode fazer anova)
bartlett.test(ss ~ A, data = dadosestoque)

#Teste de tukey:
lsmeans(ec_t, pairwise~A, adjust="tukey")

modelo_anova <- aov(log(ss) ~ A, data = dadosestoque)
summary(modelo_anova)

# Teste de Tukey
# Resultados do Teste de Tukey 1
rtk1 <- HSD.test(modelo_anova, "A", group = TRUE)
head(rtk1)

## COLOCANDO LETRINHAS
tpsEC <- data.frame(
  XX = c(0.7,1.7,2.8),
  yy = c(150, 150, 150),
  groups = c("a","a","a"),
  A = c("CSP","FSP",'NC')
)
dadosestoque$A <- factor(dadosestoque$A, levels = c("CSP", "FSP", "NC"))

fig7 <- ggplot(data = dadosestoque, aes(x = A, y = ss, fill = A)) +
  geom_violin(width = .5, show.legend = FALSE) +
  geom_boxplot(width = 0.2,outlier.shape = NA)+
  geom_jitter(alpha = .4, show.legend = FALSE, 
              position = position_jitter(width = .15, seed = 0)) +
  scale_fill_manual(values = c("darkorange","cyan4","darkgreen")) +
  labs(x = "LUS", y = "Carbon Stock (Mg/ha)", fill = "A") +
  geom_text(data = tpsEC, aes(x = XX, y = yy, label = groups), vjust = -0.5)+
  stat_summary(fun = mean, geom = "point", shape = 3, size = 3, color = "white")+
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 14))+ #adicionando o tipo de fonte e tamanho
  theme(legend.position = "none") 
plot(fig7)
graf7<- plot_grid(fig7, ncol = 1) #criando a figura do grafico com as 3 figuras, ncol=1 (apenas 1 coluna)- um embaixo do outro

plot(graf7) 

ggsave("graf7.png",graf7, width = 18, height = 10, units = "cm") ##ggsave salva a figura direto no diretório q vc está trabalhando, aí vc nomea a figura, coloca a extensão (pdf, svg, jpeg) e coloca as dimensões


###fig 8 BARRAS
## Calcular o desvio padrão por camada

dados.media1 <- dadosprof1 %>%
  dplyr::group_by(A) %>%
  dplyr::summarise(media = mean(EC, na.rm = TRUE),
                   desvio = sd(EC, na.rm = TRUE))
dados.media1$H <- c("B", "B", "B")

dados.media2 <- dadosprof2 %>%
  dplyr::group_by(A) %>%
  dplyr::summarise(media = mean(EC, na.rm = TRUE),
                   desvio = sd(EC, na.rm = TRUE))
dados.media2$H <- c("C", "C", "C")

dados.media3 <- dadosprof3 %>%
  dplyr::group_by(A) %>%
  dplyr::summarise(media = mean(EC, na.rm = TRUE),
                   desvio = sd(EC, na.rm = TRUE))
dados.media3$H <- c("D", "D", "D")

dados.media4 <- dadosprof4 %>%
  dplyr::group_by(A) %>%
  dplyr::summarise(media = mean(EC, na.rm = TRUE),
                   desvio = sd(EC, na.rm = TRUE))
dados.media4$H <- c("E", "E", "E")

dados.media5 <- dadosprof5 %>%
  dplyr::group_by(A) %>%
  dplyr::summarise(media = mean(EC, na.rm = TRUE),
                   desvio = sd(EC, na.rm = TRUE))
dados.media5$H <- c("F", "F", "F")

dados.mediaMS <- dadosMS %>%
  dplyr::group_by(A) %>%
  dplyr::summarise(media = mean(EC, na.rm = TRUE),
                   desvio = sd(EC, na.rm = TRUE))
dados.mediaMS$H <- c("A", "A", "A")
head(dados.media1)

### gráfico por trincheiras
dados$A <- factor(dados$A, levels = c("Nc", "FSP", "CSP"))
fig7a <- dados %>%
  group_by(T, O) %>%
  ggplot(aes(x = T, y = EC, fill = O)) +
  scale_fill_manual(values = c("#827717", "orangered4", "sienna4","#4E342E","sienna","#6D4C41"), 
                    labels = c("MS","0-20","20-40","40-60","60-80", "80-100")) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(y = "Carbon Stock (Mg/Ha)", 
       x = "Tricheiras", 
       fill = "Camadas") +
  geom_bar(stat = "summary", fun = "mean", color = "black", width = 0.6, alpha = 0) +
  labs(x=("Trenches"),y=expression("Carbon stock (Mg.ha"^-1*")"), fill = "Layers") +
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 14))+ #adicionando o tipo de fonte e tamanho
  theme(legend.position = "top")

fig7a <- ggdraw() + draw_plot(fig7a)

plot(fig7a) 
grafT<- plot_grid(fig7a, ncol = 1) #criando a figura do grafico com as 3 figuras, ncol=1 (apenas 1 coluna)- um embaixo do outro

plot(grafT) 

ggsave("grafT.png",grafT, width = 22, height = 10, units = "cm") ##ggsave salva a figura direto no diretório q vc está trabalhando, aí vc nomea a figura, coloca a extensão (pdf, svg, jpeg) e coloca as dimensões



## Gráfico de barras com desvio padrão

dados_combinados <- rbind(dados.media1, dados.media2, dados.media3, dados.media4,dados.media5, dados.mediaMS)
head(dados_combinados)
dim(dados_combinados)

## Graficos de barras do estoque

fig8<-dados_combinados %>%
  group_by(A, H) %>%
  ggplot(aes(x = A, y = media, fill = H)) +
  scale_fill_manual(values = c("#827717", "orangered4","orange3","#4E342E","sienna", "brown"),
                    labels = c("MS","0-20","20-40","40-60","60-80", "80-100")) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_bar(stat = "summary", fun = "mean", color = "black", width = 0.7, alpha = 0) +
  labs(x=("LUS"),y=expression("Carbon stock (Mg.ha"^-1*")"), fill = "Layers") +
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 12))+ #adicionando o tipo de fonte e tamanho
  theme(legend.position = "top")

fig8 <- ggdraw() + draw_plot(fig7b)

graf8<- plot_grid(fig8, ncol = 1) #criando a figura do grafico com as 3 figuras, ncol=1 (apenas 1 coluna)- um embaixo do outro

plot(graf8) 

ggsave("fig8.png",graf8, width = 18, height = 10, units = "cm") ##ggsave salva a figura direto no diretório q vc está trabalhando, aí vc nomea a figura, coloca a extensão (pdf, svg, jpeg) e coloca as dimensões


### EC fig 9
####### para colocar as letrinhas
## ANOVA
modelo_anova <- aov(EC ~ A, data = dados%>%
                      filter(H == "0-20"))
rt1 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(EC ~ A, data = dados%>%
                      filter(H == "20-40"))
rt2 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(EC ~ A, data = dados%>%
                      filter(H == "40-60"))
rt3 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(EC ~ A, data = dados%>%
                      filter(H == "60-80"))
rt4 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(EC ~ A, data = dados%>%
                      filter(H == "80-100"))
rt5 <- HSD.test(modelo_anova, "A", group = TRUE)
modelo_anova <- aov(EC ~ A, data = dados%>%
                      filter(H == "MS"))
rtms <- HSD.test(modelo_anova, "A", group = TRUE)

summary(modelo_anova)
head(rtms)
# Teste de Tukey
# Resultados do Teste de Tukey 1

# Convertendo resultados de Tukey em dataframes
ec1t <- as.data.frame(rt1$groups)
ec1t$A <- rownames(ec1t)
ec1f <- ec1t %>%
  mutate(A = factor(A, levels = c("NC", "FSP", "CSP"))) %>%
  arrange(A)
ec2t <- as.data.frame(rt2$groups)
ec2t$A <- rownames(ec2t)
ec2f <- ec2t %>%
  mutate(A = factor(A, levels = c("NC", "FSP", "CSP"))) %>%
  arrange(A)
ec3t <- as.data.frame(rt3$groups)
ec3t$A <- rownames(ec3t)
ec3f <- ec3t %>%
  mutate(A = factor(A, levels = c("NC", "FSP", "CSP"))) %>%
  arrange(A)
ec4t <- as.data.frame(rt4$groups)
ec4t$A <- rownames(ec4t)
ec4f <- ec4t %>%
  mutate(A = factor(A, levels = c("NC", "FSP", "CSP"))) %>%
  arrange(A)
ec5t <- as.data.frame(rt5$groups)
ec5t$A <- rownames(ec5t)
ec5f <- ec5t %>%
  mutate(A = factor(A, levels = c("NC", "FSP", "CSP"))) %>%
  arrange(A)
ectms <- as.data.frame(rtms$groups)
ectms$A <- rownames(ectms)
ecmsf <- ectms %>%
  mutate(A = factor(A, levels = c("NC", "FSP", "CSP"))) %>%
  arrange(A)
head(ec1t)
tukeyec <- rbind(ec1f, ec2f, ec3f, ec4f, ec5f, ecmsf)
head(tukeyec)

tukeyec$xx <- c(0.7,1,1.3,1.7,2,2.3,2.7,3,3.3,3.7,4,4.3,4.7,5,5.3,5.7,6,6.3)
#### tukey$yy <- seq(from = (min(tukey$D, na.rm = TRUE)+0.15), to = (max(tukey$D, na.rm = TRUE)*0.95), by=0.1)
tukeyec$yy <- tukeyec$EC*1.3


dados$A <- factor(dados$A, levels = c("NC", "FSP", "CSP"))

fig9<- ggplot(dados, aes(x = H, y = EC, fill = A)) + 
  geom_boxplot() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  geom_text(data = tukeyec, aes(x = tukeyec$xx, y = tukeyec$yy, label = groups), vjust = -0.5)+
  scale_fill_manual(values = c("darkgreen", "cyan4", "darkorange")) +
  labs(x=("Layers"),y=expression("Carbon stock (Mg.ha"^-1*")"), fill = " ") +
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 12))+ #adicionando o tipo de fonte e tamanho
  theme(legend.position = "top") #posição da legenda (opções "top", "bottom") e tem mais coisas q dá pra fazer aqui

plot(fig9)

fig9 <- ggdraw() + draw_plot(fig9)

graffig9<- plot_grid( fig9, ncol = 1) #criando a figura do grafico com as 3 figuras, ncol=1 (apenas 1 coluna)- um embaixo do outro

plot(graffig9) 

ggsave("graffig9.png",graffig9, width = 20, height = 10, units = "cm") ##ggsave salva a figura direto no diretório q vc está trabalhando, aí vc nomea a figura, coloca a extensão (pdf, svg, jpeg) e coloca as dimensões
