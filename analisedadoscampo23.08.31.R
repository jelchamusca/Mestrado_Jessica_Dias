#' ---
#' Título: Analises de dados de solos
#' Autor: Jessica Chamusca
#' Data: 05-09-2023
#' --- so testando p ver se salvou

getwd()
dir()
list.files()
library("tidyverse")
library("here")
dadoscampo <- read.csv2(file = "dados/DadoscampoV1.csv",header = TRUE, sep = ";", quote = "\"")
head(dadoscampo)
ncol(dadoscampo)
nrow(dadoscampo)
colnames(dadoscampo)
dim(dadoscampo)
str(dadoscampo)
summary(dadoscampo)
ggplot(data = dadoscampo, aes(x = EstC, fill = Transecto)) +
  geom_histogram(alpha = .4) +
  labs(title = "Com sobreposiçao")
ggplot(data = dadoscampo, aes(x = EstC, fill = Transecto)) +
  geom_density(alpha = .4) +
  labs(title = "Com sobreposiçao")
  ##Somar valor de EstC
soma <- dadoscampo %>% 
  select(Area, EstC) %>% 
  filter(Area == 'A0') # Para esta linha de código funcionar, a coluna Area 
# precisa ser um Fator.
soma <- sum(soma$EstC)
#Area 2,5
soma2 <- dadoscampo %>% 
  select(Area, EstC) %>% 
  filter(Area == 'A2,5') 
soma2 <- sum(soma2$EstC) 
#Area Controle
somac <- dadoscampo %>% 
  select(Area, EstC) %>% 
  filter(Area == 'AC') 
somac <- sum(somac$EstC) 
somas <- c("soma", "soma2", "somac")

  ##Grafico de barras do Estc
ggplot(data = dadoscampo, aes(x = Area, y = EstC, fill = Area)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Gráfico de Barras", x = "Areas", 
       y = "Estoque de Carbono no solo", fill = "Areas")
#Colocar label de somas
  
  ##Gráfico box splot das densidades
  ggplot(data = dadoscampo, aes(y = D, x = Area, fill = Area)) +
    geom_boxplot() +
  geom_jitter(size = .5)
  
#fazer filtro das profundidades

  ##Regressão linear
#Teste de normalidade
  ## Pacotes
  library(car)
  library(ggpubr)
  library(ggforce)
  library(lsmeans)
  library(lmtest)
  library(sjPlot)
  library(nlme)
  library(vegan)
  library(rdist)
  
  Densidade <- lm(D ~ Area, data = dadoscampo)
  qqPlot(Densidade)

  ## Teste de Shapiro-Wilk
  densidade_modelo <- residuals(Densidade)
  shapiro.test(densidade_modelo)
  ## Teste de homogeneidade de variância
  leveneTest(D ~ as.factor(Area), data = dadoscampo)
  # Pesquisar o que quer dizer esses teste.
  

  ## Análise Teste T 
  
#Teste T entre Ac e A2,5
  dadossemA0 <- dadoscampo %>% 
    dplyr::filter(Area %in% c("A2,5" , "AC"))
  head(dadossemA0)
  t.test(D ~ Area, data = dadossemA0, var.equal = FALSE)
#Welch Two Sample t-test
#data:  D by Area
#t = 4.4177, df = 27.968, p-value = 0.0001362
#alternative hypothesis: true difference in means between group A2,5 and group AC is not equal to 0
#95 percent confidence interval:
#0.1014980 0.2770217
#sample estimates:
#mean in group A2,5   mean in group AC 
# 1.0886946          0.8994347 

#Teste T entre Ac e A0
  dadossem2.5 <- dadoscampo %>% 
    dplyr::filter(Area %in% c("A0" , "AC"))
  head(dadossem2.5)
  t.test(D ~ Area, data = dadossem2.5, var.equal = FALSE)
  
#t = 5.042, df = 28.295, p-value = 2.406e-05
#alternative hypothesis: true difference in means between group A0 and group AC is not equal to 0
#95 percent confidence interval:
#0.1145933 0.2712955
#sample estimates:
#mean in group A0 mean in group AC 
#1.0923791        0.8994347  

  #Teste T entre A2,5 e A0
  dadossemAC <- dadoscampo %>% 
    dplyr::filter(Area %in% c("A0" , "A2,5"))
  head(dadossemAC)
  t.test(D ~ Area, data = dadossemAC, var.equal = FALSE)
  
  #t = 0.094307, df = 27.764, p-value = 0.9255
  #alternative hypothesis: true difference in means between group A0 and group AC is not equal to 0
  #95 percent confidence interval:
  # -0.07637754  0.08374667
  #sample estimates:
  # mean in group A0 mean in group A2,5 
  #1.092379           1.088695
  
  #Resumo: há diferença significativa entre A0 e AC, e entre A2,5 e AC, porém não existe entre A0 e A2,5.
  
  # Gráfico
  boxplotA0Ac <- ggplot(data = dadossem2.5, aes(x = Area, y = D, color = Area)) + 
    geom_boxplot(fill = c("darkorange", "cyan4"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    scale_color_manual(values = c("darkorange", "cyan4")) +
    labs(x = "Área", 
         y = expression(paste("Densidade Aparente")), size = 15) +
    theme(legend.position = "none")
  plot(boxplotA0Ac)
  
  boxplotACA2.5 <- ggplot(data = dadossemA0, aes(x = Area, y = D, color = Area)) + 
    geom_boxplot(fill = c("cyan4", "darkgreen"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    scale_color_manual(values = c("cyan4", "darkgreen")) +
    labs(x = "Área", 
         y = expression(paste("Densidade Aparente")), size = 15) +
    theme(legend.position = "none")
  plot(boxplotACA2.5)
  
  boxplotA0A2.5 <- ggplot(data = dadossemAC, aes(x = Area, y = D, color = Area)) + 
    geom_boxplot(fill = c("darkorange", "darkgreen"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    scale_color_manual(values = c("darkorange", "darkgreen")) +
    labs(x = "Área", 
         y = expression(paste("Densidade Aparente")), size = 15) +
    theme(legend.position = "none")
    plot(boxplotA0A2.5)
  
  ## Gráfico pareado com variáveis contínuas
  #trabalhar nessa tabela depois
  library("GGally")
  dadoscampo %>%
    dplyr::select(Area, Transecto, Prof) %>%
    GGally::ggpairs(aes(color = dadoscampo$Area)) +
    scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    theme_bw()
  ## Análise ANOVA da densidade
#Anova AC e A0  
  D_anova <- aov(D ~ Area, data = dadossem2.5)
  anova(D_anova)
  
 # Response: D
  #Df  Sum Sq  Mean Sq F value    Pr(>F)    
  #Area       1 0.30459 0.304589  26.012 1.611e-05 ***
  #Residuals 31 0.36299 0.011709                      
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  # Teste T da Umidade

  t.test(Ug ~ Area, data = dadossem2.5, var.equal = F)
  # Valor p-value = 2.972e-08 para Umidade entre AC e A0
  t.test(Ug ~ Area, data = dadossemAC, var.equal = F)
  # Valor p-value = 0.371 para Umidade entre A2,5 e A0
  t.test(Ug ~ Area, data = dadossemA0, var.equal = F)
  # Valor p-value = 4.359e-08 para Umidade entre A2,5 e AC
  