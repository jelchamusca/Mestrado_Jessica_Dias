#' ---
#' Título: Analises de dados de solos
#' Autor: Jessica Chamusca
#' Data: 05-09-2023
#' --- 

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
ggplot(data = dadoscampo, aes(x = D, fill = Area)) +
  geom_histogram(alpha = .4) +
  labs(title = "Com sobreposiçao")
ggplot(data = dadoscampo, aes(x = D, fill = Area)) +
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
dadosprof1 <- dadoscampo %>% 
  filter(Prof == '0-20')
  ggplot(data = dadosprof1, aes(y = D, x = Area, fill = Area)) +
    geom_boxplot() +
  geom_jitter(size = .5)
  
dadosprof2 <- dadoscampo %>% 
    filter(Prof == '20-40')
  ggplot(data = dadosprof2, aes(y = D, x = Area, fill = Area)) +
    geom_boxplot() +
    geom_jitter(size = .5)

dadosprof3 <- dadoscampo %>% 
    filter(Prof == '40-60')
  ggplot(data = dadosprof3, aes(y = D, x = Area, fill = Area)) +
    geom_boxplot() +
    geom_jitter(size = .5)


  #####Regressão linear
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
    #Teste de normalidade
  
  #Para o conjunto de dados geral, Densidade pela Area.
  DensidadeT <- lm(D ~ Area, data = dadoscampo)
  qqPlot(DensidadeT)
  
  #Para o conjunto de dados da profundidade 1 (0-20)cm, Densidade pela Area.
  Densidade1 <- lm(D ~ Area, data = dadosprof1)
  qqPlot(Densidade1)

  #Para o conjunto de dados profundidade 2 (20-40)cm, Densidade pela Area.
  Densidade2 <- lm(D ~ Area, data = dadosprof2)
  qqPlot(Densidade2)
  
  #Para o conjunto de dados profundidade 3 (40-60)cm, Densidade pela Area.
  Densidade3 <- lm(D ~ Area, data = dadosprof3)
  qqPlot(Densidade3)
  
  #Para o conjunto de dados geral, Umidade pela Area.
  UmidadeT <- lm(D ~ Area, data = dadoscampo)
  qqPlot(UmidadeT)
  
  #Para o conjunto de dados da profundidade 1 (0-20)cm, Umidade pela Area.
  Umidade1 <- lm(D ~ Area, data = dadosprof1)
  qqPlot(Umidade1)
  
  #Para o conjunto de dados profundidade 2 (20-40)cm, Umidade pela Area.
  Umidade2 <- lm(D ~ Area, data = dadosprof2)
  qqPlot(Umidade2)
  
  #Para o conjunto de dados profundidade 3 (40-60)cm, Umidade pela Area.
  Umidade3 <- lm(D ~ Area, data = dadosprof3)
  qqPlot(Umidade3)
  
    ## Teste de Shapiro-Wilk
  #Para todos os dados
  densidade_modelo <- residuals(DensidadeT)
  shapiro.test(densidade_modelo)
  #W = 0.99105, p-value = 0.9718
  
  #Para prof 1
  densidade_1 <- residuals(Densidade1)
  shapiro.test(densidade_1)
  #W = 0.90977, p-value = 0.1154
  
  #Para prof 2
  densidade_2 <- residuals(Densidade2)
  shapiro.test(densidade_2)
  #W = 0.95588, p-value = 0.5881
  
  #Para prof 3
  densidade_3 <- residuals(Densidade3)
  shapiro.test(densidade_3)
  #W = 0.98195, p-value = 0.9771
  
    ## Teste de homogeneidade de variância
  #Para todos os dados
  leveneTest(D ~ as.factor(Area), data = dadoscampo)
  #Levene's Test for Homogeneity of Variance (center = median)
  #Df F value Pr(>F)
  #group  2  0.0553 0.9463
  #45

  ## Análise Teste T 
  
#Teste T entre Ac e A2,5
  dadossemA0 <- dadoscampo %>% 
    dplyr::filter(Area %in% c("A2,5" , "AC"))
  head(dadossemA0)
  t.test(D ~ Area, data = dadossemA0, var.equal = FALSE)
#t = 4.4177, df = 27.968, p-value = 0.0001362

#Teste T entre Ac e A0
  dadossem2.5 <- dadoscampo %>% 
    dplyr::filter(Area %in% c("A0" , "AC"))
  head(dadossem2.5)
  t.test(D ~ Area, data = dadossem2.5, var.equal = FALSE)
#t = 5.042, df = 28.295, p-value = 2.406e-05

  #Teste T entre A2,5 e A0
  dadossemAC <- dadoscampo %>% 
    dplyr::filter(Area %in% c("A0" , "A2,5"))
  head(dadossemAC)
  t.test(D ~ Area, data = dadossemAC, var.equal = FALSE)
  #t = 0.094307, df = 27.764, p-value = 0.9255
 
  #Teste T entre Ac e A0 para prof1
  dadossem2.5prof1 <- dadosprof1 %>% 
    dplyr::filter(Area %in% c("A0" , "AC"))
  head(dadossem2.5prof1)
  t.test(D ~ Area, data = dadossem2.5prof1, var.equal = FALSE)
  #t = 4.0474, df = 8.2688, p-value = 0.003452
  
  #Teste T entre Ac e A0 para prof2
  dadossem2.5prof2 <- dadosprof2 %>% 
    dplyr::filter(Area %in% c("A0" , "AC"))
  head(dadossem2.5prof2)
  t.test(D ~ Area, data = dadossem2.5prof2, var.equal = FALSE)
  #t = 2.8224, df = 7.3706, p-value = 0.02435
  
  #Teste T entre Ac e A0 para prof3
  dadossem2.5prof3 <- dadosprof3 %>% 
    dplyr::filter(Area %in% c("A0" , "AC"))
  head(dadossem2.5prof3)
  t.test(D ~ Area, data = dadossem2.5prof3, var.equal = FALSE)
  #t = 2.2572, df = 7.7294, p-value = 0.05508
  
  #Teste T entre A2,5 e A0 prof1
  dadossemACprof1 <- dadosprof1 %>% 
    dplyr::filter(Area %in% c("A0" , "A2,5"))
  head(dadossemACprof1)
  t.test(D ~ Area, data = dadossemACprof1, var.equal = FALSE)
  #t = 1.0923, df = 7.6926, p-value = 0.3077
  
  #Teste T entre A2,5 e A0 prof2
  dadossemACprof2 <- dadosprof2 %>% 
    dplyr::filter(Area %in% c("A0" , "A2,5"))
  head(dadossemACprof2)
  t.test(D ~ Area, data = dadossemACprof2, var.equal = FALSE)
  #t = 0.4842, df = 8.9954, p-value = 0.6398
  
  #Teste T entre A2,5 e A0 prof3
  dadossemACprof3 <- dadosprof3 %>% 
    dplyr::filter(Area %in% c("A0" , "A2,5"))
  head(dadossemACprof3)
  t.test(D ~ Area, data = dadossemACprof3, var.equal = FALSE)
  #t = -1.0307, df = 5.4752, p-value = 0.346
  
  #Teste T entre Ac e A2,5 prof1
  dadossemA0prof1 <- dadosprof1 %>% 
    dplyr::filter(Area %in% c("A2,5" , "AC"))
  head(dadossemA0prof1)
  t.test(D ~ Area, data = dadossemA0prof1, var.equal = FALSE)
  #t = 3.8254, df = 5.6577, p-value = 0.009735
  
  #Teste T entre Ac e A2,5 prof2
  dadossemA0prof2 <- dadosprof2 %>% 
    dplyr::filter(Area %in% c("A2,5" , "AC"))
  head(dadossemA0prof2)
  t.test(D ~ Area, data = dadossemA0prof2, var.equal = FALSE)
  #t = 2.3987, df = 6.3164, p-value = 0.05133
  
  #Teste T entre Ac e A2,5 prof3
  dadossemA0prof3 <- dadosprof3 %>% 
    dplyr::filter(Area %in% c("A2,5" , "AC"))
  head(dadossemA0prof3)
  t.test(D ~ Area, data = dadossemA0prof3, var.equal = FALSE)
  #t = 2.3837, df = 6.4346, p-value = 0.0517
  
  #######Resumo: há diferença significativa entre A0 e AC, e entre A2,5 e AC, apenas na prof 0-20.
  #Não existe diferença significativa entre A0 e A2,5.
  
  
  # Gráfico densidade
  #Boxplot da A0 e AC em 0-20
  boxplotA0Ac <- ggplot(data = dadossem2.5prof1, aes(x = Area, y = D, color = Area)) + 
    geom_boxplot(fill = c("darkorange", "cyan4"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    scale_color_manual(values = c("darkorange", "cyan4")) +
    labs(x = "Área", 
         y = expression(paste("Densidade Aparente")), size = 15) +
    theme(legend.position = "none")
  plot(boxplotA0Ac)
  
  #Boxplot da A2,5 e AC em 0-20
  boxplotACA2.5 <- ggplot(data = dadossemA0prof1, aes(x = Area, y = D, color = Area)) + 
    geom_boxplot(fill = c("cyan4", "darkgreen"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    scale_color_manual(values = c("cyan4", "darkgreen")) +
    labs(x = "Área", 
         y = expression(paste("Densidade Aparente")), size = 15) +
    theme(legend.position = "none")
  plot(boxplotACA2.5)
  
  boxplotprof1 <- ggplot(data = dadosprof1, aes(x = Area, y = D, color = Area)) + 
    geom_boxplot(fill = c("darkorange", "darkgreen","cyan4"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    labs(x = "Área", 
         y = expression(paste("Densidade Aparente na profundidade 0-20cm")), size = 15) +
    theme(legend.position = "none")
    plot(boxplotprof1)
  
  ## Gráfico pareado com variáveis contínuas
  #trabalhar nessa tabela depois
  library("GGally")
  dadoscampo %>%
    dplyr::select(Area, Transecto, Prof) %>%
    GGally::ggpairs(aes(color = dadoscampo$Area)) +
    scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    theme_bw()
  
  
  # Teste T da Umidade
#para os dados gerais
  t.test(Ug ~ Area, data = dadossem2.5, var.equal = F)
  # Valor p-value = 2.972e-08 para Umidade entre AC e A0
  t.test(Ug ~ Area, data = dadossemAC, var.equal = F)
  # Valor p-value = 0.371 para Umidade entre A2,5 e A0
  t.test(Ug ~ Area, data = dadossemA0, var.equal = F)
  # Valor p-value = 4.359e-08 para Umidade entre A2,5 e AC
#para os dados por profundidade
  t.test(Ug ~ Area, data = dadossem2.5prof3, var.equal = F)
  # Valor p-value = 0.001838 para Umidade entre AC e A0, prof 0-20 - significativo
  # Valor p-value = 0.0009023 para Umidade entre AC e A0, prof 20-40  - significativo
  # Valor p-value = 0.003265 para Umidade entre AC e A0, prof 40-60 - significativo
  t.test(Ug ~ Area, data = dadossemACprof3, var.equal = F)
  # Valor p-value = 0.8429 para Umidade entre A2,5 e A0 prof 0-20
  # Valor p-value = 0.8044 para Umidade entre A2,5 e A0 prof 20-40
  # Valor p-value = 0.2555 para Umidade entre A2,5 e A0 prof 40-60
  t.test(Ug ~ Area, data = dadossemA0prof3, var.equal = F)
  # Valor p-value = 0.002609 para Umidade entre A2,5 e AC prof 0-20 - significativo
  # Valor p-value = 0.0008099 para Umidade entre A2,5 e AC prof 20-40 - significativo
  # Valor p-value =0.002759 para Umidade entre A2,5 e AC prof 40-60 - significativo
  
  boxplotprof1Ug <- ggplot(data = dadosprof1, aes(x = Area, y = Ug, color = Area)) + 
    geom_boxplot(fill = c("darkorange", "darkgreen","cyan4"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    labs(x = "Área", 
         y = expression(paste("Umidade na profundidade 0-20cm")), size = 15) +
    theme(legend.position = "none")
  plot(boxplotprof1Ug)
  
  boxplotprof2Ug <- ggplot(data = dadosprof2, aes(x = Area, y = Ug, color = Area)) + 
    geom_boxplot(fill = c("darkorange", "darkgreen","cyan4"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    labs(x = "Área", 
         y = expression(paste("Umidade na profundidade 20-40cm")), size = 15) +
    theme(legend.position = "none")
  plot(boxplotprof2Ug)
  
  boxplotprof3Ug <- ggplot(data = dadosprof3, aes(x = Area, y = Ug, color = Area)) + 
    geom_boxplot(fill = c("darkorange", "darkgreen","cyan4"), width = 0.5, 
                 color = "black", outlier.shape = NA, alpha = 0.7) +
    geom_jitter(shape = 20, position = position_jitter(0.2), color = "black", cex = 5) +
    labs(x = "Área", 
         y = expression(paste("Umidade na profundidade 40-60cm")), size = 15) +
    theme(legend.position = "none")
  plot(boxplotprof3Ug)
  
  
  ## Análise ANOVA da densidade
  #Anova AC e A0  
  D_anova <- aov(D ~ Area, data = dadossem2.5)
  anova(D_anova)
  
  # Response: D
  #Df  Sum Sq  Mean Sq F value    Pr(>F)    
  #Area       1 0.30459 0.304589  26.012 1.611e-05 ***
  #Residuals 31 0.36299 0.011709                      
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  