# Mestrado_Jessica_Dias
Codigo no R utilizado para análises e gráficos, 05/09/2023.
Primeiro foi feita a analise de normalidade para os dados de densidade e de umidade por area e profundidades. Em seguida foi feito o teste de shapiro-wilk, teste da homogeneidade da variância, e em seguida o teste T. Os gráficos para os valores com diferença significativa também foram plotados.
Em seguida, Anova:
# Anova Para prof 0-20 cm   p= 0.0012 <0.05 - Significativo
# Para prof 20-40 cm; p= 0,0175 <0.05 Significativo
# Para prof 40-60 cm; p= 0,25 >0.05
Teste de tukey:
Para as profundidades de:
0-20 contrast    estimate     SE df t.ratio p.value
 A 0 - A 2,5    0.102 0.0655 19   1.563  0.2855 
 A 0 - AC       0.294 0.0676 19   4.352  0.0010 < 0,05 significativo
 A 2,5 - AC     0.192 0.0655 19   2.932  0.0222 < 0,05 significativo
 
 20-40:  contrast    estimate     SE df t.ratio p.value
 A 0 - A 2,5   0.0507 0.0476 19   1.066  0.5459
 A 0 - AC      0.1529 0.0491 19   3.111  0.0151 <0,05 significativo
 A 2,5 - AC    0.1021 0.0476 19   2.147  0.1068 
 
 40-60: contrast    estimate     SE df t.ratio p.value
 A 0 - A 2,5  -0.0221 0.0669 19  -0.331  0.9416
 A 0 - AC      0.0886 0.0691 19   1.282  0.4219
 A 2,5 - AC    0.1107 0.0669 19   1.655  0.2478

 Teste de shapiro wilk:
0-20 cm: p-value = 0.3526 >0,05 - é normal
20-40 cm : p-value = 0.44 >0,05 - é normal
40-60 cm : p-value = 0.49 >0,05 - é normal

Teste de bartlett homogeneidade:
#prof 0-20 :p-value = 0.9153 > 0,01 - é homogeneo???
#prof 20-40 :p-value = 0.7421 > 0,01 - é homogeneo???
#prof 40-60 :p-value = 0.11 > 0,01 - é homogeneo??
