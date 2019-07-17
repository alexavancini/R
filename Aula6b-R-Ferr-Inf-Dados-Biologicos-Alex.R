setwd("D:/")
d1=read.table("Desempenho.txt", header = T)
d2=read.table("Estudo.txt", header = T)
d3=read.table("hidrolise.txt", header = T)
attach(d3)
d3

plot(D_fisico, D_Psicologico)
plot(Bioestatistica, Biofisica)
plot(tempo, hidrol)

cor.test(D_fisico, D_Psicologico)
cor.test(Bioestatistica, Biofisica)
cor.test(tempo, hidrol)
      
m=lm(hidrol~tempo) #lm = quando nossos dados são normais - ~ = em relação a...
summary(m)
p=predict(m)
p

lines(tempo,p)
r=residuals(m)
r
shapiro.test(r)

#Foi instalado o pacote hnp
require(hnp)

hnp(m,print=T)







      #Mulheres: t = 4.5274, df = 6, p-value = 0.003986 (Boa correlação p < 0.05)
      #Bioestatistica: t = -0.74002, df = 9, p-value = 0.4781 (Pouca correlação = p > 0.05)
      #Hidrolise: t = 34.57, df = 3, p-value = 5.322e-05, cor: 0.9987472


#    t = 
#    df = graus de liberdade
#    p-value = Valor de p (confiança)
#    cor = Correlação (ro)
      
#Igual ou diferente = bicaudal
#Maior ou menor = unicaudal
      
#Teste de correlação deste gráfico
      #H0: Auxencia de associação
      #Ha: Presença de associaçã
      
#Resultado: Há forte/fraco associação positiva/negativa entre o desempenho físico e psicológico das mulheres

    
#Gráfico ANOVA = boxplot?
  
#Residio: O que não conseguimos controlar

