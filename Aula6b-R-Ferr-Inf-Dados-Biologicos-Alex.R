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
      
m=lm(hidrol~tempo) #lm = quando nossos dados s�o normais - ~ = em rela��o a...
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







      #Mulheres: t = 4.5274, df = 6, p-value = 0.003986 (Boa correla��o p < 0.05)
      #Bioestatistica: t = -0.74002, df = 9, p-value = 0.4781 (Pouca correla��o = p > 0.05)
      #Hidrolise: t = 34.57, df = 3, p-value = 5.322e-05, cor: 0.9987472


#    t = 
#    df = graus de liberdade
#    p-value = Valor de p (confian�a)
#    cor = Correla��o (ro)
      
#Igual ou diferente = bicaudal
#Maior ou menor = unicaudal
      
#Teste de correla��o deste gr�fico
      #H0: Auxencia de associa��o
      #Ha: Presen�a de associa��
      
#Resultado: H� forte/fraco associa��o positiva/negativa entre o desempenho f�sico e psicol�gico das mulheres

    
#Gr�fico ANOVA = boxplot?
  
#Residio: O que n�o conseguimos controlar

