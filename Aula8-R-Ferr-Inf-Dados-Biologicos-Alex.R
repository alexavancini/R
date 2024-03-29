#Aula do dia 23/05/19
#para ser normal: 1)Dados seres independentes; 2)Vari�ncia constante

#An�lise de vari�ncia = Quebra da vari�ncia para ver se tem erro de tratamento ou n�o
#Teste T compara M�dia!
#Teste F An�lise de vari�ncia, que analisa as duas vari�ncias: Aleat�ria e determin�stica




setwd("D:/")
d=read.table("peso_anova.txt", header=T)
d
attach(d)
summary(d)
plot(TRAT, PESO)

m=lm(PESO~TRAT)
m1=aov(PESO~TRAT)

summary(m)
summary(m1)

tapply(PESO,TRAT,mean) #Faz a m�dia dos tratamentos sem quebrar a matriz de dados.

plot(TukeyHSD(m1))
TukeyHSD(m1) #Compara a diferen�a
plot.design(PESO~TRAT)

##GRAFICO

med=tapply(PESO,TRAT,mean)
sd=tapply(PESO,TRAT,sd)
b=barplot(med,ylim=c(0,40))
arrows(b,med-sd,b,med+sd,length = 0.1,angle = 90,code = 3)

text(0.7,35,"a")
text(1.9,35, "ab")
text(3.1,35, "a")
text(4.3,35, "b")
#Comando text = coloca um texto aleat�rio no eixo x e y no gr�fico



