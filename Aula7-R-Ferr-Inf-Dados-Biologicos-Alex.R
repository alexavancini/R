#test T --> Testa a media
#Var Test --> Testa a variância
#0:90 --> Faz uma contagem de 0 a 90
#hnp --> calcula a homicetastidade --> gráfico prob. "meio normal" simulado em envelope.
#Tornar uma curva concova uma reta é só trabalhar com ela na fomracriativa

setwd("D:/")
d=read.table("dim.txt", header=T)
d
attach(d)
summary(d)

plot(xv,yv)
m=lm(yv~xv) #lm --> regreção
summary(m)

x=0:90
y=predict(m,list(xv=x))
y
lines(x,y)
r=residuals(m)
r

plot(density(r))
plot(m)

shapiro.test(r)

install.packages("hnp")
require(hnp)
hnp(m,print=T)


m1=lm(yv~xv+I(xv^2))
summary(m1)
