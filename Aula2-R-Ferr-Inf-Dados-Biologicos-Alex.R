#Dois tipo de dados: Discreto e cont�nuos
#Discretos: N� inteiros
#Cont�unos: Decimais

#Distribui��o normal: Trabalhar com dados cont�nuos
#Fun��o: dnorm

#Fun��o exponencial [positiva e negativa]: Dados discretos

#Normal - LM
#Binomial - GLM (Quando se sabe o total)
#Poisson - GLM (Quando n�o se sabe o total)

#Media = Media aritim�tica
#Fun��o: mean()

#Sigma = Desvio padr�o
#fun��o sd()

#Lambida = Taxa de ocorr�ncia (Quantas vezes ocorreu pela (/) a quantidade de vezes que foi amostrado)


d=read.table("tibia.txt", header=T)
d
summary(d)
attach(d)
hist(tibia)
hist(tibia, breaks = 50)
hist(tibia, breaks = 50, ylab = "Frequ�ncia", xlab = "Medida da Tibia")
hist(tibia, breaks = 50, ylab = "Frequ�ncia", xlab = "Medida da Tibia", xlim = c(0.14,0.40))
hist(tibia, breaks = 50, ylab = "Frequ�ncia", xlab = "Medida da Tibia", xlim = c(0.14,0.40), main="Histograma da Tibia")
hist(tibia, breaks = 50, ylab = "Frequ�ncia", xlab = "Medida da Tibia", xlim = c(0.14,0.40), main="Histograma da Tibia", col = "darkgray")
?hist
hist(tibia, breaks = 3, ylab = "Frequ�ncia", xlab = "Medida da Tibia", xlim = c(0.14,0.40), main="Histograma da Tibia", col=c("darkgray", "blue","green"))
sd=sd(tibia)
m=mean(tibia)

x=seq(0.14,0.4,0.01)
x
labels=x
axis(1,at=b, labels=labels)

rn=dnorm(x, m, sd)
rn
b=barplot(rn,xlab = "Vari�vel", ylab = "Freq. Esp.", ylim =c(0,10))

rp=pnorm(x,m,sd)
rp
b=barplot(rp,xlab = "Vari�vel", ylab = "Freq. Esp.", ylim =c(0,1))

###Distribui��o binomial
p=0.5
n=10
x=0:10
rb=dbinom(x,n,p)
rb
b=barplot(rb,xlab = "Vari�vel", ylab = "Freq. Esp.", ylim =c(0,10), xlim=c(0,10))

##Dist. poisson
x=0:10
lambda=4
rpo=dpois(x,lambda)
b=barplot(rpo,xlab = "Vari�vel", ylab = "Probabilidade", ylim =c(0,4))
labels=x
axis(1,at=b, labels=labels)

freq_esp=n*rpo
freq_esp
labels=x
axis(1,at=b, labels=labels)
b=barplot(freq_esp,xlab = "Vari�vel", ylab = "Probabilidade", ylim =c(0,4)xlim=c(0,10))
