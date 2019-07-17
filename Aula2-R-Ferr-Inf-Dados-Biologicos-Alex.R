#Dois tipo de dados: Discreto e contínuos
#Discretos: Nº inteiros
#Contíunos: Decimais

#Distribuição normal: Trabalhar com dados contínuos
#Função: dnorm

#Função exponencial [positiva e negativa]: Dados discretos

#Normal - LM
#Binomial - GLM (Quando se sabe o total)
#Poisson - GLM (Quando não se sabe o total)

#Media = Media aritimética
#Função: mean()

#Sigma = Desvio padrão
#função sd()

#Lambida = Taxa de ocorrência (Quantas vezes ocorreu pela (/) a quantidade de vezes que foi amostrado)


d=read.table("tibia.txt", header=T)
d
summary(d)
attach(d)
hist(tibia)
hist(tibia, breaks = 50)
hist(tibia, breaks = 50, ylab = "Frequência", xlab = "Medida da Tibia")
hist(tibia, breaks = 50, ylab = "Frequência", xlab = "Medida da Tibia", xlim = c(0.14,0.40))
hist(tibia, breaks = 50, ylab = "Frequência", xlab = "Medida da Tibia", xlim = c(0.14,0.40), main="Histograma da Tibia")
hist(tibia, breaks = 50, ylab = "Frequência", xlab = "Medida da Tibia", xlim = c(0.14,0.40), main="Histograma da Tibia", col = "darkgray")
?hist
hist(tibia, breaks = 3, ylab = "Frequência", xlab = "Medida da Tibia", xlim = c(0.14,0.40), main="Histograma da Tibia", col=c("darkgray", "blue","green"))
sd=sd(tibia)
m=mean(tibia)

x=seq(0.14,0.4,0.01)
x
labels=x
axis(1,at=b, labels=labels)

rn=dnorm(x, m, sd)
rn
b=barplot(rn,xlab = "Variável", ylab = "Freq. Esp.", ylim =c(0,10))

rp=pnorm(x,m,sd)
rp
b=barplot(rp,xlab = "Variável", ylab = "Freq. Esp.", ylim =c(0,1))

###Distribuição binomial
p=0.5
n=10
x=0:10
rb=dbinom(x,n,p)
rb
b=barplot(rb,xlab = "Variável", ylab = "Freq. Esp.", ylim =c(0,10), xlim=c(0,10))

##Dist. poisson
x=0:10
lambda=4
rpo=dpois(x,lambda)
b=barplot(rpo,xlab = "Variável", ylab = "Probabilidade", ylim =c(0,4))
labels=x
axis(1,at=b, labels=labels)

freq_esp=n*rpo
freq_esp
labels=x
axis(1,at=b, labels=labels)
b=barplot(freq_esp,xlab = "Variável", ylab = "Probabilidade", ylim =c(0,4)xlim=c(0,10))
