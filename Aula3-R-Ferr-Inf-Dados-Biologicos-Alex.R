#--Dados--
#Aleatorio ou determinista
#Aleatorio = n�p controla = x = vari�vel aleat�ria
#Histograma = Um tipo de gr�fico de barra. No eixo x a vari�vel x (aleat�ria)
#1� Com os dados coletados e em m�os, ESCOLHER UMA DISTRIGUI��O DE FREQU�NCIA
#                                   --> POISSON --> BINOMIAL --> NORMAL
#Tipos de dados: Discretos (inteiros) e Continuos (decimais)
#Para dados discretos = Poisson e Binomial
#                     --> Binomial = N�o conhecemos o tamanho amostral
#                     --> Poisson = XXXX / Binomial = Dado de ocrrencia (Presente/Ausente - Escolha: Isso/aquilo (2))
#                     --> Poisson = Sabemos a quantidade = n�mero do tamanho amostral

#Para dados continuos = Normal

par(mfrow=c(1,2))
num=0:6
n=60 #amostras
freq_obs=c(3,12,17,13,9,3,3)
b=barplot(freq_obs,xlab = "N� Nematoides", ylab = "Freq. Nematoides", ylim=c(0,20), xlim=c(0,6))
labels=num
axis(1,at=b,labels=labels)
total=sum(freq_obs*num) 
lambda=total/n
 
rp=dpois(num,lambda) #Probabilidade esperada!

dif=(1-sum(rp))
dif=(1-sum(rp))/length(num) #length(num) acerta os dados para 1 para poder usar o X Quadrado

rp=rp+dif
sum(rp)
freq_esp=rp*n
b=barplot(freq_esp,xlab = "N� Nematoides", ylab = "Freq. esperada Nematoides", ylim=c(0,20), xlim = c(0,8))
labels=num
axis(1,at=b,labels=labels)
chisq.test(freq_obs,p=rp) #chisq.test(obs/exp) = Teste X Quadrado

####Distribui��o normal

d=read.table("asa.txt", header=T) #header = cabe�alho = T = True = N�o l� o cabe�alho como dado
d
summary(d)
attach(d)
asa
