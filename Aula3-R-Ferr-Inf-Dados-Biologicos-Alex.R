#--Dados--
#Aleatorio ou determinista
#Aleatorio = nãp controla = x = variável aleatória
#Histograma = Um tipo de gráfico de barra. No eixo x a variável x (aleatória)
#1º Com os dados coletados e em mãos, ESCOLHER UMA DISTRIGUIÇÃO DE FREQUÊNCIA
#                                   --> POISSON --> BINOMIAL --> NORMAL
#Tipos de dados: Discretos (inteiros) e Continuos (decimais)
#Para dados discretos = Poisson e Binomial
#                     --> Binomial = Não conhecemos o tamanho amostral
#                     --> Poisson = XXXX / Binomial = Dado de ocrrencia (Presente/Ausente - Escolha: Isso/aquilo (2))
#                     --> Poisson = Sabemos a quantidade = número do tamanho amostral

#Para dados continuos = Normal

par(mfrow=c(1,2))
num=0:6
n=60 #amostras
freq_obs=c(3,12,17,13,9,3,3)
b=barplot(freq_obs,xlab = "Nº Nematoides", ylab = "Freq. Nematoides", ylim=c(0,20), xlim=c(0,6))
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
b=barplot(freq_esp,xlab = "Nº Nematoides", ylab = "Freq. esperada Nematoides", ylim=c(0,20), xlim = c(0,8))
labels=num
axis(1,at=b,labels=labels)
chisq.test(freq_obs,p=rp) #chisq.test(obs/exp) = Teste X Quadrado

####Distribuição normal

d=read.table("asa.txt", header=T) #header = cabeçalho = T = True = Não lê o cabeçalho como dado
d
summary(d)
attach(d)
asa
