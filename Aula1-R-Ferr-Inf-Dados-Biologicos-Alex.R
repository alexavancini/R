d=read.table("tibia.txt",header=T) #nomea como 'd' o arquivo com os dados
d #após nomeado na linha cima, chama os dados do arquivo
attach(d)
tibia #chama os dados do arquivo
mean(tibia) #media
m=mean(tibia)
m
sd(tibia) #desvio padrão
sd=sd(tibia)
sd
o=ordered(tibia) #chama de 'o' a ordeação dos dados do arquivo chamado
o #chama o arquivo de forma ordenada, conforme nomeada na linha anterior
hist(tibia) #cria um histograma do arquivo dos dados
hist(tibia, breaks=50)
#dnorm(x,mean, sd)
x=seq(0.14,0.4,0.01)
x
rn=dnorm(x,m,sd)
rn
barplot(rn) #cria um barblot
b=barplot(rn, ylim=c(0,10), main="frequência") #'ylim=limite do eixo y' #'main = Título'
b #chama o barplot
label=x #????
axis(1,at=b, labels=label) #Insere os eixos no histograma

