setwd("D:/")
tib=read.table("tibia.txt", header=T)
tib
attach(tib)
summary(tib)
#No summary, 1st Qu.: significa o "Primeiro quartil"
#No msummary, 3rd Qu.: significa o "Terceiro quartil"

###Medidas de posição
media=mean(tibia)
media
mediana=median(tibia)
mediana
#Moda: O R NÃO CALCULA A MODA
o=order(tibia, decreasing=F) #Para fazer crescente só alterar o atributo verdadeiro ou falso (T/F)
o
tibia[o]
variancia=var(tibia)
variancia
desvpad=sd(tibia) #sd=standard desviation
desvpad

###coeficiente de variação o R não calcula
CV=(desvpad/media)*100
CV

###Erro padrão = O desvio padrão "do mundo lá fora", uma amostra bem grande, que podemos dize que ela é representativa do mundo la fora
###Erro padrão o R não calcula
n=length(tibia)
errpad=desvpad/sqrt(n)
errpad

##Quartil (dividindo por 4 a distribuição dos dados)
quartil=quantile(tibia)
quartil

###Intervalo de confiância o R não calcula
##O valor pode ser escolhido de acordo com a pesquisa
#Utilizado neste código o valor de 95%
#Intervalo de confiancia:

ic=function(x, conf=0.95){
  n=length(x)
  media=mean(x)
  variancia=var(x)
  quantis=qt(c((1-conf)/2,1-(1-conf/2),df=n-1))
  ic=media+quantis*sqrt(variancia/n)
  return(ic)
}

ic(tibia)
