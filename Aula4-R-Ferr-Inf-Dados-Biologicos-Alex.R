setwd("D:/")
tib=read.table("tibia.txt", header=T)
tib
attach(tib)
summary(tib)
#No summary, 1st Qu.: significa o "Primeiro quartil"
#No msummary, 3rd Qu.: significa o "Terceiro quartil"

###Medidas de posi��o
media=mean(tibia)
media
mediana=median(tibia)
mediana
#Moda: O R N�O CALCULA A MODA
o=order(tibia, decreasing=F) #Para fazer crescente s� alterar o atributo verdadeiro ou falso (T/F)
o
tibia[o]
variancia=var(tibia)
variancia
desvpad=sd(tibia) #sd=standard desviation
desvpad

###coeficiente de varia��o o R n�o calcula
CV=(desvpad/media)*100
CV

###Erro padr�o = O desvio padr�o "do mundo l� fora", uma amostra bem grande, que podemos dize que ela � representativa do mundo la fora
###Erro padr�o o R n�o calcula
n=length(tibia)
errpad=desvpad/sqrt(n)
errpad

##Quartil (dividindo por 4 a distribui��o dos dados)
quartil=quantile(tibia)
quartil

###Intervalo de confi�ncia o R n�o calcula
##O valor pode ser escolhido de acordo com a pesquisa
#Utilizado neste c�digo o valor de 95%
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
