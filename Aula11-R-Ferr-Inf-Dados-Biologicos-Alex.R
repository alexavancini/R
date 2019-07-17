#Análise de covariância ANCOVA
#O que é regreção? --> Análise de...
#Y = var. resposta
#X = var. explanatoria
#E = erro
#x~Y+E

#X = informa tipo de análise
#Y = informa tipo de família distribuição




#Primeira coisa que se faz é: Eles são contínuos ou discretos? (Para conseguir saber que teste usar)
#Segunda coisa: Olhar a ou as variáveis explhanatórias para saber quais testes utilizar.
                #Categóricas = ANOVA ou análise de desvio
                #Dados são um mix = contínua e categóricas = ANCOVA (Para ANCOVA os dados devem ser independentes; erro tem que ser normal e variância constante)

#Continuo = Distribuição normal
#discreto = 2 principais: Binomial (Verdadeiro/Falso, ou Vivo/Moro, ou Macho/Fêmea, etc) e poasson (Contagem e proporçãp, falha ou sucesso)

#obs: Regressão linear = Análise de regressão na normal
#obs: Regressão logística = Regressão na binomial
#obs: Regressão log linear = Regressão em poisson
#obs: Análise de desvio = Na binomial e poisson
#obs: Análise em bin erro poiss = análise binomial na Ancova.

#Modelos lineares = Regressão, Anova e Ancova (Em R)


#=======================================
#Análise de covarância combina elementros de regressão com covariância.
#Na ANCOVA o procedimento se dá da seguinte forma
#Tipos de modelo
    #saturado
    #maximo
    #adequado
    #nulo

setwd("D:/")
d=read.table("frutos.txt", header = T)
d
attach(d)
summary(d)

plot(
  Raiz,
  Fruto,
  type = "n",
  ylab = "Produção do fruto",
  xlab = "Tamanho da raiz",
  ylim = c(0,120)
  )
#Vem o gráfico sem os resultados por conta do type=n

fr=split(Fruto,Herbivoro) #fr = split = divide os dados do fruto com o herbivoro e divide o herbivoro nos tipos de herbivoro que a tabela apresenta, neste caso, presente e ausente. isto se dá graças ao comando split.
rz=split(Raiz,Herbivoro)
points(rz[[1]],fr[[1]],pch=19,col="black") #rz1 = grupo semparado pelo split de raiz herbivoro ausente, fr1= grupo dos frutos herbivoro ausente.
points(rz[[2]],fr[[2]],pch=1,col="black") #rz1  e fr2 são os dois grupos com herbivoro presente
                                          #o comando points colocou pontos emcima dos dados separados que o split separou

tapply(Fruto,Herbivoro,mean)

m=lm(Fruto~Herbivoro*Raiz) #Entre outras coisas nos dará o valor de p que será maior ou menor que 0.05 por exemplo, significativo ou não significativo.
#Fruto=Variável resposta
#Herbivoro=Variável explanatória
summary(m)     #summary normal
summary.aov(m) #(aov=anova)


m1=lm(Fruto~Herbivoro*Raiz)
m1=update(m,~.-Herbivoro:Raiz)
#Comando update repete o modelo retirando determinado dado, neste exemplo foi retirado Herbivoro porque foi demonstrado anteriormente que era não era significativo para estra análise
summary(m1)
summary.aov(m1)

predict(m1)


#Vamos criar um novo vetor para poder fazer uma reta contínua

x=seq(0,11,0.1)
length(x)
phv=rep("ausente",111)
R=predict(m1,list(Herbivoro=factor(phv),Raiz=x))
lines(x,R,lty=2,lwd=3)


x=seq(0,11,0.1)
length(x)
phv=rep("presente",111)
R=predict(m1,list(Herbivoro=factor(phv),Raiz=x))
lines(x,R,lty=2,lwd=3)

