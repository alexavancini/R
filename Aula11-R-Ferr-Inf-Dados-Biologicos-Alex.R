#An�lise de covari�ncia ANCOVA
#O que � regre��o? --> An�lise de...
#Y = var. resposta
#X = var. explanatoria
#E = erro
#x~Y+E

#X = informa tipo de an�lise
#Y = informa tipo de fam�lia distribui��o




#Primeira coisa que se faz �: Eles s�o cont�nuos ou discretos? (Para conseguir saber que teste usar)
#Segunda coisa: Olhar a ou as vari�veis explhanat�rias para saber quais testes utilizar.
                #Categ�ricas = ANOVA ou an�lise de desvio
                #Dados s�o um mix = cont�nua e categ�ricas = ANCOVA (Para ANCOVA os dados devem ser independentes; erro tem que ser normal e vari�ncia constante)

#Continuo = Distribui��o normal
#discreto = 2 principais: Binomial (Verdadeiro/Falso, ou Vivo/Moro, ou Macho/F�mea, etc) e poasson (Contagem e propor��p, falha ou sucesso)

#obs: Regress�o linear = An�lise de regress�o na normal
#obs: Regress�o log�stica = Regress�o na binomial
#obs: Regress�o log linear = Regress�o em poisson
#obs: An�lise de desvio = Na binomial e poisson
#obs: An�lise em bin erro poiss = an�lise binomial na Ancova.

#Modelos lineares = Regress�o, Anova e Ancova (Em R)


#=======================================
#An�lise de covar�ncia combina elementros de regress�o com covari�ncia.
#Na ANCOVA o procedimento se d� da seguinte forma
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
  ylab = "Produ��o do fruto",
  xlab = "Tamanho da raiz",
  ylim = c(0,120)
  )
#Vem o gr�fico sem os resultados por conta do type=n

fr=split(Fruto,Herbivoro) #fr = split = divide os dados do fruto com o herbivoro e divide o herbivoro nos tipos de herbivoro que a tabela apresenta, neste caso, presente e ausente. isto se d� gra�as ao comando split.
rz=split(Raiz,Herbivoro)
points(rz[[1]],fr[[1]],pch=19,col="black") #rz1 = grupo semparado pelo split de raiz herbivoro ausente, fr1= grupo dos frutos herbivoro ausente.
points(rz[[2]],fr[[2]],pch=1,col="black") #rz1  e fr2 s�o os dois grupos com herbivoro presente
                                          #o comando points colocou pontos emcima dos dados separados que o split separou

tapply(Fruto,Herbivoro,mean)

m=lm(Fruto~Herbivoro*Raiz) #Entre outras coisas nos dar� o valor de p que ser� maior ou menor que 0.05 por exemplo, significativo ou n�o significativo.
#Fruto=Vari�vel resposta
#Herbivoro=Vari�vel explanat�ria
summary(m)     #summary normal
summary.aov(m) #(aov=anova)


m1=lm(Fruto~Herbivoro*Raiz)
m1=update(m,~.-Herbivoro:Raiz)
#Comando update repete o modelo retirando determinado dado, neste exemplo foi retirado Herbivoro porque foi demonstrado anteriormente que era n�o era significativo para estra an�lise
summary(m1)
summary.aov(m1)

predict(m1)


#Vamos criar um novo vetor para poder fazer uma reta cont�nua

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

