setwd("D:/")
Orange=read.table("Orange.txt", header = T)
Orange
attach(Orange)
d[order(Tree,age,circumference),]
names(Orange)
                  #Vari�vel dependente = X, quem vai causar varia��es no nosso objeto de estudo
                  #Vari�vel independente � o nosso objeto de estudo, o = Y

par(mfrow=c(1,1),mar=c(6,6,6,6))

range(age)
range(circumference) #para saber os limites para poder colocar o xlim e ylim

plot(
     age,circumference, 
     xlab="Idade da �rvore", 
     ylab="Circunfer�ncia do tronco", 
     xlim=c(0,1700),
     ylim=c(0,250),
     main="Laranjeiras",
     font.main=2,
     col.main="orange",
     cex.lab=1,
     pch=6, #Seta o desenho do gr�fico de pontos --> Conferir tabela na internet pch
     col.axis="red"
     )

points(500,100, pch=2, col="red") #Insere um ponto na orienta��o 500 no eixo X e 100 no eixo Y
#age por primeiro � o eixo X. Circumference em segundo � o eixo Y no gr�fico.

abline(lm(circumference~age),lwd=2,col="green") #abline coloca uma linha. lwd=expessura da linha
abline(h=60) #insere uma linha horizontal na orienta��o 60 do eixo Y
abline(v=1000) #insere uma linha verticanl na orienta��o do eixo X

lines(age,circumference)
legend(locator(1),c("Dados"),pch=c(20), col = "green", cex=0.8)


###Gr�fico de barras

m=c(2345,1356,2425,1563)
m
sd=c(365,290,389,305)
sd
par(
    mfrow=c(1,2),
    mar=c(4,4,4,4), 
    cex.lab=1, 
    cex.axis=1
    )

b=barplot(
          m,
          main="Barplot frequ�ncia dos insetos",
          ylab="N� insetos",
          xlab="M�dia dos insetos em 4 regi�es",
          ylim=c(0,3000),
          col = "red"
          )

labels=c("F1","F2", "F3", "F4") #Estabelece o valor dos labels
axis(1,at=b, labels = labels) #Chama e executa os labels

###colocar a barra de desvio padr�o
bar=barplot(
          m,
          ylim = c(0,3000),
          ylab="Media e desvio padr�o",
          col=c("green","blue","red","black")) #Lista de cores para as barras


arrows(                  #Arrows coloca as linhas pretas em cima do gr�fico
          bar,
          m-sd, 
          bar, 
          m+sd, 
          length=0.1,
          angle=90,
          code=3,
          col="blue"
      )

###Boxplot
trees=read.table("tree.txt",header = T)
trees
names(trees)

boxplot(
        trees[,1:3],
        col.main="blue",
        main="Trees",
        font.main=3,
        col=c("green", "yellow", "white"),
        pch=19
       )

legend(
        "topright", 
        fill=c("green","yellow","white"),
        c("Girth","Height","Volume"),
        bty="n"
      )

###Pizza
pedacos=c(2358,145,358,223,314)
nomes=c("insetos", "Mam�feros", "Anf�bios", "Aves", "Repteis")

pie(
        pedacos,
        main="Floresta",
        labels=c("69,39%","4.26%", "10.53%", "6.56%", "9.24%"),
        col=c("red", "green", "yellow", "black", "blue"),
        cex=2
   )


###Gr�fico de 3 eixos PGR
install.packages("akima")
library(akima)
zz=interp(hay,pH,FR)

image(zz,col = topo.colors (42), ylab = "pH")