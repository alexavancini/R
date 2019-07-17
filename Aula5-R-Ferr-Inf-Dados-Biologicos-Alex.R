setwd("D:/")
Orange=read.table("Orange.txt", header = T)
Orange
attach(Orange)
d[order(Tree,age,circumference),]
names(Orange)
                  #Variável dependente = X, quem vai causar variações no nosso objeto de estudo
                  #Variável independente é o nosso objeto de estudo, o = Y

par(mfrow=c(1,1),mar=c(6,6,6,6))

range(age)
range(circumference) #para saber os limites para poder colocar o xlim e ylim

plot(
     age,circumference, 
     xlab="Idade da Árvore", 
     ylab="Circunferência do tronco", 
     xlim=c(0,1700),
     ylim=c(0,250),
     main="Laranjeiras",
     font.main=2,
     col.main="orange",
     cex.lab=1,
     pch=6, #Seta o desenho do gráfico de pontos --> Conferir tabela na internet pch
     col.axis="red"
     )

points(500,100, pch=2, col="red") #Insere um ponto na orientação 500 no eixo X e 100 no eixo Y
#age por primeiro é o eixo X. Circumference em segundo é o eixo Y no gráfico.

abline(lm(circumference~age),lwd=2,col="green") #abline coloca uma linha. lwd=expessura da linha
abline(h=60) #insere uma linha horizontal na orientação 60 do eixo Y
abline(v=1000) #insere uma linha verticanl na orientação do eixo X

lines(age,circumference)
legend(locator(1),c("Dados"),pch=c(20), col = "green", cex=0.8)


###Gráfico de barras

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
          main="Barplot frequência dos insetos",
          ylab="Nº insetos",
          xlab="Média dos insetos em 4 regiões",
          ylim=c(0,3000),
          col = "red"
          )

labels=c("F1","F2", "F3", "F4") #Estabelece o valor dos labels
axis(1,at=b, labels = labels) #Chama e executa os labels

###colocar a barra de desvio padrão
bar=barplot(
          m,
          ylim = c(0,3000),
          ylab="Media e desvio padrão",
          col=c("green","blue","red","black")) #Lista de cores para as barras


arrows(                  #Arrows coloca as linhas pretas em cima do gráfico
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
nomes=c("insetos", "Mamíferos", "Anfíbios", "Aves", "Repteis")

pie(
        pedacos,
        main="Floresta",
        labels=c("69,39%","4.26%", "10.53%", "6.56%", "9.24%"),
        col=c("red", "green", "yellow", "black", "blue"),
        cex=2
   )


###Gráfico de 3 eixos PGR
install.packages("akima")
library(akima)
zz=interp(hay,pH,FR)

image(zz,col = topo.colors (42), ylab = "pH")