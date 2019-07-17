setwd("D:/")
d=read.table("worms.txt",header=T)
d
#sub1=d[c]


sub1=d[,c(1,7)]
sub1

attach(sub1)

Worm.density #Mostra apenas os valores da última coluna com título Worm.density
Field.Name #Mostra apenas os valores da última coluna com título Field.Name

attach(d)
d
d[order(Slope),] #Ordena o data frame com base nos valores de Slope
d[order(Worm.density),] #Ordena o data frame com base nos valores de Worm.density
d[rev(order(Worm.density)),] #Ordena reversamente o data frame com base nos valores de Worm.density

d[order(Vegetation, Worm.density),] #Ordena o dataframe com base nas colinas dos dados de "Vegetation" e "Worm.density"
d[order(Vegetation, Worm.density),c(4,7,5,3)] #idem do de cima mas espefica apenas as tabelas que quer visualizar(4,7,5,3)

d[Damp==T,] #Mostra os dados que na coluna Damp aqueles que tem valor verdadeiro (TRUE)
median(Worm.density) #Faz a mediana dos dados da coluna Worm.density
d[Worm.density>median(Worm.density),] #Mostra os valores da coluna Worm.density que possuem valors maiores que a mediana
d[Worm.density>median(Worm.density)&Soil.pH<5.2,]#Idem do de cima mas agora apenas o que também possuem ph<5.2

d[Vegetation=="Grassland",]#Mostra apenas os dados do dataframe que na coluna Vegetarion seja Grassland
d[!Vegetation=="Grassland",]#mostra todos os dados do dataframe que NÃO contenha Grassland
d[-which(Damp==F),] # Retira não sei o que dos quais a columa F é falso


