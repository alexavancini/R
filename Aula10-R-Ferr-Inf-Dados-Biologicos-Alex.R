#Dados cont�nuos = vari�vel resposta cont�nua = numeros que contenham decimais = v�rgula
#dados cont�nuos = aleatoriedade = res�duo (erro)
#dados cont�nuos = determin�sticas = Nossa pergunta/O que n�s estamos testando/O fator
#dados cont�nuos - Dados normais (gr�fico em sino) --> A m�dia (aritim�tica) s�o os dados que mais representam os dados, os dois extremos s�o minoria
#sd = desv�o padr�o
#mi e sigma = desvio padr�o
#Vari�ncia homoced�stica = n�o varia com a varia��o dos dados, os dados mudam apenas a m�dia aritim�tica

#Teste = shapiro = se os dados s�o normais
#Teste = Var Test e barflett = Se a vari�ncia � homocedastica ou n�o.

#Faz as an�lises e depois "pergunta" para o res�dio
#An�lise estat�stca = relacionar o y com o x + e
#O x pode ser = cont�nuo ou categ�rico

#Vari�vel explanat�ria cont�nua = An�lise de regre��o (2 tipos)
                                    #regress�o linear (uma reta)
                                    #regre��o quadr�tica (uma curva)

#vari�vel categ�rica = An�lise Anova
#Separar os dados em determin�sctivos e res�duos
        #analisar os categ�ricos
#2 tipos de Anova
    #Oneway (Um fator)
    #Twoway (dois fagores)
    #Multiway (Multifatores)

#Vari�vel cont�nua e categ�rica --> Mistura elementos de regress�o com anova --> (ANCOVA)

setwd("D:/")
d=read.table("dieta2.txt", header = T)
d
attach(d)
summary(d) #observar que m�dia e mediana s�o pr�ximas --> Isso pode indicar que se trata de dados normais

#Vamos analisar o ganho em rela��o ao suplemento pela dieta = ganho ~ suplemento * dieta

m=lm(ganho~dieta*suplemento)

m=aov(ganho~dieta*suplemento)

summary(m)
#Na regre��o n�o fazemos summary.aov, porque n�o faz sentido, aov � an�lise de vari�ncia, n�o � o que eu estou perguntando, estou perguntando o valor de p.

#Grau de liberdade � importante porque ele vai esticar ou n�o

#valor de F = var fator / var res�duo
        #Se for 1 os dois s�o iguais
        #Se n�o for iguais ser� distante de 1

r=residuals(m) #Separa os valores res�duos (aleat�rio) do efeito (deterministico)
r
#h0 = n�o tem diren�a = normal
#h1 = tem diferen�a = n�o normal
shapiro.test(r) #p>005 fica com h0 ---- p<005 rejeita h0 e fica com h1.

require(hnp)
hnp(m,print=T) #executar 3 vezes, se estiver dentro de 10% ok. (Por conta do algor�timo que sorteia n�meros)
tapply(ganho,list(suplemento,dieta),mean)
plot(density(r))

#Tukey testa a m�dia
#Tukey s� faz com anova

TukeyHSD(m) #Neste passo compara a tabelinha maluca
#Tabela: p > 005 = igual
#Tabela: p < 005 = <diferente
#comparar linha x linha
#comparar coluna x coluna
#nomear letrinha maiuscula e minuscula para n�o cunfundir


#Gr�fico da Anova = barplot

med=tapply(ganho,list(dieta,suplemento),mean)
b=barplot(med,beside=T,ylim=c(0,50),col=c("gray","black")) #colocar label para x e para y
sd=tapply(ganho,list(suplemento,dieta),sd)
arrows(b,med-sd,b-med+sd,length = 0.1,angle=90,code = 3)

legend("topright", c("cevada","trigo"), fill=c("gray","black"), bty = "n", horiz = T)
text(1.5,35,"a")
text(2.5,35,"b")
text(4.5,35,"a")
text(5.5,35,"b")
