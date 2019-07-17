#Dados contínuos = variável resposta contínua = numeros que contenham decimais = vírgula
#dados contínuos = aleatoriedade = resíduo (erro)
#dados contínuos = determinísticas = Nossa pergunta/O que nós estamos testando/O fator
#dados contínuos - Dados normais (gráfico em sino) --> A média (aritimética) são os dados que mais representam os dados, os dois extremos são minoria
#sd = desvío padrão
#mi e sigma = desvio padrão
#Variância homocedástica = não varia com a variação dos dados, os dados mudam apenas a média aritimética

#Teste = shapiro = se os dados são normais
#Teste = Var Test e barflett = Se a variância é homocedastica ou não.

#Faz as análises e depois "pergunta" para o resídio
#Análise estatístca = relacionar o y com o x + e
#O x pode ser = contínuo ou categórico

#Variável explanatória contínua = Análise de regreção (2 tipos)
                                    #regressão linear (uma reta)
                                    #regreção quadrática (uma curva)

#variável categórica = Análise Anova
#Separar os dados em determinísctivos e resíduos
        #analisar os categóricos
#2 tipos de Anova
    #Oneway (Um fator)
    #Twoway (dois fagores)
    #Multiway (Multifatores)

#Variável contínua e categórica --> Mistura elementos de regressão com anova --> (ANCOVA)

setwd("D:/")
d=read.table("dieta2.txt", header = T)
d
attach(d)
summary(d) #observar que média e mediana são próximas --> Isso pode indicar que se trata de dados normais

#Vamos analisar o ganho em relação ao suplemento pela dieta = ganho ~ suplemento * dieta

m=lm(ganho~dieta*suplemento)

m=aov(ganho~dieta*suplemento)

summary(m)
#Na regreção não fazemos summary.aov, porque não faz sentido, aov é análise de variância, não é o que eu estou perguntando, estou perguntando o valor de p.

#Grau de liberdade é importante porque ele vai esticar ou não

#valor de F = var fator / var resíduo
        #Se for 1 os dois são iguais
        #Se não for iguais será distante de 1

r=residuals(m) #Separa os valores resíduos (aleatório) do efeito (deterministico)
r
#h0 = não tem dirença = normal
#h1 = tem diferença = não normal
shapiro.test(r) #p>005 fica com h0 ---- p<005 rejeita h0 e fica com h1.

require(hnp)
hnp(m,print=T) #executar 3 vezes, se estiver dentro de 10% ok. (Por conta do algorítimo que sorteia números)
tapply(ganho,list(suplemento,dieta),mean)
plot(density(r))

#Tukey testa a média
#Tukey só faz com anova

TukeyHSD(m) #Neste passo compara a tabelinha maluca
#Tabela: p > 005 = igual
#Tabela: p < 005 = <diferente
#comparar linha x linha
#comparar coluna x coluna
#nomear letrinha maiuscula e minuscula para não cunfundir


#Gráfico da Anova = barplot

med=tapply(ganho,list(dieta,suplemento),mean)
b=barplot(med,beside=T,ylim=c(0,50),col=c("gray","black")) #colocar label para x e para y
sd=tapply(ganho,list(suplemento,dieta),sd)
arrows(b,med-sd,b-med+sd,length = 0.1,angle=90,code = 3)

legend("topright", c("cevada","trigo"), fill=c("gray","black"), bty = "n", horiz = T)
text(1.5,35,"a")
text(2.5,35,"b")
text(4.5,35,"a")
text(5.5,35,"b")
