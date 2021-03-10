#Treinamento de RStudio by Marinho

#Antes de começar, vamos instalar todos os pacotes necessários:
install.packages("readxl") #para ler do excel
install.packages("nortest") #teste de normalidade
install.packages("reshape2") #manipulação de dados
install.packages("qualityTools") #gráfico da probabilidade normal
install.packages("SixSigma") #Análise de capabilidade
install.packages("pareto.chart") #pareto tipo 1
install.packages("paretochart") #pareto topo 2
install.packages("qcc") #cartas de controle
install.packages("ggpubr") #análise de dados
install.packages("rstatix") #testes de hipóteses
install.packages("pwr") #poder amostral
install.packages("ggplot2") #gráficos variados
install.packages("RcmdrPlugin.DoE") #Fazer DoE
install.packages("forecast") #gerar forecast em série temporais
install.packages("xts") #gerar séries temporais
install.packages("qicharts2") #cartas de controle
install.packages("srm") #Superfície de resposta

#Arrumar a pasta de trabalho
#Mostrar a pasta atual
getwd()

#Setar workdirectory para a sua pasta. Mude o código abaixo!
setwd("C:/Users/mariro11/OneDrive - DRiV Inc/Curso RStudio/Datasets")

#Carregar um Dataset
library("readxl")
vendas <- read_excel("VENDASBRA.xlsx")
View(vendas) #Visualiza em forma de tabela

#Analisando os dados
#Histograma. nclasses é o número de colunas
hist(vendas$'Vendas Betim', nclass = 15)

#Teste de normalidade Anderson-Darling
#tem outros testes de normalidade abaixo!
library("nortest")
summary(vendas$'Vendas Betim') #Mostra um resumo dos dados
m<-mean(vendas$'Vendas Betim') #Grava a média em m
std<-sqrt(var(vendas$'Vendas Betim')) #Grava o desvio padrão em std
x<-vendas$'Vendas Betim' #Grava os valores em x
hist(x, probability = T, nclass = 15) #Plota o histograma.
#Não esqueça de usar o probability = T para o próximo comando funcionar

#Vamos plotar a curva da distribuição (PDF)
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)

#E por fim o anderson queridinho.
ad.test(vendas$'Vendas Betim')

#Manipulando dados
vendas2<- read_excel("VENDAS2.xlsx")
View(vendas2)
#Veja como os dados estão arrumados. Isso é ruim para o RStudio

library("reshape2")
vendas2a<- melt(vendas2, id.vars = 1)
View(vendas2a)
#Agora ele arrumou, mas baguçou os nomes das colunas. Então...
colnames(vendas2a)<- c("Região", "Mês", "Vendas")

#Analisando um dataset
grampo<- read_excel("GRAMPO C.xlsx")
show(grampo) #Outra forma de apresentar os dados
x<- grampo$`Dim Interna`
m<- mean(grampo$`Dim Interna`)
std<- sqrt(var(grampo$`Dim Interna`))
summary(x)
hist(x, probability = T, nclass = 10)
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)
ad.test(x)
boxplot(x, horizontal = TRUE) #Se não usar o horizontal fica na vertical

#Agora o gráfico da probabilidade nornmal
#Quando se deparar com uma linha install.packages, normalmente é
#porque você vai precisar instalar o pacote, nesse caso, tire o
#comentário dessa linha e rode-a.
#install.packages("qualityTools")
library("qualityTools")
ppPlot(x, "normal")
help("ppPlot") #quando precisar de ajuda use help("comando") ou ?comando

#Séries Temporais
inspec<- read_excel("INSPEC.xlsx")
View(inspec)
inspec.timeseries<- ts(inspec$`Tempo Inspeção`)
#Sempra que usar a função ts, seus dados ficam na estrutura de séries
#temporáis. Elas são bem chatas de trabalhar, mas as vezes, necessárias.
plot(inspec.timeseries)

#Séries Temporais - Outro exercício
uniao<- read_excel("UNIAO.xlsx")
head(uniao) #head é uma forma de apresentar os primeiros dados do dataset
uniao.ts<- ts(uniao$Torque)
plot(uniao.ts)
#ou usando a linguagem mais orientada à objeto:
plot.ts(uniao$Torque)

#O boxplot de novo
boxplot(inspec$`Tempo Inspeção`, horizontal = TRUE)
summary(inspec$`Tempo Inspeção`)

#Gage R&R
#nota: o arquivo tem que ser feito assim:
#operador | Peça | Repetição
#   1        1        1
#   2        1        1
#   1        2        1
#   2        2        1
#...
#   1        1        2
#   2        1        2
#   1        2        2
#   2        2        2
   
torque<- read_excel("TORQUE.xlsx")
gdo = gageRRDesign(2, 10, 2, method = "crossed", randomize = FALSE)
#Vamos lá: Gage R&R, 2 operadores, 10 peças, 2 medições.
#Método Cruzado e sem aleatorizar os ensaios (para facilitar aqui.)
?gageRRDesign
response(gdo) = torque$Torque #defina a coluna de resposta
gdo = gageRR(gdo, tolerance = 10) #gere o estudo
summary(gdo) #Analise
plot(gdo) #Plote

#Análise de Capabilidade
library("SixSigma")
piece<- read_excel("ES01246.xlsx")
piece.values<- piece$Altura
ss.ca.z(piece.values, LSL = 31.1, USL = 33.1) #Z-Bench
ss.ca.cp(piece.values, LSL = 31.1, USL = 33.1, ci = TRUE) #Cp
ss.ca.cpk(piece.values, LSL = 31.1, USL = 33.1, ci = TRUE) #Cpk
ss.study.ca(piece.values, LSL = 31.1, USL = 33.1, Target = 32.1) #Estudo

#outra forma (melhor eu acho)
library("qualityTools")
library("qcc")
altura<- qcc(piece.values, type = "xbar.one", nsigmas = 3, plot = TRUE)
#no type usei xbar.one porque não há subgrupo, são vaores individuais.
process.capability(altura, spec.limits = c(31.1, 33.1))

#Se precisar dar aquela limpada no ambiente, senão pule!
dev.off() #limpa os gráficos
rm(list=ls()) #para limpara as variáveis, mas daí tem que rodar as bibliotecas de novo.
shell("cls") #limpa o console

#Ferramentas da Qualidade
#Pareto
ippm<- read_excel("IPPM.xlsx")
ippm.values<- ippm$Quantidade
help("pareto.chart")
help("paretoChart")
names(ippm.values)<- ippm$Defeito #Dar nomes às colunas
pareto.chart(ippm$Quantidade)
#outro melhor
paretoChart(ippm.values, showTable = TRUE)

#ishikawa
help("cause.and.effect")
ishikawa<- read_excel("FISHBONE.xlsx")
cause.and.effect(cause = list(Medição=ishikawa$Medição,
                              Material=ishikawa$`Matéria Prima`,
                              Mão_de_Obra=ishikawa$`Mão de Obra`,
                              Meio_Ambiente=ishikawa$`Meio Ambiente`,
                              Método=ishikawa$Método,
                              Máquina=ishikawa$Máquina),
                 effect = "IPPM Alto")

#Testes de Hipóteses
#teste t para 1 amostra
grampo<- read_excel("GRAMPO C.xlsx")
help(t.test)
t.test(grampo$`Dim Interna`, mu=20.25) #mu é a média

#Bônus para análise dos dados!
library("ggpubr")
ggboxplot(grampo$`Dim Interna`) # outro tipo de boxplot para aprender
shapiro.test(grampo$`Dim Interna`) #Outro tipo de teste de normalidade
ggqqplot(grampo$`Dim Interna`) # outro tipo de teste de probabilidade

#Teste t para duas amostras
library("rstatix")
grampo2<- read_excel("GRAMPO C2.xlsx")

#opção 1: desconsiderando variâncias iguais
t.test(grampo2$`DI Probel`, grampo2$`DI Grampofix`, var.equal = FALSE)

#opção 2: considerando variâncias iguais
t.test(grampo2$`DI Probel`, grampo2$`DI Grampofix`, var.equal = TRUE)

#Teste para iguais variâncias
#para tirar a dúvida acima, de qual usar:
var.test(grampo2$`DI Probel`, grampo2$`DI Grampofix`)

#Teste t pareado
pareado<- read_excel("APCAB.xlsx")
t.test(pareado$Instr_A, pareado$Instr_B, paired = TRUE)

#Poder amostral
library("pwr")
help("pwr.t.test")
d = 3.12/2.43 #Effect Size (Cohen's d) Differences / STDEV

#Quero saber o poder
pwr.t.test(n=12, d=d, type = "two.sample")

#Quero saber a quantidade
pwr.t.test(d=d, power=.95, type = "two.sample")

#Teste de proporções
#1 sample
prop.test(2772, 71668, .04)

#2 Sample
prop.test(x = c(3718, 3215), n = c(106411, 85419))

#Regressão
#Scatterplot e correlação
hotmelt<- read_excel("HOTMELT.xlsx")
head(hotmelt)
scatter.smooth(hotmelt$Temperatura, hotmelt$Tempo)

#outro jeito
plot(hotmelt$Temperatura, hotmelt$Tempo)

#Cálculo de correlação
help(cor.test)
cor(hotmelt$Temperatura, hotmelt$Tempo)
cor.test(hotmelt$Temperatura, hotmelt$Tempo, method = "pearson")

#Fit Model
tinta<- read_excel("TINTA.xlsx")
head(tinta)
library("ggplot2")

#Para plotar os dados ainda sem criar a regressão
ggplot(tinta, aes(x=`Taxa Agitação`, y=Impurezas)) + geom_point() + stat_smooth()
cor(tinta$`Taxa Agitação`, tinta$Impurezas)
cor.test(tinta$`Taxa Agitação`, tinta$Impurezas)
help ("lm")

#Agora vamos criar o modelo
model = lm(tinta$Impurezas ~ tinta$"Taxa Agitação")
#Nessa fórumula, o que stá antes do ~ é o y e o que está depois, o x

#Agora sim plotamos com o modelo linear
ggplot(tinta, aes(x=`Taxa Agitação`, y=Impurezas)) + geom_point() + stat_smooth(method = lm)
summary(model) #Apresentamos o resumo
confint(model) #depois os intervalos de confiança.

#outras funções
coefficients(model)
fitted(model)
residuals(model)
anova(model)
vcov(model)
influence(model)

#Anova
carpete<- read_excel("CARPETE.xlsx")
View(carpete)
help(aov)

#Para criar o model de anova
fit<- aov(Tempo_Seg ~ Carpete_Tipo, data = carpete)
#Para criar uma área de plotar gráfico de 4 quadrantes, 2 a 2.
layout(matrix(c(1,2,3,4),2,2))
plot(fit) #Plota
summary(fit) #Mostra o sumário

#DoE
#Fatorial Completo
adesao<- read_excel("ADESAO.xlsx")
View(adesao)

#Para o DoE existe um menu onde faremos as análises. Para abrir:
library("RcmdrPlugin.DoE")
resposta=adesao$TempCol

#Fatorial Fracionado
doorpanel<- read_excel("DOORPANEL2.xlsx")
View(doorpanel)
library("RcmdrPlugin.DoE")
resposta2=doorpanel$Produção

#Otimizador
grampo3<- read_excel("GRAMPO C3.xlsx")
resposta3=grampo3$`%Desperdício`
library("RcmdrPlugin.DoE")

#Superfície de resposta
library("srm")

#Para o gráfico de contorno, 2D
contour(rsmModel.4, ~x1+x2+x3, at = xs(rsmModel.4))

#Para o gráfico de superfície, 3D
persp(rsmModel.4, ~x1+x2+x3, at = xs(rsmModel.4))

#Cartas de Controle
# X-barra e R
library("qicharts2")
cinto<- read_excel("CINTOSEG.xlsx")
help("qic")
View(cinto)

#Para fazer os gráficos, o comando é quase o mesmo, muda o tipo apenas.
#Nota: para o gráfico com subgrupos x é o tamanho do subgrupo.
qic(cinto$Medidas, chart = "xbar", x = cinto$Grupo)
qic(cinto$Medidas, chart = "mr", x = cinto$Grupo)
qic(cinto$Medidas, chart = "s", x = cinto$Grupo)

#Carta I-MR
#Aqui como não há subgrupo, não há x
extintor<- read_excel("EXTINTOR.xlsx")
qic(extintor$Torque, chart = "i")
qic(extintor$Torque, chart = "mr")

#Carta p
#Já pra os gráficos que podem ter tamanhos de lote diferentes
#o argumento n é obrigatório. n é o tamanho do lote
retrabalho<- read_excel("RETRABALHO.xlsx")
qic(retrabalho$Retrabalho, chart = "p", n=retrabalho$Produção)

#Carta np
doorpanel<- read_excel("DOORPANEL.xlsx")
qic(doorpanel$Rejeições, chart = "pp", n=doorpanel$Amostra)

#Carta u
kombi<- read_excel("KOMBI.xlsx")
qic(kombi$Defeitos, chart = "u", n=kombi$Peças)

#Carta c
notafiscal<- read_excel("NOTFISC.xlsx")
View(notafiscal)

#Uma coisa interessante. Sempre que quisermos mostrar antes e depois
#podemos usar o freeze, que apenas coloca uma linha divisória no mesmo
#gráfico (não muda os limetes), ou o part, que divide em 2 e muda limites
qic(notafiscal$`NF NOK`, chart = "c", freeze = 31, part.labels = c("Antes", "Depois"))
qic(notafiscal$`NF NOK`, chart = "c", part = 31, part.labels = c("Antes", "Depois"))

#Tendência
library("forecast")
library("xts")
library("ggplot2")

#Plotando um gráfico temporal mas com o xts
ippm2<- read_excel("IPPM2.xlsx")
ippm2$Mês <- as.Date(ippm2$Mês)
ippm2.ts <- xts(ippm2$IPPM, ippm2$Mês)
plot(ippm2.ts)

#Plotando um gráfico com as previsões usando timeseries
ippm2.ts <- ts(ippm2$IPPM)
summary(ippm2.ts)

#Precisamos criar um dataframe com uma coluna do tipo ts
ippm2.df <- data.frame(ippm = ippm2.ts, as.numeric(time(ippm2.ts)))
names(ippm2.df) <- c("ippm","data")
plot(ippm2.ts)

#Depois criamos um modelo mas nao com lm e sim com tslm pois é um ts...
model<-tslm(ippm~trend, ippm2.df)
summary(model)

#Plotamos o modelo linear para ver graficamente
ggplot(ippm2.df, aes(x=data, y=ippm)) + geom_point() + stat_smooth(method=lm)

#Geramos o forecast
fcst<-forecast(model, h=12)
fcst
autoplot(fcst)

#teste de ts
teste.ts <- ts(ippm2$IPPM, frequency = 1, start = ippm2[1, 1], end = ippm2[nrow(ippm2), 1])
print(teste.ts)

#Especial QualityTools
#Pareto - DEFINE
library("qualityTools")
ippm<- read_excel("IPPM.xlsx")
View(ippm)
#Temos que transformar a talela em um vetor de texto
defects<-rep(ippm$Defeito[1:7], ippm$Quantidade)
paretoChart(defects)

#Gage R&R - MEASURE
torque<- read_excel("TORQUE.xlsx")
torque.values<-torque$Torque
cg(torque.values, target = 50, tolerance = c(45, 55))

gdo = gageRRDesign(2, 10, 2, method = "crossed", randomize = FALSE)
response(gdo) = torque$Torque #defina a coluna de resposta
gdo = gageRR(gdo, tolerance = 10) #gere o estudo
plot(gdo)

#Capability - ANALYZE
piece<- read_excel("ES01246.xlsx")
piece.values<- piece$Altura
pcr(piece.values, "normal", lsl = 31.1, usl = 33.1)
pcr(piece.values, boxcox = TRUE, "normal", lsl = 31.1, usl = 33.1)
qqPlot(piece.values, "normal")
ppPlot(piece.values, "normal")

#DoE - IMPROVE
#Full
adesao<- read_excel("ADESAO.xlsx")
?facDesign
fdo = facDesign(k = 2, replicates = 4)
name(fdo) = c("Método", "Tipo")
lows(fdo) = c("Manual", "A")
highs(fdo) = c("Spray", "B")
summary(fdo)
response(fdo) = adesao$TempCol
effectPlot(fdo, classic = TRUE)
interactionPlot(fdo)
lm.1=lm(adesao$TempCol ~ A*B, data = fdo)
summary(lm.1)
paretoPlot(fdo)
normalPlot(fdo)

#Fractional
doorpanel<- read_excel("DOORPANEL2A.xlsx")
View(doorpanel)
fdo.frac = fracDesign(k = 5, gen = "E = ABCD")
name(fdo)=c("Temperatura", "Frequência", "Fornecedor", "Pressão", "Velocidade")
lows(fdo) = c(80, 10, "A", 20, 40)
highs(fdo) = c(100, 15, "B", 30, 50)
summary(fdo.frac)
response(fdo.frac) = doorpanel$Produção
aliasTable(fdo.frac)
fracChoose()
effectPlot(fdo.frac)
interactionPlot(fdo.frac)
paretoPlot(fdo.frac)
normalPlot(fdo.frac)
wirePlot3(B, D, E, doorpanel$Produção, data = fdo.frac)
contourPlot3(B, D, E, doorpanel$Produção, data = fdo.frac)
simProc(12, 22, 42)
