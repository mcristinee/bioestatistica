

### Aqui temos o primeiro gráfico do artigo, gerado com dados hipotéticos apenas para demostrar a curva normal.

hist(rnorm(9000,mean=130,sd=3),freq=FALSE, xlab = "Comprimento Peixes", ylab = "Probabilidade", main = "",ylim = c(0,0.25)) #Aqui geramos o histograma
curve(dnorm(x,mean=130,sd=3),col=2,lty=1,lwd=2,add=TRUE) # Inserimos a curva em forma de sino


#### Vamos gerar dados de comprimento de peixes para as análises e gráficos
#Usei a função rnorm para gerar dados com distribuição normal, 
#recortei e colei abaixo para podermos trabalhar sempre com os mesmos dados

machos = c(24.5, 25.6, 26, 27.9, 28.4, 28.6, 29.3, 29.8, 30,	30, 30, 30, 31.1, 31.4, 31.5, 31.8, 31.9, 32.1, 32.2, 32.5, 32.6, 33.1, 33.4, 33.5, 33.7, 33.9, 34.3, 34.9, 37.4, 38.2) #dados brutos dos machos
femeas =c(22.4, 23.6, 23.6, 23.6, 25.2, 25.5, 25.9, 26.4, 26.6, 26.9, 26.9, 27.3, 27.5, 27.8, 27.8, 28.2, 28.4, 28.4, 28.8, 29, 29, 29.5, 29.6, 30, 30, 30.1, 31.6, 32, 32.3, 42.8) #dados brutos das femeas
sexo = factor(rep(c("macho","femea"),each=30)) #aqui criamos a coluna de sexo dos peixes para podermos realizar as análises
peixe = c(machos,femeas)# Unimos as duas variáveis, machos e femeas em uma única coluna
dados= data.frame(sexo,peixe) # Aqui criamos a tabela com sexos e os dados de comprimento, tem 2 colunas
head(dados) # verificamos as 5 primeiras linhas dos dados, pra ver se está tudo ok


par(mfrow = c(1,2),cex.axis=1.3,cex.lab=1.6,las=1,bty="l", tcl=0.3,pch=16,cex=1.4,family = "serif") # coloquei as configurações dos gráficos, tamanho de letras, numero de graficos na janela, etc
hist(femeas, main  = "", xlab = "Fêmeas", ylab = "") # o histograma das femeas
mtext("Frequência",side=2, line=2.7,cex=1.8,las=3)#inserindo o rótulo do eixo y
hist(machos, main= "" , xlab = "Machos", ylab = "") # histograma dos machos


## Agora vamos gerar o boxplot

par(mfrow =c(1,1),cex.axis=1.3,cex.lab=1.6,las=1,bty="l", tcl=0.3,pch=16,cex=1.4,family = "serif") # Linha de configuração do gráfico
boxplot(peixe~sexo, col="grey",ylab = "",xlab="Sexo",xaxt="n") # boxplot machos e femeas
axis(side=1, at=c(1,2),labels=c("",""),tcl=-.5) # eixo x
text(1.2,c(43,32.8,30,28.45,26.85,23),c("Outlier","Máximo","Quartil 75","Mediana","Quartil 25","Mínimo")) #Inserindo anotações dentro do gráfico
mtext(c("Fêmeas","Machos"), side = 1, line = 1, at=c(1,2), cex=1.5) # Rótulos de X
mtext(c("Comprimento (cm)"), side = 2, line = 2.7, cex=1.8, las=3)# Rotulo do y

## Aqui geramos uma função simples, pra agruparmos e calcularmos os principais parâmetros dos nossos dados.
describe = function(x){
  if(class(m)!="matrix"){
    stop("Vacilou, o vetor precisa ser uma matriz")} # colocando o pressuposto de que a tabela de dados tem que ser uma matriz.
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  q25 <- function(y){
    quantile(y,c(.25))
  }
  q75 <- function(z){
    quantile(z,c(.75))
  }
  N <- apply(x,2,length) #número de elementos do vetor
  Media <- apply(x,2,mean) #média de cada coluna do vetor
  Desvio_Padrao <- apply(x,2,sd) #desvio padrao para cada coluna do vetor
  Minimo <- apply(x,2,min) #Valor minimo para cada coluna do vetor
  Maximo <- apply(x,2,max) #Valor maximo para cada coluna do vetor
  Mediana <- apply(x,2,median) #Mediana para cada variavel
  Moda <- apply(x,2,getmode) #Moda 
  variancia <- apply(x,2,var) # Variancia
  quantil25 <- apply(x,2,q25) #Quantil 25%
  quantil75 <- apply(x,2,q75) #Quantil 75%
  resultados <- data.frame(N,Minimo,Maximo,Moda,Media,Mediana,quantil25,quantil75,Desvio_Padrao,variancia) #Colocamos todos os resultados como um data.frame nesse objeto
  return (round(resultados,2)) # Pedimos para funçao nos retornar aquilo que está contido no objeto resultados. 
}

dado = as.matrix(data.frame(machos,femeas)) # Criamos um data.frame com 1 coluna para os machos e 1 para as fêmeas. A funçao as.matrix trasnforma a tabela em matriz, para a funáo describe funcionar. 
describe(dado) # Calculamos e visualizamos os parametros.
a= print(describe(dado)) # Armazenamos esses resultados no objeto "a"
write.csv(a,file = "Describe.csv") #Salvamos um arquivo .csv com esses resultados na nossa pasta de trabalho.


## AGORA VAMOS REALIZAR O TESTE-T Comparando os comprimentos de machos e femeas para verificar se há diferença no tamanho dos peixes de acordo com o sexo.

head(dados) # Primeiro verificamos a estrutura dos dados
shapiro.test(dados$peixe[dados$sexo=="macho"]) #agora indexamos os machos e verificamos a normalidade dos dados
shapiro.test(dados$peixe[dados$sexo=="femea"]) #fazemos a mesma coisa com as femeas

bartlett.test(dados$peixe~dados$sexo) #Realizamos o teste de homogeneidade de variancias.

t.test(dados$peixe~dados$sexo) #realizamos o teste-t. E verificamos o p-value para saber a significancia do teste.


## ANOVA
#Utilizamos a função rnorm, mas copiei e colei abaixo para trabalharmos com os mesmos dados.
pop1 <- c(21,20,22,20,21,21,20,19,18,20) # primeira população de mexilhões
pop2 <- c(25,27,25,26,24,25,24,25,25,26) #segunda população de mexilhões
pop3 <- c(23,21,22,24,22,22,22,21,21,24) #terceira população de mexilhões

table <-data.frame(pop1,pop2,pop3) #juntamos os dados em 3 colunas em um objeto chamado table

var(table$pop1) #verificamos a variancia dentro das populações
var(table$pop2)#verificamos a variancia dentro das populações
var(table$pop3)#verificamos a variancia dentro das populações
bartlett.test (table) #realizamos o teste de homogeneidade de variancias

(media.1=mean(pop1)) #Salvamos as médias de cada população | Colocamos tudo dentro dos parenteses para podermos atribuir e olhar os dados na mesma linha
(media.2=mean(pop2)) #Salvamos as médias de cada população
(media.3=mean(pop3)) #Salvamos as médias de cada população

peso=c(pop1,pop2,pop3) #salvamos os dados em um objeto chamado peso
praia=factor(rep(c("praia1","praia2","praia3"),each=10)) # criamos os fatores 

boxplot(peso~praia, col="grey") # Olhamos a distribuição dos dados no boxplot

#Agora vamos plotar os pontos e as retas até a média, para termos ideia de como o calculo da ANOVA funciona.
plot(1:30,peso, pch=rep(c(15,16),each=10),col=rep(1:3,each=10)) # Primeiro plotamos os pesos. 
for(i in 1:10) #agora as linhas da primeira praia
{
  lines(c(i,i),c(peso[i],media.1),col=1)
}
for(j in 11:20) # agora as linhas da segunda praia
{
  lines(c(j,j),c(peso[j],media.2),col=2)
}
for(k in 21:30) # e por fim as linhas da terceira praia
{
  lines(c(k,k),c(peso[k],media.3),col=3)
}

lines(c(1,10),c(media.1,media.1),col=1) #Agora a média da pop 1
lines(c(11,20),c(media.2,media.2),col=2)#Agora a média da pop 2
lines(c(21,30),c(media.3,media.3),col=3)#Agora a média da pop 3

#Podemos agora verificar a tabela da ANOVA.
ANV = aov(peso~praia) #realizamos a análise de variancia ANOVA 
summary(ANV) #verificamos a tabela de resultados. 


