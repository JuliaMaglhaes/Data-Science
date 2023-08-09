## Rede Neural Recorrente

### Instação de pacote

pacotes <- c("rattle","rnn","ggplot2","dplyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


## Leitura do arquivo csv
weatherAUS <- read.csv("weatherAUS.csv")
View(weatherAUS)

#extrair somente colunas 1 and 14  e primeiras 3040 linhas (Albury location)
data <- weatherAUS[1:3040,14:15]
summary(data)

##ele da a descrição estatística do objeto
?summary

## Momento da Etapa de pré-processamento
#Garantir que chegaremos em resultados interessantes

#Dado limpo
# na.omit significa lidar com os valores faltantes
data_cleaned <- na.omit(data)
?na.omit

## Conceito de dado de entrada em RNN
#Nós temos os dados que vao ser as variaveis targets e features. Nós separamos elas, pegamos as 3mil primeiras linhas
#Só 3000 linhas para processar mais rapido e podermos dividir

data_used <- data_cleaned[1:3000,]

x <- data_used[,1] #feature
y <- data_used[,2] #target

head(x)
head(y)

## Dimensões:
#1. Quantidade de amostras
#2. Quantas vezes vai passar a amostra (tempo)
#3. Número de variaveis(features)

#Passando dessa forma por causa da lib. Precisa assim, construir uma matriz

?matrix

#nrow = núuumero de linha igual a 30. A coluna 1 por exemplo, 30 primeiras observações, 2º coluna, 30 primeiras observações.
#Isso 100 vezes por causa do 3000. Isso por estarmos trabalhando com dado sequencialmente. Isso não como se fossem os batchs.
#PRECISAMOS DE RECORRÊNCIA! NÃO SE PODE MANDAR TUDO DE UMA VEZ!

X <- matrix(x, nrow = 30)
Y <- matrix(y, nrow = 30)

## Normalizar
Yscaled <- (Y - min(Y)) / (max(Y) - min(Y))
Y <- Yscaled

Xscaled <- (X - min(X)) / (max(X) - min(X))
X <- Xscaled

## Train test split
train=1:80
test=81:100


set.seed(12)
model <- trainr(Y = Y[,train],
                X = X[,train],
                learningrate = 0.01,
                hidden_dim = 15,
                network_type = "rnn",
                numepochs = 100)

model$error
#poucas épocas?
plot(colMeans(model$error),type='l',xlab='epoch',ylab='errors')


Yp <- predictr(model, X[,test])

#Percentual de variação em uma variável explicada por outra
#por enquanto: entenda que é um percentual de variação explicada

rsq <- function(y_actual,y_predict)
{
  cor(y_actual,y_predict)^2
}


Ytest <- matrix(Y[,test], nrow = 1)
Ytest <- t(Ytest)
Ypredicted <- matrix(Yp, nrow = 1)
Ypredicted <- t(Ypredicted)

result_data <- data.frame(Ytest)
result_data$Ypredicted <- Ypredicted     

rsq(result_data$Ytest,result_data$Ypredicted)

mean(result_data$Ytest)
mean(result_data$Ypredicted)

plot(as.vector(t(result_data$Ytest)), col = 'red', type='l',
     main = "Actual vs Predicted Humidity: testing set",
     ylab = "Y,Yp")
lines(as.vector(t(Yp)), type = 'l', col = 'black')
legend("bottomright", c("Predicted", "Actual"),
       col = c("red","black"),
       lty = c(1,1), lwd = c(1,1))



