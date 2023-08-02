## Dataset Boston House

## Instalação dos pacotes
pacotes <- c("MASS","neuralnet","ISLR","mlbench","neuralnet","rpart")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

## Importa os pacotes

library("MASS")
#pacote para criar árvore de decisão
library("rpart")

## Garante a reprodutibilidade 
set.seed(0)

## Baixa os dados. Boston é o dataset. Ele ta dentro do MASS
data <- Boston


## Verificar se existem nulos. O is.na é uma função e dentro dela passando o dataset e ela informa todos os casos que tem na
#Valores entre chaves é pra subconjunto
data[is.na(data) == TRUE]

## Train-Test Split
#Pega 80% para transformar em treinamento. 0.8 da quantidade total de linha
train_test_split_index <- 0.8 * nrow(data)

##2 datasets. Um para treino outro teste. Isso está puxando a partir da quantidade de linhas que foram defenidas acima
train <- data.frame(data[1:train_test_split_index,])
test <- data.frame(data[(train_test_split_index+1): nrow(data),])

## Vendo o que significa o rpart
?rpart
#rpart(formula, data, weights, subset, na.action = na.rpart, method, model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...)


## Criando a árvore

# Modelo  da árvore
#Depois do ~ vocês põe toda as variáveis. Ela pega tudo para não escrever. Regrida medv contra toda as variáveis no caso
fit_tree <- rpart(medv ~.,method="anova", data=train)

?predict

#Serve para fazer uma predição no conjunto de teste no modelo que fez acima
tree_predict <- predict(fit_tree,test)

# Cálcula o erro quadratico médio
#Tira a média do tree_predict - o valor real que acontece no modelo de teste e ela ao quadrado
#Vamos usar ele para comparar com as redes neurais, no caso fazer um benchmark
mse_tree <- mean((tree_predict - test$medv)^2)


## NeuralNet
#As redes neurais prevem melhor quando o valor tá menor, então é comum normalizar as variáveis. Entre 0 e 1, melhor opção!
##Boa leitura:
##https://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.MinMaxScaler.html

?apply
#Pega um conjunto dedados e aplica

set.seed(0)

#Pega o valor máximo, aplica a função max no data. O 2 é coluna.
max_data <- apply(data, 2, max) 
min_data <- apply(data, 2, min)

#Isso melhora a performance das redes, dado que estamos escalando
scaled <- scale(data,center = min_data, scale = max_data - min_data)

#Sample é amostra. Você tira ela randomizada
index = sample(1:nrow(data),round(0.70*nrow(data)))

train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])

#Pacote de rede neural. Pacote simplificado para redes neurais artificiais. Ele possui bastante abstração
library(neuralnet)

## Leitura
?neuralnet
#https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf

## Fit de neuralnet
#Executar testes com diferentes arquiteturas
#medv é a target - saida
#crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat são as features
#data é onde você está tirando os dados, os treinamentos no caso
#hidden são as camadas escondidas. Isso dita a arquitetura do modelo. Ai vai informando a quantidade de neuronios por camada
#linear.output = T informa que a saida é linear por causa do medv que é quantitativo. Não existe a preocupação de função de ativação no momento.
#Por padrão vai ser usado função logística
nn <- neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
                data=train_data,hidden=c(5,4,3,2),linear.output=T)
plot(nn)

?compute

#Quer usar apenas as 13 primeiras colunas. [linha, coluna]
pr.nn <- compute(nn,test_data[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_data$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE_nn <- mean((pr.nn_ - test.r)^2)

#Valor real dos preços da casa
plot(test_data$medv,type = 'l',col="red",xlab = "x", ylab = "Valor Residencia")
#Valor previsto
lines(pr.nn$net.result,col = "blue")