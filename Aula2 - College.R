## Dataset College

## Importa as libs

library(ISLR)
library(neuralnet)

data <- College

##Nesse dataset a variável é qualitativa! O campo Private.

##Fazendo tratativa nos dados. Modificando a qualitativa por categorica  com essa mudança de se for Yes, 1, se não, 0
##Vamos prever uma categórica
private <- ifelse(data$Private == 'Yes', 1, 0)

##Padronizar dados para melhor performance
#A primeira coluna não interessa
#Isso melhora a performance das redes, dado que estamos escalando com o scale
data <- data[,2:18]
max_data <- apply(data, 2, max) 
min_data <- apply(data, 2, min)
scaled <- data.frame(scale(data,center = min_data, scale = max_data - min_data))

#Inclui variável explicada (target) e modifica para o 0 e 1
scaled$Private <- private

set.seed(0)

#train test split
#Pegando 70% da base para treinamento e o resto para teste
index = sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])

## Utiliza o neuralnet

set.seed(0)

?neuralnet

##Jeito rápido de escrever todos os campos
##Nome de todas as colunas usando names. Com o comando, o n vira o nome de todas as colunas
n = names(train_data)
f <- as.formula(paste("Private ~", paste(n[!n %in% "Private"], collapse = " + ")))
##A target não é variável quantitativa e sim categórica, por isso o linear é Falso. A variável é CATEGÓRICA
nn <- neuralnet(f,data=train_data,hidden=c(5,4),linear.output=F)
plot(nn)

?compute

pr.nn <- compute(nn,test_data[,1:17])

##Sapply é semelhante ao apply. Muda pro tipo de dados que você está passando. Nela você está arredondando os n´=umeros
pr.nn$net.result <- sapply(pr.nn$net.result,round,digits=0)
pr.nn$net.result

table(test_data$Private,pr.nn$net.result)

Acc <- (62+158) / (62+158+7+6)

#CART comparação

set.seed(0)

## Árvore
fit_tree <- rpart(f,method="class", data=train_data)
tree_predict <- predict(fit_tree,test_data,type = "class")
table(test_data$Private,tree_predict)

##Verifica a acurácia
Acc_tree <- (58+159) / (58+159+11+5)