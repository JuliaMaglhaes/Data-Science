###Garante reprodutividade
set.seed(0)

###Lê o arquivo
data <- read.csv(file = "winequality-red.csv")

###Pega todas as observações e escala. Números próximos de 0 são melhores, rede neural não é performartica
data <- scale(data)

###Train Test Split
###nrowdata informa a quantidade de linhas. O * 0.8 é pra treinar 80% e os 20% que sobram é de teste
train_test_split_index <- 0.8 * nrow(data)


###Tudo antes da , é quantidade de linha, depois é a coluna. Assim, pondo nada na coluna pega tudo. - Lembrando que vai pegar até a linha 1279 que é o a quantidade separada
train <- data.frame(data[1:train_test_split_index,])

###Pegue tudo que começa do 1280 até o fime
test <- data.frame(data[(train_test_split_index+1): nrow(data),])

###Padronizar dados para melhor performance
###Pega todas as linhas de 1 até 11. A 12 é uma coluna target
train_x <- data.frame(train[1:11])

###Pega a 12 é uma coluna target
train_y <- data.frame(train[12])

test_x <- data.frame(test[1:11])
test_y <- data.frame(test[12])

###Transposição da matriz para facilitar. T é para transpor
train_x <- t(train_x)
train_y <- t(train_y)

test_x <- t(test_x )
test_y <- t(test_y)


