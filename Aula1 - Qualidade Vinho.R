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

###Arquitetura de rede

###Quantos neuronios tenho na entrada, escondida e saida
### A função dim fala número de linhas e colunas

### x são as features, y são as targets e hidden+neurons é a quantidade de neuronios

getLayerSize <- function(X, y, hidden_neurons) {
  n_x <- dim(X)[1] #quantidade de linhas de x = neurônios da camada de entrada
  n_h <- hidden_neurons #quantidade de neurônios na camada escondida
  n_y <- dim(y)[1] #quantidade de linhas de y = neurônios da camada de saída
  
  ###lista constroi um conjunto de valores
  
  size <- list("n_x" = n_x,
               "n_h" = n_h,
               "n_y" = n_y)
  
  return(size)
}

###Tamanho e arquitetura - entrada, escondida e saida

layer_size <- getLayerSize(train_x, train_y, hidden_neurons = 4)
layer_size

###Mostrar o valor
layer_size$n_x
layer_size$n_h
layer_size$n_y


###Precisa agora fazer as definições inicializando os parâmetros. Isso chutando os valores com base em uma distribuição uniforme

initializeParameters <- function(X, layer_size){
  
  n_x <- layer_size$n_x
  n_h <- layer_size$n_h
  n_y <- layer_size$n_y
  
  
  ###Perguntas: 
  ##Quantos pesos precisam ser chutados? quantidade de neuronios camada de entrada * escondida
  
  ###O runif que está chutando
  ###0.01 é pra facilitar, apenas
  
  W1 <- matrix(runif(n_h * n_x), nrow = n_h, ncol = n_x, byrow = TRUE) * 0.01
  W2 <- matrix(runif(n_y * n_h), nrow = n_y, ncol = n_h, byrow = TRUE) * 0.01
  
  params <- list("W1" = W1,
                 "W2" = W2)
  
  return (params)
}

###Inicializando

init_params <- initializeParameters(train_x, layer_size)
lapply(init_params, function(x) dim(x))

###Função de ativação
###Sigmoid
###Exp é para logaritmos exponenciais

sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}


###ForwardPropagation

forwardPropagation <- function(X, params, layer_size){
  
  n_h <- layer_size$n_h
  n_y <- layer_size$n_y
  
  W1 <- params$W1
  W2 <- params$W2
  
  ###Para multiplicar matriz precisa de %*% para passar por todos e não * apenas
  
  Z1 <- W1 %*% X
  A1 <- sigmoid(Z1)
  Z2 <- W2 %*% A1
  A2 <- sigmoid(Z2)
  
  cache <- list("Z1" = Z1,
                "A1" = A1, 
                "Z2" = Z2,
                "A2" = A2)
  
  return (cache)
}

fwd_prop <- forwardPropagation(train_x, init_params, layer_size)


###Função de custo
###Fazendo isso porque deve ter gerado erros
###Função de erro quadratico médio

computeCost <- function(y, cache) {
  
  m <- dim(y)[2]
  A2 <- cache$A2
  
  cost <- sum((y-A2)^2)/m
  
  return (cost)
}


cost <- computeCost(train_y, fwd_prop)


###Backpropagation
###Identificar o quanto tem que alterar do peso baseado nas etapas anteriores

backwardPropagation <- function(X, y, cache, params, layer_size){
  
  m <- dim(X)[2]
  
  n_x <- layer_size$n_x
  n_h <- layer_size$n_h
  n_y <- layer_size$n_y
  
  A2 <- cache$A2
  A1 <- cache$A1
  W2 <- params$W2
  
  
  dZ2 <- A2 - y
  dW2 <- 1/m * (dZ2 %*% t(A1)) 
  
  
  dZ1 <- (t(W2) %*% dZ2) * (1 - A1^2)
  dW1 <- 1/m * (dZ1 %*% t(X))
  
  
  grads <- list("dW1" = dW1, 
                "dW2" = dW2)
  
  return(grads)
}

###Atualiza pesos com base na etapa anterior, nos grads

updateParameters <- function(grads, params, learning_rate){
  
  W1 <- params$W1
  W2 <- params$W2
  
  dW1 <- grads$dW1
  dW2 <- grads$dW2
  
  
  W1 <- W1 - learning_rate * dW1
  W2 <- W2 - learning_rate * dW2
  
  updated_params <- list("W1" = W1,
                         "W2" = W2)
  
  return (updated_params)
}


###Treina o modelo

trainModel <- function(X, y, num_iteration, hidden_neurons, lr){
  
  ##Cria arquitetura
  layer_size <- getLayerSize(X, y, hidden_neurons)
  ##Escuta os valores do peso
  init_params <- initializeParameters(X, layer_size)
  
  cost_history <- c()
  
  ##Loop
  for (i in 1:num_iteration) {
    fwd_prop <- forwardPropagation(X, init_params, layer_size)
    cost <- computeCost(y, fwd_prop)
    back_prop <- backwardPropagation(X, y, fwd_prop, init_params, layer_size)
    update_params <- updateParameters(back_prop, init_params, learning_rate = lr)
    init_params <- update_params
    cost_history <- c(cost_history, cost)
    
  }
  
  model_out <- list("updated_params" = update_params,
                    "cost_hist" = cost_history)
  
  return (model_out)
}


###Aplica o treinamento

##Numero de interação
EPOCHS = 200
##Quantos neuronios temos camada escondida
HIDDEN_NEURONS = 40

LEARNING_RATE = 0.01

train_model <- trainModel(train_x, train_y, hidden_neurons = HIDDEN_NEURONS, num_iteration = EPOCHS, lr = LEARNING_RATE)

###Gera previsões
layer_size <- getLayerSize(test_x, test_y, HIDDEN_NEURONS)
params <- train_model$updated_params
fwd_prop <- forwardPropagation(test_x, params, layer_size)
y_pred <- fwd_prop$A2
compare <- rbind(y_pred,test_y)

###Verifica função custo
plot(train_model$cost_hist)
