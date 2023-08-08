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
s
## Leitura do arquivo csv
weatherAUS <- read.csv("weatherAUS.csv")
View(weatherAUS)

#extrair somente colunas 1 and 14  e primeiras 3040 linhas (Albury location)
data <- weatherAUS[1:3040,14:15]
summary(data)