rm(list=ls())

# extração dos dados de treino e teste
train <- read.csv('data/dataset_treino.csv')
test <- read.csv('data/dataset_teste.csv')

save(list=ls(), file = 'data/load_data.RData')