rm(list=ls())

load("data/data_transformed.RData")

library(caret)

# seleção com base no conjunto de dados de treino fornecido
select_data_train <- full_data_transformed %>% na.omit()

select_data_train <- data.frame(sapply(select_data_train, as.numeric))

dim(select_data_train)

# método de reamostragem CV 
control <- trainControl(method="cv",number=5)
# obtendo os scores de importância das variáveis utilizando o random forest
model <- train(Score ~ ., data = select_data_train, method = "rf", preProcess = c("scale", "center"), trControl = control)
model

library(randomForest)

# obtendo a importância das variáveis
imp <- importance(model$finalModel)

df_imp <- data.frame(variables = rownames(imp),importance(model$finalModel))
variables <- df_imp[order(df_imp$IncNodePurity, decreasing = TRUE),]
# listando as 6 primeiras e últimas variáveis (ordem decrescente) mais importantes considerado a impureza do nó
head(variables)
tail(variables)

save(list=ls(), file = "data/features.RData")
