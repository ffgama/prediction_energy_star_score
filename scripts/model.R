rm(list=ls())

load("data/data_transformed.RData")
load("data/features.RData")

# load the library
library(caret)
library(dplyr)
library(kernlab)

#  selecionando apenas o dataset de treino
full_data_transformed_train <- full_data_transformed[!is.na(full_data_transformed$Score),]

set.seed(123)

# definindo o percentual do split
split <- createDataPartition(y = full_data_transformed_train$Score, p = 0.7, list = FALSE)

# convertendo todas as variáveis para valores numéricos
full_data_transformed_train <- data.frame(sapply(full_data_transformed_train, as.numeric))

# removendo variáveis que não são do nosso interesse utilizando como referência as variáveis mais importantes
full_data_transformed_train <- full_data_transformed_train %>% select(-c(MeteredAreasEnergy, ParentPropertyId,NumberBuildings,
                                                                         ThirdLargestPropertyType,ThirdLargestPropertGrossFloorArea,
                                                                         MeteredAreasWater,Occupancy,WaterRequired,Borough,
                                                                         WeatherNormalizedSiteNaturalGasIntensity,
                                                                         SecondLargestPropertyType,SecondLargestPropertyGrossFloorArea,
                                                                         PropertyGFA,ElectricityUseGridPurchase,
                                                                         LargestPropertTypeGrossFloorArea,
                                                                         WeatherNormalizedSiteElectricity,NaturalGas,
                                                                         WeatherNormalizedSiteNaturalGas,CommunityBoard,
                                                                         NTA,
                                                                         CouncilDistrict,DirectGHGEmissionsMetricTons,
                                                                         DOFGrossFloorArea,CensusTract,WaterUseAllWaterSources,
                                                                         TotalGHGEmissionsMetricTons,
                                                                         WeatherNormalizedSiteElectricityIntensity,
                                                                         WaterIntensityAllWaterSources,PropertyId
))

# 70% treinamento 
# Criando dados de treino e de teste
data_train <- full_data_transformed_train[split,]
data_test <- full_data_transformed_train[-split,]
dim(data_test) ; dim(data_test)

# criação dos modelo
model_ksvm <- ksvm(Score ~., data = data_train, kernel="laplacedot", kpar=list(sigma=0.20), C = 1.35, nu = 0.9, type="nu-svr")

predict_ksvm <- predict(model_ksvm, data_test)

# preparando o data.frame
result  <- cbind(predict_ksvm, data_test$Score)
colnames(result) <- c('Predicted','Actual')
result <- as.data.frame(result)

# valores menores que 0 e acima de 100 são trucados para dentro do intervalo [0,100]
result$Predicted[result$Predicted < 0 ] <- 0
result$Predicted[result$Predicted > 100 ] <- 100

result$Predicted <- round(result$Predicted)

# calculando o erro (MAE) - Mean Absolute Error
erro <- round(mean(abs(result$Actual - result$Predicted)),4)
erro

########################################### KAGGLE ###########################################

# dados de teste (kaggle)
data_test_kaggle <- full_data_transformed[is.na(full_data_transformed$Score),]
data_test_kaggle <- data.frame(sapply(data_test_kaggle, as.numeric))

predict_ksvm_kaggle <- predict(model_ksvm, data_test_kaggle)

result  <- cbind(predict_ksvm_kaggle)
colnames(result) <- c('Predicted')
result <- as.data.frame(result)

result$Predicted[result$Predicted < 0 ] <- 0
result$Predicted[result$Predicted > 100 ] <- 100
result$Predicted <- round(result$Predicted)

df_predictions <- data.frame(PropertyId = data_test_kaggle$PropertyId, result)
colnames(df_predictions) <- c("Property Id", "Score")

# gravando as predicoes
# write.csv(df_predictions,file="submissoes/df_predictions_8.csv", row.names=FALSE)