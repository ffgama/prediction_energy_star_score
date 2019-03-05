rm(list=ls())

# carga de dados
load('data/load_data.RData')

library(dplyr)
# dimensão dos datasets
dim(train)
dim(test)

# movendo a variável energy star score para o final
train <- train %>% select(1:28,30:60, 29)

# criando a variável energy star score no dataset de teste
test$ENERGY.STAR.Score <- NA

# percebemos em ambos datasets existe uma coluna com nome diferente.
colnames(train) == colnames(test)

# unificamos o nome das colunas de ambos datasets
train <- train %>% rename(OrderId = Order)

# combinando os datasets 
full_data <- rbind(train, test)

# dimensão e estrutura do dataset
dim(full_data)
str(full_data)

# substituir todos os pontos para facilitar a leitura
colnames(full_data) <- gsub('[.]', replacement = '', colnames(full_data))
# observar quais variáveis podemos modificar
colnames(full_data)

# renomear as colunas do dataset
old_colnames <- c('BBL10digits','NYCBoroughBlockandLotBBLselfreported','NYCBuildingIdentificationNumberBIN',
                  'Address1selfreported','PrimaryPropertyTypeSelfSelected','ListofAllPropertyUseTypesatProperty',
                  'LargestPropertyUseTypeGrossFloorAreaftÂ²','X2ndLargestPropertyUseType','X2ndLargestPropertyUseGrossFloorAreaftÂ²',
                  'X3rdLargestPropertyUseType','X3rdLargestPropertyUseTypeGrossFloorAreaftÂ²','NumberofBuildingsSelfreported',
                  'SiteEUIkBtuftÂ²','WeatherNormalizedSiteEUIkBtuftÂ²','WeatherNormalizedSiteElectricityIntensitykWhftÂ²',
                  'WeatherNormalizedSiteNaturalGasIntensitythermsftÂ²','WeatherNormalizedSourceEUIkBtuftÂ²','FuelOil1UsekBtu',
                  'FuelOil2UsekBtu','FuelOil4UsekBtu','FuelOil56UsekBtu','Diesel2UsekBtu','DistrictSteamUsekBtu','NaturalGasUsekBtu',
                  'WeatherNormalizedSiteNaturalGasUsetherms','ElectricityUseGridPurchasekBtu','WeatherNormalizedSiteElectricitykWh',
                  'TotalGHGEmissionsMetricTonsCO2e','DirectGHGEmissionsMetricTonsCO2e','IndirectGHGEmissionsMetricTonsCO2e',
                  'PropertyGFASelfReportedftÂ²','WaterUseAllWaterSourceskgal','WaterIntensityAllWaterSourcesgalftÂ²',
                  'SourceEUIkBtuftÂ²','ENERGYSTARScore')


new_colnames <- c('BBL', 'BoroughBlockLotBBL', 'BuildingID','Address1','PrimaryPropertyType','AllTypesProperty',
                  'LargestPropertTypeGrossFloorArea','SecondLargestPropertyType','SecondLargestPropertyGrossFloorArea',
                  'ThirdLargestPropertyType','ThirdLargestPropertGrossFloorArea','NumberBuildings','SiteEUI',
                  'WeatherNormalizedSiteEUI','WeatherNormalizedSiteElectricityIntensity','WeatherNormalizedSiteNaturalGasIntensity',
                  'WeatherNormalizedSourceEUI','FuelOil1','FuelOil2','FuelOil4','FuelOil56','Diesel2','DistrictSteam',
                  'NaturalGas','WeatherNormalizedSiteNaturalGas','ElectricityUseGridPurchase','WeatherNormalizedSiteElectricity',
                  'TotalGHGEmissionsMetricTons','DirectGHGEmissionsMetricTons','IndirectGHGEmissionsMetricTons',
                  'PropertyGFA','WaterUseAllWaterSources','WaterIntensityAllWaterSources','SourceEUI','Score') 


full_data <- full_data %>% rename_at(vars(old_colnames), ~new_colnames)

# sum(is.na(full_data$ParentPropertyId))

# estrutura do dataset
str(full_data)

full_data$Borough <- factor(full_data$Borough, 
                            labels = c(1:length(levels(full_data$Borough))))

# susbstindo por 1111111 onde encontrar "Not Applicable: Standalone Property"
levels(full_data$ParentPropertyId)[59] <- paste(rep(1, nchar(as.character(full_data$ParentPropertyId[1]))),sep=" ", collapse = "")

full_data$ParentPropertyId <- factor(full_data$ParentPropertyId, 
                                  labels = round(as.integer(levels(full_data$ParentPropertyId))))
# DOFGrossFloorArea
full_data$DOFGrossFloorArea <- as.factor(full_data$DOFGrossFloorArea)

# PrimaryPropertyType
full_data$PrimaryPropertyType <- factor(full_data$PrimaryPropertyType, 
                                        labels = c(1:length(levels(full_data$PrimaryPropertyType))))
# full_data$AllTypesProperty
full_data$AllTypesProperty <- factor(full_data$AllTypesProperty, 
                                     labels = c(1:length(levels(full_data$AllTypesProperty))))
# LargestPropertyUseType
full_data$LargestPropertyUseType <- factor(full_data$LargestPropertyUseType, 
                                     labels = c(1:length(levels(full_data$LargestPropertyUseType))))
# LargestPropertTypeGrossFloorArea
full_data$LargestPropertTypeGrossFloorArea <- as.factor(full_data$LargestPropertTypeGrossFloorArea)
# SecondLargestPropertyType
full_data$SecondLargestPropertyType <- factor(full_data$SecondLargestPropertyType, 
                                              labels = c(1:length(levels(full_data$SecondLargestPropertyType))))
# SecondLargestPropertyGrossFloorArea
full_data$SecondLargestPropertyGrossFloorArea <- factor(full_data$SecondLargestPropertyGrossFloorArea, 
                                                        labels = c(1:length(levels(full_data$SecondLargestPropertyGrossFloorArea))))
# ThirdLargestPropertyType
full_data$ThirdLargestPropertyType <- factor(full_data$ThirdLargestPropertyType, 
                                             labels = c(1:length(levels(full_data$ThirdLargestPropertyType))))
# ThirdLargestPropertGrossFloorArea
full_data$ThirdLargestPropertGrossFloorArea <- factor(full_data$ThirdLargestPropertGrossFloorArea, 
                                                      labels = c(1:length(levels(full_data$ThirdLargestPropertGrossFloorArea))))
# MeteredAreasEnergy
full_data$MeteredAreasEnergy <- factor(full_data$MeteredAreasEnergy, 
                                       labels = c(1:length(levels(full_data$MeteredAreasEnergy))))
# MeteredAreasWater
full_data$MeteredAreasWater <- factor(full_data$MeteredAreasWater, 
                                      labels = c(1:length(levels(full_data$MeteredAreasWater))))
# WeatherNormalizedSiteEUI
full_data$WeatherNormalizedSiteEUI[full_data$WeatherNormalizedSiteEUI == "Not Available"] <- NA
full_data$WeatherNormalizedSiteEUI <- as.numeric(as.character(full_data$WeatherNormalizedSiteEUI))  

# WeatherNormalizedSiteElectricityIntensity
full_data$WeatherNormalizedSiteElectricityIntensity <- as.numeric(full_data$WeatherNormalizedSiteElectricityIntensity)
# WeatherNormalizedSiteNaturalGasIntensity
full_data$WeatherNormalizedSiteNaturalGasIntensity <- as.numeric(full_data$WeatherNormalizedSiteNaturalGasIntensity)
# WeatherNormalizedSourceEUI
full_data$WeatherNormalizedSourceEUI <- as.numeric(full_data$WeatherNormalizedSourceEUI)
# FuelOil1
full_data$FuelOil1[full_data$FuelOil1 == "Not Available"] <- NA
full_data$FuelOil1 <- as.numeric(as.character(full_data$FuelOil1))  
# FuelOil2
full_data$FuelOil2[full_data$FuelOil2 == "Not Available"] <- NA
full_data$FuelOil2 <- as.numeric(as.character(full_data$FuelOil2)) 
# FuelOil4
full_data$FuelOil4[full_data$FuelOil4 == "Not Available"] <- NA
full_data$FuelOil4 <- as.numeric(as.character(full_data$FuelOil4)) 
# FuelOil56
full_data$FuelOil56[full_data$FuelOil56 == "Not Available"] <- NA
full_data$FuelOil56 <- as.numeric(as.character(full_data$FuelOil56))  
# Diesel2
full_data$Diesel2[full_data$Diesel2 == "Not Available"] <- NA
full_data$Diesel2 <- as.numeric(as.character(full_data$Diesel2))  
# DistrictSteam
full_data$DistrictSteam[full_data$DistrictSteam == "Not Available"] <- NA
full_data$DistrictSteam <- as.numeric(as.character(full_data$DistrictSteam))  
# ElectricityUseGridPurchase
full_data$ElectricityUseGridPurchase[full_data$ElectricityUseGridPurchase == "Not Available"] <- NA
full_data$ElectricityUseGridPurchase <- as.numeric(as.character(full_data$ElectricityUseGridPurchase))  
# WeatherNormalizedSiteElectricity
full_data$WeatherNormalizedSiteElectricity[full_data$WeatherNormalizedSiteElectricity == "Not Available"] <- NA
full_data$WeatherNormalizedSiteElectricity <- as.numeric(as.character(full_data$WeatherNormalizedSiteElectricity))  
# NaturalGas
full_data$NaturalGas[full_data$NaturalGas == "Not Available"] <- NA
full_data$NaturalGas <- as.numeric(as.character(full_data$NaturalGas))  
# WeatherNormalizedSiteNaturalGas
full_data$WeatherNormalizedSiteNaturalGas[full_data$WeatherNormalizedSiteNaturalGas == "Not Available"] <- NA
full_data$WeatherNormalizedSiteNaturalGas <- as.numeric(as.character(full_data$WeatherNormalizedSiteNaturalGas))  
# TotalGHGEmissionsMetricTons
full_data$TotalGHGEmissionsMetricTons[full_data$TotalGHGEmissionsMetricTons == "Not Available"] <- NA
full_data$TotalGHGEmissionsMetricTons <- as.numeric(as.character(full_data$TotalGHGEmissionsMetricTons)) 
# DirectGHGEmissionsMetricTons
full_data$DirectGHGEmissionsMetricTons[full_data$DirectGHGEmissionsMetricTons == "Not Available"] <- NA
full_data$DirectGHGEmissionsMetricTons <- as.numeric(as.character(full_data$DirectGHGEmissionsMetricTons)) 
# IndirectGHGEmissionsMetricTons
full_data$IndirectGHGEmissionsMetricTons[full_data$IndirectGHGEmissionsMetricTons == "Not Available"] <- NA
full_data$IndirectGHGEmissionsMetricTons <- as.numeric(as.character(full_data$IndirectGHGEmissionsMetricTons)) 
# WaterUseAllWaterSources
full_data$WaterUseAllWaterSources[full_data$WaterUseAllWaterSources == "Not Available"] <- NA
full_data$WaterUseAllWaterSources <- as.numeric(as.character(full_data$WaterUseAllWaterSources)) 
# WaterIntensityAllWaterSources
full_data$WaterIntensityAllWaterSources[full_data$WaterIntensityAllWaterSources == "Not Available"] <- NA
full_data$WaterIntensityAllWaterSources <- as.numeric(as.character(full_data$WaterIntensityAllWaterSources)) 
# WaterRequired
full_data$WaterRequired <- factor(full_data$WaterRequired, 
                                  labels = c(NA,0,1))
# DOFBenchmarkingSubmissionStatus
full_data$DOFBenchmarkingSubmissionStatus[full_data$DOFBenchmarkingSubmissionStatus == ""] <- NA
# CensusTract
full_data$CensusTract <- as.factor(full_data$CensusTract)
# NTA
full_data$NTA[full_data$NTA == ""] <- NA
full_data$NTA <- factor(full_data$NTA, 
                  labels = c(NA,1:139))

# resumo estatístico
summary(full_data)

# buscando as variáveis numéricas com valores missing 
sapply(full_data, function(x) sum(is.na(x)))

# proporção de missing values
prop_NA <- apply(full_data, 2, function(x) round(sum(is.na(x))/length(x), digits = 4))
# ordenando os resultados
prop_NA[order(prop_NA)]

# tratamento de missing values das variáveis
# Imputação
# DOFGrossFloorArea
# substituindo os NA pela moda (valor mais frequente da distribuição)
full_data$DOFGrossFloorArea[is.na(full_data$DOFGrossFloorArea)] <- 60000
# TotalGHGEmissionsMetricTons - substituindo pela mediana
full_data$TotalGHGEmissionsMetricTons[is.na(full_data$TotalGHGEmissionsMetricTons)] <- median(full_data$TotalGHGEmissionsMetricTons, na.rm = TRUE)
# DirectGHGEmissionsMetricTons - substituindo pela mediana
full_data$DirectGHGEmissionsMetricTons[is.na(full_data$DirectGHGEmissionsMetricTons)] <- median(full_data$DirectGHGEmissionsMetricTons, na.rm = TRUE)
# IndirectGHGEmissionsMetricTons - substituindo pela mediana
full_data$IndirectGHGEmissionsMetricTons[is.na(full_data$IndirectGHGEmissionsMetricTons)] <- median(full_data$IndirectGHGEmissionsMetricTons, na.rm = TRUE)
# ElectricityUseGridPurchase - substituindo pela mediana
full_data$ElectricityUseGridPurchase[is.na(full_data$ElectricityUseGridPurchase)] <- median(full_data$ElectricityUseGridPurchase, na.rm = TRUE)
# WeatherNormalizedSiteElectricity - substituindo pela mediana
full_data$WeatherNormalizedSiteElectricity[is.na(full_data$WeatherNormalizedSiteElectricity)] <- median(full_data$WeatherNormalizedSiteElectricity, na.rm = TRUE)

# WaterRequired
full_data$WaterRequired[is.na(full_data$WaterRequired)]
summary(full_data$WaterRequired)
# removendo o nivel NAs
full_data$WaterRequired <- factor(full_data$WaterRequired, levels = c(0,1))
# substituindo os NA pela moda (valor mais frequente da distribuição)
full_data$WaterRequired[is.na(full_data$WaterRequired)] <- 1

# NTA
str(full_data$NTA)
summary(full_data$NTA)
full_data$NTA<- factor(full_data$NTA, labels = c(1:139))

# dados com missing > 5%
# selecionando algumas colunas de referência para imputação
impute_full_data <- full_data %>% select(PropertyId, PropertyName, DOFGrossFloorArea, AllTypesProperty, NumberBuildings, Occupancy,
                                         MeteredAreasWater, MeteredAreasEnergy,YearBuilt,
                                         NaturalGas, WeatherNormalizedSiteEUI, WeatherNormalizedSiteNaturalGas,
                                         CommunityBoard, CouncilDistrict, CensusTract, NTA, WaterUseAllWaterSources,
                                         WaterIntensityAllWaterSources)  
library(DMwR)
# imputação utilizando o KNN com valor de k arbitrário
full_data_transf <- knnImputation(impute_full_data, k = 18, scale = T)
str(full_data_transf)

# incorporando as imputações ao dataset original
full_data$NaturalGas <- full_data_transf$NaturalGas
full_data$WeatherNormalizedSiteEUI <- full_data_transf$WeatherNormalizedSiteEUI
full_data$WeatherNormalizedSiteNaturalGas <- full_data_transf$WeatherNormalizedSiteNaturalGas
full_data$CommunityBoard <- full_data_transf$CommunityBoard
full_data$CouncilDistrict <- full_data_transf$CouncilDistrict
full_data$CensusTract <- full_data_transf$CensusTract
full_data$NTA <- full_data_transf$NTA
full_data$WaterUseAllWaterSources <- full_data_transf$WaterUseAllWaterSources
full_data$WaterIntensityAllWaterSources <- full_data_transf$WaterIntensityAllWaterSources

full_data_transformed <- subset(full_data, select = names(full_data))

str(full_data_transformed)

# eliminando variáveis desnecessárias
full_data_transformed <- full_data_transformed %>% select(-c('OrderId','PropertyName','ParentPropertyName','BBL','BoroughBlockLotBBL','YearBuilt',
                                                             'BuildingID','Address1','Address2','PostalCode','StreetNumber','StreetName',
                                                             'FuelOil1','FuelOil2','FuelOil4','DistrictSteam','FuelOil56','Diesel2','ReleaseDate',
                                                             'DOFBenchmarkingSubmissionStatus','Latitude','Longitude'))
summary(full_data_transformed)

# ajustando o tipo das variáveis
# CommunityBoard
full_data_transformed$CommunityBoard <- as.integer(full_data_transformed$CommunityBoard)
# CouncilDistrict
full_data_transformed$CouncilDistrict <- as.integer(full_data_transformed$CouncilDistrict)

# verificando/tratando outlier
boxplot(full_data_transformed$CommunityBoard)
boxplot(full_data_transformed$WeatherNormalizedSiteNaturalGas)
full_data_transformed$CommunityBoard[full_data_transformed$CommunityBoard > 30] <- median(full_data_transformed$CommunityBoard)
full_data_transformed$WeatherNormalizedSiteNaturalGas[full_data_transformed$WeatherNormalizedSiteNaturalGas > 5e+06] <- median(full_data_transformed$CommunityBoard)


# análise de correlação
full_dataset_cor <- full_data_transformed %>%
  na.omit() 

full_dataset_cor <- as.data.frame(sapply(full_dataset_cor, function(x) as.numeric(x)))

full_dataset_cor %>%
  cor(full_dataset_cor) %>% round(digits = 1) %>%
  ggcorrplot(hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             title = "Correlação entre as variáveis",
             lab_size = 3,tl.cex = 11, tl.srt = 90,
             method="square", 
             colors =c("#00344c", "#66cfff", "#00344c"), 
             ggtheme=theme_minimal() + theme(plot.title = element_text(hjust = 0.5)))


save(full_data_transformed, file = "data/data_transformed.RData")
