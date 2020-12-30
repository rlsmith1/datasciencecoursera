


# libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(GGally)
library(Hmisc)
library(e1071)


# questions ---------------------------------------------------------------


# 1 
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData <- data.frame(diagnosis, predictors)
trainIndex <- createDataPartition(diagnosis, p = 0.50, list = FALSE)
training <- adData[trainIndex,]
testing <- adData[-trainIndex,]


# 2 
data(concrete)

set.seed(1000)
inTrain <- createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training <- mixtures[inTrain,]
testing <- mixtures[-inTrain,]

training2 <- training
training2$CompressiveStrength <- cut2(training2$CompressiveStrength, g = 4)
training2 %>% ggpairs(columns = c("FlyAsh", "Age", "CompressiveStrength"), 
                      mapping = ggplot2::aes(color = CompressiveStrength))
training2 %>% ggpairs(mapping = ggplot2::aes(color = CompressiveStrength), 
                      progress = FALSE, axisLabels = "internal")


# 3 
training %>% ggplot() + geom_histogram(aes(Superplasticizer))
training %>% ggplot() + geom_histogram(aes(log(Superplasticizer)))
training %>% ggplot() + geom_histogram(aes(log(Superplasticizer + 1)))


# 4 
set.seed(3433) 
data(AlzheimerDisease)
adData <- data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[inTrain,]
testing <- adData[-inTrain,]

trainingIL <- training[,grepl("^IL", names(training))]
procTrain <- preProcess(trainingIL, method = "pca", thresh = 0.9)
procTrain


# 5 
trainingIL <- training[,grepl("^IL|diagnosis", names(training))]
testingIL <- testing[,grepl("^IL|diagnosis", names(testing))]

       # non-PCA
       model <- train(diagnosis ~ ., data = trainingIL, method = "glm")
       predict_model <- predict(model, newdata = testingIL)
       matrix_model <- confusionMatrix(predict_model, testingIL$diagnosis)
       matrix_model$overall[1]

       # PCA
       modelPCA <- train(diagnosis ~ ., data = trainingIL, method = "glm", 
                         preProcess = "pca", trControl = trainControl(preProcOptions = list(thresh = 0.8)))
       matrix_modelPCA <- confusionMatrix(testingIL$diagnosis, predict(modelPCA, testingIL))
       matrix_modelPCA$overall[1]








