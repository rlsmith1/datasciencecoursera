



# libraries ---------------------------------------------------------------

       library(tidyverse)



# 1 -----------------------------------------------------------------------

library(AppliedPredictiveModeling)
data("segmentationOriginal")
library(caret)

inTrain <- createDataPartition(y = segmentationOriginal$Case, p = 0.7, list = FALSE) # 70% training
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

set.seed(125)
modFit <- train(Class ~ ., method = "rpart", data = training)

modFit$finalModel

library(rattle)
library(rpart.plot)

fancyRpartPlot(modFit$finalModel)



# 3 -----------------------------------------------------------------------

library(pgmm)
data(olive)
olive <- olive[,-1]
head(olive)

newdata <- as.data.frame(t(colMeans(olive)))

modolive <- train(Area ~ ., method = "rpart", data = olive)
predict(modolive, newdata = newdata)



# 4 & 5 -----------------------------------------------------------------------

library(ElemStatLearn) # package isn't available for R 3.6.1


















