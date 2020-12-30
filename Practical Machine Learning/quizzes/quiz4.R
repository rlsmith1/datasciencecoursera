



# libraries ---------------------------------------------------------------

       library(tidyverse)
       library(AppliedPredictiveModeling)
       library(caret)
       library(pgmm)
       library(rpart)
       library(gbm)
       library(lubridate)
       library(forecast)
       library(e1071)
       library(randomForest)
       library(elasticnet)



# 1 -----------------------------------------------------------------------

       # don't have ElemStatLearn


# 2 -----------------------------------------------------------------------

       # question code
       set.seed(3433)
       library(AppliedPredictiveModeling)
       data(AlzheimerDisease)
       adData <- data.frame(diagnosis,predictors)
       inTrain <- createDataPartition(adData$diagnosis, p = 0.75)[[1]]
       training <- adData[inTrain,]
       testing <- adData[-inTrain,]
       
       set.seed(62433)
       
       # fit models
       mod_rf <- train(diagnosis ~ ., data = training, method = "rf")
       mod_gbm <- train(diagnosis ~ ., data = training, method = "gbm")
       mod_lda <- train(diagnosis ~ ., data = training, method = "lda")
       
       # predict using models 
       pred_rf <- predict(mod_rf, testing)
       pred_gbm <- predict(mod_gbm, testing)
       pred_lda <- predict(mod_lda, testing)
       
       # combine models
       predDF <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
       combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
       combPred <- predict(combModFit, predDF)

       # resulting accuracies
       confusionMatrix(pred_rf, testing$diagnosis)$overall[1]
       confusionMatrix(pred_gbm, testing$diagnosis)$overall[1]
       confusionMatrix(pred_lda, testing$diagnosis)$overall[1]
       confusionMatrix(combPred, testing$diagnosis)$overall[1]
       

       
# 3 -----------------------------------------------------------------------


       # question code
       set.seed(3523)
       library(AppliedPredictiveModeling)
       data(concrete)
       inTrain <- createDataPartition(concrete$CompressiveStrength, p = 0.75)[[1]]
       training <- concrete[inTrain,]
       testing <- concrete[-inTrain,]

       # set seed
       set.seed(233)
       
       # fit lasso model
       mod_lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
       
       # plot to see last coefficient to be set to zero
       plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)

       

# 4 -----------------------------------------------------------------------

       # load data, question code
       data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")
       training <- data[year(data$date) < 2012, ]
       testing <- data[year(data$date) > 2011, ]
       tstrain <- ts(training$visitsTumblr)
       
       # fit model
       mod_ts <- bats(tstrain)
       
       # forecast
       fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
       
       # how many points is true value within 95% interval
       sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / dim(testing)[1]
       
       

       
# 5 -----------------------------------------------------------------------

       # question code
       set.seed(3523)
       library(AppliedPredictiveModeling)
       data(concrete)
       inTrain <- createDataPartition(concrete$CompressiveStrength, p = 0.75)[[1]]
       training <- concrete[inTrain, ]       
       testing <- concrete[-inTrain, ]   
       
       # set seed
       set.seed(325)
       
       # support vector machine
       mod_svm <- svm(CompressiveStrength ~ ., data = training)
       
       # predict compressive strength
       pred_svm <- predict(mod_svm, testing)
       
       # find RMSE
       accuracy(pred_svm, testing$CompressiveStrength)
       
       
       
       
       
       
       
       
       
       





