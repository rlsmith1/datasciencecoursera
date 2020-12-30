
# Report: how you build your model, how you used cross validation, what you think expected out of sample error is, why you made the choices you did
# < 2000 words and < 5 figures
# submit a repo with a gh-pages branch for te HTML file describing analysis


# libraries ---------------------------------------------------------------


       library(tidyverse)
       library(tidymodels)
       library(purrr)
       library(caret)
       library(rpart)
       library(rpart.plot)
       library(rattle)
       library(randomForest)
       library(gbm)
       library(factoextra)



# data --------------------------------------------------------------------


       df_training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv") %>% as_tibble()
       df_quiz <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv") %>% as_tibble() # for use in quiz results
       

       
# explore and clean data -----------------------------------------------------------------


       dim(df_training) # 19622 observations and 160 variables
       
       # remove variables with missing values
       df_training1 <- df_training %>% select(-which(colSums(is.na(.)) > 0))
       dim(df_training1)
       
       # remove variables with near zero variance
       df_training2 <- df_training1 %>% select(-c(df_training1 %>% nearZeroVar()))
       dim(df_training2)
       
       # remove identification variables
       df_training3 <- df_training2 %>% select(-c(1:5))
       dim(df_training3)


       
# create data partition ---------------------------------------------------

       # set seed for reproducibility
       set.seed(1234)
       
       # create partition
       inTrain <- createDataPartition(df_training3$classe, p = 0.7, list = FALSE)
       df_train <- df_training3[c(inTrain),]
       df_test <- df_training3[-c(inTrain),]
       
       # check dimensions
       dim(df_train)
       dim(df_test)
       
       # visualize correlations among variables
       pca_res <- princomp(cor(df_train %>% select(-classe)))
       
       fviz_eig(pca_res) # percentage of variange explained by each PC
       fviz_pca_ind(pca_res, col.ind = "cos2", repel = TRUE)

       

# method 1: decision tree -------------------------------------------------

       # set seed
       set.seed(12345)
       
       # fit model
       model_dec_tree <- rpart(classe ~ ., data = df_train, method = "class")
       
       # plot
       fancyRpartPlot(model_dec_tree)
       
       # validate on test data
       predict_dec_tree <- predict(model_dec_tree, df_test, type = "class")
       
       # confusion matrix
       cm_dec_tree <- confusionMatrix(predict_dec_tree, df_test$classe)
       
       # plot confusion matrix results
       df_cm_dec_tree <- cm_dec_tree$table %>% as_tibble()
       
       df_cm_dec_tree %>% ggplot(aes(x = Prediction, y = n, fill = Prediction)) +
              
              geom_bar(stat = "identity") +
              facet_grid(~Reference) +
              ggtitle(paste0("Decision tree accuracy: ", round(cm_dec_tree$overall["Accuracy"], 4)*100, "%")) +
              theme_bw() +
              theme(legend.position = "none")
       
       # out-of-sample error rate is 27.22%
       
       

# method 2: random forest -------------------------------------------------

       # set seed
       set.seed(12345)
       
       # training control
       l_control_rf <- trainControl(method = "cv", number = 3, verboseIter = FALSE) 
       
       # fit model
       model_rf <- train(classe ~ ., data = df_train, method = "rf", trControl = l_control_rf)
       
       # validate on test data
       predict_rf <- predict(model_rf, df_test)
       
       # confusion matrix
       cm_rf <- confusionMatrix(predict_rf, df_test$classe)
       
       # plot confusion matrix results
       df_cm_rf <- cm_rf$table %>% as_tibble()
       
       df_cm_rf %>% ggplot(aes(x = Prediction, y = n, fill = Prediction)) +
              
              geom_bar(stat = "identity") +
              facet_grid(~Reference) +
              ggtitle(paste0("Random forest accuracy: ", round(cm_rf$overall["Accuracy"], 4)*100, "%")) +
              theme_bw() +
              theme(legend.position = "none")
       
       # out-of-sample error rate is 0.2%, probably due to overfitting
       
       
# method 3: generalized boosted model -------------------------------------------------
       
       # set seed
       set.seed(12345)
       
       # training control
       l_control_gbm <- trainControl(method = "repeatedcv", number = 5, repeats = 1) 
       
       # fit model
       model_gbm <- train(classe ~ ., data = df_train, method = "gbm", trControl = l_control_gbm, verbose = FALSE)
       
       # validate on test data
       predict_gbm <- predict(model_gbm, df_test)
       
       # confusion matrix
       cm_gbm <- confusionMatrix(predict_gbm, df_test$classe)
       
       # plot confusion matrix results
       df_cm_gbm <- cm_gbm$table %>% as_tibble()
       
       df_cm_gbm %>% ggplot(aes(x = Prediction, y = n, fill = Prediction)) +
              
              geom_bar(stat = "identity") +
              facet_grid(~Reference) +
              ggtitle(paste0("GBM accuracy: ", round(cm_gbm$overall["Accuracy"], 4)*100, "%")) +
              theme_bw() +
              theme(legend.position = "none")
       
       # out-of-sample error rate is 1.27%
       

# applying model to quiz data ---------------------------------------------

       # RF had highest accuracy, use for quiz data
       
       predict(model_rf, df_quiz)
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       
       
       


