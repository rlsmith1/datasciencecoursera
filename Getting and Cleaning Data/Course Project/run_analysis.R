




# libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)



# data --------------------------------------------------------------------

       # download file

       download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                     destfile = "Getting and Cleaning Data/Course Project/project_data.zip", method = "curl")

       # unzip

       unzip("Getting and Cleaning Data/Course Project/project_data.zip")
       
       
       # read in data frames and assign to objects

       df_features <- read.table("Getting and Cleaning Data/Course Project/UCI HAR Dataset/features.txt", 
                                 col.names = c("n", "functions")) %>% as_tibble()
       
       df_activities <- read.table("Getting and Cleaning Data/Course Project/UCI HAR Dataset/activity_labels.txt",
                                   col.names = c("code", "activity")) %>% as_tibble()
       
       df_subject_test <- read.table("Getting and Cleaning Data/Course Project/UCI HAR Dataset/test/subject_test.txt",
                                   col.names = "subject") %>% as_tibble()
       
       df_x_test <- read.table("Getting and Cleaning Data/Course Project/UCI HAR Dataset/test/X_test.txt",
                                   col.names = df_features$functions) %>% as_tibble()
       
       df_y_test <- read.table("Getting and Cleaning Data/Course Project/UCI HAR Dataset/test/y_test.txt",
                               col.names = "code") %>% as_tibble()
       
       df_subject_train <- read.table("Getting and Cleaning Data/Course Project/UCI HAR Dataset/train/subject_train.txt",
                                     col.names = "subject") %>% as_tibble()
       
       df_x_train <- read.table("Getting and Cleaning Data/Course Project/UCI HAR Dataset/train/X_train.txt",
                               col.names = df_features$functions) %>% as_tibble()
       
       df_y_train <- read.table("Getting and Cleaning Data/Course Project/UCI HAR Dataset/train/y_train.txt",
                               col.names = "code") %>% as_tibble()
       



# Merge the training and the test sets to create one data set -------------

       
       # Combine training and test sets
       
       df_x <- rbind(df_x_train, df_x_test)
       
       # Combine training and test set labels
       
       df_y <- rbind(df_y_train, df_y_test)
       
       # Combine subject info
       
       df_subject <- rbind(df_subject_train, df_subject_test)
       
       # Merge all 3
       
       df_merge <- cbind(df_subject, df_y, df_x) %>% as_tibble()



# Extract only the measurements on the mean and standard deviation --------
       
       
       df_tidy <- df_merge %>% select(subject, code, contains("mean"), contains("std"))

       

# Use descriptive activity names to name the activities in the data --------


       df_tidy$code <- df_activities[df_tidy$code, 2]
       
       
       
# Appropriately label the data set with descriptive variable names --------
       
       
       df_tidy %<>% rename("activity" = "code")
       
       names(df_tidy) %<>% 
              
              gsub("Acc", "Accelerometer", .) %>% 
              gsub("tBody", "TimeBody", .) %>% 
              gsub("Gyro", "Gyroscope", .) %>% 
              gsub("BodyBody", "Body", .) %>% 
              gsub("Mag", "Magnitude", .) %>% 
              gsub("^t", "Time", .) %>% 
              gsub("^f", "Frequency", .) %>% 
              gsub("-mean()", "Mean", ., ignore.case = TRUE) %>% 
              gsub("-std()", "STD", ., ignore.case = TRUE) %>% 
              gsub("-freq()", "Frequency", ., ignore.case = TRUE) %>% 
              gsub("angle", "Angle", .) %>% 
              gsub("gravity", "Gravity", .)
       
       

# Create second data set with averages ------------------------------------

       
       # average each variable for each activity and each subject
       
       df_final <- df_tidy %>% 
              
              group_by(subject, activity) %>% 
              summarise_all(funs(mean))


       # write to a file
       
       df_final %>% write.table("Getting and Cleaning Data/Course Project/final_data.txt", row.names = FALSE)

























