
#### run_analysis.R is a script that collects and cleans a data set for use in further analysis. It performs the following 5 steps as described by the course assignment:


1. *Downloads the dataset and assign each file to an object*

       * Download the dataset using download.file() and unzip
       * Assign each relevant .txt file to a dataframe object. See the dataset README.txt for information on each .txt file. All dataframes are converted to tibbles using as_tibble() from the tidyverse package for ease of use.


2. *Merges the training and test sets to create one dataframe*

       * df_x is created by merging x_train and x_test using rbind()
       * df_y is created by merging y_train and y_test using rbind()
       * df_subject is created by merging subject_train and subject_test using rbind()
       * df_merge is created by merging these 3 dataframes using cbind()


3. *Extracts the mean and standard deviation for each measurement*

       * df_tidy is created by using select() to subset df_merge on subject, code, and measurements on mean and standard deviation (std)
       

4. *Renames the activities in the dataset to provide descriptive activity names*

       * replace the numbers in the code column of df_tidy with corresponding activity from df_activities


5. *Renames the columns in the dataset with descriptive variable names*

       * rename the code column in df_tidy to activities using rename()
       * use gsub() to replace abbreviations in column names with full name of attribute


6. *Creates a final dataset which contains the average of each variable for each activity and subject*

       * df_final is created by grouping df_merge by subject and activity using group_by(), then summarising each variable based on mean using summarise_all()
       * export df_final into final_data.txt




