
# Function: Calculates the correlation between sulfate and nitrate for monitor IDs where the number of 
#      completely observed cases is greater than the threshold

# Inputs: directory = character vector indicating location of CSV files
#      threshold = numeric vector of length 1 indicating number of complete observations required to compute
#                    correlation between sulfate and nitrate (default = 0)



# libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr)
library(corrr)


# define function ---------------------------------------------------------


#source previously defined "complete" function for number of complete observations in each df

        source("R Programming/Week2/Functions/complete.R")



#function

corr <- function(directory, threshold = 0){
       
       #set directory
       if(grep("specdata", directory) == 1) {
              directory <- ("R Programming/Week2/Data/specdata/")
       }else {
              print("Data does not exist in this directory")
       }
       
       #create list of file names
       files <- list.files(directory)
       file_paths <- paste0(directory, sep = "/", files)
       
       #read files
       l_files <- file_paths %>% map(~read.csv(.x))
       
       #find number of complete observations for each file
       df_nobs <- complete("specdata")
       
       #set threshold
       df_nobs_thresh <- df_nobs %>% filter(nobs > threshold)
       
       #extract monitor ID numbers from files that meet threshold
       keep <- df_nobs_thresh[["ID"]]
       
       #extract files that meet threshold based on monitor ID number, and omit incomplete rows
       l_thresh <- l_files[keep] %>% map(~as_tibble(.x)) %>% map(~na.omit(.x))
       
       #find correlation between sulfate and nitrate for each extracted file
       correlation <- c()

       for(i in 1:length(l_thresh)){
               
               tmp[i] <- cor(l_thresh[[i]]$sulfate, l_thresh[[i]]$nitrate)
               correlation[i] <- tmp[i]
               
       }
       
       #return result
       correlation
       
       
}






