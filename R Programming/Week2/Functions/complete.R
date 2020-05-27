
# Function: Reports number of completely observed cases in each data file

# Inputs: directory = character vector indicating location of CSV files
#      id = integer vector indicating the monitor ID numbers to be used



# libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr)


# define function ---------------------------------------------------------


complete <- function(directory, id = 1:332){
       
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
       
       #determine which monitors to use
       l_monitors <- l_files[id] 
       
       #create dataframe containing monitor ID and number of complete observations
       df_nobs <- l_monitors %>% map(~as_tibble(.x)) %>% 
              map(~na.omit(.x)) %>% 
              map(~mutate(.x, nobs = nrow(.x))) %>% 
              map(~select(.x, 4, 5)) %>% 
              map(~unique(.x)) %>% 
              bind_rows()
       
       #return dataframe
       df_nobs
       
       
       
}







