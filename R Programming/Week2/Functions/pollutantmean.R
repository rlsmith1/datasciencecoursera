
# Function: calculates the mean of a pollutant across a specified list of monitors

# Inputs: directory = character vector indicating location of CSV files
#       pollutant = character vector indicating pollulant of interest ("sulfate" or "nitrate")
#       id = integer vector indicating the monitor ID numbers to be used


library(tidyverse)
library(purrr)


pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        #set directory
        if(grep("specdata", directory) == 1) {
                directory <- ("R Programming/Week2/Data/specdata/")
        }else {
                print("Data does not exist in this directory")
        }
        
        #create list of file names
        files <- list.files(directory)
        file_paths = paste0(directory, sep = "/", files)
        
        #read files
        l_files <- file_paths %>% map(~read.csv(.x))
 
        #determine which monitors to use
        l_monitors <- l_files[id] 

        #select pollutant and remove NA values
        if(pollutant == "sulfate"){
                l_pollutant <- l_monitors %>% map(~as_tibble(.x)) %>% map(~select(.x, 2)) %>% map(~na.omit(.x))
                
        } else if(pollutant == "nitrate"){
                l_pollutant <- l_monitors %>% map(~as_tibble(.x)) %>% map(~select(.x, 3)) %>% map(~na.omit(.x))
                
        }else{
                print("Pollutant must be sulfate or nitrate")
        }
        
        #determine pollutant mean within each monitor
        l_means <- l_pollutant %>% map(~unlist(.x)) %>% map(~mean(.x))
                
        #determine mean across monitors
        mean_pollutant <- mean(unlist(l_means))
        
        #return result
        mean_pollutant
     
        
           
}











