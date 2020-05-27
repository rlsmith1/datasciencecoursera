
# Function: calculates the mean of a pollutant across a specified list of monitors

# Inputs: directory = character vector indicating location of CSV files
#       pollutant = character vector indicating pollulant of interest ("sulfate" or "nitrate")
#       id = integer vector indicating the monitor ID numbers to be used



# libraries ---------------------------------------------------------------


library(tidyverse)
library(purrr)


# define function ---------------------------------------------------------


pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        #set directory
        if(grep("specdata", directory) == 1) {
                directory <- ("R Programming/Week2/Data/specdata/")
        }else {
                print("Data does not exist in this directory")
        }
        
        #create list of file names
        files <- list.files(directory)
        file_paths <- paste0(directory, sep = "/", files)
        
        #read files and convert to one big data frame
        l_files <- file_paths %>% map(~read.csv(.x))
        df_files <- l_files %>% map(~as_tibble(.x)) %>% bind_rows()
 
        #determine which monitors to use
        l_monitors <- l_files[id] 

        #select pollutant and remove NA values
        if(pollutant == "sulfate"){
                
                df_files_sulf <- df_files[!is.na(df_files$sulfate),]
                mean_pollutant <- df_files_sulf %>% filter(ID %in% id) %>% summarise(mean(sulfate))
                
        } else if(pollutant == "nitrate"){
                
                df_files_nitr <- df_files[!is.na(df_files$nitrate),]
                mean_pollutant <- df_files_nitr %>% filter(ID %in% id) %>% summarise(mean(nitrate))
                
        }else{
                print("Pollutant must be sulfate or nitrate")
        }
        
        #return result
        mean_pollutant
     
        
           
}















