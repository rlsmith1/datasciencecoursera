


# libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr)




# Ranking hospitals in all states -----------------------------------------

## Inputs: an outcome name (outcome) and a hospital ranking (num)

## Output:  a 2-column data frame containing the hospital in each state that has the ranking specified in num 



rankall <- function(outcome, num = "best") {
       
       ## Read outcome data
       
       data <- read.csv("R Programming/Week4/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                        colClasses = "character")
       
       
       ## Check that outcome is valid
       
       ifelse(outcome %in% c("heart attack", "heart failure", "pneumonia"), outcome, stop("invalid outcome"))
       
       
       ## select outcome
       
       y <- case_when(
              
              outcome == "heart attack" ~ 11,
              outcome == "heart failure" ~ 17,
              outcome == "pneumonia" ~ 23
              
       )
       

       ## Convert to numeric and remove NAs

              #convert to numeric
              data[,y] <- as.numeric(data[,y])
              
              #convert to numeric to sort
              data <- data[!is.na(data[,y]),]
              


       ## For each state, find the hospital of the given rank

              l_data_by_state <-  split(data, data$State)
              
              l_rank_all <- lapply(l_data_by_state, function(x, num) {
                     
                     x <- x[order(x[,y], x$Hospital.Name),]
                     
                     
                     if(class(num) == "character") {
                            
                            
                            if(num == "best") {
                                   
                                   return (x$Hospital.Name[1])
                                   
                            }
                            else if(num == "worst") {
                                   
                                   return (x$Hospital.Name[nrow(x)])
                                   
                            }
                     }
                     
                     else {
                            
                            return (x$Hospital.Name[num])
                            
                     }
                     
                     
              }, num)
              
                     
              


       ## Return a data frame with the hospital names and the (abbreviated) state name
              
              data.frame(hospital = unlist(l_rank_all), state = names(l_rank_all))

       
}






















