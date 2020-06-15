


# libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr)



# Ranking hospitals by outcome in a state ---------------------------------


## Inputs: the 2-character abbreviated name of a state (state), 
##            an outcome (outcome), 
##            and the ranking of a hospital in that state for that outcome (num)

## Output: returns a character vector with the name of the hospital that has the ranking specified by the num argument 


rankhospital <- function(state, outcome, num = "best") {
       
       ## Read outcome data
       
       data <- read.csv("R Programming/Week4/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                        stringsAsFactors = FALSE) %>% 
              as_tibble()
       
       
       ## Check that state and outcome are valid
       
       if (!(state %in% data$State)){
              
              stop("invalid state")
              
       }
       
       
       ifelse(outcome %in% c("heart attack", "heart failure", "pneumonia"), outcome, stop("invalid outcome"))
       

       ## select outcome
       
       y <- case_when(
              
              outcome == "heart attack" ~ 11,
              outcome == "heart failure" ~ 17,
              outcome == "pneumonia" ~ 23
              
       )
       
       
       ## Return hospital name in that state with the given rank
       
              #subset state and outcome
              outcome_state <- data[data$State == state, c(2, y)] 
              
              #remove NAs
              outcome_state <- outcome_state[outcome_state[,2] != "Not Available", ]
              
              #convert to numeric to sort
              outcome_state[,2] <- outcome_state[,2] %>% map(~as.numeric(.x)) 
              
              outcome_state <- as.data.frame(outcome_state)
       
              #order
              df_sorted <- outcome_state[order(outcome_state[,2], outcome_state[,1]),]
              
              #account for use of "best" and "worst"
              
              if(num == "best"){
                     
                     num <- 1
              }else if(num == "worst"){
                     
                     num <- nrow(df_sorted)
              }else if(num > nrow(df_sorted)){
                     
                     stop("NA")
              }
              
              
              #output
       
              df_sorted$Hospital.Name[num]
              
     
       
         
}






















