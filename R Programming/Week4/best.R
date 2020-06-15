


# libraries ---------------------------------------------------------------


library(tidyverse)
library(purrr)





# Find the best hospital in a state ---------------------------------------------



# Inputs: 2-character abbreviated name of a state and an outcome name

# Output: 1x1 tibble with the name of the hospital that has the best (i.e. lowest) 30-day mortality 
#      for the specified outcome in that state



best <- function(state, outcome) {
       
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
       
       
       ## Return hospital name in that state with lowest 30-day death rate
       
       outcome_state <- data[data$State == state, c(2, y)] %>% 
              arrange(Hospital.Name)
       
       outcome_state <- outcome_state[outcome_state[,2] != "Not Available", ]
       
       outcome_state[,2] <- outcome_state[,2] %>% map(~as.numeric(.x))
       
       outcome_state[outcome_state[,2] == min(outcome_state[,2]), 1]
       
       
       
}








