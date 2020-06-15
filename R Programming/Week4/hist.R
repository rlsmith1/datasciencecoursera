


# libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr)



# data --------------------------------------------------------------------

df_outcome <- read.csv("R Programming/Week4/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                       colClasses = "character")

head(df_outcome)
ncol(df_outcome)
nrow(df_outcome)


# 1. hist: 30 day mortality rates for heart attack --------------------------------------------

df_outcome[, 11] <- as.numeric(df_outcome[, 11])

hist(df_outcome[, 11])



# 2. best hospital in a state ---------------------------------------------

# inputs: 2-character abbreviated name of a state and an outcome name

# output: character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality 
#for the specified outcome in that state

names(df_outcome)

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





