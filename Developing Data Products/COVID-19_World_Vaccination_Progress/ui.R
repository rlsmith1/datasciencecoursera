
# libraries ---------------------------------------------------------------


    library(shiny)
    library(tidyverse)
    library(plotly)
    library(leaflet)



# data --------------------------------------------------------------------

    df_covid <-read_csv("owid-covid-data.csv", col_types = paste0(c(rep("c", 4), rep("d", 55)), collapse = "")) %>% 
        
        mutate(date = as.Date(date, format = "%m/%d/%y"))


# plot UIs ----------------------------------------------------------------



# Draw time course plot
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 time course data"),
    
    # Input options
    fluidRow(
           
           column(4,
                  wellPanel(
                         selectInput("continent",
                                     "Continent:",
                                     choices = unique(filter(df_covid, continent != "")$continent))),
                  
                  wellPanel(
                         selectInput("case_stat",
                                     "Case statistic:",
                                     choices = c(colnames(df_covid)[5:43]),
                                     selected = "total_cases"))
                  
           ),

           column(8,
                  plotlyOutput("p_time_course")
           )
    ),
    
    # new app
    
    # Application title
    titlePanel("Country comparisons"),
    
    # Input options
    fluidRow(
           
           column(4,
                  wellPanel(
                         selectInput("x",
                                     "x-axis:",
                                     choices = c("human_development_index", "population", "population_density", "median_age", 
                                                 "aged_65_older", "aged_70_older",
                                                 "gdp_per_capita", "extreme_poverty", "cardiovasc_death_rate", "diabetes_prevalence",
                                                 "female_smokers", "male_smokers", "handwashing_facilities", "hospital_beds_per_thousand",
                                                 "life_expectancy"),
                                     
                                     selected = "total_deaths")),
                  wellPanel(
                         selectInput("y",
                                     "y-axis:",
                                     choices = c("total_cases", "new_cases", "total_deaths", "new_deaths", "total_cases_per_million",
                                                 "new_cases_per_million", "total_deaths_per_million", "new_deaths_per_million",
                                                 "reproduction_rate", "ice_patients", "icu_patients_per_million", "hosp_patients",
                                                 "hosp_patients_per_million", "weekly_icu_admissions", "weekly_icu_admissions_per_million",
                                                 "new_tests", "total_tests", "total_tests_per_thousand", "new_tests_per_thousand", 
                                                 "tests_units", "total_vaccinations", "people_vaccinated", "people_fully_vaccinated",
                                                 "new_vaccinations", "total_vaccinations_per_hundred"),
                                     selected = "human_development_index")),
                  wellPanel(
                         selectInput("color",
                                     "color:",
                                     choices = c("continent", "location"),
                                     selected = "location")
                         
                         
                         
                  )),
           
           
           column(8, plotlyOutput("p_comparisons"))), 
    
    # new app
    
    # Application title
    titlePanel("COVID-19 Map"),
    
    
    # Input options
    fluidRow(
           
           column(3,
                  wellPanel(
                         selectInput("radius",
                                     "Circle size:",
                                     choices = c("total_cases", "new_cases", 
                                                 "total_deaths", "new_deaths", 
                                                 "total_vaccinations", "new_vaccinations")))),
           
           column(9, leafletOutput("p_map")) ),
    
    
    
    
    ))
          
















    





