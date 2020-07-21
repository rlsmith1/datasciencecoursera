


# libraries ---------------------------------------------------------------
       
       library(tidyverse)
       library(magrittr)

# data processing ---------------------------------------------------------

       # download data

       download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                     destfile = "Reproducible Research/Course project 2/StormData.csv.bz2")

       if(!exists(df_stormdata)){
              
              df_stormdata <- read.csv(bzfile("Reproducible Research/Course project 2/StormData.csv.bz2"), header = TRUE) %>% 
                     as_tibble()

       }

       # extract variables of interest

       # events: EVTYPE = weather event
       # economic: PROPDMG = approximate property damages
       #             PROPDMGEXP = units for property damage value
       #             CROPDMG = approximate crop damages
       #             CROPDMGEXP = units for crop damage value
       # health: FATALITIES = approximate number of deaths
       #             INJURIES = approximate number of injuries 

       df_stormdata2 <- df_stormdata %>% select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, FATALITIES, INJURIES)
       
       # Population health data
       
              # total fatalities by event
              
              df_fatalities <- df_stormdata2 %>% 
                     group_by(EVTYPE) %>% 
                     summarise(sum(FATALITIES)) %>% 
                     rename("total" = "sum(FATALITIES)") %>% 
                     arrange(-total) %>% 
                     head(10) %>% 
                     mutate(type = "Fatalities")
              
              # total injuries by event
              
              df_injuries <- df_stormdata2 %>% 
                     group_by(EVTYPE) %>% 
                     summarise(sum(INJURIES)) %>% 
                     rename("total" = "sum(INJURIES)") %>% 
                     arrange(-total) %>% 
                     head(10) %>% 
                     mutate(type = "Injuries")
              
              # combine
              
              df_health = rbind(df_fatalities, df_injuries)
              
       # Economic consequences data
              
              # Assign values for the property exponent data 
              
              df_stormdata3 <- df_stormdata2 %>% 
                     
                     mutate(prop_multiplier = case_when(
                            
                            
                            df_stormdata2$PROPDMGEXP == "" ~ 1,
                            df_stormdata2$PROPDMGEXP == "-" ~ 0,
                            df_stormdata2$PROPDMGEXP == "+" ~ 0,
                            df_stormdata2$PROPDMGEXP == "0" ~ 0,
                            df_stormdata2$PROPDMGEXP == "2" ~ 100,
                            df_stormdata2$PROPDMGEXP == "3" ~ 1000,
                            df_stormdata2$PROPDMGEXP == "4" ~ 10000,
                            df_stormdata2$PROPDMGEXP == "5" ~ 1e+05,
                            df_stormdata2$PROPDMGEXP == "6" ~ 1e+06,
                            df_stormdata2$PROPDMGEXP == "7" ~ 1e+07,
                            df_stormdata2$PROPDMGEXP == "B" ~ 1e+09,
                            df_stormdata2$PROPDMGEXP == "h" ~ 100,
                            df_stormdata2$PROPDMGEXP == "H" ~ 100,
                            df_stormdata2$PROPDMGEXP == "K" ~ 1000,
                            df_stormdata2$PROPDMGEXP == "m" ~ 1e+06,
                            df_stormdata2$PROPDMGEXP == "M" ~ 1e+06
                            
                            
                     )) %>% 
                     
                     mutate(crop_multiplier = case_when(
                            
                            
                            df_stormdata2$CROPDMGEXP == "" ~ 1,
                            df_stormdata2$CROPDMGEXP == "-" ~ 0,
                            df_stormdata2$CROPDMGEXP == "+" ~ 0,
                            df_stormdata2$CROPDMGEXP == "0" ~ 0,
                            df_stormdata2$CROPDMGEXP == "2" ~ 100,
                            df_stormdata2$CROPDMGEXP == "3" ~ 1000,
                            df_stormdata2$CROPDMGEXP == "4" ~ 10000,
                            df_stormdata2$CROPDMGEXP == "5" ~ 1e+05,
                            df_stormdata2$CROPDMGEXP == "6" ~ 1e+06,
                            df_stormdata2$CROPDMGEXP == "7" ~ 1e+07,
                            df_stormdata2$CROPDMGEXP == "B" ~ 1e+09,
                            df_stormdata2$CROPDMGEXP == "h" ~ 100,
                            df_stormdata2$CROPDMGEXP == "H" ~ 100,
                            df_stormdata2$CROPDMGEXP == "K" ~ 1000,
                            df_stormdata2$CROPDMGEXP == "m" ~ 1e+06,
                            df_stormdata2$CROPDMGEXP == "M" ~ 1e+06
                            
                            
                     )) 
              
              # property damages
              
              df_propdmg <- df_stormdata3 %>% 
                     mutate(propdmgval = PROPDMG * prop_multiplier) %>% 
                     group_by(EVTYPE) %>% 
                     summarise(sum(propdmgval)) %>% 
                     rename("total" = "sum(propdmgval)") %>% 
                     arrange(-total) %>% 
                     head(10) %>% 
                     mutate(type = "Property")
              
              # crop damages
              
              df_cropdmg <- df_stormdata3 %>% 
                     mutate(cropdmgval = CROPDMG * crop_multiplier) %>% 
                     group_by(EVTYPE) %>% 
                     summarise(sum(cropdmgval)) %>% 
                     rename("total" = "sum(cropdmgval)") %>% 
                     arrange(-total) %>% 
                     head(10) %>% 
                     mutate(type = "Crop")
              
              # combine
              
              df_econ = rbind(df_propdmg, df_cropdmg)
              
       


# results -----------------------------------------------------------------

## Across the US, which types of events are most harmful with respect to population health??
       
       # plot
       
       df_health %>% ggplot(aes(x = EVTYPE, y = total, fill = type)) +
              geom_bar(stat = "identity") + 
              facet_grid(~type, scales = "free") +
              coord_flip() +
              xlab("Event type") +
              ylab("Number of people") +
              ggtitle("Impact of specific weather event on population health")
              
       # Tornadoes are most harmful to population health in terms of injuries and fatalities
       
       
## Across the US, which types of events have the greatest economic consequences??

       # plot
       
       df_econ %>% ggplot(aes(x = EVTYPE, y = total, fill = type)) +
              geom_bar(stat = "identity") + 
              facet_grid(~type, scales = "free") +
              coord_flip() +
              xlab("Event type") +
              ylab("Economic cost (in billions of dollars)") +
              ggtitle("Economic consequences of specific weather event")
       
       # Floods cause the most economic damage, with regard to both property and crops
       












