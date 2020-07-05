

# libraries ---------------------------------------------------------------

library(tidyverse)



# data --------------------------------------------------------------------


df_pm25 <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/summarySCC_PM25.rds") %>% 
       as_tibble()



# plot --------------------------------------------------------------------

# Question: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#      (fips = 24510) from 1999 to 2008? 

# Use the base plotting system to make a plot answering this question.


df_p2 <- df_pm25 %>% 
       filter(fips == 24510) %>% 
       group_by(year) %>% 
       summarise(sum(Emissions))

#plot

       barplot(names.arg = df_p1$year,
               height = df_p1$total_emissions_MT,
               xlab = "Year",
               ylab = "PM2.5 Emissions (Megatons)",
               main = "Total PM2.5 Emissions by year")


#save

       dev.copy(png, file = "Exploratory Data Analysis/Week 4/Course project/plot1.png",
                height = 480, width = 480)
       
       dev.off()
       













