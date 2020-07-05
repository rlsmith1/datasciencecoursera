


# libraries ---------------------------------------------------------------

library(tidyverse)



# data --------------------------------------------------------------------

download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", 
              destfile = "Exploratory Data Analysis/Week 4/NEI_data.zip")

unzip("Exploratory Data Analysis/Week 4/NEI_data.zip")

df_pm25 <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/summarySCC_PM25.rds") %>% 
       as_tibble()



# plot --------------------------------------------------------------------


# Question: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
#      for each of the years 1999, 2002, 2005, and 2008.


df_p1 <- df_pm25 %>% group_by(year) %>% summarise(sum(Emissions))

df_p1 <- df_p1 %>% mutate(total_emissions_MT = `sum(Emissions)`/10^6)


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
























