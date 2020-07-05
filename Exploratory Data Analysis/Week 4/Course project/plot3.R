


# libraries ---------------------------------------------------------------

library(tidyverse)



# data --------------------------------------------------------------------


df_pm25 <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/summarySCC_PM25.rds") %>% 
       as_tibble()



# plot --------------------------------------------------------------------


# Question: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#      which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#      Which have seen increases in emissions from 1999–2008? 

# Use the ggplot2 plotting system to make a plot answer this question.


df_p3 <- df_pm25 %>% 
       filter(fips == 24510) %>% 
       group_by(year, type) %>% 
       summarise(sum(Emissions))


# plot

       df_p3 %>% ggplot(aes(x = factor(year), y = `sum(Emissions)`, fill = type)) +
              
              geom_bar(stat = "identity") +
              facet_grid(. ~ type) +
              xlab("Year") +
              ylab("PM2.5 Emissions (tons)") +
              ggtitle("Total PM2.5 Emissions by source type in Baltimore City") +
              theme(legend.position = "none")

#save
       
       dev.copy(png, file = "Exploratory Data Analysis/Week 4/Course project/plot3.png",
                height = 480, width = 480)
       
       dev.off()
       














