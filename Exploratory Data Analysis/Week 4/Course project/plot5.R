


# libraries ---------------------------------------------------------------

library(tidyverse)



# data --------------------------------------------------------------------


df_pm25 <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/summarySCC_PM25.rds") %>% 
       as_tibble()

df_scc <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/Source_Classification_Code.rds") %>% 
       as_tibble()



# plot --------------------------------------------------------------------


# Question: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


df_p5 <- df_pm25 %>% 
       filter(fips == 24510 & type == "ON-ROAD") %>% 
       group_by(year) %>% 
       summarise(sum(Emissions))


# plot

       df_p5 %>% ggplot(aes(x = factor(year), y = `sum(Emissions)`, fill = factor(year))) +
              
              geom_bar(stat = "identity") +
              xlab("Year") +
              ylab("PM2.5 Emissions (tons)") +
              ggtitle("Total PM2.5 Emissions from motor vehicle sources in Baltimore City") +
              theme(legend.position = "none")

#save

       dev.copy(png, file = "Exploratory Data Analysis/Week 4/Course project/plot5.png",
                height = 480, width = 480)
       
       dev.off()









