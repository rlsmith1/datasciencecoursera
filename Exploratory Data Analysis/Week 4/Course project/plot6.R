


# libraries ---------------------------------------------------------------

library(tidyverse)



# data --------------------------------------------------------------------


df_pm25 <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/summarySCC_PM25.rds") %>% 
       as_tibble()

df_scc <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/Source_Classification_Code.rds") %>% 
       as_tibble()



# plot --------------------------------------------------------------------

# Question: Compare emissions from motor vehicle sources in Baltimore City with 
#      emissions from motor vehicle sources in Los Angeles County, California (fips = 06037).
#      Which city has seen greater changes over time in motor vehicle emissions?


df_p6 <- df_pm25 %>% 
       filter(fips %in% c("24510", "06037") & type == "ON-ROAD") %>% 
       group_by(fips, year) %>% 
       summarise(sum(Emissions)) %>% 
       mutate(County = ifelse(fips == "24510", "Baltimore City, MD", "Los Angeles, CA"))
    

# plot

df_p6 %>% ggplot(aes(x = factor(year), y = `sum(Emissions)`, fill = factor(year))) +
       
       geom_bar(stat = "identity") +
       facet_grid(~County) +
       xlab("Year") +
       ylab("PM2.5 Emissions (tons)") +
       ggtitle("Total PM2.5 Emissions from motor vehicle sources in Baltimore City vs Los Angeles") +
       theme(legend.position = "none")

#save

dev.copy(png, file = "Exploratory Data Analysis/Week 4/Course project/plot6.png",
         height = 480, width = 480)

dev.off()









