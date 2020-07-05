

# libraries ---------------------------------------------------------------

library(tidyverse)



# data --------------------------------------------------------------------


df_pm25 <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/summarySCC_PM25.rds") %>% 
       as_tibble()

df_scc <- readRDS("Exploratory Data Analysis/Week 4/NEI_data/Source_Classification_Code.rds") %>% 
       as_tibble()



# plot --------------------------------------------------------------------


# Question: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

df_scc4 <- df_scc %>% filter(grepl("Fuel Comb.*Coal", EI.Sector))

df_p4 <- df_pm25 %>% 
       filter(SCC %in% df_scc4$SCC) %>% 
       group_by(year) %>% 
       summarise(sum(Emissions)) %>% 
       mutate(emissions_kilotons = `sum(Emissions)`/1000)

# plot

       df_p4 %>% ggplot(aes(x = factor(year), y = emissions_kilotons, fill = factor(year))) +
              
              geom_bar(stat = "identity") +
              xlab("Year") +
              ylab("PM2.5 Emissions (kilotons)") +
              ggtitle("Total PM2.5 Emissions from coal-combusion related sources") +
              theme(legend.position = "none")

#save

       dev.copy(png, file = "Exploratory Data Analysis/Week 4/Course project/plot4.png",
                height = 480, width = 480)
       
       dev.off()



















