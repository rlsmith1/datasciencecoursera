

# libraries ---------------------------------------------------------------


       library(shiny)
       library(tidyverse)
       library(plotly)
       library(leaflet)
       library(rgeos)
       library(rworldmap)
       library(rworldxtra)


# load data ---------------------------------------------------------------


       df_covid <- read_csv("owid-covid-data.csv", 
                            col_types = paste0(c(rep("c", 4), rep("d", 55)), collapse = "")) 
       

# format data -------------------------------------------------------------

       df_covid <- df_covid %>% mutate(date = as.Date(date, format = "%m/%d/%y"))
       
       # current
       df_covid_current <- df_covid %>% filter(date == "2021-03-03")
       
       # get world map
       sp_wmap <- getMap(resolution = "high")
       
       # get centroids
       centroids <- rgeos::gCentroid(sp_wmap, byid = TRUE)
       
       # convert to df
       df_location <- centroids %>% 
              as.data.frame() %>% 
              rownames_to_column(var = "location") %>% 
              as_tibble() %>% 
              dplyr::rename("long" = "x", "lat" = "y") %>% 
              add_row(location = "United States", long = -98.56, lat = 39.8)
       

       
# plots -------------------------------------------------------------------


       # Time course plot code
       shinyServer(function(input, output) {
       
           output$p_time_course <- renderPlotly({
       
                  # Continents over time
                  p1 <- df_covid %>% filter(continent == input$continent & !is.na(input$case_stat)) %>% 
                         
                         ggplot(aes_string(x = "date", y = input$case_stat)) +
                         geom_line(aes(color = location)) +
                         theme_bw() 
                  
                  # print plotly
                  ggplotly(p1)
                  
                  
                  
           })
           
           output$p_map <- renderLeaflet({
                  
                  df_subset <- df_covid_current %>% 
                         dplyr::select(c(location, input$radius)) %>% 
                         dplyr::rename("value" = 2) %>% 
                         left_join(df_location, by = "location")

                  df_subset  %>% 
                         
                         leaflet() %>% 
                         addTiles() %>% 
                         addCircles(radius = df_subset$value*10^-1.5,
                                    label = paste0(df_subset$location, ": ", df_subset$value),
                                    weight = 1)
                  
                  
           })
           
           output$p_comparisons <- renderPlotly({
                  
                   print(df_covid_current)
                   
                   print(input$x)
                   
                   print(input$y)
                   
                   print(input$color)
                   
                   
                  p3 <- df_covid_current %>% filter(location != "World") %>% 
                         
                         ggplot() +
                         geom_point(aes_string(x = input$x, y = input$y, color = input$color, label = "location")) +
                         theme_bw()
                  
                  
                  ggplotly(p3)
                  

                                    
                  
           })
           

       
})



       
       
       

       
       
       
       
       
       
       
       
       


