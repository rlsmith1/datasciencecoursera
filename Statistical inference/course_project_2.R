


# libraries ---------------------------------------------------------------

library(tidyverse)



# data --------------------------------------------------------------------


data("ToothGrowth")

df_toothgrowth <- as_tibble(ToothGrowth)

head(df_toothgrowth)

# summary -----------------------------------------------------------------

summary(ToothGrowth)

# exploratory data analysis -----------------------------------------------

# convert dose to factor

df_toothgrowth <- df_toothgrowth %>% mutate(dose = as.factor(dose))

# plot tooth length as a function of dose, by supplement

df_toothgrowth %>% ggplot(aes(x = dose, y = len, fill = dose)) +
       
       geom_boxplot() +
       facet_grid(~supp) +
       ggtitle("Tooth length vs dose by delivery method") + 
       ylab("Tooth length")

# plot tooth length as function of delivery method by dose

df_toothgrowth %>% ggplot(aes(x = supp, y = len, fill = supp)) +
       
       geom_boxplot() +
       facet_grid(~dose) +
       ggtitle("Tooth length vs delivery method by dosage") + 
       xlab("Delivery method") +
       ylab("Tooth length")


# compare tooth growth supp and dose --------------------------------------

# Does tooth length depend on delivery method?

      t.test(len ~ supp, data = df_toothgrowth) 
      
      # p-value is 0.06, confidence interval contains zero, so supplement delivery method doesn't affect tooth length
      
# Does tooth length depend on dose?
      
      # compare 0.5 to 1.0
      
              t.test(len ~ dose, data = df_toothgrowth %>% filter(dose == c("0.5","1")))
              
      # compare 0.5 to 2.0
              
              t.test(len ~ dose, data = df_toothgrowth %>% filter(dose == c("0.5","2")))
      
      # compare 1.0 to 2.0
              
              t.test(len ~ dose, data = df_toothgrowth %>% filter(dose == c("1","2")))

       # all p-values less than 0.05 no CIs cross 0, therefore we can reject the null hypothesis and assume that dose affects tooth length


# conclusions -------------------------------------------------------------

# include assumptions for conclusions








