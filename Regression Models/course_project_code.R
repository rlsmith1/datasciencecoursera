

# Questions ---------------------------------------------------------------

       # 1: Is automatic of manual transmission better for mpg?
       # 2: Quantify the mpg difference between automatic and manual transmissions

       # use regression models and exploratory data analysis

# libraries ---------------------------------------------------------------

       library(tidyverse)



# data --------------------------------------------------------------------

       data("mtcars")

       df_mtcars <- as_tibble(mtcars) %>% mutate(am = as.factor(am)) %>% mutate(cyl = as.factor(cyl))



# exploratory data analysis ------------------------------------------------


       df_mtcars %>% ggplot(aes(x = am, y = mpg, fill = am)) +
              
              geom_violin(alpha = 0.7) +
              geom_boxplot(width = 0.3, alpha = 0.7) +
              geom_point(size = 2) +
              
              xlab("Transmission") +
              ylab("Miles per gallon (mpg)") +
              ggtitle("Miles per gallon by transmission type") +
              scale_x_discrete(labels = c("0" = "automatic", "1" = "manual")) +
              
              theme_bw() +
              theme(legend.position = "none")
              
       

# Simple linear regression model --------------------------------------------------

       fit_lm <- lm(mpg ~ am, data = df_mtcars)
       summary(fit_lm)
       
       coef(fit_lm)[[1]] # automatic has average mpg of 17.147
       coef(fit_lm)[[1]] + coef(fit_lm)[[2]] # manual has average mpg of 24.392
       
       summary(fit_lm)$coefficients[,4] # p-values are statistically significant (p < 0.05)
       summary(fit_lm)$r.squared # r squared is only 0.36, therefore only about a third of variance in mpg can be attributed to transmission type alone
       
       
       # plot
       df_mtcars %>% ggplot(aes(x = as.numeric(as.character(am)), y = mpg)) +
              
              geom_point() +
              geom_abline(aes(intercept = coef(fit_lm)[[1]], slope = coef(fit_lm)[[2]])) +
       
              scale_x_discrete(limits = c(0, 1)) +
              xlim(-0.5, 1.5) +
              
              theme_bw()
       
       

# Multivariable linear regression -----------------------------------------


       # perform ANOVA to determine what other variables contribute to the variance
       aov(mpg ~ ., data = df_mtcars) %>% summary() # cyl, disp, and wt all have p-values < 0.05, suggesting they also contribute to the variance
       
       # include these variables in the model
       fit_multi <- lm(mpg ~ am + cyl + disp + wt + hp, data = df_mtcars)
       
       summary(fit_multi)
       
       summary(fit_multi)$r.squared # r-squared is 0.84, much higher than the previous model
       
       # thus, wt, hp, and 6 cyl are confounding variables in the relationship between am and mpg (p < 0.05)
       # holding these confounding variables constant, manual has on average 1.8 mpg higher than automatic
       
# Residual plots -----------------------------------------------------------

       par(mfrow = c(2,2))
       plot(fit_multi) # residuals are homoscedastic (have approximately the same variance)
                     # and approximately normally distributed as per the quantile plot
       
       

              
       
       
       









