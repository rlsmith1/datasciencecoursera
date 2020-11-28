

library(tidyverse)
data("mtcars")
mtcars <- as_tibble(mtcars) %>% mutate(cyl = as.factor(cyl))


# 1
fit1 <- lm(mpg ~ cyl + wt, mtcars)


# 2
fit2 <- lm(mpg ~ cyl, mtcars)

# 3
fit3 <- lm(mpg ~ cyl*wt, mtcars)
summary(fit3)
summary(fit1)

library(lmtest)
lrtest(fit1, fit3) %>% summary() # likelihood ratio test comparing the two models


# 4
fit4 <- lm(mpg ~ I(wt * 0.05) + cyl, mtcars)


# 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit5 <- lm(y ~ x)
max(influence(fit5)$hat)


# 6 
influence.measures(fit5) 









