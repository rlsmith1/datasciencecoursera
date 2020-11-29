


# libraries ---------------------------------------------------------------

       library(tidyverse)
       library(MASS)



# questions ---------------------------------------------------------------

shuttle <- shuttle %>% as_tibble %>% mutate(useNum = as.factor(ifelse(use == "auto", 1, 0)))

# 1

fit1 <- glm(useNum ~ wind - 1, data = shuttle, family = "binomial")
summary(fit1)

exp(coef(fit1)[[1]])/exp(coef(fit1)[[2]]) #odds ratio

# 2 
fit2 <- glm(useNum ~ wind + magn - 1, data = shuttle, family = "binomial")
summary(fit2)

exp(coef(fit2)[[1]])/exp(coef(fit2)[[2]])

# 3
fit3 <- glm((1 - as.numeric(as.character(useNum))) ~ wind - 1, data = shuttle, family = "binomial")
summary(fit3)


# 4

data("InsectSprays")
InsectSprays <- InsectSprays %>% as_tibble()

fit4 <- glm(count ~ spray - 1, data = InsectSprays, family = "poisson")

exp(coef(fit4)[[1]])/exp(coef(fit4)[[2]]) # estimated relative risk


# 5
fit5a <- glm(count ~ spray, data = InsectSprays, family = "poisson", offset = log(count + 1))
fit5b <- glm(count ~ spray, data = InsectSprays, family = "poisson", offset = log(count + 1) + log(10))

coef(fit5a)
coef(fit5b) # intercept changes, but coefficient estimates are unchanged


# 6 
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

knots <- 0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xmat <- cbind(1, x, splineTerms)
fit6 <- lm(y ~ xmat - 1)
yhat <- predict(fit6)

summary(fit6)
coef(fit6)[2:3] %>% sum()










