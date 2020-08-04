


# libraries ---------------------------------------------------------------

       library(tidyverse)
       library(purrr)



# simulations -------------------------------------------------------------


       # set seed for reproducibility
       
              set.seed(1234)
       
       # set parameters
       
              n <- 40
              lambda <- 0.2
              sims <- 1000

       # run simulations
              
              m_sims <- replicate(sims, rexp(n, lambda))
              
       # calculate sample mean
              
              m_means <- apply(m_sims, 2, mean)
              
       # plot histogram
              
              data.frame(m_means) %>% ggplot() +
                     
                     geom_histogram(aes(x = m_means, y = ..count.., fill = ..count..)) +
                     xlab("mean") +
                     ylab("frequency") +
                     ggtitle("Exponential function simulation means")


# Sample vs theoretical mean ----------------------------------------------
           
       # mean of an exponential distribution is 1/lambda
              
       # plot sample mean on histogram
              
              data.frame(m_means) %>% ggplot() +
                     
                     geom_histogram(aes(x = m_means, y = ..count.., fill = ..count..)) +
                     geom_vline(aes(xintercept = mean(m_means)), lwd = 2, col = "red") +
                     xlab("mean") +
                     ylab("frequency") +
                     ggtitle("Exponential function simulation means")
              
       # compare sample and theoretical means
              
              data.frame(sample_mean = mean(m_means), theoretical_mean = 1/lambda)
              
       # 95% confidence interval on sample mean
              
              t.test(m_means)[4]
              

                 
# Sample vs theoretical variance ------------------------------------------

       # variance of an exponential distribution is ((1/lambda)^2)/n
       # stdev of an exponential distribution is (1/lambda)/sqrt(n)
              
       df_var <- data.frame(sample = c(var(m_means), sd(m_means)),
                  theoretical = c( (((1/lambda)^2)/n), ((1/lambda)/sqrt(n)))) 
              
       row.names(df_var) <- c("variance", "stdev")
       
       df_var
              


# Distribution is approximately normal ------------------------------------

       
       data.frame(m_means) %>% ggplot(aes(x = m_means)) +
              
              geom_histogram(aes(y = ..density.., fill = ..density..)) + # plot sample data
              geom_vline(aes(xintercept = mean(m_means)), lwd = 1, col = "black") + # sample mean
              geom_density() + # sample distribution density
              stat_function(fun = dnorm,
                            args = list( mean = 1/lambda, sd = df_var[2,2]),
                            color = "red") + # theoretical distribution density
              geom_vline(xintercept = 1/lambda, color = "red", linetype = "dashed") + # theoretical mean
              
              xlab("mean") +
              ylab("density") +
              ggtitle("Exponential function simulation means")
       
# assume sampling without replacement      

       
       
       
       
       
       
       
       
       
       





