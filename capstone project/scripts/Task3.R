

# Tasks to accomplish
# 
# Build basic n-gram model - using the exploratory analysis you performed, 
#      build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
# Build a model to handle unseen n-grams - 
#      in some cases people will want to type a combination of words that does not appear in the corpora. 
#      Build a model to handle cases where a particular n-gram isn't observed.

# Questions to consider
# 
# How can you efficiently store an n-gram model (think Markov Chains)?
# How can you use the knowledge about word frequencies to make your model smaller and more efficient?
# How many parameters do you need (i.e. how big is n in your n-gram model)?
# Can you think of simple ways to "smooth" the probabilities 
#      (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
#        How do you evaluate whether your model is any good?
#        How can you use backoff models to estimate the probability of unobserved n-grams?



# libraries ---------------------------------------------------------------

       
       library(tidytext)
       library(tidyverse)


# read in english data ----------------------------------------------------


       str_eng_news <- readLines("capstone project/data/final/en_US/en_US.news.txt")
       str_eng_blogs <- readLines("capstone project/data/final/en_US/en_US.blogs.txt")
       str_eng_twitter <- readLines("capstone project/data/final/en_US/en_US.twitter.txt")



       
# format data -------------------------------------------------------------
       

       # convert each to tibble
       df_eng_news <- tibble(line = 1:length(str_eng_news), text = str_eng_news)
       df_eng_blogs <- tibble(line = 1:length(str_eng_blogs), text = str_eng_blogs)
       df_eng_twitter <- tibble(line = 1:length(str_eng_twitter), text = str_eng_twitter)

       
       

# unnest to 4-grams -------------------------------------------------------


       # sample each
       set.seed(2021)
       
       df_eng_news_spl <- sample_n(df_eng_news, size = nrow(df_eng_news)*0.05)
       df_eng_blogs_spl <- sample_n(df_eng_blogs, size = nrow(df_eng_blogs)*0.05)
       df_eng_twitter_spl <- sample_n(df_eng_twitter, size = nrow(df_eng_twitter)*0.05)
       
       # combine into one
       df_eng_spl <- bind_rows(df_eng_news_spl, df_eng_blogs_spl, df_eng_twitter_spl)
       
       # 4-grams
       df_eng_4gram <- df_eng_spl %>% unnest_tokens(`4gram`, text, token = "ngrams", n = 4)
       
       # separate
       df_eng_4grams_sep <- df_eng_4gram %>% separate(`4gram`, c("word1", "word2", "word3", "word4"), sep = " ")
       
       # filter out NAs and other languages
       df_eng_4grams_filt <- df_eng_4grams_sep %>% filter(!is.na(word1), !is.na(word2), !is.na(word3), !is.na(word4))
       
       
       

# estimate based on frequency ---------------------------------------------

        
       # unigrams
       df_eng_unigrams_filt <- df_eng_4grams_filt %>% dplyr::select(c(word1))
       df_eng_unigrams_count <- df_eng_4grams_filt %>% count(word1, sort = TRUE)
       
       # bigrams: word1 + word2, word2 + word3, word3 + word4
       df_eng_bigrams_filt <- df_eng_4grams_filt %>% dplyr::select(c(word1, word2))
       df_eng_bigrams_count <- df_eng_bigrams_filt %>% count(word1, word2, sort = TRUE)
       
       # trigrams: word1 + word2 + word3, word2 + word3 + word4
       df_eng_trigrams_filt <- df_eng_4grams_filt %>% dplyr::select(c(word1, word2, word3))
       df_eng_trigrams_count <- df_eng_trigrams_filt %>% count(word1, word2, word3, sort = TRUE)
       
       # 4grams: word1 + word2 + word3 + word4
       df_eng_4grams_count <- df_eng_4grams_filt %>% count(word1, word2, word3, word4, sort = TRUE)
       
       # save objects
        save(df_eng_unigrams_filt, df_eng_unigrams_count,
             df_eng_bigrams_filt, df_eng_bigrams_count, 
             df_eng_trigrams_filt, df_eng_trigrams_count,
             df_eng_4grams_filt, df_eng_4grams_count,
             file = "capstone project/task3_objects.Rdata")
        
        load("capstone project/task3_objects.Rdata")

        
# N-gram model ------------------------------------------------------------


        # predict the last word of an n-gram from previous n-1 word sequence (MLE)
        
                # n = 1
                f_bigrams_freq <- function(word.1){
                        
                        df_eng_bigrams_count %>% filter(word1 == word.1) %>% 
                                mutate(freq = n/sum(n))
                        
                }
        
                # n = 2
                f_trigrams_freq <- function(word.1, word.2){
                        
                        df_eng_trigrams_count %>% filter(word1 == word.1 & word2 == word.2) %>% 
                                mutate(freq = n/sum(n))
                        
                }
                
                # n = 3
                f_4grams_freq <- function(word.1, word.2, word.3){
                        
                        df_eng_4grams_count %>% filter(word1 == word.1 & word2 == word.2 & word3 == word.3) %>% 
                                mutate(freq = n/sum(n))
                        
                }
                
        # smooth probability distributions by assigning non-zero probabilities to unseen N-grams
                
                # if trigram doesn't exist, back up to bigram
                f_backoff_trigram <- function(word.1, word.2, word.3) {
                        
                        if (nrow(df_eng_4grams_count %>% filter(word1 == word.1, word2 == word.2, word3 == word.3)) > 0) {
                                
                                df <- df_eng_4grams_count %>% 
                                        filter(word1 == word.1, word2 == word.2, word3 == word.3) %>% 
                                        mutate(freq = n/sum(n))
                                
                        } else {
                                
                                df <- df_eng_trigrams_count %>% 
                                        filter(word1 == word.2, word2 == word.3) %>% 
                                        mutate(freq = n/sum(n))
                                
                        }
                                
                        return(df)
                        
                }
                
                # if bigram doesn't exist, back up to unigram
                f_backoff_bigram <- function(word.1, word.2) {
                        
                        if (nrow(df_eng_trigrams_count %>% filter(word1 == word.1, word2 == word.2)) > 0) {
                                
                                df <- df_eng_trigrams_count %>% 
                                        filter(word1 == word.1, word2 == word.2) %>% 
                                        mutate(freq = n/sum(n))
                                
                        } else {
                                
                                df <- df_eng_bigrams_count %>% 
                                        filter(word1 == word.2) %>% 
                                        mutate(freq = n/sum(n))
                                
                        }
                        
                        return(df)
                        
                }
                
                # combine to back off
                f_backoff <- function(word.1, word.2, word.3){
                        
                        
                        if (nrow(f_backoff_trigram(word.1, word.2, word.3) > 0)) {
                                
                                df <- f_backoff_trigram(word.1, word.2, word.3)
                                
                        } else {
                                
                                df <- f_backoff_bigram(word.2, word.3)
                                
                        }
                        
                        return(df)

                }

                
        # Explore model!
        f_backoff("eat", "my", "soup")
                
        
        
        
        
        
        
        
       
       
       
       
       
       
       
              
       
       
