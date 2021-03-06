


# Tasks to accomplish
# 
# Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora. 
# Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.

# Questions to consider
# 
# Some words are more frequent than others - what are the distributions of word frequencies? 
#        What are the frequencies of 2-grams and 3-grams in the dataset? 
#        How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 
#        How do you evaluate how many of the words come from foreign languages? 
#        Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?



# libraries ---------------------------------------------------------------

       library(tidytext)
       library(tidyverse)
       library(wordcloud)
        library(ggwordcloud)
        library(textdata)
        library(igraph)
        library(ggraph)

       data(stop_words)





# read in english data ----------------------------------------------------


       str_eng_news <- readLines("capstone project/data/final/en_US/en_US.news.txt")
       str_eng_blogs <- readLines("capstone project/data/final/en_US/en_US.blogs.txt")
       str_eng_twitter <- readLines("capstone project/data/final/en_US/en_US.twitter.txt")


       
# format data -------------------------------------------------------------

       # convert each to tibble
       df_eng_news <- tibble(line = 1:length(str_eng_news), text = str_eng_news)
       df_eng_blogs <- tibble(line = 1:length(str_eng_blogs), text = str_eng_blogs)
       df_eng_twitter <- tibble(line = 1:length(str_eng_twitter), text = str_eng_twitter)
       
       # unnest tokens
       df_eng_news_unnest <- df_eng_news %>% 
               unnest_tokens(word, text) %>% # automatically makes everything lowercase and removes punctuation
               mutate(source = "news")
       
       df_eng_blogs_unnest <- df_eng_blogs %>% 
               unnest_tokens(word, text) %>% 
               mutate(source = "blogs")
       
       df_eng_twitter_unnest <- df_eng_twitter %>% 
               unnest_tokens(word, text) %>% 
               mutate(source = "twitter")

       # remove numbers since we're looking at words
       df_eng_news_unnest <- df_eng_news_unnest %>% filter(!grepl("[0-9]", word))
       df_eng_blogs_unnest <- df_eng_blogs_unnest %>% filter(!grepl("[0-9]", word))
       df_eng_twitter_unnest <- df_eng_twitter_unnest %>% filter(!grepl("[0-9]", word))
       
       # combine into one
       df_eng <- rbind(df_eng_news_unnest, df_eng_blogs_unnest, df_eng_twitter_unnest)
       
       # remove stop words
       df_eng <- df_eng %>% anti_join(stop_words, by = "word")
       
       # save so I don't have to remake
       save(df_eng_news, df_eng_blogs, df_eng_twitter, file = "capstone project/outputs/dfs.Rdata")
       
       save(df_eng_news_unnest, df_eng_blogs_unnest, df_eng_twitter_unnest, df_eng, 
            file = "capstone project/outputs/unnested_dfs.Rdata")
       

       
       
# one word frequencies -----------------------------------------------------------------

       
       # frequency of words (not including extremely common "stop words")
       df_eng_wordcount <- df_eng %>% count(word, sort = TRUE)     
       
       p_eng_wordcount <- df_eng_wordcount %>% head(n = 20) %>% mutate(word = reorder(word, n)) %>% 
              
              ggplot(aes(x = n, y = word)) +
              geom_col(fill = "#00BFC4") +
               theme_bw() +
               ggtitle("Most frequently used words in the English language")
       
       # wordcloud
       p_eng_wordcloud <- ggplot(df_eng_wordcount %>% head(n = 200), aes(label = word, size = n)) +
               geom_text_wordcloud_area(rm_outside = TRUE) +
               scale_size_area(max_size = 10) +
               theme_minimal()
       
       # save
       save(df_eng_wordcount, p_eng_wordcount, p_eng_wordcloud,
            file = "capstone project/outputs/one_word_freqs.Rdata")
       

       
       

# sentiment analysis ------------------------------------------------------


       df_afinn <- get_sentiments("afinn")
       df_bing <- get_sentiments("bing")
       df_nrc <- get_sentiments("nrc")
       
       # top sentiments
       p_eng_nrc <- df_eng %>% 
               inner_join(df_nrc, by = "word") %>% 
               count(sentiment, sort = TRUE) %>% 
               mutate(sentiment = reorder(sentiment, n)) %>%  
               
               ggplot(aes(x = n, y = sentiment)) +
               geom_col(fill = "#C77CFF") +
               theme_bw() +
               ggtitle("Frequency of NRC sentiments in English language")
       
       p_eng_afinn <- df_eng %>% 
               inner_join(df_afinn, by = "word") %>% 
               count(value, sort = TRUE) %>% 
               mutate(value = reorder(value, n)) %>%  
               
               ggplot(aes(x = n, y = as.factor(value))) +
               geom_col(fill = "#C77CFF") +
               ylab("value") +
               theme_bw() +
               ggtitle("Frequency of AFINN sentiments in English language")

       p_eng_bing <- df_eng %>% 
               inner_join(df_bing, by = "word") %>% 
               count(sentiment, sort = TRUE) %>% 
               mutate(sentiment = reorder(sentiment, n)) %>%  
               
               ggplot(aes(x = n, y = sentiment)) +
               geom_col(fill = "#C77CFF") +
               theme_bw() +
               ggtitle("Frequency of Bing et al. sentiments in English language")
       
       # summary sentiment plots
       
               # AFINN
               df_eng_afinn <- df_eng %>% inner_join(df_afinn, by = "word")
               df_eng_afinn_count <- df_eng_afinn %>% count(value, source, sort = TRUE)
               df_eng_afinn_count <- df_eng_afinn_count %>% 
                       group_by(source) %>% 
                       mutate(proportion = n/sum(n))
               
               p_eng_afinn_source <- df_eng_afinn_count %>% 
                       
                       ggplot(aes(x = as.factor(value), y = proportion, fill = source)) +
                       geom_col() +
                       facet_wrap(~source) +
                       
                       theme_bw() +
                       xlab("value") +
                       ggtitle("AFINN sentiment analysis of each source") +
                       theme(legend.position = "none")

               # Bing
               df_eng_bing <- df_eng %>% inner_join(df_bing, by = "word")
               df_eng_bing_count <- df_eng_bing %>% count(sentiment, source, sort = TRUE)
               df_eng_bing_count <- df_eng_bing_count %>% 
                       group_by(source) %>% 
                       mutate(proportion = n/sum(n))
               
               p_eng_bing_source <- df_eng_bing_count %>% 

                       ggplot(aes(x = sentiment, y = proportion, fill = source)) +
                       geom_col() +
                       facet_wrap(~source) +
                       
                       theme_bw() +
                       ggtitle("Bing et al. sentiment analysis of each source") +
                       theme(legend.position = "none")
               
               # NRC
               df_eng_nrc <- df_eng %>% inner_join(df_nrc, by = "word")
               df_eng_nrc_count <- df_eng_nrc %>% count(sentiment, source, sort = TRUE)
               df_eng_nrc_count <- df_eng_nrc_count %>% 
                       group_by(source) %>% 
                       mutate(proportion = n/sum(n))
               
               p_eng_nrc_source <- df_eng_nrc_count %>% 
                       
                       ggplot(aes(x = sentiment, y = proportion, fill = source)) +
                       geom_col() +
                       facet_wrap(~source) +
                       
                       theme_bw() +
                       ggtitle("NRC sentiment analysis of each source") +
                       theme(legend.position = "none",
                             axis.text.x = element_text(angle = 45, hjust = 0.9))
               
        # word count plots
               
               # AFINN
               df_eng_afinn_wordcounts <- df_eng_afinn %>% 
                       count(word, value, sort = TRUE) %>% 
                       mutate(sentiment = ifelse(value > 0, "positive", "negative"))
               
               p_eng_afinn_wordcounts <- df_eng_afinn_wordcounts %>% 
                       group_by(sentiment) %>% 
                       slice_max(order_by = n, n = 10) %>% 
                       ungroup() %>% 
                       mutate(word = reorder(word, n)) %>% 

                       ggplot(aes(x = n, y = word, fill = sentiment)) +
                       geom_col(show.legend = FALSE) +
                       facet_wrap(~sentiment, scales = "free_y") +
                       theme_bw() +
                       ggtitle("contribution of words to AFINN sentiment")
               
               # Bing
               df_eng_bing_wordcounts <- df_eng_bing %>% count(word, sentiment, sort = TRUE) 
               
               p_eng_bing_wordcounts <- df_eng_bing_wordcounts %>% 
                       group_by(sentiment) %>% 
                       slice_max(order_by = n, n = 10) %>% 
                       ungroup() %>% 
                       mutate(word = reorder(word, n)) %>% 
                       
                       ggplot(aes(x = n, y = word, fill = sentiment)) +
                       geom_col(show.legend = FALSE) +
                       facet_wrap(~sentiment, scales = "free_y") +
                       theme_bw() +
                       ggtitle("contribution of words to Bing et al. sentiment")
               
               # NRC
               df_eng_nrc_wordcounts <- df_eng_nrc %>% 
                       count(word, sentiment, sort = TRUE) %>% 
                       mutate(pos_neg = case_when(
                               
                               sentiment %in% c("positive", "trust", "joy", "surprise", "anticipation") ~ "positive",
                               sentiment %in% c("anger", "disgust", "fear", "negative", "sadness") ~ "negative"
                               
                       ))
               
               p_eng_nrc_wordcounts <- df_eng_nrc_wordcounts %>% 
                       group_by(pos_neg) %>% 
                       slice_max(order_by = n, n = 20) %>% 
                       ungroup() %>% 

                       ggplot(aes(x = n, y = word, fill = pos_neg)) +
                       geom_col(show.legend = FALSE) +
                       facet_wrap(~pos_neg, scales = "free_y") +
                       theme_bw() +
                       ggtitle("contribution of words to NRC sentiment")
               

        # save sentiment plots
               save(p_eng_nrc, p_eng_afinn, p_eng_bing, 
                    p_eng_afinn_source, p_eng_bing_source, p_eng_nrc_source, 
                    p_eng_afinn_wordcounts, p_eng_bing_wordcounts, p_eng_nrc_wordcounts,
                    file = "capstone project/outputs/sentiment_plots.Rdata")
               
               
               
       
# 2-grams ------------------------------------------------------------------
   
               
        # sample from each 
        df_eng_news_spl <- sample_n(df_eng_news, size = nrow(df_eng_news)*0.1)
        df_eng_blogs_spl <- sample_n(df_eng_blogs, size = nrow(df_eng_blogs)*0.1)
        df_eng_twitter_spl <- sample_n(df_eng_twitter, size = nrow(df_eng_twitter)*0.1)
        
        # combine into one
        df_eng_spl <- bind_rows(df_eng_news_spl, df_eng_blogs_spl, df_eng_twitter_spl)
        
       # unnest to 2-gram
       df_eng_bigram <- df_eng_spl %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
       
       # remove lines with numbers 
       df_eng_bigram <- df_eng_bigram %>% filter(!grepl("[0-9]", bigram))
       
       # remove stop words
       df_eng_bigrams_sep <- df_eng_bigram %>% separate(bigram, c("word1", "word2"), sep = " ")
       df_eng_bigrams_filt <- df_eng_bigrams_sep %>%
               filter(!word1 %in% stop_words$word) %>%
               filter(!word2 %in% stop_words$word) %>% 
               filter(!is.na(word1), !is.na(word2))
       
       # count
       df_eng_bigrams_count <- df_eng_bigrams_filt %>% count(word1, word2, sort = TRUE)

       # unite
       df_eng_bigrams_count <- df_eng_bigrams_count %>% unite(bigram, word1, word2, sep = " ")
       
       # plot
       p_eng_bigrams_count <- df_eng_bigrams_count %>% head(n = 20) %>% mutate(bigram = reorder(bigram, n)) %>% 
               
               ggplot(aes(x = n, y = bigram)) +
               geom_col(fill = "#F8766D") +
               theme_bw() +
               ggtitle("Most frequent bigrams in the English language")
       
       # visualize network of bigrams
       igraph_eng_bigrams <- df_eng_bigrams_filt %>% 
               filter(!is.na(word1), !is.na(word2)) %>% 
               count(word1, word2, sort = TRUE) %>% 
               filter(n > 200) %>% 
               graph_from_data_frame()
       
       set.seed(2021)
       
       p_bigram_network <- ggraph(igraph_eng_bigrams, layout = "fr") +
               geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, end_cap = circle(.07, "inches")) +
               geom_node_point(color = "lightblue", size = 5) +
               geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
               theme_void()
       

       
# 3-grams ------------------------------------------------------------------

       
       # unnest to 3-gram
       df_eng_trigram <- df_eng_spl %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
       
       # remove lines with numbers 
       df_eng_trigram <- df_eng_trigram %>% filter(!grepl("[0-9]", trigram))
       
       # remove stop words
       df_eng_trigrams_sep <- df_eng_trigram %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")
       df_eng_trigrams_filt <- df_eng_trigrams_sep %>%
               filter(!word1 %in% stop_words$word) %>%
               filter(!word2 %in% stop_words$word) %>% 
               filter(!word3 %in% stop_words$word) %>% 
               filter(!is.na(word1), !is.na(word2), !is.na(word3))
       
       # count
       df_eng_trigrams_count <- df_eng_trigrams_filt %>% count(word1, word2, word3, sort = TRUE)

       # unite
       df_eng_trigrams_count <- df_eng_trigrams_count %>% unite(trigram, word1, word2, word3, sep = " ")
       
       # plot
       p_eng_trigrams_count <- df_eng_trigrams_count %>% head(n = 20) %>% mutate(trigram = reorder(trigram, n)) %>% 
               
               ggplot(aes(x = n, y = trigram)) +
               geom_col(fill = "#7CAE00") +
               theme_bw() +
               ggtitle("Most frequent trigrams in the English language")
       
       
        # save bigram and trigram plots
       save(p_eng_bigrams_count, p_bigram_network, p_eng_trigrams_count,
            file = "capstone project/outputs/bi_tri_plots.Rdata")
  
       
       
         
# How many unique words to cover 50% of all word instances? ---------------
       
       # How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?  
       
       # include stop words this time
       df_eng_all <- rbind(df_eng_news_unnest, df_eng_blogs_unnest, df_eng_twitter_unnest)

       # frequency of all words including stop words
       df_eng_wordcount_all <- df_eng_all %>% count(word, sort = TRUE)     
       
       # loop through to calculate
       n_total.words<- sum(df_eng_wordcount_all$n)
       half <- 0.5*n_total.words
       most <- 0.9*n_total.words
       
       c <- 0
       i <- 1 
       while(c < half){
               c <- c + df_eng_wordcount_all[i,]$n;
               i <- i+1;
       }
       n_half <- i
       
       while(c < most){
               c <- c + df_eng_wordcount_all[i,]$n;
               i <- i+1;
       }
       n_90 <- i

       # save
       save(n_half, n_90, file = "capstone project/outputs/n_unique_words.Rdata")
       
       
       
       


