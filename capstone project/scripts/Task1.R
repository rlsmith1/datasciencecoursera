



# libraries ---------------------------------------------------------------

       library(tidyverse)
       library(tm)
       library(LaF)



# read in data ------------------------------------------------------------


       # start with english twitter
       determine_nlines("capstone project/data/final/en_US/en_US.twitter.txt")

       str_eng_twitter_sample <- sample_lines("capstone project/data/final/en_US/en_US.twitter.txt", n = 10000)

       # determine file size
       file.info("capstone project/data/final/en_US/en_US.blogs.txt")$size
       
       # all three US files
       str_eng_news <- readLines("capstone project/data/final/en_US/en_US.news.txt")
       str_eng_blogs <- readLines("capstone project/data/final/en_US/en_US.blogs.txt")
       str_eng_twitter <- readLines("capstone project/data/final/en_US/en_US.twitter.txt")
       
       # length of longest line
       str_eng_news %>% nchar() %>% summary()
       str_eng_blogs %>% nchar() %>% summary()
       str_eng_twitter %>% nchar() %>% summary()

              
       
# explore regex -----------------------------------------------------------------


       str_eng_twitter[grepl("^i think", str_eng_twitter)] # beginning
       str_eng_twitter[grepl("you\r$", str_eng_twitter)] # end
       str_eng_twitter[grepl("[Bb][Uu][Ss][Hh]", str_eng_twitter)] #anything within character class
       str_eng_twitter[grepl("^[Ii] am", str_eng_twitter)] 
       str_eng_twitter[grepl("^[0-9][a-zA-Z]", str_eng_twitter)]
       str_eng_twitter[grepl("[^?.]\r$", str_eng_twitter)] # caret in character class indicates matches any characters NOT in indicated class
       
       str_eng_twitter[grepl("9.11", str_eng_twitter)] # dot means it can be any character
       str_eng_twitter[grepl("flood|fire", str_eng_twitter)] # or metacharacter
       str_eng_twitter[grepl("^([Gg]ood|[Bb]ad)", str_eng_twitter)] # alternatives can be expressions, not just literals
       str_eng_twitter[grepl("[Gg]eorge( [Ww]\\.)? [Bb]ush)", str_eng_twitter)] # question mark means indicated expression is optimal, escape character dot
       str_eng_twitter[grepl("(.*)", str_eng_twitter)] # star means repeat any number of times, including none
       str_eng_twitter[grepl("(.+)", str_eng_twitter)] # plus means at least one of the item
       str_eng_twitter[grepl("[0-9]+ (.*)[0-9]", str_eng_twitter)] 
       str_eng_twitter[grepl("[Oo]bama( +[^ ]+ +){1,5} terrorist", str_eng_twitter)] # curly brackets to define min & max times to match an expression
              # at least one space, followed by something that's not a space (see between 1 and 5 times) (between 1 & 5 word-like options)
       str_eng_twitter[grepl("+([a-zA-Z]+) +\\1 +", str_eng_twitter)] # space followed by one (or more) characters, followed by at least one space,
              # followed by exact same match that we saw within the parentheses 
       str_eng_twitter[grepl("^s(.*)s", str_eng_twitter)] # star is greedy, matches longest possible string that satisfies regex
       str_eng_twitter[grepl("^s(.*?)s", str_eng_twitter)] # question mark changes greediness of star
       
       
       

# quiz questions ----------------------------------------------------------

       # see above for 1-3
       
       # 4
       length(str_eng_twitter[grepl("love", str_eng_twitter)])/length(str_eng_twitter[grepl("hate", str_eng_twitter)])

       # 5
       str_eng_twitter[grepl("biostats", str_eng_twitter)]
       
       # 6
       str_eng_twitter[grepl("A computer once beat me at chess, but it was no match for me at kickboxing", str_eng_twitter)]
       
       
       
       
       
       
       
