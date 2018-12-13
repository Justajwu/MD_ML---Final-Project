# Header ------------------------------------------------------------------

# GitHub/MD_ML---Final-Project/Cleaning_and_Analysis.R

# PROJECT: MDML FINAL PROJECT
# AUTHOR: Hongye Wu, James Wu
# DATE: 2018-12-13

# Setup -------------------------------------------------------------------

#source("NewAPIScript.R")
require(tidyverse)
require(lubridate)
require(ggplot2)
require(tidytext)
library(tm)

raw_df <- read_csv("articles_df.csv")
data("stop_words")


# Cleaning ----------------------------------------------------------------

# Adding time features
clean_df <- raw_df %>% rename_all(snakecase::to_snake_case) 

articles_df <- clean_df %>%
  mutate(date = date(published_at),
         month = month(date),
         weekday = weekdays(date)) %>% 
  select(-author)

# we need to get rid of the disney websites that are somehow linked to ABC's API search
# and proper nouns such as the news stations' names

# key word appearance function
wordsearch_binary <- function(keyword = NA, string){
  if(!is.na(keyword)){
    as.numeric(any(grep(keyword,string,ignore.case = T)))
  }
}

# Get rows with word appearing
wordsearch <- function(columns,data,keyword){
  require(dplyr)
  tempdf <- as.tibble(data)
  tempdf %>%
    mutate_at(vars(columns),funs(sapply(.,function(x) wordsearch_binary(keyword,x)))) %>%
    mutate(rownum = row_number()) %>%
    filter_at(vars(columns), any_vars(. == 1)) %>%
    .$rownum
}

# Get rid of rows with disney websites
articles_df <- articles_df[-wordsearch(c("url"),articles_df,keyword = "disney"),]

# make list of proper nouns 
proper_noun <- c("abc news","cnn","bbc news","breitbart","fox news","the hill","huffington post",
                 "new york times","american conservative", "news") %>% 
  as_tibble() %>% 
  rename("word" = "value")

## DF that only contains source id and title
title_df <- articles_df %>% 
  select(source_id, title) 

# remove stop words and source names for unigram df
title_uni <- title_df %>%
  unnest_tokens(word, title) %>% 
  anti_join(stop_words) %>% 
  anti_join(proper_noun)

# Bigrams 
# lemmatized  
title_bi <- title_df %>% 
  unnest_tokens(bigram, title, token = 'ngrams', n=2)

title_bi <- title_bi %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  mutate(word1 = SnowballC::wordStem(word1),
         word2 = SnowballC::wordStem(word2)) %>% 
  filter(!word1 %in% stop_words$word,
         !word1 %in% proper_noun$word) %>%
  filter(!word2 %in% stop_words$word,
         !word2 %in% proper_noun$word) %>% 
  unite(bigram, word1, word2, sep = " ")



# Exploratory -------------------------------------------------------------

## Number of articles per news source in the month
articles_df %>%
  ggplot() +
  geom_bar(aes(x = source_name)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Number of articles by Newssource by time
newssource <- c("abc-news","cnn","bbc-news","breitbart-news","fox-news","the-hill","the-huffington-post",
                "the-new-york-times","the-american-conservative")

articles_df %>% 
  ggplot(aes(x = factor(date))) +
  geom_bar(aes(fill = source_name)) +
  labs(x = "Date Published",
       y = "Number of Related Articles Published",
       color = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Does image matter 
articles_df %>%
  mutate(image = ifelse(is.na(url_to_image), 0, 1)) %>% 
  group_by(source_name, image) %>% 
  count() 

# The 10 most frequently used words
title_uni %>% count(word) %>% arrange(desc(n)) %>% slice(1:10) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# The 10 most frequently used words by source
title_uni %>% group_by(source_id) %>% count(word) %>% arrange(desc(n)) %>% slice(1:10) %>% View()

# Count the 20 most frequent stemmed words 
title_uni %>% mutate(word = SnowballC::wordStem(word)) %>% 
  count(word) %>% arrange(desc(n)) %>% slice(1:20)

# Count the 20 most frequent stemmed words by source
title_uni %>% group_by(source_id) %>%  mutate(word = SnowballC::wordStem(word)) %>% 
  count(word) %>% arrange(desc(n)) %>% slice(1:20)



# top 20 bigrams
bigram_clean %>% group_by(source_id) %>% count(bigram) %>% arrange(desc(n)) %>% slice(1:20) %>% View()




# Further cleaning --------------------------------------------------------



# Feature engineering -------------------------------------------------------



# Predictive model --------------------------------------------------------



# Subsetting --------------------------------------------------------------







