#source("NewAPIScript.R")
require(tidyverse)
require(lubridate)
require(ggplot2)
require(tidytext)
library(tm)


# Set up ------------------------------------------------------------------


raw_df <- read_csv("articles_df.csv")

### Adding time features ###
clean_df <- raw_df %>% rename_all(snakecase::to_snake_case) 

articles_df <- clean_df %>%
  mutate(date = date(published_at),
         month = month(date),
         weekday = weekdays(date)) %>% 
  select(-author)

## DF that only contains source id and title
title_df <- articles_df %>% 
  select(source_id, title) 

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


data("stop_words")

# remove stop words
title_clean <- title_df %>%
  unnest_tokens(word, title) %>% 
  anti_join(stop_words)

# The 10 most frequently used words
title_clean %>% count(word) %>% arrange(desc(n)) %>% slice(1:10) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# The 10 most frequently used words by source
title_clean %>% group_by(source_id) %>% count(word) %>% arrange(desc(n)) %>% slice(1:10) %>% View()

# Count the 20 most frequent stemmed words 
title_clean %>% mutate(word = SnowballC::wordStem(word)) %>% 
  count(word) %>% arrange(desc(n)) %>% slice(1:20)

# Count the 20 most frequent stemmed words by source
title_clean %>% group_by(source_id) %>%  mutate(word = SnowballC::wordStem(word)) %>% 
  count(word) %>% arrange(desc(n)) %>% slice(1:20)

## Bigrams Analysis
bigram_df <- title_df %>% 
  unnest_tokens(bigram, title, token = 'ngrams', n=2)

bigram_clean <- bigram_df %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% unite(bigram, word1, word2, sep = " ")

# top 20 bigrams
bigram_clean %>% group_by(source_id) %>% count(bigram) %>% arrange(desc(n)) %>% slice(1:20) %>% View()



# Further cleaning --------------------------------------------------------

cleanCorpus <- function(myCorpus) {
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords,stopwords("en"))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, tolower)
  return(myCorpus)
}



