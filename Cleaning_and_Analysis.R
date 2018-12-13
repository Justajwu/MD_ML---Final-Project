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
library(corpus)
library(snakecase)
require(randomForest)
library(ROCR)
library(broom)


raw_df <- read_csv("articles_df.csv")
data("stop_words")


# Cleaning ----------------------------------------------------------------

# Adding time features + row number
clean_df <- raw_df %>% rename_all(snakecase::to_snake_case) 

articles_df <- clean_df %>%
  mutate(date = date(published_at),
         month = month(date),
         weekday = weekdays(date),
         id = row_number()) %>%
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
articles_df <- articles_df[-wordsearch(c(names(articles_df)),articles_df,keyword = "disney"),]

# add column of political leanings
articles_df <- articles_df %>% 
  mutate(ideology = case_when(
    source_id %in% c("bbc-news","the-huffington-post","the-new-york-times") ~ "liberal",
    source_id %in% c("abc-news","cnn","the-hill") ~ "moderate",
    source_id %in% c("breitbart-news","fox-news", "the-american-conservative") ~ "conservative"
  )) %>% 
  mutate(ideology = as.factor(ideology))


# make list of proper nouns 
proper_noun <- c("abc news", "abc","cnn","bbc news", "bbc","breitbart news", "breitbart",
                 "fox news", "fox","the hill", "hill","huffington post",
                 "new york times","american conservative", "news") %>% 
  as_tibble() %>% 
  rename("word" = "value")

## DF that only contains source id and title
title_df <- articles_df %>% 
  select(id,ideology, title) 


# remove stop words and source names for unigram df
title_uni <- title_df %>%
  unnest_tokens(word, title) %>%
  mutate(word = SnowballC::wordStem(word)) %>% 
  anti_join(stop_words) %>% 
  anti_join(proper_noun)

# Bigrams 
 
title_bi <- title_df %>% 
  unnest_tokens(bigram, title, token = 'ngrams', n=2)
# lemmatized 
title_bi <- title_bi %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  mutate(word1 = SnowballC::wordStem(word1),
         word2 = SnowballC::wordStem(word2)) %>% 
  filter(!word1 %in% stop_words$word,
         !word1 %in% proper_noun$word) %>%
  filter(!word2 %in% stop_words$word,
         !word2 %in% proper_noun$word) %>% 
  unite(bigram, word1, word2, sep = " ")

# Trigrams
title_tri <- title_df %>% 
  unnest_tokens(trigram, title, token = 'ngrams', n=3)

title_tri <- title_tri %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  mutate(word1 = SnowballC::wordStem(word1),
         word2 = SnowballC::wordStem(word2),
         word3 = SnowballC::wordStem(word3)) %>% 
  filter(!word1 %in% stop_words$word,
         !word1 %in% proper_noun$word,
         !word3 %in% proper_noun$word) %>%
  filter(!word2 %in% stop_words$word,
         !word2 %in% proper_noun$word,
         !word3 %in% proper_noun$word) %>% 
  unite(trigram, word1, word2, word3, sep = " ")


# Exploratory -------------------------------------------------------------

## Articles
# Number of articles per news source in the month
articles_df %>%
  ggplot() +
  geom_bar(aes(x = source_name)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Number of articles by Newssource by time
newssource <- c("abc-news","cnn","bbc-news","breitbart-news","fox-news","the-hill","the-huffington-post",
                "the-new-york-times","the-american-conservative")

articles_df %>% 
  ggplot(aes(x = factor(date))) +
  geom_bar(aes(fill = source_name)) +
  labs(x = "Date Published",
       y = "Number of Related Articles Published",
       color = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Does image matter 
articles_df %>%
  mutate(image = ifelse(is.na(url_to_image), 0, 1)) %>% 
  group_by(source_name, image) %>% 
  count() 

## Unigram

# The 10 most frequently used words
title_uni %>% count(word) %>% arrange(desc(n)) %>% slice(1:10) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# The 20 most frequently used words by ideology
title_uni %>% group_by(ideology) %>% count(word) %>% arrange(desc(n)) %>% slice(1:20) %>% View()

# calcute tf_idf 
unigram_tf_idf <- title_uni %>%
  count(ideology, word) %>%
  bind_tf_idf(word, ideology, n) %>%
  arrange(desc(tf_idf)) %>% slice(1:20)
unigram_tf_idf

# document term matrix
hundred.most.common.uni= title_uni %>% count(word) %>% arrange(desc(n)) %>% slice(1:100)
w=title_uni %>% count(ideology, word) %>% cast_dtm(ideology, word, n)
top_dtm_uni= as.matrix(w[1:dim(w)[1], intersect(dimnames(w)[[2]], hundred.most.common.uni$word)])

## Bigrams

# top 10 bigrams
title_bi %>% group_by(ideology) %>% count(bigram) %>% arrange(desc(n)) %>% slice(1:20) %>% View()  
  # ggplot(aes(fct_reorder(bigram, n), n)) +
  # geom_col() +
  # coord_flip() +
  # labs(x = NULL)+
  # facet_wrap(~ source_id)

# document term matrix (should we include???)
# hundred.most.common.bi= title_bi %>% count(bigram) %>% arrange(desc(n)) %>% slice(1:100)
# 
# ## Trigrams
# title_tri %>% group_by(ideology) %>% count(trigram) %>% arrange(desc(n)) %>% slice(1:20) %>% View()


# Sentiment Analysis ------------------------------------------------------
##Mesh a negative word (ie. "no","-n't","never","not") into the next word (negation of sentiments)
str_negate <- function(x) {
  str_split <- unlist(strsplit(x=x, split=" "))
  is_negative <- grepl("\\bnot\\b|n't|\\bnever\\b|\\bno\\b",str_split,ignore.case=T)
  negate_me <- append(FALSE,is_negative)[1:length(str_split)]
  str_split[negate_me==T]<- paste0("not_",str_split[negate_me==T])
  paste(str_split,collapse=" ")
}

sentiment_df <- articles_df %>%
  select(id,source_name,title,ideology) %>%
  rowwise() %>%
  mutate(title_neg = str_negate(title))


##Create uni-grams
# remove stop words and source names for unigram df
title_neg_uni <- sentiment_df %>%
  unnest_tokens(word, title_neg) %>% 
  anti_join(stop_words) %>% 
  anti_join(proper_noun)

## Create a modified lexicon
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

# Extra words we might want to add to the lexicon
ExtraWords <- data.frame(
  word = c("death","die","died","dying","dead"),
  sentiment = c(rep("sadness",5))
)

nrc_mod <- nrc %>% 
  bind_rows(ExtraWords)

nrc_mod <- nrc_mod %>%
  mutate(word = paste0("not_",word),
         sentiment = case_when(
           sentiment == "trust" ~ "fear",
           sentiment == "fear" ~ "trust",
           sentiment == "negative" ~ "positive",
           sentiment == "sadness" ~ "joy",
           sentiment == "anger" ~ "joy",
           sentiment == "surprise" ~ "anticipation",
           sentiment == "positive" ~ "negative",
           sentiment == "disgust" ~ "approval",
           sentiment == "joy" ~ "sadness",
           sentiment == "anticipation" ~ "surprise")
  ) %>%
  bind_rows(nrc_mod)

## Sentiment Analysis by source
sources <- title_neg_uni %>%
  group_by(source_name,id) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id,source_name, total_words)

by_source_sentiment <- title_neg_uni %>%
  inner_join(nrc_mod, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(sources) %>%
  group_by(source_name, sentiment) %>%
  summarize(words = sum(n),
            total_words = sum(total_words),
            percentage = words/total_words) %>%
  ungroup()

# top 10 sentiments by source
by_source_sentiment %>% group_by(source_name, sentiment) %>% arrange(desc(percentage)) %>% slice(1:10) %>% View()
# plots
by_source_plot <- by_source_sentiment %>% group_by(source_name, sentiment) %>% 
  arrange(desc(percentage)) %>% slice(1:10) %>% 
  ggplot(aes(x= sentiment)) +
  geom_bar(aes(y = percentage), stat = "identity")+
  scale_y_continuous(labels=scales::percent)+
  coord_flip()+
  facet_wrap(~source_name)

## Sentiment Analysis by ideology
ideologies <- title_neg_uni %>%
  group_by(ideology,id) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id,ideology, total_words)

by_ideology_sentiment <- title_neg_uni %>%
  inner_join(nrc_mod, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(ideologies) %>%
  group_by(ideology, sentiment) %>%
  summarize(words = sum(n),
            total_words = sum(total_words),
            percentage = words/total_words) %>%
  ungroup()

# top 10 sentiments by ideology
by_ideology_sentiment %>% group_by(ideology, sentiment) %>% arrange(desc(percentage)) %>% slice(1:10) %>% View()
# plots
by_ideo_plot <- by_ideology_sentiment %>% group_by(ideology, sentiment) %>% 
  arrange(desc(percentage)) %>% slice(1:10) %>% 
  ggplot(aes(x= sentiment)) +
  geom_bar(aes(y = percentage), stat = "identity")+
  scale_y_continuous(labels=scales::percent)+
  coord_flip()+
  facet_wrap(~ideology)

## Poisson test between conservative and liberal news outlets
sentiment_differences <- by_ideology_sentiment %>%
  filter(ideology != "moderate") %>%
  group_by(sentiment) %>%
  do(tidy(poisson.test(.$words, .$total_words)))

sentiment_differences

# Feature engineering -------------------------------------------------------
## Create features for number of words per sentiment in each sentence
ideology_join <- title_neg_uni %>%
  inner_join(nrc_mod, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0))# %>%
#  inner_join(ideologies) %>%
#  mutate(percent_sentence = n/total_words) %>%
#  ungroup()

articles_df <- articles_df %>%
  left_join(spread(ideology_join,sentiment,n,fill = 0)) %>%
  mutate_at(vars(unique(ideology_join$sentiment)),funs(replace_na(0)))
  

## Create features for whether headline has word in interested topics
topics <- c("mueller","migrant","fire","climate","trump","border","ocasio-cortez")

articles_df <- articles_df %>%
  rowwise() %>%
  mutate(mueller = wordsearch_binary(topics[1],title),
         migrant = wordsearch_binary(topics[2],title),
         fire = wordsearch_binary(topics[3],title),
         climate = wordsearch_binary(topics[4],title),
         trump = wordsearch_binary(topics[5],title),
         border = wordsearch_binary(topics[6],title),
         ocasiocortez = wordsearch_binary(topics[7],title)
         )


# Predictive model --------------------------------------------------------

set.seed(123)
split_size = floor(nrow(articles_df)/2)
train_ind <- sample(seq_len(nrow(articles_df)), size = split_size)
prep_df <- articles_df %>%
  select(id,ideology, weekday, anger, anticipation, approval, disgust , 
                    fear, joy, negative, positive, sadness, surprise, trust, mueller, 
                    migrant, fire, climate, trump, border, ocasiocortez) %>% 
  transmute_all(as.factor)

train <- prep_df[train_ind, ] 
test <- prep_df[-train_ind, ]

# see if there's any row wihtout any sentiments
train %>% mutate()

rf_model <- randomForest(ideology ~ ., 
                         data=train, ntree=200)

# compute AUC of this model on the test dataset  
test$predicted.probability.rf <- predict(rf_model, newdata=test, type="prob")[,2]
test$ideo.outcome <- predict(rf_model, newdata=test, type="response")
test.pred.rf <- prediction(test$predicted.probability.rf, test$ideology)
test.perf.rf <- performance(test.pred.rf, "auc")
auc <- 100*test.perf.rf@y.values[[1]]
cat('the auc score is ', 100*test.perf.rf@y.values[[1]], "\n") 

# recall and precision for a threshold of 0.5
threshold <- 0.5
test <- test %>% mutate(prediction = case_when(
  predicted.probability < threshold ~ F,
  predicted.probability >= threshold ~ T
))
    # need to figure out if the true cases can be represented by ideology = ideo.outcome
cat('At the threshold of ', threshold, ' the precision is ', 
    100*nrow(filter(test, prediction==T, ideology = ideo.outcome))/nrow(filter(test, prediction==T)),
    '%, and the recall is ', 100*nrow(filter(test, prediction==T, ideology = ideo.outcome))/nrow(filter(test, ideology = ideo.outcome)),
    '%\n')

# performance plot
plot.data.rf <- test %>% arrange(desc(predicted.probability.rf)) %>% 
  mutate(numrank = row_number(), percent.ideology = cumsum(ideology)/numrank,
         method = rep("Random Forest",n())) %>% 
  select(numrank, percent.ideology,method)

# calibration?


# Subsetting --------------------------------------------------------------







