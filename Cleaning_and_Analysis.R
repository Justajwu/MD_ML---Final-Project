#source("NewAPIScript.R")
require(tidyverse)
require(lubridate)
require(ggplot2)
require(tidytext)

### Read in data ###
raw_df <- read_csv("articles_df.csv")


### Cleaning ###
articles_df <- raw_df


### Adding features ###
articles_df <- articles_df %>%
  mutate(date = date(publishedAt),
         month = month(date),
         weekday = weekdays(date))


### Exploratory ###

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


### Comparison of words ###



