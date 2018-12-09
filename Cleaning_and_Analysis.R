source("NewAPIScript.R")
require(lubridate)
require(ggplot2)

### Cleaning ###


### Exploratory ###

## Number of articles by Newssource by time
articles_df %>% 
  count(source_id,date = date(publishedAt)) %>%
  right_join(expand(.,source_id = newssource,date)) %>%
  mutate(n = replace_na(n,0) ) %>%
  ggplot(aes(y = n, x = date, color = source_id)) +
  geom_line() +
  labs(x = "Date Published",
       y = "Number of Related Articles Published",
       color = "")
  