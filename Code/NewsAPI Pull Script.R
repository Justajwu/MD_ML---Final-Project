require(lubridate)
require(tidyverse)

apiKey_retriever <- function( api_file = 'apiKey.cred' ){
  apiKey <- readChar( api_file, file.info(api_file)$size )
  apiKey <- gsub( pattern = "[^a-zA-Z0-9]", replacement = '', x = apiKey )
  return( apiKey )
}


everything_api_call_creator <- function( q = NA,
                                         sources = NA,
                                         domains = NA,
                                         excludeDomains = NA,
                                         from = NA, # e.g. '2018-01-01T00:00:00'
                                         to = NA, # e.g. '2018-10-25T11:00:00'
                                         language = 'en',
                                         sortBy = 'popularity', #e.g. 'popularity'
                                         pageSize = 30,
                                         page = NA,
                                         apiKey = apiKey_retriever() ){
  require( utils )
  api_call <- 'https://newsapi.org/v2/everything'
  first_set <- F
  if( !is.na(q) ){
    first_set <- T
    api_call <- paste0( api_call, '?q=', q )
  }
  if( !is.na(sources) ){
    if( first_set ){
      api_call <- paste0( api_call, '&sources=', sources )
    } else {
      api_call <- paste0( api_call, '?sources=', sources )
      first_set <- T
    }
    
  }
  if( !is.na(domains) ){
    if( first_set ){
      api_call <- paste0( api_call, '&domains=', domains )
    } else {
      api_call <- paste0( api_call, '?domains=', domains )
      first_set <- T
    }
    
  }
  if( !is.na(excludeDomains) ){
    if( first_set ){
      api_call <- paste0( api_call, '&excludeDomains=', excludeDomains )
    } else {
      api_call <- paste0( api_call, '?excludeDomains=', excludeDomains )
      first_set <- T
    }
    
  }
  if( !is.na(from) ){
    if( first_set ){
      api_call <- paste0( api_call, '&from=', from )
    } else {
      api_call <- paste0( api_call, '?from=', from )
      first_set <- T
    }
    
  }
  if( !is.na(to) ){
    if( first_set ){
      api_call <- paste0( api_call, '&to=', to )
    } else {
      api_call <- paste0( api_call, '?to=', to )
      first_set <- T
    }
    
  }
  if( !is.na(language) ){
    if( first_set ){
      api_call <- paste0( api_call, '&language=', language )
    } else {
      api_call <- paste0( api_call, '?language=', language )
      first_set <- T
    }
    
  }
  if( !is.na(sortBy) ){
    if( first_set ){
      api_call <- paste0( api_call, '&sortBy=', sortBy )
    } else {
      api_call <- paste0( api_call, '?sortBy=', sortBy )
      first_set <- T
    }
    
  }
  if( !is.na(pageSize) ){
    if( first_set ){
      api_call <- paste0( api_call, '&pageSize=', pageSize )
    } else {
      api_call <- paste0( api_call, '?pageSize=', pageSize )
      first_set <- T
    }
    
  }
  if( !is.na(page) ){
    if( first_set ){
      api_call <- paste0( api_call, '&page=', page )
    } else {
      api_call <- paste0( api_call, '?page=', page )
      first_set <- T
    }
    
  }
  if( !is.na(apiKey) ){
    if( first_set ){
      api_call <- paste0( api_call, '&apiKey=', apiKey )
    } else {
      api_call <- paste0( api_call, '?apiKey=', apiKey )
    }
  } else {
    stop( 'Must provide an API key' )
  }
  
  return( utils::URLencode( api_call ) )
}


retrieve_news_json <- function( api_call_creator = everything_api_call_creator,
                                output_file = NA, 
                                ... ){
  require( jsonlite )
  # create the API call
  api_call <- api_call_creator( ... )
  if( !is.na(output_file) ){
    # store the json file
    download.file( api_call, destfile = output_file )
  }
  # read in the json file as a list
  news <- jsonlite::fromJSON( api_call )
  # return 
  return( news )
}

#abc_articles <- retrieve_news_json(sources = "abc-news")
#cnn_articles <- retrieve_news_json(sources = "cnn")
#bbc_articles <- retrieve_news_json(sources = "bbc-news")
#breitbart_articles <- retrieve_news_json(sources = "breitbart-news")
#fox_articles <- retrieve_news_json(sources = "fox-news")
#hill_articles <- retrieve_news_json(sources = "the-hill")
#huffington_articles <- retrieve_news_json(sources = "the-huffington-post")
#nyt_articles <- retrieve_news_json(sources = "the-new-york-times")
#acons_articles <- retrieve_news_json(sources = "the-american-conservative")

#News Sources
newssource <- c("abc-news","cnn","bbc-news","breitbart-news","fox-news","the-hill","the-huffington-post", "the-new-york-times","the-american-conservative")

#Function to make each pull's data.frame compatible to row bind
change.source <- function(df){
  return(cbind(data.frame(source_id = df$articles$source$id,source_name = df$articles$source$name),df$articles[,-1]))
}

##Create the data tibble
#Set timeout
options(timeout = 4000000)
#News API only gathers maximum of 100 articles for each news source per request. 
#Write function to iteratively grab up to n day's worth of 30 articles per source. Recommend doing around 10
#day each time
gather_articles <- function(ndays = 10){
  today <- '2018-12-10T23:59:59'
  earlytoday <- '2018-12-10T00:00:00'
  output <- tibble()
  for(ntry in 1:ndays){
    articles_df <- tibble()
    for(source in newssource){
      articles <- retrieve_news_json(sources = source,from = earlytoday,to = today)
      if(articles$totalResults != 0)  articles_df <- bind_rows(articles_df,change.source(articles))
      Sys.sleep(5)
    }
    articles_df <- articles_df %>%
      mutate(publishedAt = ymd_hms(publishedAt))
    
    output <- bind_rows(output,articles_df)
    
    t <- ymd_hms(today) - hms("24:00:00")
    today = paste0(year(t),"-",sprintf("%02d", month(t)),"-",sprintf("%02d", day(t)),"T",sprintf("%02d", hour(t)),":",sprintf("%02d", minute(t)),":",sprintf("%02d", second(t)))
    
    et <- ymd_hms(earlytoday) - hms("24:00:00")
    earlytoday <- paste0(year(et),"-",sprintf("%02d", month(et)),"-",sprintf("%02d", day(et)),"T",sprintf("%02d", hour(et)),":",sprintf("%02d", minute(et)),":",sprintf("%02d", second(et)))
    Sys.sleep(60)
  }
  output
}

articles_df <- gather_articles()

#write_csv(articles_df,"articles_df_20-30days.csv")
#art1 <- read_csv("articles_df_10days.csv")
#art2 <- read_csv("articles_df_10-20days.csv")
#art3 <- read_csv("articles_df_20-30days.csv")
#
#articles_df <- bind_rows(art1,art2,art3)
#write_csv(articles_df,"articles_df.csv")