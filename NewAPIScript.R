apiKey_retriever <- function( api_file = 'apiKey.cred' ){
  apiKey <- readChar( api_file, file.info(api_file)$size )
  apiKey <- gsub( pattern = "[^a-zA-Z0-9]", replacement = '', x = apiKey )
  return( apiKey )
}

everything_api_call_creator <- function( q = 'jennifer hill',
                                         sources = NA,
                                         domains = NA,
                                         excludeDomains = NA,
                                         from = NA, # e.g. '2018-01-01T00:00:00'
                                         to = NA, # e.g. '2018-10-25T11:00:00'
                                         language = 'en',
                                         sortBy = NA, #e.g. 'popularity'
                                         pageSize = NA,
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