
## Load Required Packages ##
library(kevbrostwitteR)
library(ROAuth)

## Set up Twitter Credentials only if not cached ##
if (!has_oauth_token()) {
  api_key <- "I4Ab4AWwyW3E9EkyXoQvzry7a"
  api_secret <- "pcXBrAbdApWLa3TsQiRZ3ZJzHeEIvRJa7EnVkPDhcMTvfk4vrT"
  access_token <- "4312033336-IrXbWJkWGOyebv2j2tYHvdDjKyMiJUZe50KvkHB"
  access_token_secret <- "ai4m1x4olpHhKwD79MBhaElPTbC0EH0bjZGlNSquS8rAU"
  
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
}

## Statistical Quotes to Tweet ##

tweet_quotes <- function(){
  
}

## New Journal Articles to Tweet ##

tweet_journals <- function(){
  
}

## Rstats Gurus to Retweet ##

retweet_gurus <- function(){
  r.gurus <- c("hadleywickham", "JennyBryan", "xieyihui", "Rbloggers", 
               "timelyportfolio", "MACSImaths", "rstudio")
  guru <- sample(r.gurus, size = 1)
  guru.tweets <- twitteR::userTimeline(guru)
  retweeted.ids <- read.table("retweeted_ids.txt", header = TRUE)
  
  old.popularity <- 0
  top.tweet.id <- NA
  for (i in 1:length(guru.tweets)) {
    if (guru.tweets[[i]]$id %in% retweeted.ids){
      break
    } else {
      current.popularity <- as.integer(guru.tweets[[i]]$favoriteCount) 
                            + as.integer(guru.tweets[[i]]$retweetCount)
      if (current.popularity >= old.popularity) {
        old.popularity <- current.popularity
        top.tweet.id <- i
      }
    }
  }
  
  write.table(guru.tweets[[top.tweet.id]]$id, file = "retweeted_ids.txt", 
              row.names = FALSE, append = TRUE, col.names = FALSE)
  twitteR::retweetStatus(guru.tweets[[top.tweet.id]])
  return(structure(list(guru = guru, popularity = old.popularity)))
}


## Decide what to tweet/retweet ##

tweet_to_send <- function(){
  value <- floor(runif(1, min = 0, max = 1.9))
  temp <- switch(as.character(value),
                      "0" = c("retweet_gurus"),
                      #"1" = c("tweet_journals"),
                      #"2" = c("tweet_quotes"),
                      "1" = c("take_a_break")
  )
  return(temp)
}

## Tweet/Retweet the content ##
twitterbot <- function(tweet.to.send = tweet_to_send()) {
  
  if (tweet.to.send == "retweet_gurus") {   
    temp <- retweet_gurus()
    con <- file("twitterbot_log.txt", open = "a")
    cat(paste("\n", format(Sys.time(), "%a %b %d %X %Y") ,": Retweeted ", temp$guru, 
             "; Popularity ", temp$popularity, sep = ""), file = con)
    cat("\n-----------------------------------------------------------------", file = con)
    close(con)
  } else if (tweet.to.send == "tweet_journals") {
    print("test")
  } else if (tweet.to.send == "tweet_quotes") {
    print("test")
  } else if (tweet.to.send == "take_a_break") {
   con <- file("twitterbot_log.txt", open = "a")
   cat(paste("\n", format(Sys.time(), "%a %b %d %X %Y"), ": Nothing posted", 
             sep = ""), file = con)
   cat("\n------------------------------------------", file = con)
   close(con)
  }
}