#
# The following script will direct you on how to download tweets for sentiments 
# about airlines (or anything else) and analyze them. We also score the tweets by 
# comparing against positive and negative word lists. 
#
# Requires files: positive-words.txt and negative-words.txt
# By default set up for 500 tweets for each airline (change to any number you like
#     but remember that Twitter will limit large downloads)
# 
library(twitteR)   ## for fetching the tweets
library(plyr)      ## for breaking the data into manageable pieces
library(ROAuth)    ## for R authentication
library(stringr)   ## for string processing
library(ggplot2)   ## for plotting the results
#
# You will need to create a twitter account if you do not already have one
# Then you will need to create an app at apps.twitter.com or at https://dev.twitter.com/ (followed by My Apps)
# After that you will be able to obtain the following:
# (1) api_key
# (2) api_secret
# (3) access_token
# (4) access_token_secret
# I have replaced my own with "_____________" (replace with your own values within quotes)
#
# It would be a good idea to look at the instructions at the following site:
# https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-1-extract-tweets/
#
# RUN THE FOLLOWING COMMANDS IN SEQUENCE (ONE BY ONE)
# 

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
api_key <- "____________________"
api_secret <- "_______________________"

# configure RCurl options
RCurlOptions <- list(capath=system.file("CurlSSL", "cacert.pem", package = "RCurl"),
                     ssl.verifypeer = FALSE)
options(RCurlOptions = RCurlOptions)
cred <- OAuthFactory$new(consumerKey=api_key, consumerSecret=api_secret,
                         requestURL=reqURL, accessURL=accessURL, authURL=authURL)

download.file(url="https://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

cred$handshake(cainfo="cacert.pem")
save(cred,file="cred.Rdata")
# load("cred.Rdata")
#
access_token <- "_________________________"
access_token_secret <- "_______________________________"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#
# In case you do not already have them, download a list of positive and negative words
#
posText <- read.delim("positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
pos.words = c(posText, 'upgrade')
neg.words = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')
#
# The following commands download 500 tweets each for 5 airlines
#
delta_tweets = searchTwitter('@delta', n=500)
jetblue_tweets = searchTwitter('@jetblue', n=500)
united_tweets = searchTwitter('@united', n=500)
american_tweets = searchTwitter('@AmericanAir', n=500)
SW_tweets = searchTwitter('@SouthwestAir', n=500)

delta_txt = sapply(delta_tweets, function(t) t$getText() )
jetblue_txt = sapply(jetblue_tweets, function(t) t$getText() )
united_txt = sapply(united_tweets, function(t) t$getText() )
american_txt = sapply(american_tweets, function(t) t$getText() )
SW_txt = sapply(SW_tweets, function(t) t$getText() )

noof_tweets = c(length(delta_txt), length(jetblue_txt),length(united_txt),length(american_txt), length(SW_txt))

airline<- c(delta_txt,jetblue_txt,united_txt,american_txt,SW_txt)

#
#
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores = score.sentiment(airline, pos.words,neg.words , .progress='text')
scores$airline = factor(rep(c("Delta", "JetBlue","United","American","Southwest"), noof_tweets))
scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score <0)
scores$neutral <- as.numeric(scores$score==0)
#
# 
# Save the dataframe as an rds file to use later
#
saveRDS(scores,"scores.rds")