## +-------------------------------------------------------------------------------------------------------+ 
# There are some good packages for sentiment analysis. This example shows how to do this with the            
# SENTIMENTR package. As you will see, this makes it quite easy (my reason for doing this the "hard way"     
# was to give you a better sense for how sentiments are scored). You will, of course, want to read the       
# package documentation and tutorials/vignettes online
#
## +-------------------------------------------------------------------------------------------------------+
#
setwd('D:\\R data')
library(sentimentr)
library(ggplot2)
scores <- readRDS("scores.rds")
#
# The function get_sentences does sentence boundary disambiguation. This function call isn't absolutely 
# necessary, but included for efficiency purposes
#
tweet = get_sentences(as.character(scores$text)) # sentence boundary disambiguation 
sentiment = sentiment_by(tweet)                  # score sentiments
#
# This information is placed in a data frame
sentiment = data.frame(sentiment)
sentiment$airline = scores$airline
#
# Summary and plots of tweet sentiments: overall and by airline
#
by(sentiment$ave_sentiment,sentiment$airline,summary)
qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Tweet Sentiment Histogram: All Airlines")
##
##
ggplot(subset(sentiment,airline %in% c("American")), aes(x=ave_sentiment)) +  
  geom_histogram(binwidth=0.1,col="Black",fill="Red") + ggtitle("Tweet Sentiment Histogram: American")
ggplot(subset(sentiment,airline %in% c("Delta")), aes(x=ave_sentiment)) +  
  geom_histogram(binwidth=0.1,col="Black",fill="Blue") + ggtitle("Tweet Sentiment Histogram: Delta")
ggplot(subset(sentiment,airline %in% c("JetBlue")), aes(x=ave_sentiment)) +  
  geom_histogram(binwidth=0.1,col="Black",fill="Green") + ggtitle("Tweet Sentiment Histogram: JetBlue")
ggplot(subset(sentiment,airline %in% c("Southwest")), aes(x=ave_sentiment)) +  
  geom_histogram(binwidth=0.1,col="Black",fill="Brown") + ggtitle("Tweet Sentiment Histogram: Southwest")
ggplot(subset(sentiment,airline %in% c("United")), aes(x=ave_sentiment)) +  
  geom_histogram(binwidth=0.1,col="Black",fill="Magenta") + ggtitle("Tweet Sentiment Histogram: United")

