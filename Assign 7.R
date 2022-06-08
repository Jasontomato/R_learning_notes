setwd('D:\\R data')
library(sentimentr)
library(ggplot2)
library(dplyr)
library('tm')
library('e1071');
scores <- read.csv('Wine Data for Lab.csv')

tweet = get_sentences(as.character(scores$description)) # sentence boundary disambiguation 
sentiment = sentiment_by(tweet)                  # score sentiments
#
# This information is placed in a data frame
sentiment = data.frame(sentiment)
sentiment$variety = scores$variety
sentiment$price = scores$price
sentiment$points = scores$points
sentiment$country = scores$country
#
# Summary and plots of tweet sentiments: overall and by airline
#
# Q1 a
qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Tweet Sentiment Histogram: All Wines")


by(sentiment$ave_sentiment,sentiment$variety,summary)

# Q1 b

sentiment %>% select(ave_sentiment) %>% summarise(ave_sentiment_score = mean(ave_sentiment))


# Q1 c
sentiment %>% 
  filter(country == 'New Zealand') %>% 
  select(country,ave_sentiment) %>% 
  summarise(ave_sentiment_score = mean(ave_sentiment))

# Q1 d
sentiment %>% 
  filter(price>=20 & price <=29.99) %>% 
  select(country,ave_sentiment) %>% 
  summarise(ave_sentiment_score = mean(ave_sentiment))

# Q2
model = lm(points~ave_sentiment,data = sentiment)
summary(model)
model2 = lm(points~ave_sentiment+price,data = sentiment)
summary(model2)

# Q3
standard = quantile(sentiment$points,c(0.4),na.rm = TRUE)
sentiment$label = ifelse(sentiment$points>standard,'Positive','Negative')
# text processing
winejudge <- as.character(tweet)
# 
tweetvector <- as.vector(winejudge);    # Create vector
tweetsource <- VectorSource(tweetvector); # Create source
tweetcorpus <- Corpus(tweetsource);       # Create corpus
#
# PERFORMING THE VARIOUS TRANSFORMATIONS on "traincorpus" and "testcorpus" DATASETS 
# SUCH AS TRIM WHITESPACE, REMOVE PUNCTUATION, REMOVE STOPWORDS.
tweetcorpus <- tm_map(tweetcorpus,content_transformer(stripWhitespace));
tweetcorpus <- tm_map(tweetcorpus,content_transformer(tolower));
tweetcorpus <- tm_map(tweetcorpus, content_transformer(removeWords),stopwords("english"));
tweetcorpus <- tm_map(tweetcorpus,content_transformer(removePunctuation));
tweetcorpus <- tm_map(tweetcorpus,content_transformer(removeNumbers))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
tweetcorpus <- tm_map(tweetcorpus, content_transformer(removeNumPunct))

# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
tweetcorpus <- tm_map(tweetcorpus, content_transformer(removeURL))


# Create TermDocumentMatrix
tdm1 <- TermDocumentMatrix(tweetcorpus)
tdm1 = removeSparseTerms(tdm1, 0.95)
dtm_matrix <- t(tdm1);
dtm_df <- data.frame(sentiment$label,as.matrix(dtm_matrix))
# partition dataset with seed = 12345
set.seed(12345)
train <- sample(nrow(dtm_df),0.7*nrow(dtm_df))
dftrain <- dtm_df[train,]
dftest <- dtm_df[-train,]
# build naive bayes model to predict the model
model.bayes <- naiveBayes(dftrain[,-1],dftrain[,1]);

# PREDICTION
Predictions <- predict(model.bayes,dftest[,-1])
(confusion = table(dftest[,1],Predictions))
Accuracy.bayes = (confusion[1,1]+confusion[2,2])/sum(confusion)
Accuracy.bayes

# linear regression

dtm_df.linear <- data.frame(sentiment$ave_sentiment,as.matrix(dtm_matrix))
set.seed(12345)
train <- sample(nrow(dtm_df.linear),0.7*nrow(dtm_df.linear))
dftrain.linear <- dtm_df.linear[train,]
dftest.linear <- dtm_df.linear[-train,]

model.linear <- lm(sentiment.ave_sentiment~.,data = dftrain.linear)
# PREDICTION

pred_linear<-predict(model.linear,newdata=dftest[,-1])
RMSE_linear<-sqrt(mean((dftest.linear$sentiment.ave_sentiment-pred_linear)^2,na.rm=TRUE))
RMSE_linear

## Q4 hclust ####
matrix1 <- as.matrix(tdm1)
distMatrix <- dist(scale(matrix1))
fit <- hclust(distMatrix, method="ward.D2")

# plot dendrogram ####
plot(fit, cex=0.9, hang=-1,
     main="Word Cluster Dendrogram")

## Word Cloud
library(wordcloud)
library(RColorBrewer)
m <- as.matrix(tdm1)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
#
# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1,
          random.order = F, colors = pal)

