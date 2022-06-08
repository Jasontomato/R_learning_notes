library(ggplot2) # for plotting the results
#
# Load the R dataset scores.RDA
scores <- readRDS("scores.rds")

delta_airline <- subset(scores, scores$airline=="Delta")
jetblue_airline <- subset(scores,scores$airline=="JetBlue")
united_airline <- subset(scores,scores$airline=="United")
american_airline <- subset(scores,scores$airline=="American")
SW_airline <- subset(scores,scores$airline=="Southwest")

delta_airline$polarity <- ifelse(delta_airline$score >0,"positive",ifelse(delta_airline$score < 0,"negative",ifelse(delta_airline$score==0,"Neutral",0)))
jetblue_airline$polarity <- ifelse(jetblue_airline$score >0,"positive",ifelse(jetblue_airline$score < 0,"negative",ifelse(jetblue_airline$score==0,"Neutral",0)))
united_airline$polarity <- ifelse(united_airline$score >0,"positive",ifelse(united_airline$score < 0,"negative",ifelse(united_airline$score==0,"Neutral",0)))
american_airline$polarity <- ifelse(american_airline$score >0,"positive",ifelse(american_airline$score < 0,"negative",ifelse(american_airline$score==0,"Neutral",0)))
SW_airline$polarity <- ifelse(SW_airline$score >0,"positive",ifelse(SW_airline$score < 0,"negative",ifelse(SW_airline$score==0,"Neutral",0)))


qplot(factor(polarity), data=delta_airline, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments - Delta Airlines")
qplot(factor(score), data=delta_airline, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - Delta Airlines")
qplot(factor(polarity), data=jetblue_airline, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle(" Customer Sentiments - JetBlue Airlines ")
qplot(factor(score), data=jetblue_airline, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - JetBlue Airlines")
qplot(factor(polarity), data=united_airline, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments - United Airlines")
qplot(factor(score), data=united_airline, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - United Airlines ")

qplot(factor(polarity), data=american_airline, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments - American Airlines")
qplot(factor(score), data=american_airline, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - American Airlines ")
qplot(factor(polarity), data=SW_airline, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments - Southwest Airlines")
qplot(factor(score), data=SW_airline, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - Southwest Airlines ")


df = ddply(scores, c("airline"), summarise,
           pos_count=sum( positive ),
           neg_count=sum( negative ),
           neu_count=sum(neutral))

df$total_count = df$pos_count +df$neg_count + df$neu_count

df$pos_prcnt_score = round( 100 * df$pos_count / df$total_count )
df$neg_prcnt_score = round( 100 * df$neg_count / df$total_count )
df$neu_prcnt_score = round( 100 * df$neu_count / df$total_count )

attach(df)
lbls <-paste(df$airline,df$pos_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(pos_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = "Positive Comparative Analysis - Airlines")

lbls <-paste(df$airline,df$neg_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(neg_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = " Negative Comparative Analysis - Airlines")

lbls <-paste(df$airline,df$neu_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(neu_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = "Neutral Comparative Analysis - Airlines")
