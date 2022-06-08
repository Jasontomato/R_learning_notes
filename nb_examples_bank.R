#
# Read the data. 
#
df <- read.csv("UniversalBank.csv")
df[,1] <- NULL
df$ZIP.Code <- NULL
df$Personal.Loan <- as.factor(df$Personal.Loan)
df$Family <- as.factor(df$Family)
df$Education <- as.factor(df$Education)
df$Securities.Account <- as.factor(df$Securities.Account)
df$CD.Account <- as.factor(df$CD.Account)
df$Online <- as.factor(df$Online)
df$CreditCard <- as.factor(df$CreditCard)
#
#
library("caret")
set.seed(12345)
inTrain <- createDataPartition(df$Personal.Loan, p=0.6, list=FALSE)
#
dftrain <- data.frame(df[inTrain,])
dfvalidation <- data.frame(df[-inTrain,])
#
# We require the library e1071
library(e1071)
# Can handle both categorical and numeric input, 
# but output must be categorical
model <- naiveBayes(Personal.Loan~., data=dftrain)
model
prediction <- predict(model, newdata = dfvalidation[,-8])
table(dfvalidation$Personal.Loan,prediction,dnn=list('actual','predicted'))
model$apriori
#
# For class probabilities
predicted.probability <- predict(model, newdata = dfvalidation[,-8], type="raw")
#
# The first column is class 0, the second is class 1
PL <- as.numeric(dfvalidation$Personal.Loan)-1
prob <- predicted.probability[,2]
df1 <- data.frame(prediction, PL, prob)
#
#
df1S <- df1[order(-prob),]
df1S$Gains <- cumsum(df1S$PL)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success")
lines(df1S$Gains,col="blue")
abline(0,sum(df1S$PL)/nrow(df1S),lty = 2, col="red")
#
#
model <- naiveBayes(Personal.Loan~CreditCard+Online, data=dftrain)
model
prediction <- predict(model, newdata = dfvalidation[,-8])
table(dfvalidation$Personal.Loan,prediction,dnn=list('actual','predicted'))
# For class probabilities
predicted.probability <- predict(model, newdata = dfvalidation[,-8], type="raw")
#
# The first column is class 0, the second is class 1
PL <- as.numeric(dfvalidation$Personal.Loan)-1
prob <- predicted.probability[,2]
df1 <- data.frame(prediction, PL, prob)
#
#
df1S <- df1[order(-prob),]
df1S$Gains <- cumsum(df1S$PL)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success")
lines(df1S$Gains,col="blue")
abline(0,sum(df1S$PL)/nrow(df1S),lty = 2, col="red")
