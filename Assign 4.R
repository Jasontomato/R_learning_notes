setwd('G:\\Œ“µƒ‘∆∂À”≤≈Ã\\Graduate course\\R dataset')
library(readxl)
library(caret)
library(fastDummies)
library(class)
library(pROC)
library(e1071)
df <- read_excel('credit3.xlsx',sheet = 'Data')
summary(df)

# data processing
df$PROFITABLE = ifelse(df$NPV>0,1,0)
df_dummy = dummy_cols(df,select_columns = c('CHK_ACCT','SAV_ACCT',
                                            'HISTORY','JOB','TYPE'))
drops = c('CHK_ACCT','SAV_ACCT','HISTORY','JOB','TYPE','OBS#','NPV','CREDIT_EXTENDED')
df_dummy = df_dummy[ , !(names(df_dummy) %in% drops)]
# data normalization,  if we need normalize observation, with mean and std in train data
fun <- function(x){ 
  a <- mean(x) 
  b <- sd(x) 
  (x - a)/(b)}

df_dummy[,1:15] <- apply(df_dummy[,1:15], 2, fun) # 2 indicate column
df_dummy[,17:41] <- apply(df_dummy[,17:41], 2, fun)

set.seed(12345)
train <- sample(nrow(df_dummy),0.7*nrow(df_dummy))
dftrain <- df_dummy[train,]
dfvalidation <- df_dummy[-train,]


train_input <- as.matrix(dftrain[,-16])
train_output <- as.vector(dftrain[,16])
validate_input <- as.matrix(dfvalidation[,-16])

kmax <- 15
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
for (i in 1:kmax){
  prediction <- knn(train_input, train_input,train_output$PROFITABLE, k=i)
  prediction2 <- knn(train_input, validate_input,train_output$PROFITABLE, k=i)
  #
  # The confusion matrix for training data is:
  CM1 <- table(prediction, dftrain$PROFITABLE)
  # The training error rate is:
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for validation data is: 
  CM2 <- table(prediction2, dfvalidation$PROFITABLE)
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}
plot(c(1,kmax),c(0,0.5),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(12, 0.5, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
z <- which.min(ER2)
cat("Minimum Validation Error k:", z)
ER2[12]
points(z,ER2[z],pch=2,col='red')
# sensitivity & specificity in validation
prediction_max <- knn(train_input, validate_input,train_output$PROFITABLE, k=14)
CM_max <- table(prediction_max, dfvalidation$PROFITABLE)
sensitivity = CM_max[2,2]/(CM_max[2,2]+CM_max[2,1])
specificity = CM_max[1,1]/(CM_max[1,1]+CM_max[1,2])
#predict
applicant = data.frame(AGE=27,SAV_ACCT_0=1,SAV_ACCT_1=0,SAV_ACCT_2=0,SAV_ACCT_3=0,SAV_ACCT_4=0,
                       CHK_ACCT_0=0,CHK_ACCT_1=1,CHK_ACCT_2=0,CHK_ACCT_3=0,
                       NUM_CREDITS=1,DURATION=12,PRESENT_RESIDENT=1,
                       HISTORY_1=0,HISTORY_0=0,HISTORY_2=1,HISTORY_3=0,HISTORY_4=0,EMPLOYMENT=1,NUM_DEPENDENTS=0,
                       JOB_2=1,JOB_0=0,JOB_1=0,JOB_3=0,RENT=1,INSTALL_RATE=3,GUARANTOR=0,OTHER_INSTALL=0,OWN_RES=0,TELEPHONE=1,FOREIGN=0,
                       REAL_ESTATE=0,AMOUNT_REQUESTED=4500,
                       TYPE_2=1,TYPE_0=0,TYPE_1=0,TYPE_3=0,TYPE_4=0,TYPE_5=0,TYPE_6=0)
predition_one = knn(train_input, applicant,train_output$PROFITABLE, k=14,prob = T)
predicted.probability_knn <- attr(predition_one, "prob")

#lift chart
predition_valid = knn(train_input, validate_input,train_output$PROFITABLE, k=14,prob = T)
probability_valid <- attr(predition_valid, "prob")

df_lift = data.frame(cbind(predition_valid,probability_valid))
df_lift$actural = dfvalidation$PROFITABLE
df_lift$predition_valid = df_lift$predition_valid-1

pro <-df_lift$probability_valid
df_lift_order <- df_lift[order(-pro),]
df_lift_order$Gains <- cumsum(df_lift_order$actural)
plot(df_lift_order$Gains,type="n",main="Lift Chart of KNN",xlab="Number of Cases",ylab="Cumulative Success")
lines(df_lift_order$Gains)
abline(0,sum(df_lift_order$actural)/nrow(df_lift_order),lty = 2, col="red")



#naive
df_cate = df
drops_2 = c('NPV','OBS#','CREDIT_EXTENDED')
df_cate = df_cate[ , !(names(df_cate) %in% drops_2)]
fac_cols = c(1,5,20)
df_cate[,-(fac_cols)] = lapply(df_cate[,-(fac_cols)], factor) 

# data partition
set.seed(12345)
train <- sample(nrow(df_cate),0.7*nrow(df_cate))
dftrain_naive <- df_cate[train,]
dfvalidation_naive <- df_cate[-train,]
# model training
model <- naiveBayes(dftrain_naive$PROFITABLE~., data=dftrain_naive)
model
prediction.naive <- predict(model, newdata = dfvalidation_naive[,-21])
CM_naive <- table(dfvalidation_naive$PROFITABLE,prediction.naive,dnn=list('actual','predicted'))
CM_naive
err_naive <- (CM_naive[1,2]+CM_naive[2,1])/sum(CM_naive)
# predict
applicant2 = data.frame(AGE=27,CHK_ACCT='1',SAV_ACCT='0',NUM_CREDITS='1',DURATION=12,HISTORY='1',PRESENT_RESIDENT='1',
                        EMPLOYMENT='1',JOB='2',NUM_DEPENDENTS='0',
                        RENT='1',INSTALL_RATE='3',GUARANTOR='0',OTHER_INSTALL='0',OWN_RES='0',TELEPHONE='1',FOREIGN='0',
                        REAL_ESTATE='0',TYPE='2',AMOUNT_REQUESTED=4500)
predition_one2 = predict(model,newdata=applicant2)
predicted.probability.naive <- predict(model, newdata = applicant2, type="raw")

#lift chart
probability_valid_naive =predict(model, newdata = dfvalidation_naive, type="raw")
df_lift_naive = data.frame(cbind(prediction.naive,probability_valid_naive))
df_lift_naive$actural = dfvalidation_naive$PROFITABLE
df_lift_naive$prediction = df_lift_naive$prediction-1

pro2 <-df_lift_naive$X1
df_lift_order_n <- df_lift_naive[order(-pro2),]
df_lift_order_n$actural <- as.numeric(df_lift_order_n$actural)
df_lift_order_n$Gains <- cumsum(df_lift_order_n$actural)
plot(df_lift_order_n$Gains,type="n",main="Lift Chart of Naive Bayes",xlab="Number of Cases",ylab="Cumulative Success")
lines(df_lift_order_n$Gains)
abline(0,sum(df_lift_order_n$actural)/nrow(df_lift_order_n),lty = 2, col="red")


# logistic regression
num_cols = c(2,3,6,9,19,21)
df_logi = df_cate
df_logi[,-(num_cols)] = lapply(df_logi[,-(num_cols)], as.numeric)
#data partition
set.seed(12345)
train <- sample(nrow(df_logi),0.7*nrow(df_logi))
dftrain_logi <- df_logi[train,]
dfvalidation_logi <- df_logi[-train,]

model_two <- glm(PROFITABLE~.,data =dftrain_logi, family = 'binomial')
summary(model_two)
dfvalidation_logi_pre<- predict(model_two, newdata=dfvalidation_logi, type = "response")
dfvalidation_logi_pr<- ifelse(dfvalidation_logi_pre>0.5,1,0)  
CM_logi <- table(dfvalidation_logi_pr, dfvalidation_logi$PROFITABLE)
err_logi = (CM_logi[2,1]+CM_logi[1,2])/sum(CM_logi)

#ROC of three predictor
prediction.knn = knn(train_input, validate_input,train_output$PROFITABLE, k=14,prob = T)
predicted.probability.knn <- attr(prediction.knn, "prob")
prediction.naive <- predict(model, newdata = dfvalidation_naive[,-21],type = "raw")
predicted.probability.naive <- prediction.naive[,2]

par(pty="s")
library(pROC)
roc_rose <- plot(roc(dfvalidation$PROFITABLE, predicted.probability.knn),
                 print.auc = TRUE, col = "blue", xlab="1-Specificity")

roc_rose <- plot(roc(dfvalidation_naive$PROFITABLE, predicted.probability.naive), 
                 print.auc = TRUE, col = "green", print.auc.y = .4, add = TRUE)
roc_rose <- plot(roc(dfvalidation_logi$PROFITABLE, dfvalidation_logi_pre), 
                 print.auc = TRUE, col = "red", print.auc.y = .6, add = TRUE)
legend(0.6, 0.3, c("logistic","knn","naive bayess"),lty=c(1,1), col=c("red","blue","green"))
