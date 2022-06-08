setwd('D:\\R data')
library(readxl)
library(caret)
library(class)
library(pROC)
library(e1071)
library(tree)
df <- read_excel('credit3.xlsx',sheet = 'Data')

summary(df)

# data processing
df$PROFITABLE = ifelse(df$NPV>0,1,0)
df [,c(3:4,7,10,20)]= lapply(df[,c(3,4,7,10,20)],as.factor)
df_require = df
drops = c('OBS#','NPV','CREDIT_EXTENDED')
df_require = df_require[ , !(names(df_require) %in% drops)]
# data normalization,  if we need normalize observation, with mean and std in train data
# fun <- function(x){
#   a <- mean(x)
#   b <- sd(x)
#   (x - a)/(b)}
# 
# df_dummy[,1:15] <- apply(df_dummy[,1:15], 2, fun) # 2 indicate column
# df_dummy[,17:41] <- apply(df_dummy[,17:41], 2, fun)

set.seed(12345)
train <- sample(nrow(df_require),0.7*nrow(df_require))
dftrain <- df_require[train,]
dftest <- df_require[-train,]

tree.profit=tree(as.factor(PROFITABLE)~.,dftrain)
summary(tree.profit)
plot(tree.profit)
text(tree.profit,pretty=0)

tree.pred=predict(tree.profit,dftrain,type="class")
# compute the confusion matrix and error rate in train dataset
(CM = table(dftrain$PROFITABLE,tree.pred))
(err_rate = (CM[1,2]+CM[2,1])/sum(CM))

# in test dataset
tree.pred.test=predict(tree.profit,dftest,type="class")
(CM = table(dftest$PROFITABLE,tree.pred.test))
(err_rate_test = (CM[1,2]+CM[2,1])/sum(CM))

# 4.use cv.tree to purn the tree
set.seed(5)
cv.prune=cv.tree(tree.profit,FUN=prune.misclass)
names(cv.prune)
cv.prune
# The following plots help identify best size
plot(cv.prune$size,cv.prune$dev,type="b")

# plot the best tree
prune.tree=prune.misclass(tree.profit,best=12)
plot(prune.tree)
text(prune.tree,pretty=0)

# compute the test accuracy
tree.best.test=predict(prune.tree,dftest,type="class")
(CM2 = table(dftest$PROFITABLE,tree.best.test))
(acc_test = (CM2[1,1]+CM2[2,2])/sum(CM2))

# 7. classify one customer
applicant = data.frame(AGE=27,SAV_ACCT='4',
                       CHK_ACCT='1',
                       NUM_CREDITS=1,DURATION=12,PRESENT_RESIDENT=1,
                       HISTORY='1',EMPLOYMENT=1,NUM_DEPENDENTS=0,
                       JOB='2',RENT=1,INSTALL_RATE=3,GUARANTOR=0,OTHER_INSTALL=0,OWN_RES=0,TELEPHONE=1,FOREIGN=0,
                       REAL_ESTATE=0,AMOUNT_REQUESTED=4500,
                       TYPE='2')
applicant$SAV_ACCT = factor(applicant$SAV_ACCT, levels = c('0','1','2','3','4'))
applicant$CHK_ACCT = factor(applicant$CHK_ACCT, levels = c('0','1','2','3'))
applicant$HISTORY = factor(applicant$HISTORY, levels = c('0','1','2','3','4'))
applicant$JOB = factor(applicant$JOB, levels = c('0','1','2','3'))
applicant$TYPE = factor(applicant$TYPE, levels = c('0','1','2','3','4','5','6'))
predict_one = predict(prune.tree,applicant)
predict_one

# 9.predict and NPV in test dataset

df_require2 = df
drops2 = c('OBS#','CREDIT_EXTENDED')
df_require2 = df_require2[ , !(names(df_require2) %in% drops2)]
set.seed(12345)
train2 <- sample(nrow(df_require2),0.7*nrow(df_require2))
dftrain2 <- df_require2[train2,]
dftest2 <- df_require2[-train2,]

df.npv = dftest2
df.npv$predict = predict(prune.tree,dftest2,type="class")
sum(df.npv[which(df.npv$predict==1), 21])

# 10. logistic model
logi.model <- glm(PROFITABLE~.,data =dftrain, family = 'binomial')
summary(logi.model)
df.test.npv = dftest2
df.test.npv$predict.num<- predict(logi.model, newdata=df.test.npv, type = "response")
df.test.npv$predict<- ifelse(df.test.npv$predict.num>0.5,1,0) 

# predict NPV
sum(df.test.npv[which(df.test.npv$predict==1), 21])

# 11. regression tree to predict the NPV
df_require3 = df
drops3 = c('OBS#','CREDIT_EXTENDED','PROFITABLE')
df_require3 = df_require3[ , !(names(df_require3) %in% drops3)]
set.seed(12345)
train3 <- sample(nrow(df_require3),0.7*nrow(df_require3))
dftrain3 <- df_require3[train3,]
dftest3 <- df_require3[-train3,]

library(MASS)
tree.NPV=tree(NPV~.,dftrain3)
summary(tree.NPV)
tree.reg.pred=predict(tree.NPV,dftrain3)
# calculate RMSE in train
sqrt(mean((dftrain3$NPV - tree.reg.pred)^2))
# 12.
tree.reg.pred.t=predict(tree.NPV,dftest3)
sqrt(mean((dftest3$NPV - tree.reg.pred.t)^2))
# 13. get best prune tree
set.seed(5)
cv.prune.reg=cv.tree(tree.NPV)
names(cv.prune.reg)
cv.prune.reg
plot(cv.prune.reg$size,cv.prune.reg$dev,type='b')
# prune
tree.prune.reg=prune.tree(tree.NPV,best=2)
plot(tree.prune.reg)
text(tree.prune.reg,pretty=0)

# 14.test rmse of best pruned tree
yhat=predict(tree.prune.reg,newdata=dftest3)
sqrt(mean((dftest3$NPV - yhat)^2))


# 15.
best.tree.proft = dftest3
best.tree.proft$predict = yhat
# predict net profit
sum(best.tree.proft[which(best.tree.proft$predict>0), 21])

# 16. linear regression calcualte Net profit

linear.model <- lm(NPV~.,data =dftrain3)
summary(linear.model)
df.test.npv.l = dftest3
df.test.npv.l$predict.num<- predict(linear.model, newdata=df.test.npv.l )

sum(df.test.npv.l[which(df.test.npv.l$predict.num>0), 21])

sum(df.test.npv.l[,21])
