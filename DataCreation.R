knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)
library(Rlab)  
library(caret)
library(adabag)
library(randomForest,verbose=FALSE)
#source("/Users/sabad/Downloads/PredictingBinaries.r")

#========================Generating Data==================== 

set.seed(20190927)

#--------------1M observations dataset 1--------------

#Amount <- rnorm(1000000, mean = 5000, sd = 1000) 
TransactionAmount <- rbinom(1000000, 1, 0.3)          # = 0 if the amount of purchase is less than 1000
TransactionLocation <- rbinom(1000000, 1, 0.4)        # = 0 if the transaction is done where the buyer lives
TransactionTime <-rbinom(1000000, 1, 0.5)             # = 0 if the transaction is not done during 12 - 5 .A.M
#CreditLimit <- rnorm(1000000, mean = 20000, sd = 4000)
CreditLimit <- rbinom(1000000, 1, 0.13)               # = 0 if the amount of purchase is less than credit limit
Overdraft <- rbinom(1000000, 1, 0.1)                  # = 0 if the buyer has not had overdraft during the past 6 months
Country <- rbinom(1000000, 1, 0.16)                   # = 0 if the transaction location is not in another country
ShoppedBefore <- rbinom(1000000, 1, 0.5)              # = 0 if the buyer has shopped there before
#HowMuch <- rnorm(1000000, mean = 20000, sd = 4000)    
HowMuch <- rbinom(1000000, 1, 0.3)                    # = 0 if the amount of purchase is less than 1000
for (i in 1:length(ShoppedBefore))
  {if (ShoppedBefore[i] == 1) 
      {HowMuch[i] = 0}}
#ShoppingFreq <- rnorm(1000000, mean = 50, sd = 10)    
ShoppingFreq <- rbinom(1000000, 1, 0.25)              # = 0 if the buyer frequently purchase from this shop
ShopProximite <- rbinom(1000000, 1, 0.6)              # = 0 if the shop is near where the buyer lives
UsualProximite <- rbinom(1000000, 1, 0.3)             # = 0 if the transaction happens near where they usually spend
PrevTranProx <- rbinom(1000000, 1, 0.2)               # = 0 if the transaction happens near their previous transactions
OnlineTransaction <- rbinom(1000000, 1, 0.5)          # = 0 if the transaction is not online
BillPayment <- rbinom(1000000, 1, 0.5)                # = 0 if the transaction is a bill payment
PreAuthorized <- rbinom(1000000, 1, 0.5)              # = 0 if the payment is a pre-authorized

sim1_1M <- data.frame(TransactionAmount,TransactionLocation,TransactionTime,
                      CreditLimit, Overdraft,Country , ShoppedBefore, HowMuch,
                      ShoppingFreq, ShopProximite, UsualProximite, PrevTranProx,
                      OnlineTransaction, BillPayment, PreAuthorized)


sim1_1M[(sim1_1M$TransactionLocation == 1 ||
          sim1_1M$ShoppedBefore == 1 ||
          sim1_1M$ShopProximite == 1 ||
          sim1_1M$UsualProximite  == 1 ||
          sim1_1M$PrevTranProx == 1), "Country"] <- 1


sim1_1M$Fraud = 0
sim1_1M["FraudResults"] = 0

# each of these variables contributes to the results with different probabilities - MAX = 28

sim1_1M[,"Fraud"] <- 3*sim1_1M$TransactionAmount+ 2*sim1_1M$TransactionLocation+ 3*sim1_1M$TransactionTime+
    4*sim1_1M$CreditLimit+ 2*sim1_1M$Overdraft+ 3*sim1_1M$Country+ sim1_1M$ShoppedBefore+ 2*sim1_1M$HowMuch+
    2*sim1_1M$ShoppingFreq+ sim1_1M$ShopProximite+ sim1_1M$UsualProximite+ sim1_1M$PrevTranProx+
    3*sim1_1M$OnlineTransaction+ sim1_1M$BillPayment+ sim1_1M$PreAuthorized

sim1_1M[(sim1_1M$Fraud<=11),"FraudResults"] <- 0
sim1_1M[(sim1_1M$Fraud>11),"FraudResults"] <- 1


Fraud_1 = sample(c(0,1), size = 1000000, replace = TRUE, prob=c(0.2,0.8))
Fraud_0 = sample(c(0,1), size = 1000000, replace = TRUE, prob=c(0.8,0.2))


sim1_1M[(sim1_1M$Overdraft==0 && 
         sim1_1M$Country==0 && 
         sim1_1M$ShoppedBefore==0),"FraudResults"] <- sample(Fraud_0,size=1)

sim1_1M[(sim1_1M$OnlineTransaction==0 && 
    sim1_1M$BillPayment==0 && 
    sim1_1M$PreAuthorized==0),"FraudResults"] <- sample(Fraud_0, size=1)

sim1_1M[(sim1_1M$ShopProximite==1 && 
    sim1_1M$ShoppedBefore==1 && 
    sim1_1M$UsualProximite==1 && 
    sim1_1M$PrevTranProx==1), "FraudResults"] <- sample(Fraud_1, size=1)


sim1_1M[(sim1_1M$TransactionLocation == 0 &&
     sim1_1M$ShoppedBefore == 0 &&
     sim1_1M$ShopProximite == 0 &&
     sim1_1M$UsualProximite == 0 &&
     sim1_1M$PrevTranProx == 0), "FraudResults"] <- sample(Fraud_0,size=1)


str(sim1_1M)
head(sim1_1M)
sim1_1M %>% 
  group_by( FraudResults ) %>% 
  summarise( percent = 100 * n() / nrow( sim1_1M ) )

sim1_1M$Fraud <- NULL

write.csv(sim1_1M, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v1.csv", row.names = TRUE)

#--------------1M observations dataset 2--------------

#Amount <- rnorm(1000000, mean = 5000, sd = 1000) 
TransactionAmount <- rbinom(1000000, 1, 0.5)          # = 0 if the amount of purchase is less than 1000
TransactionLocation <- rbinom(1000000, 1, 0.5)        # = 0 if the transaction is done where the buyer lives
TransactionTime <-rbinom(1000000, 1, 0.7)             # = 0 if the transaction is not done during 12 - 5 .A.M
#CreditLimit <- rnorm(1000000, mean = 20000, sd = 4000)
CreditLimit <- rbinom(1000000, 1, 0.34)               # = 0 if the amount of purchase is less than credit limit
Overdraft <- rbinom(1000000, 1, 0.18)                  # = 0 if the buyer has not had overdraft during the past 6 months
Country <- rbinom(1000000, 1, 0.6)                   # = 0 if the transaction location is not in another country
ShoppedBefore <- rbinom(1000000, 1, 0.32)              # = 0 if the buyer has shopped there before
#HowMuch <- rnorm(1000000, mean = 20000, sd = 4000)    
HowMuch <- rbinom(1000000, 1, 0.8)                    # = 0 if the amount of purchase is less than 1000
for (i in 1:length(ShoppedBefore))
{if (ShoppedBefore[i] == 1) 
{HowMuch[i] = 0}}
#ShoppingFreq <- rnorm(1000000, mean = 50, sd = 10)    
ShoppingFreq <- rbinom(1000000, 1, 0.09)              # = 0 if the buyer frequently purchase from this shop
ShopProximite <- rbinom(1000000, 1, 0.69)              # = 0 if the shop is near where the buyer lives
UsualProximite <- rbinom(1000000, 1, 0.5)             # = 0 if the transaction happens near where they usually spend
PrevTranProx <- rbinom(1000000, 1, 0.5)               # = 0 if the transaction happens near their previous transactions
OnlineTransaction <- rbinom(1000000, 1, 0.9)          # = 0 if the transaction is not online
BillPayment <- rbinom(1000000, 1, 0.8)                # = 0 if the transaction is a bill payment
PreAuthorized <- rbinom(1000000, 1, 0.1)              # = 0 if the payment is a pre-authorized

sim2_1M <- data.frame(TransactionAmount,TransactionLocation,TransactionTime,
                      CreditLimit, Overdraft,Country , ShoppedBefore, HowMuch,
                      ShoppingFreq, ShopProximite, UsualProximite, PrevTranProx,
                      OnlineTransaction, BillPayment, PreAuthorized)


sim2_1M[(sim2_1M$TransactionLocation == 1 ||
           sim2_1M$ShoppedBefore == 1 ||
           sim2_1M$ShopProximite == 1 ||
           sim2_1M$UsualProximite  == 1 ||
           sim2_1M$PrevTranProx == 1), "Country"] <- 0


sim2_1M$Fraud = 0
sim2_1M["FraudResults"] = 0

# each of these variables contributes to the results with different probabilities - MAX = 28

sim2_1M[,"Fraud"] <- 3*sim2_1M$TransactionAmount+ 2*sim2_1M$TransactionLocation+ 3*sim2_1M$TransactionTime+
  4*sim2_1M$CreditLimit+ 2*sim2_1M$Overdraft+ 3*sim2_1M$Country+ sim2_1M$ShoppedBefore+ 2*sim2_1M$HowMuch+
  2*sim2_1M$ShoppingFreq+ sim2_1M$ShopProximite+ sim2_1M$UsualProximite+ sim2_1M$PrevTranProx+
  3*sim2_1M$OnlineTransaction+ sim2_1M$BillPayment+ sim2_1M$PreAuthorized

sim2_1M[(sim2_1M$Fraud<=9),"FraudResults"] <- 0
sim2_1M[(sim2_1M$Fraud>9),"FraudResults"] <- 1


Fraud_1 = sample(c(0,1), size = 1000000, replace = TRUE, prob=c(0.1,0.9))
Fraud_0 = sample(c(0,1), size = 1000000, replace = TRUE, prob=c(0.9,0.1))

sim2_1M[(sim2_1M$CreditLimit==1 && 
           sim2_1M$ShoppedBefore==0 && 
           sim2_1M$PreAuthorized==1),"FraudResults"] <- sample(Fraud_1,size=1)

sim2_1M[(sim2_1M$HowMuch==0 && 
           sim2_1M$ShoppingFreq==0 && 
           sim2_1M$OnlineTransaction==0),"FraudResults"] <- sample(Fraud_1,size=1)

sim2_1M[(sim2_1M$CreditLimit==1 && 
           sim2_1M$BillPayment==1 && 
           sim2_1M$UsualProximite==0),"FraudResults"] <- sample(Fraud_0, size=1)

sim2_1M[(sim2_1M$TransactionAmount==1 && 
           sim2_1M$TransactionTime==0 && 
           sim2_1M$ShoppedBefore==0 && 
           sim2_1M$ShoppingFreq==0 &&
           sim2_1M$PreAuthorized=1), "FraudResults"] <- sample(Fraud_1, size=1)


sim2_1M[(sim2_1M$TransactionLocation == 0 &&
           sim2_1M$ShoppedBefore == 1 &&
           sim2_1M$ShopProximite == 0 &&
           sim2_1M$UsualProximite == 0 &&
           sim2_1M$ShopProximite==1), "FraudResults"] <- sample(Fraud_1,size=1)


str(sim2_1M)
head(sim2_1M)
sim2_1M %>% 
  group_by( FraudResults ) %>% 
  summarise( percent = 100 * n() / nrow( sim2_1M ) )

sim2_1M$Fraud <- NULL


write.csv(sim2_1M, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v2.csv", row.names = TRUE)
# #============Resample 1000 Observations================
# 
# library(dplyr)
# memory.size()
# memory.limit(1089.68)
# 
# #train set
# 
# fraud = sim100M %>%
#   filter(sim100M$FraudResults == "1")
# fraudrows = sample(nrow(fraud),1000,replace=F)
# train_fraud = fraud[fraudrows,]
# 
# nfraud = sim100M %>%
#   filter(sim100M$FraudResults == "0")
# nfraudrows = sample(nrow(nfraud),1000,replace=F)
# train_nfraud = nfraud[nfraudrows,]
# train = rbind(train_fraud,train_nfraud)
# 
# rows <- sample(nrow(train),1600)
# train1 <- train[rows, ]
# 
# #valid <- train[-rows,]
# # ---------------------------------------------------
# 
# #validation set
# valid1 = sim100M[-fraudrows,]
# valid2 = sim100M[-nfraudrows,]
# valid3 = rbind(valid1,valid2)
# valid = valid3[sample(nrow(valid3),4000,replace=F),]
# 
# 
# #============Models================
# 
# #regression
# mode = glm(FraudResults~.,data=train1,family="binomial")
# p1=predict(mode,newdata=valid,type="response")
# p1
# beta = mode$coefficients
# beta
# 
# #random forest
# 
# bow=randomForest(FraudResults~CreditLimit+ShoppingFreq+TransactionAmount+HowMuch+OnlineTransaction,data=train1)
# print(varImpPlot(bow))
# as.matrix(bow$importance/max(bow$importance))
# pbow=predict(bow,valid)
# 
# 
# #boosting
# #changing to factor variables
# 
# train1$FraudResults = as.factor(train1$FraudResults)
# #str(train1)
# 
# boh=boosting(FraudResults~.,data=train1)
# pboh=predict(boh,valid,type="prob")$prob[,2]
# as.matrix(boh$importance/max(boh$importance))
# 
# train1$FraudResults = as.numeric(train1$FraudResults)
# #cor = cor(sim100M)
# #ROC
# 
# roc(valid$FraudResults,p1,col="blue")$AUC
# roc(valid$FraudResults,pbow,col="red")$AUC
# roc(valid$FraudResults,pboh,col="dark green")$AUC
# 
# legend(.6,.3, legend=c("Regression","Random Forest","Boosting"),col=c("blue","red","dark green"),lty=1)
# 
# #lift
# 
# lift(valid$FraudResults,p1,col="blue")
# lift(valid$FraudResults,pbow,lines=TRUE,col="red")
# lift(valid$FraudResults,pboh,lines=TRUE,col="dark green")
# 
# legend(0.4,1.8,c("Regression","Random Forest","Boosting"),col=c("blue","red","dark green"),lty=1)
# 
# #============== cross validation ================
# library(parallel)
# detectCores(all.tests = FALSE, logical = TRUE)
# 
# select <- 
# 
# 
