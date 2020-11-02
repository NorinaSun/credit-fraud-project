knitr::opts_chunk$set(echo = TRUE)
install.packages("Rlab") 
install.packages("adabag")
install.packages("caret")
library(tidyverse,verbose=FALSE)
library(Rlab)  
library(caret)
library(adabag)
library(randomForest,verbose=FALSE)
source("/Users/sabad/Downloads/PredictingBinaries.r")

#========================Generating Data==================== 

set.seed(20190927)

#--------------Generating 100M observations--------------

#Amount <- rnorm(10000, mean = 5000, sd = 1000) 
TransactionAmount <- rbinom(10000, 1, 0.3)          # = 0 if the amount of purchase is less than 1000
TransactionLocation <- rbinom(10000, 1, 0.4)        # = 0 if the transaction is done where the buyer lives
TransactionTime <-rbinom(10000, 1, 0.5)             # = 0 if the transaction is not done during 12 - 5 .A.M
#CreditLimit <- rnorm(10000, mean = 20000, sd = 4000)
CreditLimit <- rbinom(10000, 1, 0.13)               # = 0 if the amount of purchase is less than credit limit
Overdraft <- rbinom(10000, 1, 0.1)                  # = 0 if the buyer has not had overdraft during the past 6 months
Country <- rbinom(10000, 1, 0.16)                   # = 0 if the transaction location is not in another country
ShoppedBefore <- rbinom(10000, 1, 0.5)              # = 0 if the buyer has shopped there before
#HowMuch <- rnorm(10000, mean = 20000, sd = 4000)    
HowMuch <- rbinom(10000, 1, 0.3)                    # = 0 if the amount of purchase is less than 1000
for (i in 1:length(ShoppedBefore))
  {if (ShoppedBefore[[i]] == 1) 
      {HowMuch[[i]] = 0}}
#ShoppingFreq <- rnorm(10000, mean = 50, sd = 10)    
ShoppingFreq <- rbinom(10000, 1, 0.25)              # = 0 if the buyer frequently purchase from this shop
ShopProximite <- rbinom(10000, 1, 0.6)              # = 0 if the shop is near where the buyer lives
UsualProximite <- rbinom(10000, 1, 0.3)             # = 0 if the transaction happens near where they usually spend
PrevTranProx <- rbinom(10000, 1, 0.2)               # = 0 if the transaction happens near their previous transactions
OnlineTransaction <- rbinom(10000, 1, 0.5)          # = 0 if the transaction is not online
BillPayment <- rbinom(10000, 1, 0.5)                # = 0 if the transaction is a bill payment
PreAuthorized <- rbinom(10000, 1, 0.5)              # = 0 if the payment is a pre-authorized

sim100M <- data.frame(TransactionAmount,TransactionLocation,TransactionTime,
                      CreditLimit, Overdraft,Country , ShoppedBefore, HowMuch,
                      ShoppingFreq, ShopProximite, UsualProximite, PrevTranProx,
                      OnlineTransaction, BillPayment, PreAuthorized)

for (i in 1:length(sim100M$TransactionLocation))
  {if (sim100M$TransactionLocation[[i]] == 1 ||
       sim100M$ShoppedBefore[[i]] == 1 ||
       sim100M$ShopProximite[[i]] == 1 ||
       sim100M$UsualProximite[[i]] == 1 ||
       sim100M$PrevTranProx[[i]] == 1) 
       {sim100M$Country[[i]] = 1}}

Fraud = NULL
sim100M["FraudResults"] = NULL

# each of these variables contributes to the results with different probabilities - MAX = 28

for (i in 1:length(sim100M$TransactionAmount)){
  Fraud[[i]] <- 3*sim100M$TransactionAmount[[i]]+ 2*sim100M$TransactionLocation[[i]]+ 3*sim100M$TransactionTime[[i]]+
    4*sim100M$CreditLimit[[i]]+ 2*sim100M$Overdraft[[i]]+ 3*sim100M$Country[[i]] + sim100M$ShoppedBefore[[i]]+ 2*sim100M$HowMuch[[i]]+
    2*sim100M$ShoppingFreq[[i]]+ sim100M$ShopProximite[[i]]+ sim100M$UsualProximite[[i]]+ sim100M$PrevTranProx[[i]]+
    3*sim100M$OnlineTransaction[[i]]+ sim100M$BillPayment[[i]]+ sim100M$PreAuthorized[[i]]
  if (Fraud[[i]] <=6) {
    sim100M$FraudResults[[i]] =0
  } else {
    sim100M$FraudResults[[i]] =1
  }
}

if (sim100M$Overdraft==0 && 
    sim100M$Country==0 && 
    sim100M$ShoppedBefore==0)
   {sim100M$FraudResults==0}

if (sim100M$OnlineTransaction==0 && 
    sim100M$BillPayment==0 && 
    sim100M$PreAuthorized==0) 
   {sim100M$FraudResults==0}

if (sim100M$ShopProximite==1 && 
    sim100M$ShoppedBefore==1 && 
    sim100M$UsualProximite==1 && 
    sim100M$PrevTranProx==1) 
   {sim100M$FraudResults==1}


for (i in 1:length(sim100M$TransactionLocation))
    {if (sim100M$TransactionLocation[[i]] == 0 &&
         sim100M$ShoppedBefore[[i]] == 0 &&
         sim100M$ShopProximite[[i]] == 0 &&
         sim100M$UsualProximite[[i]] == 0 &&
         sim100M$PrevTranProx[[i]] == 0) 
         {sim100M$FraudResults[[i]] = 0}}


str(sim100M)
head(sim100M)
sim100M %>% 
  group_by( FraudResults ) %>% 
  summarise( percent = 100 * n() / nrow( sim100M ) )


#============Resample 1000 Observations================

library(dplyr)
memory.size()
memory.limit(1089.68)

#train set

fraud = sim100M %>%
  filter(sim100M$FraudResults == "1")
fraudrows = sample(nrow(fraud),1000,replace=F)
train_fraud = fraud[fraudrows,]

nfraud = sim100M %>%
  filter(sim100M$FraudResults == "0")
nfraudrows = sample(nrow(nfraud),1000,replace=F)
train_nfraud = nfraud[nfraudrows,]
train = rbind(train_fraud,train_nfraud)

rows <- sample(nrow(train),1600)
train1 <- train[rows, ]

#valid <- train[-rows,]
# ---------------------------------------------------

#validation set
valid1 = sim100M[-fraudrows,]
valid2 = sim100M[-nfraudrows,]
valid3 = rbind(valid1,valid2)
valid = valid3[sample(nrow(valid3),4000,replace=F),]


#============Models================

#regression
mode = glm(FraudResults~.,data=train1,family="binomial")
p1=predict(mode,newdata=valid,type="response")
p1
beta = mode$coefficients
beta

#random forest

bow=randomForest(FraudResults~CreditLimit+ShoppingFreq+TransactionAmount+HowMuch+OnlineTransaction,data=train1)
print(varImpPlot(bow))
as.matrix(bow$importance/max(bow$importance))
pbow=predict(bow,valid)


#boosting
#changing to factor variables

train1$FraudResults = as.factor(train1$FraudResults)
#str(train1)

boh=boosting(FraudResults~.,data=train1)
pboh=predict(boh,valid,type="prob")$prob[,2]
as.matrix(boh$importance/max(boh$importance))

train1$FraudResults = as.numeric(train1$FraudResults)
#cor = cor(sim100M)
#ROC

roc(valid$FraudResults,p1,col="blue")$AUC
roc(valid$FraudResults,pbow,col="red")$AUC
roc(valid$FraudResults,pboh,col="dark green")$AUC

legend(.6,.3, legend=c("Regression","Random Forest","Boosting"),col=c("blue","red","dark green"),lty=1)

#lift

lift(valid$FraudResults,p1,col="blue")
lift(valid$FraudResults,pbow,lines=TRUE,col="red")
lift(valid$FraudResults,pboh,lines=TRUE,col="dark green")

legend(0.4,1.8,c("Regression","Random Forest","Boosting"),col=c("blue","red","dark green"),lty=1)

#============== cross validation ================
library(parallel)
detectCores(all.tests = FALSE, logical = TRUE)

select <- 


