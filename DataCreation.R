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

for (i in 1:length(sim1_1M$TransactionLocation))
  {if (sim1_1M$TransactionLocation[i] == 1 ||
       sim1_1M$ShoppedBefore[i] == 1 ||
       sim1_1M$ShopProximite[i] == 1 ||
       sim1_1M$UsualProximite[i] == 1 ||
       sim1_1M$PrevTranProx[i] == 1) 
       {sim1_1M$Country[i] = 1}}

Fraud = NULL
sim1_1M["FraudResults"] = NULL

# each of these variables contributes to the results with different probabilities - MAX = 28

for (i in 1:length(sim1_1M$TransactionAmount)){
  Fraud[i] <- 3*sim1_1M$TransactionAmount[i]+ 2*sim1_1M$TransactionLocation[i]+ 3*sim1_1M$TransactionTime[i]+
    4*sim1_1M$CreditLimit[i]+ 2*sim1_1M$Overdraft[i]+ 3*sim1_1M$Country[i] + sim1_1M$ShoppedBefore[i]+ 2*sim1_1M$HowMuch[i]+
    2*sim1_1M$ShoppingFreq[i]+ sim1_1M$ShopProximite[i]+ sim1_1M$UsualProximite[i]+ sim1_1M$PrevTranProx[i]+
    3*sim1_1M$OnlineTransaction[i]+ sim1_1M$BillPayment[i]+ sim1_1M$PreAuthorized[i]
  if (Fraud[i] <=9) {
    sim1_1M$FraudResults[i] =0
  } else {
    sim1_1M$FraudResults[i] =1
  }
}

if (sim1_1M$Overdraft==0 && 
    sim1_1M$Country==0 && 
    sim1_1M$ShoppedBefore==0)
   {sim1_1M$FraudResults==0}

if (sim1_1M$OnlineTransaction==0 && 
    sim1_1M$BillPayment==0 && 
    sim1_1M$PreAuthorized==0) 
   {sim1_1M$FraudResults==0}

if (sim1_1M$ShopProximite==1 && 
    sim1_1M$ShoppedBefore==1 && 
    sim1_1M$UsualProximite==1 && 
    sim1_1M$PrevTranProx==1) 
   {sim1_1M$FraudResults==1}


for (i in 1:length(sim1_1M$TransactionLocation))
    {if (sim1_1M$TransactionLocation[i] == 0 &&
         sim1_1M$ShoppedBefore[i] == 0 &&
         sim1_1M$ShopProximite[i] == 0 &&
         sim1_1M$UsualProximite[i] == 0 &&
         sim1_1M$PrevTranProx[i] == 0) 
         {sim1_1M$FraudResults[i] = 0}}


str(sim1_1M)
head(sim1_1M)
sim1_1M %>% 
  group_by( FraudResults ) %>% 
  summarise( percent = 100 * n() / nrow( sim1_1M ) )

#--------------1M observations dataset 2--------------


TransactionAmount <- rbinom(1000000, 1, 0.5)          
TransactionLocation <- rbinom(1000000, 1, 0.5)        
TransactionTime <-rbinom(1000000, 1, 0.7)             
CreditLimit <- rbinom(1000000, 1, 0.34)               
Overdraft <- rbinom(1000000, 1, 0.18)                  
Country <- rbinom(1000000, 1, 0.6)                   
ShoppedBefore <- rbinom(1000000, 1, 0.32)              
HowMuch <- rbinom(1000000, 1, 0.8)                   
for (i in 1:length(ShoppedBefore))
{if (ShoppedBefore[i] == 1) 
{HowMuch[i] = 0}}
    
ShoppingFreq <- rbinom(1000000, 1, 0.09)              
ShopProximite <- rbinom(1000000, 1, 0.69)              
UsualProximite <- rbinom(1000000, 1, 0.5)            
PrevTranProx <- rbinom(1000000, 1, 0.5)               
OnlineTransaction <- rbinom(1000000, 1, 0.9)          
BillPayment <- rbinom(1000000, 1, 0.8)               
PreAuthorized <- rbinom(1000000, 1, 0.1)             

sim2_1M <- data.frame(TransactionAmount,TransactionLocation,TransactionTime,
                      CreditLimit, Overdraft,Country , ShoppedBefore, HowMuch,
                      ShoppingFreq, ShopProximite, UsualProximite, PrevTranProx,
                      OnlineTransaction, BillPayment, PreAuthorized)

for (i in 1:length(sim2_1M$TransactionLocation))
{if (sim2_1M$TransactionLocation[i] == 1 ||
     sim2_1M$ShoppedBefore[i] == 1 ||
     sim2_1M$ShopProximite[i] == 1 ||
     sim2_1M$UsualProximite[i] == 1 ||
     sim2_1M$PrevTranProx[i] == 1) 
{sim2_1M$Country[i] = 0}}

Fraud1 = NULL
sim2_1M["FraudResults"] = NULL

# each of these variables contributes to the results with different probabilities - MAX = 28

for (i in 1:length(sim2_1M$TransactionAmount)){
  Fraud1[i] <- 3*sim2_1M$TransactionAmount[i]+ 2*sim2_1M$TransactionLocation[i]+ 3*sim2_1M$TransactionTime[i]+
    4*sim2_1M$CreditLimit[i]+ 2*sim2_1M$Overdraft[i]+ 3*sim2_1M$Country[i] + sim2_1M$ShoppedBefore[i]+ 2*sim2_1M$HowMuch[i]+
    2*sim2_1M$ShoppingFreq[i]+ sim2_1M$ShopProximite[i]+ sim2_1M$UsualProximite[i]+ sim2_1M$PrevTranProx[i]+
    3*sim2_1M$OnlineTransaction[i]+ sim2_1M$BillPayment[i]+ sim2_1M$PreAuthorized[i]
  if (Fraud[i] <=22) {
    sim2_1M$FraudResults[i] =0
  } else {
    sim2_1M$FraudResults[i] =1
  }
}


if (sim1_1M$CreditLimit[i] == 1 &&
   sim1_1M$ShoppedBefore[i] == 0 && 
   sim1_1M$PreAuthorized[i] == 1)
   {sim1_1M$FraudResults==1}

if (sim1_1M$HowMuch[i] == 0 &&
   sim1_1M$ShoppingFreq[i] == 0 && 
   sim1_1M$OnlineTransaction[i] == 0)
   {sim1_1M$FraudResults==0}


if (sim1_1M$CreditLimit[i] == 1 &&
    sim1_1M$BillPayment[i] == 1 && 
    sim1_1M$UsualProximite[i] == 0)
    {sim1_1M$FraudResults==0}

if (sim1_1M$TransactionAmoun[i] == 1 &&
   sim1_1M$TransactionTime[i] == 0 && 
   sim1_1M$ShoppedBefore[i] == 0 &&
   sim1_1M$ShoppingFreq[i] == 0 &&
   sim1_1M$PreAuthorized[i] == 1)
   {sim1_1M$FraudResults==1}



for (i in 1:length(sim1_1M$TransactionLocation))
  {if (sim1_1M$TransactionLocation[i] == 0 &&
      sim1_1M$ShoppedBefore[i] == 1 &&
      sim1_1M$ShoppingFreq[i] == 0 &&
      sim1_1M$UsualProximite[i] == 0 &&
      sim1_1M$ShopProximite[i] == 1) 
      {sim1_1M$FraudResults[i] = 1}}

str(sim2_1M)
head(sim2_1M)
sim2_1M %>% 
  group_by( FraudResults ) %>% 
  summarise( percent = 100 * n() / nrow( sim2_1M ) )

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


