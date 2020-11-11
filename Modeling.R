library(class)
library(data.table)
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)
library(Rlab)  
library(caret)
library(adabag)
library(randomForest,verbose=FALSE)

#load the data
data_v1 = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v1.csv")
data_v2 = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v2.csv")

data = data_v1[sample(nrow(data_v1), 10000), ]


#predict and train functions
train_md = function(data,md){
  
  if(md == "reg"){
    
    # Start the clock!
    ptm <- proc.time()
    
    trained_model = glm(as.factor(unlist(FraudResults))~.,data=data,family="binomial")
  }
  else if(md == "rf"){
    
    # Start the clock!
    ptm <- proc.time()
    
    trained_model = randomForest(as.factor(FraudResults)~CreditLimit+ShoppingFreq+TransactionAmount+HowMuch+OnlineTransaction,data=data)
  }
  else if(md == "boosting"){
    data$FraudResults = as.factor(unlist(data$FraudResults))

    # Start the clock!
    ptm <- proc.time()
    
    trained_model = boosting(FraudResults~.,data=data)
  }
  else{
    print("invalid model selection passed")
  }
  
  # Stop the clock
  runtime = proc.time() - ptm
  elapsed_time = runtime[3]
  
  #return the model and the elapsed time
  return(list("trained_model"= trained_model, "time"= elapsed_time))
}

predict_md = function(data,model){
  
  # Start the clock!
  ptm <- proc.time()
  
  #generate the prediction
  prediction = predict(model,newdata = data)
  
  # Stop the clock
  runtime = proc.time() - ptm
  elapsed_time = runtime[3]
  
  #return the prediction value and the elapsed time
  return(list("prediction"= prediction, "time" = elapsed_time ))
}

#streaming simulation
streaming = function(data,min_n,md){
  
  train_list_time = numeric(0)
  predict_list_time = numeric(0)
  
  for(i in min_n:nrow(data)){
    sprintf("now on data size", i)

    #training the model
    train_results = train_md(data[0:i,],md)
    #getting what it returns
    trained_model = train_results["trained_model"]
    train_list_time[i] = train_results["time"]
 
    #predicting with new observation
    predict_results = predict_md(data[i:nrow(data),], trained_model)
    #getting what it returns
    predictions = predict_results["prediction"]
    predict_list_time[i] = as.numeric(predict_results["time"]) / (nrow(data) - i)
  }
  return(list("train_time" = train_list_time, "predict_time" = predict_list_time))
}

results = streaming(data, 100, "reg")
train_time = results["train_time"]
predict_time = results["predict_time"]
