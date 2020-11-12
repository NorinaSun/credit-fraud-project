library(class)
library(data.table)
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)
library(Rlab)
library(caret)
library(adabag)
library(randomForest,verbose=FALSE)
library(purrr)
library(dplyr)


#load the data
data_v1_full = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v1.csv")
data_v2_full = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v2.csv")


#predict and train functions
train_md = function(data,md){

  if(md == "reg"){

    # Start the clock!
    ptm <- proc.time()

    trained_model = glm(as.factor(FraudResults)~TransactionAmount+TransactionTime+Overdraft+Country+
                          ShoppedBefore+HowMuch+ShoppingFreq+ShopProximite+UsualProximite+PrevTranProx
                        +OnlineTransaction+BillPayment+PreAuthorized,data=data,family="binomial")

  }
  else if(md == "rf"){

    # Start the clock!
    ptm <- proc.time()
    trained_model = randomForest(as.factor(FraudResults)~.,data=data)
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
  return(list("predictions"= prediction, "time" = elapsed_time))
}

#streaming simulation
streaming = function(data,min_n,max_n,md){
  
  #define test and train
  train_data = data[0:max_n,]
  test_data = data[max_n:nrow(data),]
  
  #defining empty lists to be filled
  train_list_time = list()
  predict_list_time = list()
  accuracy_list = list()
  dataset_size = list()
  
  for(i in seq(min_n,max_n,1000)){
    print(i)
    dataset_size[[i]] = i
    #training the model
    train_results = train_md(train_data[0:i,],md)

    #getting what it returns
    trained_model = train_results["trained_model"]
    train_list_time[[i]] = as.numeric(train_results$time)
  
    #predicting with new observation
    rownames(test_data) <- NULL
    predict_results = predict_md(test_data, trained_model)
    
    #getting what it returns
    predictions = predict_results$prediction$trained_model

    predict_list_time[[i]] = as.numeric(predict_results$time) / nrow(test_data)
    
    results = confusionMatrix(as.factor(predictions), as.factor(test_data$FraudResults))
    accuracy_list[[i]] = results$overall['Accuracy']
  }
  
  return(list("train_time" = train_list_time, "predict_time" = predict_list_time, "accuracy"= accuracy_list, "size" = dataset_size))
  
}

#taking a subset of the data
data_v1 = data_v1_full[0:5000,-c(1)]
data_v2 = data_v2_full[0:5000,-c(1)]

#choosing the params
min_n = 1000
max_n = 4000

#running the script
results = streaming(data_v1, min_n,max_n, "rf")

results_df = do.call(rbind, Map(data.frame, observation_size = results$size, train_time = results$train_time, predict_time = results$predict_time, accuracy = results$accuracy))
rownames(results_df) = results_df$observation_size
results_df$observation_size = NULL

#plot the results
# plot(seq(min_n,nrow(results)),results[min_n:nrow(results),1],
#      xlab = "Number of Observations", ylab = "Time for Fit and Predictions (in seconds)",
#      main = "Time Versus Dataset Size")


