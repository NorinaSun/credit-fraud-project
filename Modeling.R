# library(class)
# library(data.table)
# knitr::opts_chunk$set(echo = TRUE)
# library(tidyverse,verbose=FALSE)
# library(Rlab)
# library(caret)
# library(adabag)
# library(randomForest,verbose=FALSE)
# library(purrr)
# library(dplyr)
# 
# 
# #load the data
# data_v1_full = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v1.csv")
# data_v2_full = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v2.csv")

#models
logistic_regression = function(train_data, test_data){
  
  #TRAINING
  
  # Start the clock!
  train_start <- proc.time()
  
  #train the model
  trained_model = glm(as.factor(FraudResults)~TransactionAmount+TransactionTime+Overdraft+Country+
                        ShoppedBefore+HowMuch+ShoppingFreq+ShopProximite+UsualProximite+PrevTranProx
                      +OnlineTransaction+BillPayment+PreAuthorized,data=train_data,family="binomial")
  
  
  # Stop the clock
  train_time = proc.time() - train_start
  elapsed_train_time = train_time[3]
  
  #TESTING
  
  #subsetting the data
  data_subset = select(test_data,TransactionAmount,TransactionTime,Overdraft,Country,
                       ShoppedBefore,HowMuch,ShoppingFreq,ShopProximite,UsualProximite,PrevTranProx,
                       OnlineTransaction,BillPayment,PreAuthorized)
  
  # Start the clock!
  test_start <- proc.time()
  
  #generate the prediction
  prediction_probabilities = predict(trained_model,newdata = test_data, type="response")
  
  # Stop the clock
  test_time = proc.time() - test_start
  elapsed_test_time = train_time[3]
  
  prediction= ifelse(prediction_probabilities > 0.5 ,1,0)

  return(list("predictions" = prediction, "train_time" = elapsed_train_time, "test_time" = elapsed_test_time))
  
}
  
randomForestmd = function(train_data, test_data){
  
  #TRAINING
  
  # Start the clock!
  train_start <- proc.time()
  
  #train the model
  trained_model = randomForest(as.factor(FraudResults)~.,data=train_data)
  
  # Stop the clock
  train_time = proc.time() - train_start
  elapsed_train_time = train_time[3]
  
  #TESTING

  # Start the clock!
  test_start <- proc.time()
  
  #generate the prediction
  prediction = predict(trained_model,newdata = test_data)
  
  # Stop the clock
  test_time = proc.time() - test_start
  elapsed_test_time = train_time[3]
  
  return(list("predictions" = prediction, "train_time" = elapsed_train_time, "test_time" = elapsed_test_time))
  
}

#streaming simulation
streaming = function(data,min_n,max_n,FUN){
  
  #define test and train
  train_data = data[0:max_n,]
  test_data = data[max_n:nrow(data),]
  
  #defining empty lists to be filled
  train_list_time = list()
  predict_list_time = list()
  accuracy_list = list()
  dataset_size = list()
  
  for(i in seq(min_n,max_n,1000)){
    #keeping track of observation size//where we are in the loop
    print(i)
    dataset_size[[i]] = i
    
    #resetting indicies for test set
    rownames(test_data) <- NULL
    
    #run the model
    results = FUN(train_data[0:i,], test_data)
    
    #train time
    train_list_time[[i]] = as.numeric(results$train_time)
    
    #test time
    predict_list_time[[i]] = as.numeric(results$test_time) / nrow(test_data)
    
    #predictions accuracy
    results = confusionMatrix(as.factor(results$predictions), as.factor(test_data$FraudResults))
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
results = streaming(data_v1, min_n,max_n, logistic_regression)

results_df = do.call(rbind, Map(data.frame, observation_size = results$size, train_time = results$train_time, predict_time = results$predict_time, accuracy = results$accuracy))
rownames(results_df) = results_df$observation_size
results_df$observation_size = NULL

#plot the results
# plot(seq(min_n,nrow(results)),results[min_n:nrow(results),1],
#      xlab = "Number of Observations", ylab = "Time for Fit and Predictions (in seconds)",
#      main = "Time Versus Dataset Size")


