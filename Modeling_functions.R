library(class)
library(data.table)
#knitr::opts_chunk$set(echo = TRUE)
library(tidyverse,verbose=FALSE)
library(Rlab)
library(caret)
library(adabag)
library(randomForest,verbose=FALSE)
library(purrr)
library(dplyr)
library(tidyverse)
library(klaR)

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
  myvars <- c("TransactionAmount","TransactionTime","Overdraft","Country",
              "ShoppedBefore","HowMuch","ShoppingFreq","ShopProximite","UsualProximite","PrevTranProx",
              "OnlineTransaction","BillPayment","PreAuthorized")

  data_subset = test_data[myvars]
  
  # Start the clock!
  test_start <- proc.time()
  
  #generate the prediction
  prediction_probabilities = predict(trained_model,newdata = data_subset, type="response")
  
  # Stop the clock
  test_time = proc.time() - test_start
  elapsed_test_time = test_time[3]
  
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
  elapsed_test_time = test_time[3]
  
  return(list("predictions" = prediction, "train_time" = elapsed_train_time, "test_time" = elapsed_test_time))
  
}

knn_md = function(train_data, test_data){
  
  #TRAINING
  
  #separate the data
  Y_train = train_data$FraudResults 
  train_data$FraudResults = NULL
  test_data$FraudResults = NULL
  
  # Start the clock!
  start <- proc.time()

  #train the model and generate predictions
  prediction <- knn(train=train_data,test = test_data, cl=Y_train, k=10)
  
  # Stop the clock
  time = proc.time() - start
  elapsed_time = time[3]
  
  
  return(list("predictions" = prediction, "train_time" = elapsed_time, "test_time" = elapsed_time))
  
}

nbc = function(train_data, test_data){
  
  #TRAINING
  train_data$Country <- NULL
  
  # Start the clock!
  train_start <- proc.time()
  
  #train the model
  trained_model = train(as.factor(FraudResults)~.,'nb', data = train_data)
  
  # Stop the clock
  train_time = proc.time() - train_start
  elapsed_train_time = train_time[3]
  
  #TESTING
  test_data$Country <- NULL
  # Start the clock!
  test_start <- proc.time()
  
  #generate the prediction
  prediction = predict(trained_model,newdata = test_data)
  
  # Stop the clock
  test_time = proc.time() - test_start
  elapsed_test_time = test_time[3]
  
  return(list("predictions" = prediction, "train_time" = elapsed_train_time, "test_time" = elapsed_test_time))
  
}

#streaming simulation
streaming = function(data,min_n,max_n,increment,FUN){
  
  #define test and train
  train_data = data[0:max_n,]
  test_data = data[max_n:nrow(data),]
  
  #defining empty lists to be filled
  train_list_time = list()
  predict_list_time = list()
  accuracy_list = list()
  dataset_size = list()
  
  for(i in seq(min_n,max_n,increment)){
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

#changing data simulation 
data_change_simulation = function(old_data,new_data,test_data,FUN){
  
  #creating the fractions vectors for each dataset
  old_data_frac = append(rep(1,10),seq(1,0,-0.1))
  new_data_frac = append(seq(0,1,0.1),rep(1,10))
  
  #init the accuracy list to store the values
  accuracy_list = list()
  
  for (i in seq(1,length(old_data_frac))){
    print(i)
    #creating the combined dataset
    old_df = sample_frac(old_data, size = old_data_frac[i])
    new_df = sample_frac(new_data, size = new_data_frac[i])
    
    train_data = rbind(old_df,new_df)
    
    #getting the results
    returned = FUN(train_data,test_data)
    
    #adding accuracy to list
    results = confusionMatrix(as.factor(returned$predictions), as.factor(test_data$FraudResults))
    accuracy_list[i] = as.numeric(results$overall['Accuracy'])
    
  }
  
  return(accuracy_list)
  
}

#wrappers for loop testing
changing_data_analysis = function(data_v1_full, data_v2_full, n_train, n_test,FUN){
  
  #balancing the second dataset
  new_data = rbind(data_v2_full[sample(which(data_v2_full$FraudResults == 0),500000),],
                   data_v2_full[sample(which(data_v2_full$FraudResults == 1),500000),])
  rows = sample(nrow(new_data))
  new_data = new_data[rows,]
  rownames(new_data) = NULL
  
  #taking a subset of the data just for testing purposes
  old_data_subset = data_v1_full[0:n_train,]
  new_data_subset = new_data[0:n_train,]
  n_end = n_train + n_test
  test_data_subset = new_data[n_train:n_end,]
  rownames(test_data_subset) = NULL
  
  #getting accuracy list
  accuracy_list = data_change_simulation(old_data_subset,new_data_subset,test_data_subset,FUN)
  
  return(accuracy_list)
}

n_observation_analysis = function(data,max_n, min_n,increment, FUN){
  
  #running the script
  results = streaming(data, min_n, max_n,increment, FUN)
  
  results_df = do.call(rbind, Map(data.frame, observation_size = results$size, train_time = results$train_time, predict_time = results$predict_time, accuracy = results$accuracy))
  rownames(results_df) = results_df$observation_size
  results_df$observation_size = NULL
  
  return(results_df)
  
}

