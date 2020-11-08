library(class)
library(data.table)

#generate some artificial data
sample = data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))

#define function that receives observation and predicts using a fake model
predict = function(data){
  #grab the observations
  obs_predict = data[nrow(data),]
  obs_train = data[1:nrow(data)-1,]
  
  # Start the clock!
  ptm <- proc.time()
  
  #fit and predict
  model <- knn(obs_train[,-10],obs_predict[,-10],cl=obs_train$X10,k=10)
  
  # Stop the clock
  runtime = proc.time() - ptm
  elapsed_time = runtime[3]

  return(elapsed_time)
}

#define function that sends the obvs
stream = function(data,min_n){
  summed_time = 0
  time_list = numeric(0)
  for(i in min_n:nrow(data)){
    #getting prediction time
    elapsed_time = predict(data[0:i,])
    summed_time = summed_time + elapsed_time
    #adding the time to a list
    average_time = summed_time/nrow(data)
    time_list[i] = average_time
  }
  return(time_list)
}

#specify min number of observations to start with. Make sure its > k
min_n = 50

#get the results
results = data.frame(stream(sample,min_n))

#plot the results
plot(seq(min_n,nrow(results)),results[min_n:nrow(results),1],
     xlab = "Number of Observations", ylab = "Time for Fit and Predictions (in seconds)",
     main = "Time Versus Dataset Size")
