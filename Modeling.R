source("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/Modeling_functions.R")

#load the data
data_v1_full = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v1.csv")
data_v2_full = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data_v2.csv")

# #taking a subset of the data
# data_v1 = data_v1_full[0:5000,-c(1)]
# data_v2 = data_v2_full[0:5000,-c(1)]
# test_data = data_v2_full[5000:6000,-c(1)]
# 
# 
# data_change_simulation(data_v1,data_v2,test_data,knn_md)







#taking a subset of the data
data_v1 = data_v1_full[0:5000,-c(1)]
data_v2 = data_v2_full[0:5000,-c(1)]

#choosing the params
min_n = 1000
max_n = 4000

#running the script
results = streaming(data_v1, min_n,max_n, knn_md)

results_df = do.call(rbind, Map(data.frame, observation_size = results$size, train_time = results$train_time, predict_time = results$predict_time, accuracy = results$accuracy))
rownames(results_df) = results_df$observation_size
results_df$observation_size = NULL

#plot the results
# plot(seq(min_n,nrow(results)),results[min_n:nrow(results),1],
#      xlab = "Number of Observations", ylab = "Time for Fit and Predictions (in seconds)",
#      main = "Time Versus Dataset Size")


