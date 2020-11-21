#import the functions
source("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/Modeling_functions.R")


#load the data
data_v1 = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data/data_v1.csv")[,-c(1)]
data_v2 = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/data/data_v2.csv")[,-c(1)]

#run the n observation analysis first
#ACCURACY TRAIN (1000 OBVS) | TEST (1000 OBVS) | 20 OBVS + INCREMENT 5 EACH TIME
lgreg_df_accuracy = data.frame(n_observation_analysis(data_v1[0:2000,],1000,20,5, logistic_regression))
rf_df_accuracy = data.frame(n_observation_analysis(data_v1[0:2000,],1000,20,5, randomForestmd))
knn_df_accuracy = data.frame(n_observation_analysis(data_v1[0:2000,],1000,20,5, knn_md))
nbc_df_accuracy = data.frame(n_observation_analysis(data_v1[0:2000,],1000,20,5, nbc))

write.csv(lgreg_df_accuracy, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/lgreg_df_accuracy.csv", row.names = TRUE)
write.csv(rf_df_accuracy, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/rf_df_accuracy.csv", row.names = TRUE)
write.csv(knn_df_accuracy, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/knn_df_accuracy.csv", row.names = TRUE)
write.csv(nbc_df_accuracy, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/nbc_df_accuracy.csv", row.names = TRUE)


#SPEED TRAIN (1M OBVS) | TEST (100 000 OBVS) | 1000 OBVS + INCREMENT 1000 EACH TIME
# lgreg_df_speed = data.frame(n_observation_analysis(data_v1,990000,100000,100000, logistic_regression))
# rf_df_speed = data.frame(n_observation_analysis(data_v1,990000,10000,10000, randomForestmd))
# knn_df_speed = data.frame(n_observation_analysis(data_v1,990000,10000,10000, knn_md))
# nbc_df_speed = data.frame(n_observation_analysis(data_v1,990000,10000,10000, nbc))
# 
# write.csv(lgreg_df_speed, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/lgreg_df_speed.csv")
# write.csv(rf_df_speed, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/rf_df_speed.csv")
# write.csv(knn_df_speed, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/knn_df_speed.csv")
# write.csv(nbc_df_speed, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/nbc_df_speed.csv")


#CHANGING DATA ANALYSIS

lgreg_accuracy_list = data.frame(unlist(changing_data_analysis(data_v1, data_v2, 9000, 1000,logistic_regression)))
rf_accuracy_list = data.frame(unlist(changing_data_analysis(data_v1, data_v2, 9000, 1000,randomForestmd)))
knn_accuracy_list = data.frame(unlist(changing_data_analysis(data_v1, data_v2, 9000, 1000,knn_md)))
nbc_accuracy_list = data.frame(unlist(changing_data_analysis(data_v1, data_v2, 9000, 1000,nbc)))


write.csv(lgreg_accuracy_list, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/lgreg_accuracy_list.csv")
write.csv(rf_accuracy_list, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/rf_accuracy_list.csv")
write.csv(knn_accuracy_list, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/knn_accuracy_list.csv")
write.csv(nbc_accuracy_list, "/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/nbc_accuracy_list.csv")



