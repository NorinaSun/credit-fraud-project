library(ggplot2)
library(scales)


lgreg_df_accuracy = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/lgreg_df_accuracy.csv")
rf_df_accuracy = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/rf_df_accuracy.csv")
knn_df_accuracy = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/knn_df_accuracy.csv")
nbc_df_accuracy = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/nbc_df_accuracy.csv")


#plot the results

#ACCURACY
plot(lgreg_df_accuracy$X,lgreg_df_accuracy$accuracy,
     xlab = "Number of Observations", ylab = "Accuracy",
     main = "Accuracy Versus Dataset Size for Logistic Regression")
abline(lm(lgreg_df_accuracy$accuracy ~ lgreg_df_accuracy$X, data = lgreg_df_accuracy), col = "red")

plot(rf_df_accuracy$X,rf_df_accuracy$accuracy,
     xlab = "Number of Observations", ylab = "Accuracy",
     main = "Accuracy Versus Dataset Size for RandomForest")
abline(lm(rf_df_accuracy$accuracy ~ rf_df_accuracy$X, data = rf_df_accuracy), col = "red")

plot(knn_df_accuracy$X,knn_df_accuracy$accuracy,
     xlab = "Number of Observations", ylab = "Accuracy",
     main = "Accuracy Versus Dataset Size for KNN")
abline(lm(knn_df_accuracy$accuracy~ knn_df_accuracy$X, data = knn_df_accuracy), col = "red")

plot(nbc_df_accuracy$X,nbc_df_accuracy$accuracy,
     xlab = "Number of Observations", ylab = "Accuracy",
     main = "Accuracy Versus Dataset Size for Naive Bayes")
abline(lm(nbc_df_accuracy$accuracy ~ nbc_df_accuracy$X, data = nbc_df_accuracy), col = "red")


#TRAIN TIME
plot(lgreg_df_accuracy$X,lgreg_df_accuracy$train_time,
     xlab = "Number of Observations", ylab = "Time for Train (in seconds)",
     main = "Training Time Versus Dataset Size for Logistic Regression")
abline(lm(lgreg_df_accuracy$train_time ~ lgreg_df_accuracy$X, data = lgreg_df_accuracy), col = "blue")

plot(rf_df_accuracy$X,rf_df_accuracy$train_time,
     xlab = "Number of Observations", ylab = "Time for Train (in seconds)",
     main = "Training Time Versus Dataset Size for RandomForest")
abline(lm(rf_df_accuracy$train_time ~ rf_df_accuracy$X, data = rf_df_accuracy), col = "blue")

plot(knn_df_accuracy$X,knn_df_accuracy$train_time,
     xlab = "Number of Observations", ylab = "Time for Train (in seconds)",
     main = "Training Time Versus Dataset Size for KNN")
abline(lm(knn_df_accuracy$train_time ~ knn_df_accuracy$X, data = knn_df_accuracy), col = "blue")

plot(nbc_df_accuracy$X,nbc_df_accuracy$train_time,
     xlab = "Number of Observations", ylab = "Time for Train (in seconds)",
     main = "Training Time Versus Dataset Size for Naive Bayes")
abline(lm(nbc_df_accuracy$train_time ~ nbc_df_accuracy$X, data = nbc_df_accuracy), col = "blue")

#PREDICT TIME
plot(lgreg_df_accuracy$X,lgreg_df_accuracy$predict_time,
     xlab = "Number of Observations", ylab = "Time for Predict (in seconds)",
     main = "Predict Time Versus Dataset Size for Logistic Regression")
abline(lm(lgreg_df_accuracy$predict_time ~ lgreg_df_accuracy$X, data = lgreg_df_accuracy), col = "green")

plot(rf_df_accuracy$X,rf_df_accuracy$predict_time,
     xlab = "Number of Observations", ylab = "Time for Predict (in seconds)",
     main = "Predict Time Versus Dataset Size for RandomForest")
abline(lm(rf_df_accuracy$predict_time ~ rf_df_accuracy$X, data = rf_df_accuracy), col = "green")

plot(knn_df_accuracy$X,knn_df_accuracy$predict_time,
     xlab = "Number of Observations", ylab = "Time for Predict (in seconds)",
     main = "Predict Time Versus Dataset Size for KNN")
abline(lm(knn_df_accuracy$predict_time ~ knn_df_accuracy$X, data = knn_df_accuracy), col = "green")

plot(nbc_df_accuracy$X,nbc_df_accuracy$predict_time,
     xlab = "Number of Observations", ylab = "Time for Predict (in seconds)",
     main = "Predict Time Versus Dataset Size for Naive Bayes")
abline(lm(nbc_df_accuracy$predict_time ~ nbc_df_accuracy$X, data = nbc_df_accuracy), col = "green")

#CHANGES TO DATA TRUTH
lgreg_accuracy_list = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/lgreg_accuracy_list.csv")[,-c(1)]
rf_accuracy_list = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/rf_accuracy_list.csv")[,-c(1)]
knn_accuracy_list = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/knn_accuracy_list.csv")[,-c(1)]
nbc_accuracy_list = read.csv("/Users/NorinaSun/Downloads/MATH60603/GroupProject/CreditFraudProject/results/nbc_accuracy_list.csv")[,-c(1)]

X = c("1/0","1/0.1","1/0.2","1/0.3","1/0.4","1/0.5","1/0.6","1/0.7","1/0.8","1/0.9","1/1","0.9/1","0.8/1","0.7/1","0.6/1","0.5/1","0.4/1","0.3/1","0.2/1","0.1/1","0/1")

lgreg_accuracy_df = data.frame(cbind(X,lgreg_accuracy_list))
rf_accuracy_df = data.frame(cbind(X,rf_accuracy_list))
knn_accuracy_df = data.frame(cbind(X,knn_accuracy_list))
nbc_accuracy_df = data.frame(cbind(X,nbc_accuracy_list))


ggplot(lgreg_accuracy_df, aes(factor(X,levels=X), as.numeric(lgreg_accuracy_list))) + 
  geom_point() +
  labs(title = "Proportion Old/New Data Used for Training VS Accuracy, Logistic Regression",y = "Accuracy", x = "Proportion Old Data/New Data")

ggplot(rf_accuracy_df, aes(factor(X,levels=X), as.numeric(rf_accuracy_list))) + 
  geom_point() +
  labs(title = "Proportion Old/New Data Used for Training VS Accuracy, Random Forest",y = "Accuracy", x = "Proportion Old Data/New Data")

ggplot(knn_accuracy_df, aes(factor(X,levels=X), as.numeric(knn_accuracy_list))) + 
  geom_point() +
  labs(title = "Proportion Old/New Data Used for Training VS Accuracy, KNN",y = "Accuracy", x = "Proportion Old Data/New Data")

ggplot(nbc_accuracy_df, aes(factor(X,levels=X), as.numeric(nbc_accuracy_list))) + 
  geom_point() +
  labs(title = "Proportion Old/New Data Used for Training VS Accuracy, Naive Bayes",y = "Accuracy", x = "Proportion Old Data/New Data")

