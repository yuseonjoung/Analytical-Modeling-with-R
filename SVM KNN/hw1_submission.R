## downloading the library
install.packages("kernlab")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("kknn")
install.packages("caret")

library(kernlab)
library(dplyr)
library(ggplot2)
library(kknn)
library(caret)   

## load the data
getwd()

credit_card_data <- read.table("c:/Users/leajo/Documents/ISYE6501/HW1/hw1-SP22/data 2.2/credit_card_data-headers.txt", header=TRUE)
credit_card_mtx <- as.matrix(credit_card_data)  
credit_card_df <- as.data.frame(credit_card_data)

########## Question 2.1: Find a good classifier for SVM ##########

## exploring the data 

head(credit_card_mtx, 20)

str(credit_card_mtx)
summary(credit_card_mtx)

# continuous variable: A2, A3, A8, A11, A14, A15
# binary variable: A1, A9, A10, A12

ggplot(credit_card_df) +
  geom_point(aes(x = A8, y = A11 , 
                 color= R1))

ggplot(credit_card_df) +
  geom_point(aes(x = A14, y = A15, 
                 color= R1))

### find a good classifier for svm model 

# set the candidates for cost (lambda)
cost <- c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 5, 10, 100)  #E+02" means "10 the power of 2"
accuracy_svm <- vector()
pred <- rep(0, nrow(credit_card_mtx))

# iterate the cost for the best C 
for (c in cost){
  svm_model <- ksvm(credit_card_mtx[,1:10],as.factor(credit_card_mtx[,11]),
       type= "C-svc", kernel="vanilladot", C = c, scaled=TRUE)
  pred <- predict(svm_model, credit_card_mtx[,1:10])
  accuracy <- mean(pred == credit_card_mtx[,11])
  accuracy_svm <- c(accuracy_svm , accuracy)
}

accuracy_svm_df <- data.frame(cost, accuracy_svm)
accuracy_svm_df 

# plot the result 
ggplot(accuracy_svm_df) +
  geom_point(aes(x = as.numeric(row.names(accuracy_svm_df)), y = accuracy_svm)) +
  xlab("row index") +
  ylab("accuracy") 

max(accuracy_svm_df$accuracy_svm)
accuracy_svm_df[which.max(accuracy_svm_df$accuracy_svm),] # cost 0.01

### Get the coefficients for the quation with C = 0.01

# build a model: kernel = vanilladot
svm_model_0.01 <- ksvm(credit_card_mtx[,1:10], as.factor(credit_card_mtx[,11]),
                       type= "C-svc",kernel="vanilladot", C=0.01, scaled=TRUE)
svm_model_0.01

# calculate coefficients
coefficient_a <- colSums(svm_model_0.01@xmatrix[[1]] * svm_model_0.01@coef[[1]])
coefficient_a
# calculate a0
a0 <- -svm_model_0.01@b
a0

coefficient_a 

# equation of svm_model_0.01
eq <- paste(coefficient_a[1], "* x1 +", coefficient_a[2], "* x2 +", coefficient_a[3], "* x3 +", coefficient_a[4], "* x4 +", coefficient_a[5], "* x5 +", coefficient_a[6], "* x6 +", coefficient_a[7], "* x7 +", coefficient_a[8], "* x8 +", coefficient_a[9], "* x9 +", coefficient_a[10], "* x10 +", a0, "= 0")
eq

### how well the svm_model_0.01 classifies the data
pred_0.01 <- predict(svm_model_0.01, credit_card_mtx[,1:10])
f_pred_0.01 <- factor(pred_0.01)
f_R1_result <- factor(credit_card_mtx[,11])
CM_svm <- confusionMatrix(f_pred_0.01, f_R1_result)
CM_svm

########## Question 2.2: Other non-linear kernels ##########

## non-linear example: rbfdot 

# using c = 0.01
svm_model2 <- ksvm(credit_card_mtx[,1:10],credit_card_mtx[,11],type= "C-svc",kernel="rbfdot",C = 0.01,scaled=TRUE)
svm_model2

svm_model2_evaluation = sum(model_prediction2 == credit_card_mtx[,11]) / nrow(credit_card_mtx)
svm_model2_evaluation

# finding the optimal c for rbfdot 

cost <- c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 5, 10, 100)  #E+02" means "10 the power of 2"
accuracy_svm_2 <- vector()

for (c in cost){
  svm_model <- ksvm(credit_card_mtx[,1:10],as.factor(credit_card_mtx[,11]),
                    type= "C-svc", kernel="rbfdot", C = c, scaled=TRUE)
  pred <- predict(svm_model, credit_card_mtx[,1:10])
  accuracy <- mean(pred == credit_card_mtx[,11])
  accuracy_svm_2 <- c(accuracy_svm_2, accuracy)
}

accuracy_svm_df_2 <- data.frame(cost, accuracy_svm_2)
accuracy_svm_df_2 

max(accuracy_svm_df_2$accuracy_svm_2)
accuracy_svm_df_2[which.max(accuracy_svm_df_2$accuracy_svm_2),] # C = 100, Accuracy = 94.8%


########## Question 2.3: K-nearest-neighbors classification ##########

credit_card_df <- as.data.frame(credit_card_data)
head(credit_card_df)
class(credit_card_df$R1)

## create the functions for knn classifications

# kknn function for calculating accuracy at the given k-value
knn_accuracy <- function(k_value){
  pred <- rep(0, nrow(credit_card_df))
  for (i in 1:nrow(credit_card_df)){
    k_model <- kknn(R1~., credit_card_df[-i,], credit_card_df[i,], 
                    k = k_value, scale = TRUE)
    pred[i] <- round(fitted(k_model), 0)
  }
  accuracy <- mean(pred == credit_card_df$R1)
  return(accuracy)
}



# Kknn functions for returning confusion matrix at the given k-value 
knn_cm <- function(k_value){
  pred <- rep(0, nrow(credit_card_df))
  for (i in 1:nrow(credit_card_df)){
    k_model <- kknn(R1~., credit_card_df[-i,], credit_card_df[i,], 
                    k = k_value, scale = TRUE)
    pred[i] <- round(fitted(k_model), 0)
  }
  f_pred <- factor(pred)
  f_R1 <- factor(credit_card_df$R1)
  CM <- confusionMatrix(f_pred,f_R1)
  return(CM)
}


## finding the best k-value from the range 1 to 50 

accuracy_k <- rep(0, 50)

for (K in 1:50){
  accuracy <- knn_accuracy(K)
  accuracy_k[K] <- accuracy
}

# plot the result 
plot(accuracy_k, xlab="accuracy rate", ylab="k value")

best_accuracy <- max(accuracy_k)
best_accuracy 
which(accuracy_k == best_accuracy) # k-value at 12 and 15 best accuracy

# confusion matrix at k = 12, accuracy of 85.3%
knn_cm(12)

## compare the result with k = 5, k = 7, k = 25

# k = 5
knn_accuracy(5)
knn_cm(5)

# k = 7
knn_accuracy(7)
knn_cm(7)

# k = 35
knn_accuracy(35)
knn_cm(35)

