
library(kernlab)
library(dplyr)
library(ggplot2)
library(kknn)
library(caret)  
library(NbClust)

credit_card_data <- read.table("c:/Users/leajo/Documents/ISYE6501/HW1/hw1-SP22/data 2.2/credit_card_data-headers.txt", header=TRUE)
credit_card_mtx <- as.matrix(credit_card_data)  
credit_card_df <- as.data.frame(credit_card_data)
credit_card_df$A11 = as.factor(credit_card_df$A11)

############## Question 3.1 ###############

###### 3.1.a find a good classifier / cross validation / splitting the data (80:20)

#### splitting the dataset to create training set and test set
set.seed(1234)

n <- nrow(credit_card_df)
train_index <- sample.int(n, size = round(4/5*n))

train_set <- credit_card_df[train_index,]
test_set <- credit_card_df[-train_index,]

#### Find a good k using train.kknn (leave-one-out cross-validation (LOOCV))
knn_cv_1 <- train.kknn(as.factor(R1)~., data= train_set, kmax = 50, 
                       scale = TRUE)
knn_cv_1
knn_cv_1$response

#### Run the model on test set with k = 7, kernel = optimal
pred <- rep(0, nrow(test_set))

for (i in 1:nrow(test_set)){
  k_model <- kknn(R1~., test_set[-i,], test_set[i,], 
                  k = 7, kernel = "optimal", scale = TRUE)
  pred[i] <- round(fitted(k_model),0)  
}

accuracy <- mean(pred == test_set$R1)
accuracy

#### Find a good k using cv.kknn (k-fold cross validation)
acc_cv <- rep(0,50)
pred_cv <-rep(0, nrow(train_set))

for (K in 1:50) {
  cv_kknn_model_2 <- cv.kknn(R1~., data = train_set, 
                           k= K, kcv = 10, kernel = "optimal", scale = TRUE)
  cv_kknn_fitted <- cv_kknn_model_2[[1]][,2]
  pred_cv <- round(cv_kknn_fitted, 0)
  acc_cv[K] <- mean(pred_cv == train_set$R1)
}

acc_cv 
max(acc_cv) 
which.max(acc_cv) 


#### Run the model on test set with k = 7
pred_cv_2 <- rep(0, nrow(test_set))

for (i in 1:nrow(test_set)){
  k_model_test <- kknn(R1~., test_set[-i,], test_set[i,], 
                  k = 7, scale = TRUE)
  pred_cv_2[i] <- round(fitted(k_model_test),0)  
}

accuracy_2 <- mean(pred_cv_2 == test_set$R1)
accuracy_2  



############ caret 이용 - 미제출 
train.control <- trainControl(method = "cv", number = 10)  # caret 

k_model <- train(x= credit_card_df[,1:10], y= as.factor(credit_card_df[,11]) , method = "knn", trControl = train.control, preProcess = c("center", "scale"))
k_model 

########## (b)는: split the data and attempt at least one round of training, validation, and testing. 
####If that means you only validate and test one model, 
####it will be a bit of an artificial example, but that's OK. If you want to "go for the brass ring", train several models, then compare their validation performance and pick one to test.



#### Split the data set into training set, validation set and test_set 
credit_card_mtx <- as.matrix(credit_card_data) 

set.seed(12345)

n <- nrow(credit_card_mtx)
train_index_2 <- sample.int(n, size = round(7/10*n))

train_set_2 <- credit_card_mtx[train_index,]
the_rest <- credit_card_mtx[-train_index,]

n_rest <- nrow(the_rest)
test_index_2 <- sample.int(n_rest, size = round(1/2*n_rest))

test_set_2 <- the_rest[test_index_2,]
validation_set_2 <- the_rest[-test_index_2,]

#### Find the best C-value using training set
cost <- c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 5, 10, 100)  #E+02" means "10 the power of 2"
accuracy_svm <- vector()
pred_svm <- rep(0, nrow(validation_set_2))

for (c in cost){
  
  svm_model <- ksvm(train_set_2[,1:10],as.factor(train_set_2[,11]),
                    type= "C-svc", kernel="vanilladot", C = c, scaled=TRUE)
  
  pred_svm <- predict(svm_model, validation_set_2[,1:10])
  accuracy <- mean(pred_svm == validation_set_2[,11])
  accuracy_svm <- c(accuracy_svm , accuracy)
}

max_acc <- max(accuracy_svm)
which(accuracy_svm == max_acc)  # c = 0.01

#### Find the equation of the chosen model

svm_model_0.01 <- ksvm(train_set_2[,1:10], as.factor(train_set_2[,11]),
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


#### Run the model on the test set
pred_svm_test <- rep(0, nrow(test_set_2))

pred_svm_test <- predict(svm_model, test_set_2[,1:10])
accuracy_test <- mean(pred_svm_test == test_set_2[,11])
accuracy_test


############# Question 4.2 ###############
iris
head(iris)
iris_data <- data.frame(iris[-5])


#### no need to split the data, explain this in the assignments

#### scale the training dataset: explain why we need to standardize our data 
iris_scale <- scale(iris_data)
summary(iris_scale)


#### Plot the within groups sum of squares to get the idea of k 



###################### wssplot: within groups sum of squares from R blog 

#### plot the possible k values and its within groups sum of squares

wss <- (nrow(iris_scale)-1)*sum(apply(iris_scale, 2, var))
for (i in 1:7){
  set.seed(321321)
  wss[i] <- sum(kmeans(iris_scale, centers = i, nstart = 30)$withinss)
}
  
plot(1:7, wss, type = "b", xlab = "no. of clusters", ylab = "tot.withinss")


################### ignore - did not work well ######################
no_cluster <- NbClust(iris_scale, min.nc = 2, max.nc = 7, method = "kmeans")
table(no_cluster$Best.n[1,])
barplot(table(no_cluster$Best.n[1,]),
      xlab="Numer of Clusters", ylab="Number of Criteria",
      main="Number of Clusters Chosen by 26 Criteria")

###################################################################

#### k-means() with k = 3 and then show the tot.withinss
set.seed(321)

iris_model_3 <- kmeans(iris_scale, centers = 3, iter.max = 10000, nstart = 30) 
iris_model_3 $tot.withinss
iris_model_3 $withinss
iris_model_3


iris_scale$cluster <- as.factor(iris_model$cluster)
table(iris_model$cluster, iris[,5])


summary(iris_model$centers)
iris_model$center #location of the centroid of the four cluster
iris_model$cluster # where each data point belongs in the cluster


iris_model_3$cluster <- as.factor(iris_model_3$cluster)
table(iris_model_3$cluster, iris[,5])

#### Do it once again, as k-means algorithm uses a random starting point
set.seed(1357)
iris_model_3_2 <- kmeans(iris_scale, centers = 3, iter.max = 10000, nstart = 30)

iris_model_3_2$tot.withinss
iris_model_3_2$withinss

iris_model_3_2 


# withinss = a vector showing the sum of squared distance for each point from its cluster centroid
# tot.withinss = the sum of the withinss vector
# betweenss = the sum of the squared distance between each cluster centroid (? this is where I’m mostly confused)
# totss = tot.withinss + betweenss

#### find the best combination of the predictors

tot_withinss <- function(x){
  model_kmean <- kmeans(x, centers = 3, iter.max = 10000, nstart = 30)
  return(model_kmean$tot.withinss) 
  }

#### total of the withinss of each cluster points in 11 combinations
c1 <- c(1,2)
tot_withinss(iris_scale[,c1])
c2 <- c(1,3)
tot_withinss(iris_scale[,c2])
c3 <- c(1,4)
tot_withinss(iris_scale[,c3])
c4 <- c(2,3)
tot_withinss(iris_scale[,c4])
c5 <- c(2,4)
tot_withinss(iris_scale[,c5])
c6 <- c(3,4)
tot_withinss(iris_scale[,c6])
c7 <- c(1,2,3)
tot_withinss(iris_scale[,c7])
c8 <- c(1,3,4)
tot_withinss(iris_scale[,c8])
c9 <- c(1,2,4)
tot_withinss(iris_scale[,c9])
c10 <- c(2,3,4)
tot_withinss(iris_scale[,c10])


head(iris_scale[,c2])
head(iris_scale)
head(iris_scale[,c10])

#### see the best k means result 

model_kmean_best <- kmeans(iris_scale[,c6], centers = 3, iter.max = 10000, nstart = 30)

kmean_cluster <- model_kmean_best$cluster

iris_scale_result <- cbind(iris_scale, kmean_cluster) 
iris_scale_result <- data.frame(iris_scale_result)

head(iris_scale_result)

iris_scale_result$kmean_cluster <- as.factor(iris_scale_result$kmean_cluster)
ggplot(iris_scale_result, aes(x = Petal.Length, y = Petal.Width, color = kmean_cluster))+
  geom_point()

model_kmean_best$cluster <- as.factor(model_kmean_best$cluster)
table(model_kmean_best$cluster, iris[,5])

##### original two 

iris_df <- data.frame(iris)


iris_df$Species <- as.factor(iris_df$Species)
ggplot(iris_df, aes(x = Petal.Length, y = Petal.Width, color = Species))+
  geom_point()

