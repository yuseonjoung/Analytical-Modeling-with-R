install.packages("rmarkdown")
install.packages("DAAG")
install.packages("GGally")

library(rmarkdown)
library(tidyr)
library(ggplot2)
library(DAAG)
library(GGally)

crime_data <- read.table("C:/Users/leajo/Documents/ISYE6501/HW6/hw6-SP22/data 9.1/uscrime.txt", header = TRUE)
head(crime_data)


#### plot the data to check any correlation

for (i in 1:15){
  for (j in 1:15){
    if (i<j){
      plot(crime_data[,i],crime_data[,j], main="Scatterplot Example",xlab=colnames(crime_data)[i],ylab=colnames(crime_data)[j], pch=19)
    }
  }
}


ggpairs(crime_data, columns = c("Po1", "Po2", "U1", "U2", "Ineq", "Crime"),
        mapping=ggplot2::aes(color= "#3366FF"))


corr <- cor(crime_data)
round(corr, 2)

#---------------------------------- Running PCA --------------------------------

### run PCA using prcomp, with scaling the data
pca_res <- prcomp(crime_data[,-16], scale=TRUE)
names(pca_res)
summary(pca_res)

# see the principal component loading 
pca_res$rotation

dim(pca_res$x)
pca_res$x

### Visualize the PCA 


### compute the variance 
var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)
plot(cumsum(var_explained),
     xlab = "Principal Component", 
     ylab = "Proportion of Variance", 
     ylim = c(0,1), type = "b")
abline(h=0.8, col="red")


screeplot(pca_res, main = "Scree Plot", type = "line")
abline(h=1, col="red")


#---------------------------------- Get the first 4 PCs --------------------------------

#### method 1: direct from prcomp outcome using $x

PCs <- pca_res$x[,1:4]

attributes(pca_res$x)
pca_res$x
PCs

#### method 2: calculated from prcomp output: scaled data * rotation 

crime_data_scale <- as.data.frame(scale(crime_data[,1:15]))
data_mat <- as.matrix(crime_data_scale)

PCs2 <- data_mat %*% pca_res$rotation[,1:4]

pca_res$rotation[,1:4]

PCs[1,]
PCs2[1,]


#### using math 

E <- eigen(t(data_mat) %*% data_mat)
PCs3 <- data_mat %*% E$vectors[,1:4]

PCs3[1,]

#--------------------------- Regression on the first 4 PCs----------------------

# Build linear regression model with the first 4 PCs

PCCrime <- cbind(PCs, crime_data[,16])

PCCrime

as.data.frame(PCCrime)

model <- lm(V5~., data = as.data.frame(PCCrime)) ## should create regression model on a new data matrix
summary(model)

#################################################################################
### Finding the original coefficients

beta0 <- model$coefficients[1]
betas <- model$coefficients[2:5]

### transform the PC coefficients into coefficients for the original variables
alpha <- pca_res$rotation[,1:4] %*% betas
t(alpha)


### ## BUT... these coefficients above are using scaled data.
# Now, we have to convert back to the original data.
#
# When scaling, this function subtracts the mean and divides by the standard deviation, for each variable.
#
# So, alpha * (x - mean)/sd = originalAlpha * x.
# That means:
# (1) originalAlpha = alpha/sd
# (2) we have to modify the constant term a0 by alpha*mean/sd

mu <- sapply(crime_data[,1:15], mean)
sigma <- sapply(crime_data[,1:15], sd)


original_alpha <- alpha/sigma
t(original_alpha)
original_beta <- beta0 - sum(alpha*mu / sigma)

### Here are the estimates from this model: 

estimate <- as.matrix(crime_data[,1:15]) %*% original_alpha + original_beta
estimate

### calculate R^2 and R^2 adj

SSE <- sum((estimate - crime_data[,16])^2)
SStot <- sum((crime_data[,16] - mean(crime_data[,16]))^2)
1 - SSE/SStot

R2 <- 1 - SSE/SStot
R2 ## same as the PC regression
R2 - (1 - R2)*4/(nrow(crime_data)-4-1) # same as the PC regression 

######################## cross validation ############################

library(DAAG)

# do 5-fold cross-validation

r2cross <- numeric(15) # create a vector to store the R-squared values

for (i in 1:15) {
  pclist <- pca_res$x[,1:i]  # use the first i prinicipal components
  pcc <- cbind(crime_data[,16],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  c <- cv.lm(as.data.frame(pcc),model,m=5) # cross-validate 
  r2cross[i] <- 1 - attr(c,"ms")*nrow(crime_data)/sum((crime_data$Crime - mean(crime_data$Crime))^2) # calculate R-squared
}

r2cross


plot(r2cross, xlab = "Principal Component", yl= "Cross-validated R-squared with this many principal components",
     ylim = c(0,1), type = "b") ### result suggests adding 5th PC

pcc <- cbind(crime_data[,16],pca_res$x[,5])
model <- lm(V1~.,data = as.data.frame(pcc))
summary(model)

head(crime_data)
str(crime_data)


