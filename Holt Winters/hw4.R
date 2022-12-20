##### loading the library #####
library(smooth)
library(tidyverse)


##### loading the data #####
temps <- read.table("C:/Users/leajo/Documents/ISYE6501/HW4/hw4-SP22/data 7.2/temps.txt", header = TRUE)
head(temps)

### make the time series data 
temps_1 <- temps[,-1]
head(temps_1)

## make the data into time series
temps_vec <- unlist(temps_1)
temps_ts <- ts(temps_vec, start = 1996, frequency = 123)
## run the Holt Winter method 
model <- HoltWinters(temps_ts, alpha = NULL, beta = NULL, 
                     gamma = NULL, seasonal = "additive") 
plot(model)
plot(model, xlim=c(2000, 2001))

model

### run Holt Winter with the data from 1996 to 2005

temps_2 <- temps[,2:12]
head(temps_2)
temps_vec_2 <- unlist(temps_2)
temps_ts_base <- ts(temps_vec_2, start = 1996, frequency = 123)

model_2 <- HoltWinters(temps_ts_base, alpha = 0.6610618, beta = NULL, 
                     gamma = 0.6248076, seasonal = "additive") 

plot(model_2)

# prediction for the next 10 years 
temp_pred <- predict(model_2, 1230, frequency = 123)
plot(temp_pred)



## find the mean highest temperature of each day for 10 years

temp_pred_mean <- rep(0, 123)
temp_day <- rep(0,10)

for (i in 1:123){
  for (j in 1:10){
    k = i + 123 * (j - 1)
    temp_day[j] <- temp_pred[k,]
  }
  temp_pred_mean[i] <- mean(temp_day)
}

temp_pred_mean

### saving the prediction mean for 10 years
for_saving <- t(temp_pred_mean)
write.csv(for_saving, file = "temp_pred_avg.csv", row.names = TRUE)

### fitted data from 2006 - 2015
### data should start from 2006 August 

head(temps)
temps_3 <- temps[,12:21]
head(temps_3)
temps_vec_3 <- unlist(temps_3)
temps_ts_3 <- ts(temps_vec_3, start = 2006, frequency = 123)

model_3 <- HoltWinters(temps_ts_base, alpha = 0.6610618, beta = NULL, 
                       gamma = 0.6248076, seasonal = "additive") 

temps_fitted <- model_3$fitted
temps_fitted
nrow(temps_fitted)

temp_fitted_mean3 <- rep(0, 123)
temp_day_3 <- rep(0,10)

for (i in 1:123){
  for (j in 1:10){
    k = i + 123 * (j - 1)
    temp_day_3[j] <- temps_fitted[k,]
  }
  temp_fitted_mean3[i] <- mean(temp_day_3)
}

temp_fitted_mean3

for_saving3 <- t(temp_fitted_mean3)
write.csv(for_saving3, file = "temp_fitted_avg_new.csv", row.names = TRUE)


plot(model_3,temp_pred)

# uploaded the fitted data 

temps_fitted <- read.table("C:/Users/leajo/Documents/ISYE6501/HW4/save_fitted2.csv")
temps_fitted_ts <- ts(temps_fitted, start = 2006, frequency = 123)

### calculated the average of the fitted data 
temp_fitted_mean <- rep(0, 123)
temp_day_2 <- rep(0,10)

for (i in 1:123){
  for (j in 1:10){
    k = i + 123 * (j - 1)
    temp_day_2[j] <- temps_fitted[k,]
  }
  temp_fitted_mean[i] <- mean(temp_day_2)
}

temp_fitted_mean

for_saving2 <- t(temp_fitted_mean)
write.csv(for_saving2, file = "temp_fitted_avg.csv", row.names = TRUE)



####################### another prediction method #################

temps_ts_base_with_july <- 







