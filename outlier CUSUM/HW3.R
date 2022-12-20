install.packages("outliers")

#################### Question 5.1 #########################

uscrime <- read.table("C:/Users/leajo/Documents/ISYE6501/HW3/hw3-SP22/data 5.1/uscrime.txt", header = TRUE)
head(uscrime)
uscrime <- as.data.frame(uscrime)

head(uscrime)
tail(uscrime[order(uscrime$Crime),])

library("outliers")

summary(uscrime$Crime)

nrow(uscrime)

###  check if there is an outlier 
boxplot(uscrime$Crime, ylab = "no. of crime per 100,000")

### checking normality with histogram 
hist(uscrime$Crime, prob = TRUE, breaks = 10, xlab = "no. of crime per 100,000", main = "histogram of all datapoints")
lines(density(uscrime$Crime), col = 4, lwd = 2)# skewd to the right


shapiro.test(uscrime$Crime) ## rejects H0 that it is normally distributed


grubbs.test(uscrime$Crime, type = 10) # test if max value is outlier, 0.07, if we set alpha to 0..1, we could reject H0
#1993 is an outlier

grubbs.test(uscrime$Crime, type = 11) # test if both min and max value are outlier, at least not both of them are outliers


### get rid of the higest value 
uscrime_2 <- uscrime[-which.max(uscrime$Crime),]
shapiro.test(uscrime$Crime)
grubbs.test(uscrime_2$Crime, type = 10)

### again get read of the highest value and try the Grubbs' test 
uscrime_3 <- uscrime_2[-which.max(uscrime_2$Crime),]
shapiro.test(uscrime_3$Crime)
grubbs.test(uscrime_3$Crime, type = 10) # test if max value is outlier


hist(uscrime_3$Crime, prob = TRUE, breaks = 9, xlab = "no. of crime per 100,000", main = "histogram without outliers")
lines(density(uscrime_3$Crime), col = 4, lwd = 2)
boxplot(uscrime_3$Crime, ylab = "no. of crime per 100,000", main = "boxplot without outliers")

uscrime_4 <- uscrime_3[-which.max(uscrime_3$Crime),]
shapiro.test(uscrime_4$Crime)
grubbs.test(uscrime_4$Crime, type = 10)  




uscrime_5 <- uscrime_4[-which.max(uscrime_4$Crime),]
shapiro.test(uscrime_5$Crime)
grubbs.test(uscrime_4$Crime, type = 10)



### our conclusion is, 1993 and 1969 are the outliers
shapiro.test(uscrime_3$Crime)
hist(uscrime_3$Crime, prob = TRUE, breaks = 9)
lines(density(uscrime_3$Crime), col = 4, lwd = 2)

boxplot(uscrime_3$Crime, ylab = "no. of crime per 100,000") ## compared to the first boxplot, the top two numbers are close enough\



#################### Question 6.2 #########################

temps_at  <- read.table("C:/Users/leajo/Documents/ISYE6501/HW3/hw3-SP22/data 6.2/temps.txt", header = TRUE)

temps_at <- as.data.frame(temps_at)

head(temps_at)
tail(temps_at)

############### 6.2.1. unofficial end of summer ###############

### 확인 

temps <- matrix(temps_at[,2:ncol(temps_at)])
temps_vec <- as.vector(unlist((temps)))

myts <- ts(temps_vec, start=1996, frequency = 1)
plot(myts)

## base average with no change 
avg_summer <- rep(0, nrow(temps))
sd_summer <- rep(0, nrow(temps))

for (i in 1:nrow(temps)){
  avg_summer[i] <- mean(temps[[i]][1:30])
  sd_summer[i] <- sd(temps[[i]][1:30])
}

