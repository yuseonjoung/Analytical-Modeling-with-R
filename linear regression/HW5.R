install.packages("AICcmodavg")
library(AICcmodavg)

install.packages('car')
library(car)

### load the data 
crime_data <- read.table("C:/Users/leajo/Documents/ISYE6501/HW5/hw5-SP22-1/data 8.2/uscrime.txt", header = TRUE)
head(crime_data)

colnames(crime_data)

#### create a simple regression model to see each variables
simple_fit <- lm(Crime~., data = crime_data)
summary(simple_fit)
simple_fit$coefficients

## result: M, Ed, Ineq, Prob

## first see the performance of each variable 

M_fit <- lm (Crime~M, data = crime_data) # percentage of males aged 14–24 in total state population
summary(M_fit) ## no good p-value

Ed_fit <- lm (Crime~Ed, data = crime_data) # 	mean years of schooling of the population aged 25 years or over
summary(Ed_fit) 

Ineq_fit <- lm (Crime~Ineq, data = crime_data) # income inequality: percentage of families earning below half the median income
summary(Ineq_fit) ## no good p-value

Prob_fit <- lm (Crime~Prob, data = crime_data) # probability of imprisonment: ratio of number of commitments to number of offenses
summary(Prob_fit) 


So_fit <- lm (Crime~So, data = crime_data) # indicator variable for a southern state
summary(So_fit) ## no good p-value

Po1_fit <- lm (Crime~Po1, data = crime_data) # per capita expenditure on police protection in 1960
summary(Po1_fit) ## good p-value

Po2_fit <- lm (Crime~Po2, data = crime_data) # 	per capita expenditure on police protection in 1959
summary(Po2_fit) ## good p-value

LF_fit <- lm (Crime~LF, data = crime_data) # 	labour force participation rate of civilian urban males in the age-group 14-24
summary(LF_fit) ## no good p-value

M.F_fit <- lm (Crime~M.F, data = crime_data) # number of males per 100 females
summary(M.F_fit) ## no good p-value

Pop_fit <- lm (Crime~Pop, data = crime_data) # state population in 1960 in hundred thousands
summary(Pop_fit) ## good p-value

NW_fit <- lm (Crime~NW, data = crime_data) # percentage of nonwhites in the population
summary(NW_fit) ## no good p-value

U1_fit <- lm (Crime~U1, data = crime_data) # unemployment rate of urban males 14–24
summary(U1_fit) ## no good p-value

U2_fit <- lm (Crime~U2, data = crime_data) # unemployment rate of urban males 35–39
summary(U2_fit) ## no good p-value

Wealth_fit <- lm (Crime~Wealth, data = crime_data) # wealth: median value of transferable assets or family income
summary(Wealth_fit) ## good p-value

Time_fit <- lm (Crime~Time, data = crime_data) # average time in months served by offenders in state prisons before their first release
summary(Time_fit) ## no good p-value

### final choice based on p-value: Ed, Prob, Po1, Po2, Pop, Wealth  

### combination of variables

com_fit_1 <- lm(Crime~ Ed+Prob+Po1+Po2+Pop+Wealth, data = crime_data)
summary(com_fit_1) ## Ed and Wealth could have multicollinearity

com_fit_2 <- lm(Crime~ Ed+Prob+Po1+Po2+Pop, data = crime_data)
summary(com_fit_2) ## Dropped Wealth

com_fit_3 <- lm(Crime~ Prob+Po1+Po2+Pop+Wealth, data = crime_data)
summary(com_fit_3)    ### Dropeed Ed to check the alternative 

com_fit_4 <- lm(Crime~ Prob+Po1+Pop+Wealth, data = crime_data)
summary(com_fit_4) ## Dropped Po2 

com_fit_5 <- lm(Crime~ Prob+Po1+Wealth, data = crime_data)
summary(com_fit_5) ## Dropped Pop: Po1 and Pop could have multicollinearity 



#### compare the com_fits and assess the fit 

models <- list(com_fit_1, com_fit_2, com_fit_3, com_fit_4, com_fit_5)
model_names <- c('com_fit_1', 'com_fit_2', 'com_fit_3', 'com_fit_4', 'com_fit_5')

aictab(cand.set = models, modnames = model_names)

#### show the result: com_fit_5 is the best
  
#### predict the data 
Prob+Po1+Wealth
Prob = 0.04
Po1 = 12.0
Wealth = 3200
new_data <- list(Prob = 0.04, Po1 = 12.0, Wealth = 3200)
prediction <- predict(com_fit_5, new_data )
prediction


plot(crime_data$Wealth, crime_data$Ed)
 
plot(crime_data$Pop, crime_data$Po1)

vif(com_fit_1)
vif(com_fit_5)
vif(com_fit_2)


com_fit_5$coefficients
plot(com_fit_5$residuals)
