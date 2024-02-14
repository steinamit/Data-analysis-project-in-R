#Adding the relevant packages to this section
install.packages("forecast")
install.packages("ggplot2")

#1
# NUMERIC VECTOR
a<-as.numeric(123)
str(a)
a<-as.character(123)
str(a)
a<-as.factor(123)
str(a)

b<-list(1,2,3)
str(b)
b1<-list(1,"2",3)
str(b1)
c<-data.frame(b)
str(c)
str(b1)

#2
#Required to import the data and identify the object.
DATA <- read.csv("C:/ariel university/year b/production management 1/R task/DATA_324809300_NoaRubin.csv")
DATA_TS <- ts(DATA)
str(DATA_TS)

#3
#Graphs are required to be displayed.
plot(DATA, main = "Snapshot of DATA Patterns", xlab = "Data Point Index", ylab = "Measured Value")
plot(DATA_TS, main = "Time Series Graph of DATA_TS", xlab = "Timeline", ylab = "Data Value")

#4
#Defining the time series by seasons and provide a graphical presentation.
DATA_TS4 <- ts(DATA, end = 2022, frequency = 4)
DATA_TS1 <- ts(DATA, end = 2022, frequency = 1)
plot(DATA_TS4, main = "Quarterly Trends Overview", xlab = "Time (Quarter)", ylab = "Value")
plot(DATA_TS1, main = "Annual Performance Analysis", xlab = "Time (Year)", ylab = "Value")

#5
#Construct and analyze a regression model.
##The lecturer said to use the periodical series with annual resolution.
##There is a dependency on section 4
library(forecast)
dat.lm1 <- tslm(DATA_TS1 ~ trend)
summary(dat.lm1)
plot(DATA_TS1, main = "Regression Model", xlab = "Time (Year)", ylab = "Value")
lines(dat.lm1$fitted)
accuracy(dat.lm1)

#6
#Calculate a moving average and put it in a layered graph
library(forecast)
library(ggplot2)
movingAverage_3<-ma(DATA_TS1,order=3) 
movingAverage_6<-ma(DATA_TS1,order=6) 
autoplot(DATA_TS1, series = "Data") +
  autolayer(movingAverage_3, series = "3-months") +
  autolayer(movingAverage_6, series = "6-months") +
  ggtitle("Layer Graph - original series and moving average over 3 and 6 months")+
  xlab("Time")+ ylab("Value")

#7
#Predicting exponential smoothing and placing in a layered graph + RMSE
library(forecast)
fc_0.1 <- ses(DATA_TS1, h=1, alpha = 0.1)
fc_0.1$mean
fc_0.5 <- ses(DATA_TS1, h=1, alpha = 0.5)
fc_0.5$mean
fc_0.9 <- ses(DATA_TS1, h=1, alpha = 0.9)
fc_0.9$mean

fc_b <- ses(DATA_TS1)
fc_b$model

autoplot(DATA_TS1, series = "Data") +
  autolayer(movingAverage_3, series = "3-Months") +
  autolayer(movingAverage_6, series = "6-Months") +
  autolayer(fc_b, series = "Exponential Smoothing") +
  ggtitle("Layer Graph - original series, moving average and exponential smoothing")+
  xlab("Time")+ ylab("Value")

round(accuracy(fc_b), 3)

#8
#Forecast by HoltWinters
library(forecast)
DATA_TS12 <- ts(DATA, frequency = 12)
num_periods_to_remove <- 5
data_holt_winters <-  head(DATA_TS12, -num_periods_to_remove)

according_0.9<- HoltWinters(data_holt_winters, alpha=0.9, beta=0.9, gamma=0.9)
forecast_0.9 <- forecast(according_0.9, h=5)
print(forecast_0.9)

according_0.1<- HoltWinters(data_holt_winters, alpha=0.1, beta=0.1, gamma=0.1)
forecast_0.1 <- forecast(according_0.1, h=5)
print(forecast_0.1)

data_without <-HoltWinters(data_holt_winters)
data_forecast <- forecast(data_without, h=5)
print(data_forecast)

accuracy(data_forecast)

#9
#Decomposition analysis and noise treatment
library(forecast)
decomposedTimeSeries <- decompose(DATA_TS12) 
plot(decomposedTimeSeries)

print(max(DATA_TS12)-min(DATA_TS12))

noise_component <- decomposedTimeSeries$random
noise_range <- range(noise_component, na.rm = TRUE)
print(noise_range[2] - noise_range[1])

std_dev_original <- sd(DATA_TS12, na.rm = TRUE)
std_dev_noise <- sd(noise_component, na.rm = TRUE)
print(paste("STD of Original Series:", std_dev_original))
print(paste("STD of Noise:", std_dev_noise))
#The noise is smaller than the initial series because its variance is smaller relative to the variance of the original series.

noise<-decomposedTimeSeries$random 
noise1<-c(noise) 
qqnorm(noise1); qqline(noise1, col = 2) 
