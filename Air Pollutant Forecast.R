# Name: AASHIK BHARATH EGNYA VARAHAN
# Student No.: R00182866

######################################## TIME SERIES PROJECT ########################################

################################## OZONE LEVEL DETECTION FORECAST ###################################

## Library Imports
options(warn=-1)
library(forecast)
library(tseries)
library(dplyr)
library(summarytools)
library(tibbletime)
library(dplyr)

## Sets the Working Directory
setwd("D://Notes - Canvas//Semester-2//Time Series & Factor Analysis//Project - Final//")

## Reads the file from Directory
Ozone <- read.csv("Assignment Data.csv", header = FALSE, stringsAsFactors = FALSE)

## Removes the unncessary variables from the dataset
cols <- c(26, 52:55, 58, 59, 69:72)
Ozone <- Ozone[,cols]

## Now the datset contains only the key variables
## Assigning Column names 
colnames(Ozone) <- c("WSR_PK","T_PK","T_AV","T85","RH85","HT85","T70","KI","TT","SLP","SLP_")

## Mutates the character datatype to numeric values
char.names = Ozone %>% select_if(is.character) %>% colnames()
Ozone[,char.names] = lapply(Ozone[,char.names], as.numeric)

## Descriptive Statistics
summary(Ozone)

## Converts each variable to Time Series Object
Ozone.WSR_PK <- ts(Ozone$WSR_PK, frequency = 365.25, start = c(1998, 1))
Ozone.T_PK <- ts(Ozone$T_PK, frequency = 365.25, start = c(1998, 1))
Ozone.T_AV <- ts(Ozone$T_AV, frequency = 365.25, start = c(1998, 1))
Ozone.T85 <- ts(Ozone$T85, frequency = 365.25, start = c(1998, 1))
Ozone.RH85 <- ts(Ozone$RH85, frequency = 365.25, start = c(1998, 1))
Ozone.HT85K <- ts(Ozone$HT85, frequency = 365.25, start = c(1998, 1))
Ozone.T70 <- ts(Ozone$T70, frequency = 365.25, start = c(1998, 1))
Ozone.KI <- ts(Ozone$KI, frequency = 365.25, start = c(1998, 1))
Ozone.TT <- ts(Ozone$TT, frequency = 365.25, start = c(1998, 1))
Ozone.SLP <- ts(Ozone$SLP, frequency = 365.25, start = c(1998, 1))
Ozone.SLP_ <- ts(Ozone$SLP_, frequency = 365.25, start = c(1998, 1))



# WSR_PK

## Before dealing with Outliers and NAs
plot.ts(Ozone.WSR_PK)

Ozone.WSR_PK <- tsclean(Ozone.WSR_PK, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.WSR_PK)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.WSR_PK_decomposed <- decompose(Ozone.WSR_PK, "additive")
plot(Ozone.WSR_PK_decomposed)

## Test for Stationarity
adf.test(Ozone.WSR_PK)

## Seasonality Adjusting
Ozone.WSR_PK_adjusted <- Ozone.WSR_PK - Ozone.WSR_PK_decomposed$seasonal
plot(Ozone.WSR_PK_adjusted)

## Exponential Smoothing Model
WSR_PK.ETS <- HoltWinters(Ozone.WSR_PK, gamma=FALSE)
WSR_PK.ETS


## ARIMA Model
WSR_PK.Arima <- auto.arima(Ozone.WSR_PK)
WSR_PK.Arima

## Observed vs Predicted
#par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(WSR_PK.ETS, col=c("green","red"))

# For ARIMA Model
plot(WSR_PK.Arima$x, col="red")
lines(fitted(WSR_PK.Arima), col="blue")

## Residuals
# For Exponential Smoothing
WSR_PK.ETS_forecast <- forecast(WSR_PK.ETS, h=60)
plot.ts(WSR_PK.ETS_forecast$residuals)

# For ARIMA Model
WSR_PK.Arima_forecast <- forecast(WSR_PK.Arima, h=10)
plot.ts(WSR_PK.Arima_forecast$residuals)

mean(WSR_PK.ETS_forecast$residuals)
mean(WSR_PK.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(WSR_PK.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(WSR_PK.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
pacf(WSR_PK.Arima_forecast$residuals, lag.max = 20)
acf(WSR_PK.Arima_forecast$residuals, lag.max = 20)
Box.test(WSR_PK.Arima_forecast$residuals, lag=20, type="Ljung-Box")


## Normality Test
shapiro.test(WSR_PK.Arima_forecast$residuals)

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.WSR_PK, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
WSR_PK.Arima_forecast <- forecast(WSR_PK.Arima, h=30)
plot(WSR_PK.Arima_forecast)

# T_PK

## Before dealing with Outliers and NAs
plot.ts(Ozone.T_PK)

Ozone.T_PK <- tsclean(Ozone.T_PK, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.T_PK)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.T_PK_decomposed <- decompose(Ozone.T_PK, "additive")
plot(Ozone.T_PK_decomposed)

## Test for Stationarity
adf.test(Ozone.T_PK)

## Seasonality Adjusting
Ozone.T_PK_adjusted <- Ozone.T_PK - Ozone.T_PK_decomposed$seasonal
plot(Ozone.T_PK_adjusted)

## Exponential Smoothing Model
T_PK.ETS <- HoltWinters(Ozone.T_PK, gamma=FALSE)
T_PK.ETS

## ARIMA Model
T_PK.Arima <- auto.arima(Ozone.T_PK)
T_PK.Arima

## Observed vs Predicted
par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(T_PK.ETS, col=c("green","red"))

# For ARIMA Model
plot(T_PK.Arima$x, col="red")
lines(fitted(T_PK.Arima), col="blue")

## Residuals
# For Exponential Smoothing
T_PK.ETS_forecast <- forecast(T_PK.ETS, h=60)
plot.ts(T_PK.ETS_forecast$residuals)

# For ARIMA Model
T_PK.Arima_forecast <- forecast(T_PK.Arima, h=10)
plot.ts(T_PK.Arima_forecast$residuals)

mean(T_PK.ETS_forecast$residuals)
mean(T_PK.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(T_PK.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(T_PK.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
pacf(T_PK.Arima_forecast$residuals, lag.max = 20)
acf(T_PK.Arima_forecast$residuals, lag.max = 20)
Box.test(T_PK.Arima_forecast$residuals, lag=20, type="Ljung-Box")

## Normality Test
shapiro.test(T_PK.Arima_forecast$residuals)

## Best Model
T_PK.Arima <- arima(Ozone.T_PK, order = c(4,1,4))
T_PK.Arima

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.T_PK, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
T_PK.Arima_forecast <- forecast(T_PK.Arima, h=10)
T_PK.Arima_forecast
plot(T_PK.Arima_forecast)


############# NEXT #############

# T_AV

par(mfrow=c(2,1))
## Before dealing with Outliers and NAs
plot.ts(Ozone.T_AV)

Ozone.T_AV <- tsclean(Ozone.T_AV, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.T_AV)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.T_AV_decomposed <- decompose(Ozone.T_AV, "additive")
plot(Ozone.T_AV_decomposed)

## Test for Stationarity
adf.test(Ozone.T_AV)

## Seasonality Adjusting
Ozone.T_AV_adjusted <- Ozone.T_AV - Ozone.T_AV_decomposed$seasonal
plot(Ozone.T_AV_adjusted)

## Exponential Smoothing Model
T_AV.ETS <- HoltWinters(Ozone.T_AV, gamma=FALSE)
T_AV.ETS

## ARIMA Model
T_AV.Arima <- auto.arima(Ozone.T_AV)
T_AV.Arima

## Observed vs Predicted
par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(T_AV.ETS, col=c("green","red"))

# For ARIMA Model
plot(T_AV.Arima$x, col="red")
lines(fitted(T_AV.Arima), col="blue")

## Residuals
# For Exponential Smoothing
T_AV.ETS_forecast <- forecast(T_AV.ETS, h=60)
plot.ts(T_AV.ETS_forecast$residuals)

# For ARIMA Model
T_AV.Arima_forecast <- forecast(T_AV.Arima, h=10)
plot.ts(T_AV.Arima_forecast$residuals)

mean(T_AV.ETS_forecast$residuals)
mean(T_AV.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(T_AV.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(T_AV.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
pacf(T_AV.Arima_forecast$residuals, lag.max = 20)
acf(T_AV.Arima_forecast$residuals, lag.max = 20)
Box.test(T_AV.Arima_forecast$residuals, lag=20, type="Ljung-Box")

## Normality Test
shapiro.test(T_AV.Arima_forecast$residuals)

## Best Model
T_AV.Arima <- arima(Ozone.T_AV, order = c(3,1,3))
T_AV.Arima

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.T_AV, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
T_AV.Arima_forecast <- forecast(T_AV.Arima, h=30)
T_AV.Arima_forecast
plot(T_AV.Arima_forecast)

############# NEXT #############

# T85

par(mfrow=c(2,1))
## Before dealing with Outliers and NAs
sum(is.na(Ozone.T85))
plot.ts(Ozone.T85)

Ozone.T85 <- tsclean(Ozone.T85, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.T85)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.T85_decomposed <- decompose(Ozone.T85, "additive")
plot(Ozone.T85_decomposed)

## Test for Stationarity
adf.test(Ozone.T85)

## Seasonality Adjusting
Ozone.T85_adjusted <- Ozone.T85 - Ozone.T85_decomposed$seasonal
plot(Ozone.T85_adjusted)

## Exponential Smoothing Model
T85.ETS <- HoltWinters(Ozone.T85, gamma=FALSE)
T85.ETS

## ARIMA Model
T85.Arima <- auto.arima(Ozone.T85)
T85.Arima

## Observed vs Predicted
par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(T85.ETS, col=c("green","red"))

# For ARIMA Model
plot(T85.Arima$x, col="red")
lines(fitted(T85.Arima), col="blue")

## Residuals
# For Exponential Smoothing
T85.ETS_forecast <- forecast(T85.ETS, h=60)
plot.ts(T85.ETS_forecast$residuals)

# For ARIMA Model
T85.Arima_forecast <- forecast(T85.Arima, h=10)
plot.ts(T85.Arima_forecast$residuals)

mean(T85.ETS_forecast$residuals)
mean(T85.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(T85.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(T85.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
pacf(T85.Arima_forecast$residuals, lag.max = 20)
acf(T85.Arima_forecast$residuals, lag.max = 20)
Box.test(T85.Arima_forecast$residuals, lag=20, type="Ljung-Box")

## Normality Test
shapiro.test(T85.Arima_forecast$residuals)

## Best Model
T85.Arima <- arima(Ozone.T85, order = c(4,1,2))
T85.Arima

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.T85, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
T85.Arima_forecast <- forecast(T85.Arima, h=30)
T85.Arima_forecast
plot(T85.Arima_forecast)


############# NEXT #############

# RH85

par(mfrow=c(2,1))
## Before dealing with Outliers and NAs
sum(is.na(Ozone.RH85))
plot.ts(Ozone.RH85)

Ozone.RH85 <- tsclean(Ozone.RH85, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.RH85)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.RH85_decomposed <- decompose(Ozone.RH85, "additive")
plot(Ozone.RH85_decomposed)

## Test for Stationarity
adf.test(Ozone.RH85)

## Seasonality Adjusting
Ozone.RH85_adjusted <- Ozone.RH85 - Ozone.RH85_decomposed$seasonal
plot(Ozone.RH85_adjusted)

## Exponential Smoothing Model
RH85.ETS <- HoltWinters(Ozone.RH85, gamma=FALSE)
RH85.ETS

## ARIMA Model
RH85.Arima <- auto.arima(Ozone.RH85)
RH85.Arima

## Observed vs Predicted
par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(RH85.ETS, col=c("green","red"))

# For ARIMA Model
plot(RH85.Arima$x, col="red")
lines(fitted(RH85.Arima), col="blue")

## Residuals
# For Exponential Smoothing
RH85.ETS_forecast <- forecast(RH85.ETS, h=60)
plot.ts(RH85.ETS_forecast$residuals)

# For ARIMA Model
RH85.Arima_forecast <- forecast(RH85.Arima, h=10)
plot.ts(RH85.Arima_forecast$residuals)

mean(RH85.ETS_forecast$residuals)
mean(RH85.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(RH85.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(RH85.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
pacf(RH85.Arima_forecast$residuals, lag.max = 20)
acf(RH85.Arima_forecast$residuals, lag.max = 20)
Box.test(RH85.Arima_forecast$residuals, lag=20, type="Ljung-Box")

## Normality Test
shapiro.test(RH85.Arima_forecast$residuals)

## Best Model
RH85.Arima <- arima(Ozone.RH85, order = c(4,1,2))
RH85.Arima

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.RH85, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
RH85.Arima_forecast <- forecast(RH85.Arima, h=30)
RH85.Arima_forecast
plot(RH85.Arima_forecast)


############# NEXT #############

# T70

par(mfrow=c(2,1))
## Before dealing with Outliers and NAs
sum(is.na(Ozone.T70))
plot.ts(Ozone.T70)

Ozone.T70 <- tsclean(Ozone.T70, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.T70)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.T70_decomposed <- decompose(Ozone.T70, "additive")
plot(Ozone.T70_decomposed)

## Test for Stationarity
adf.test(Ozone.T70)

## Seasonality Adjusting
Ozone.T70_adjusted <- Ozone.T70 - Ozone.T70_decomposed$seasonal
plot(Ozone.T70_adjusted)

## Exponential Smoothing Model
T70.ETS <- HoltWinters(Ozone.T70, gamma=FALSE)
T70.ETS

## ARIMA Model
T70.Arima <- auto.arima(Ozone.T70)
T70.Arima

## Observed vs Predicted
par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(T70.ETS, col=c("green","red"))

# For ARIMA Model
plot(T70.Arima$x, col="red")
lines(fitted(T70.Arima), col="blue")

## Residuals
# For Exponential Smoothing
T70.ETS_forecast <- forecast(T70.ETS, h=60)
plot.ts(T70.ETS_forecast$residuals)

# For ARIMA Model
T70.Arima_forecast <- forecast(T70.Arima, h=10)
plot.ts(T70.Arima_forecast$residuals)

mean(T70.ETS_forecast$residuals)
mean(T70.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(T70.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(T70.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
pacf(T70.Arima_forecast$residuals, lag.max = 20)
acf(T70.Arima_forecast$residuals, lag.max = 20)
Box.test(T70.Arima_forecast$residuals, lag=20, type="Ljung-Box")

## Normality Test
shapiro.test(T70.Arima_forecast$residuals)

## Best Model
T70.Arima <- arima(Ozone.T70, order = c(1,0,0))
T70.Arima

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.T70, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
T70.Arima_forecast <- forecast(T70.Arima, h=30)
T70.Arima_forecast
plot(T70.Arima_forecast)


############# NEXT #############

# KI

par(mfrow=c(2,1))
## Before dealing with Outliers and NAs
sum(is.na(Ozone.KI))
plot.ts(Ozone.KI)

Ozone.KI <- tsclean(Ozone.KI, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.KI)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.KI_decomposed <- decompose(Ozone.KI, "additive")
plot(Ozone.KI_decomposed)

## Test for Stationarity
adf.test(Ozone.KI)

## Seasonality Adjusting
Ozone.KI_adjusted <- Ozone.KI - Ozone.KI_decomposed$seasonal
plot(Ozone.KI_adjusted)

## Exponential Smoothing Model
KI.ETS <- HoltWinters(Ozone.KI, gamma=FALSE)
KI.ETS

## ARIMA Model
KI.Arima <- auto.arima(Ozone.KI)
KI.Arima

## Observed vs Predicted
par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(KI.ETS, col=c("green","red"))

# For ARIMA Model
plot(KI.Arima$x, col="red")
lines(fitted(KI.Arima), col="blue")

## Residuals
# For Exponential Smoothing
KI.ETS_forecast <- forecast(KI.ETS, h=60)
plot.ts(KI.ETS_forecast$residuals)

# For ARIMA Model
KI.Arima_forecast <- forecast(KI.Arima, h=10)
plot.ts(KI.Arima_forecast$residuals)

mean(KI.ETS_forecast$residuals)
mean(KI.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(KI.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(KI.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
pacf(KI.Arima_forecast$residuals, lag.max = 20)
acf(KI.Arima_forecast$residuals, lag.max = 20)
Box.test(KI.Arima_forecast$residuals, lag=20, type="Ljung-Box")

## Normality Test
shapiro.test(KI.Arima_forecast$residuals)

## Best Model
KI.Arima <- arima(Ozone.KI, order = c(1,0,0))
KI.Arima

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.KI, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
KI.Arima_forecast <- forecast(KI.Arima, h=30)
KI.Arima_forecast
plot(KI.Arima_forecast)


############# NEXT #############

# TT

par(mfrow=c(2,1))
## Before dealing with Outliers and NAs
sum(is.na(Ozone.TT))
plot.ts(Ozone.TT)

Ozone.TT <- tsclean(Ozone.TT, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.TT)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.TT_decomposed <- decompose(Ozone.TT, "additive")
plot(Ozone.TT_decomposed)

## Test for Stationarity
adf.test(Ozone.TT)

## Seasonality Adjusting
Ozone.TT_adjusted <- Ozone.TT - Ozone.TT_decomposed$seasonal
plot(Ozone.TT_adjusted)

## Exponential Smoothing Model
TT.ETS <- HoltWinters(Ozone.TT, gamma=FALSE)
TT.ETS

## ARIMA Model
TT.Arima <- auto.arima(Ozone.TT)
TT.Arima

## Observed vs Predicted
par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(TT.ETS, col=c("green","red"))

# For ARIMA Model
plot(TT.Arima$x, col="red")
lines(fitted(TT.Arima), col="blue")

## Residuals
# For Exponential Smoothing
TT.ETS_forecast <- forecast(TT.ETS, h=60)
plot.ts(TT.ETS_forecast$residuals)

# For ARIMA Model
TT.Arima_forecast <- forecast(TT.Arima, h=10)
plot.ts(TT.Arima_forecast$residuals)

mean(TT.ETS_forecast$residuals)
mean(TT.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(TT.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(TT.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
pacf(TT.Arima_forecast$residuals, lag.max = 20)
acf(TT.Arima_forecast$residuals, lag.max = 20)
Box.test(TT.Arima_forecast$residuals, lag=20, type="Ljung-Box")

## Normality Test
shapiro.test(TT.Arima_forecast$residuals)

## Best Model
TT.Arima <- arima(Ozone.TT, order = c(5,0,4))
TT.Arima

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.TT, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
TT.Arima_forecast <- forecast(TT.Arima, h=30)
TT.Arima_forecast
plot(TT.Arima_forecast)

############# NEXT #############

# SLP_

par(mfrow=c(2,1))
## Before dealing with Outliers and NAs
sum(is.na(Ozone.SLP_))
plot.ts(Ozone.SLP_)

Ozone.SLP_ <- tsclean(Ozone.SLP_, replace.missing = TRUE)
## After Outlier and NAs Interpolation
plot.ts(Ozone.SLP_)

# It is an Additive Seasonal Time Series Component

## Decomposing the Data
Ozone.SLP__decomposed <- decompose(Ozone.SLP_, "additive")
plot(Ozone.SLP__decomposed)

## Test for Stationarity
adf.test(Ozone.SLP_)

## Seasonality Adjusting
Ozone.SLP__adjusted <- Ozone.SLP_ - Ozone.SLP__decomposed$seasonal
plot(Ozone.SLP__adjusted)

## Exponential Smoothing Model
SLP_.ETS <- HoltWinters(Ozone.SLP_, gamma=FALSE)
SLP_.ETS

## ARIMA Model
SLP_.Arima <- auto.arima(Ozone.SLP_)
SLP_.Arima

## Observed vs Predicted
par(mar=c(2.25,2.25,2.25,2.25))
par(mfrow=c(2,1))
# For Exponential Smoothing
plot(SLP_.ETS, col=c("green","red"))

# For ARIMA Model
plot(SLP_.Arima$x, col="red")
lines(fitted(SLP_.Arima), col="blue")

## Residuals
# For Exponential Smoothing
SLP_.ETS_forecast <- forecast(SLP_.ETS, h=60)
plot.ts(SLP_.ETS_forecast$residuals)

# For ARIMA Model
SLP_.Arima_forecast <- forecast(SLP_.Arima, h=10)
plot.ts(SLP_.Arima_forecast$residuals)

mean(SLP_.ETS_forecast$residuals)
mean(SLP_.ETS_forecast$residuals)

# Box-Ljung Test - ARIMA
Box.test(SLP_.Arima_forecast$residuals, lag=20, type="Ljung-Box")

# Box-Ljung Test - Exponential Smoothing
Box.test(SLP_.ETS_forecast$residuals, lag=20, type="Ljung-Box")

## PACF & ACF
par(mar=c(3,3,3,3))
par(mfrow=c(2,1))
pacf(SLP_.Arima_forecast$residuals, lag.max = 20)
acf(SLP_.Arima_forecast$residuals, lag.max = 20)
Box.test(SLP_.Arima_forecast$residuals, lag=20, type="Ljung-Box")

## Normality Test
shapiro.test(SLP_.Arima_forecast$residuals)

## Best Model
SLP_.Arima <- arima(Ozone.SLP_, order = c(1,0,0))
SLP_.Arima

## Cross-Validation
Arima_CV <- function(x, h){forecast(Arima(x, order = c(1,1,3)), h=h)}
Fit_final <- tsCV(Ozone.SLP_, Arima_CV, h=10, window=30)
plot.ts(Fit_final)

## Forecasting using ARIMA
SLP_.Arima_forecast <- forecast(SLP_.Arima, h=30)
SLP_.Arima_forecast
plot(SLP_.Arima_forecast)
