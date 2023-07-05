#Forecasting for Ukraine Homelessness 

library(forecast)
library(prophet)

#Cant use HW forecast as at least 15 observations needed to estimate seasonality. 

#Preparing Data for Time Series 

ukh.ts <- ts(Homelessness$Homeless, start = c(2022,6), end = c(2023,6), frequency = 12)

plot(ukh.ts, xlab = "Date", ylab = "Households", main = "Homelessness among Households")

----#Naive Model#---- 
NaiveForecast <- snaive(ukh.ts, h = 12)

Forecast1 <- (forecast(NaiveForecast, h = 12))

autoplot(Forecast1)

#*Note that the predictions from the Naive model do not reflect trends. 

----#Arima Model Forecast#----
AutoArima <- auto.arima(ukh.ts)

Forecast2 <- forecast(AutoArima, 
                      h = 12, 
                      level = c(80,99))

ForecastedPlot <- autoplot(Forecast2)

ForecastedPlot <- (ForecastedPlot + theme_classic())
 
ForecastedPlot + labs(title = "Forecast of Ukraine Homelessness by ARIMA (1,1,0)", 
        x = "Time", 
        y = "Households")

#Arima Model completed with auto arima. To discuss with Matt. 

#Prophet model projections 

names(Homelessness) <- c('ds', 'y', "c")

Prophet <- prophet(Homelessness)

future <- make_future_dataframe(Homelessness, periods = 365)

#Prophet future function not working- to discuss with Matt 
