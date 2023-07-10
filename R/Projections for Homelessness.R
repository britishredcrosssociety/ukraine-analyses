#Forecasting for Ukraine Homelessness 

library(forecast)
library(prophet)


#Preparing Data for Time Series 

names(Homelessness) <- c('ds', 'y', "c")

ukh.ts <- ts(Homelessness$Homeless, start = c(2022,6), end = c(2023,6), frequency = 12)

plot(ukh.ts, xlab = "Date", ylab = "Households", main = "Homelessness among Households")

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

#Arima Model completed with auto arima. 

##Projections with households with kids##

ukhc.ts <- ts(Homelessness$c, start = c(2022,6), end = c(2023,6), frequency = 12)

plot(ukhc.ts, xlab = "Date", ylab = "Households", main = "Homelessness among Households with Children")

AutoArimaC <- auto.arima(ukhc.ts)

ForecastC <- forecast(AutoArimaC, 
                      h = 12, 
                      level = c(80,99))

ForecastedPlotC <- autoplot(ForecastC)

ForecastedPlotC <- (ForecastedPlotC + theme_classic())

ForecastedPlotC + labs(title = "Forecast of Ukraine Homelessness by Household with Children by ARIMA (1,1,0)", 
                       x = "Time", 
                       y = "Households")


----#Additional Forecasting-not working#----

#Naive Model#
NaiveForecast <- snaive(ukh.ts, h = 12)

Forecast1 <- (forecast(NaiveForecast, h = 12))

autoplot(Forecast1)

#*Note that the predictions from the Naive model do not reflect trends. 

#Prophet model projections 

Prophet <- prophet(Homelessness)

future <- make_future_dataframe(Homelessness, periods = 365)

#Prophet future function not working- to discuss with Matt 