# Here's an R script that explores more sophisticated models, including Linear
# Regression, Random Forest, and XGBoost. Additionally, this script uses the 
# forecast package for ARIMA modeling.

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(xgboost)
library(forecast)

# Read the dataset
#! Matt intervention: load the actual Ukraine arrivals data
# data <- read.csv("data.csv")
data <- read_csv("data/cumulative-visas/cumulative-visas-2023-03-13.csv")

data <- 
  data |> 
  select(
    date = Date,
    visa_applications = `Number of visa applications`,
    visas_issued = `Number of visas issued`,
    arrivals = `Number of arrivals`
  ) |> 
  group_by(date) |> 
  summarise(
    visa_applications = sum(visa_applications, na.rm = TRUE),
    visas_issued = sum(visas_issued, na.rm = TRUE),
    arrivals = sum(arrivals, na.rm = TRUE)
  )

# Data preprocessing
data$date <- ymd(data$date)
data <- data %>% arrange(date)

# Feature engineering
data$visa_approval_rate <- data$visas_issued / data$visa_applications
data$arrival_rate <- data$arrivals / data$visas_issued
data$lag_arrivals <- lag(data$arrivals, 1)

# Handle missing values introduced by the lag function
data <- data %>% drop_na()

# Train/test split
# set.seed(123)
# train_indices <- createDataPartition(data$arrivals, p = 0.8, list = FALSE)
# train_data <- data[train_indices, ]
# test_data <- data[-train_indices, ]

# Train the model on all but the most recent eight weeks of data - we'll 
train_data <- data |> dplyr::slice(1:(n() - 8))
test_data <- data |> dplyr::slice((n() - 7):n())

# Evaluation function
evaluate <- function(predictions, true_values) {
  mae <- mean(abs(predictions - true_values))
  mse <- mean((predictions - true_values)^2)
  rmse <- sqrt(mse)
  
  cat("MAE:", mae, "\n")
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
}

# Linear Regression
cat("Linear Regression:\n")
lm_model <- lm(arrivals ~ visa_applications + visas_issued + visa_approval_rate + arrival_rate + lag_arrivals, data = train_data)
lm_predictions <- predict(lm_model, test_data)
evaluate(lm_predictions, test_data$arrivals)

# Random Forest
cat("\nRandom Forest:\n")
rf_model <- randomForest(arrivals ~ visa_applications + visas_issued + visa_approval_rate + arrival_rate + lag_arrivals, data = train_data, ntree = 500)
rf_predictions <- predict(rf_model, test_data)
evaluate(rf_predictions, test_data$arrivals)

# XGBoost
cat("\nXGBoost:\n")
xgb_train <- xgb.DMatrix(data = as.matrix(train_data %>% select(-date, -arrivals)), label = train_data$arrivals)
xgb_test <- xgb.DMatrix(data = as.matrix(test_data %>% select(-date, -arrivals)), label = test_data$arrivals)

xgb_params <- list(
  objective = "reg:linear",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nrounds = 100)
xgb_predictions <- predict(xgb_model, xgb_test)
evaluate(xgb_predictions, test_data$arrivals)

# ARIMA
cat("\nARIMA:\n")
arima_train <- ts(train_data$arrivals, frequency = 52)
arima_test <- ts(test_data$arrivals, frequency = 52)

arima_model <- auto.arima(arima_train, seasonal = TRUE)
arima_forecast <- forecast(arima_model, h = length(arima_test))
arima_predictions <- arima_forecast$mean

#! Matt intervention: need to convert to vectors otherwise you get NaNs
evaluate(as.vector(arima_predictions), as.vector(arima_test))
