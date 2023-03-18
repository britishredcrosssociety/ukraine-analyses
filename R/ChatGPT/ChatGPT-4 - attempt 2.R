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
set.seed(123)
train_indices <- createDataPartition(data$arrivals, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

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

evaluate(arima_predictions, arima_test)

# Compare to BRC simulation performance
brc_predictions_raw <- 
  read_csv("output-data/simulations/simulation-baseline-2022-10-03.csv")

brc_predictions <- 
  brc_predictions_raw |> 
  select(date = Date, brc_prediction = `Total arrivals`, brc_lower = `Total arrivals (lower bound)`, brc_upper = `Total arrivals (upper bound)`) |> 
  left_join(data |> select(date, arrivals), by = "date")

# Evaluation function
evaluate <- function(predictions, true_values, label = "") {
  mae <- mean(abs(predictions - true_values))
  mse <- mean((predictions - true_values)^2)
  rmse <- sqrt(mse)
  
  tibble(
    model = label,
    MAE = mae,
    MSE = mse,
    RMSE = rmse
  )
}

# Compare all models
bind_rows(
  evaluate(lm_predictions, test_data$arrivals, "Linear"),
  evaluate(rf_predictions, test_data$arrivals, "RF"),
  evaluate(xgb_predictions, test_data$arrivals, "xgboost"),
  evaluate(brc_predictions$brc_prediction, brc_predictions$arrivals, "BRC")
)

# Linear regression is the only one that performs better than BRC model - let's compare them in detail
# data$lm_predictions <- predict(lm_model, data)
data <- bind_cols(data, predict(lm_model, data, interval = "confidence"))

brc_predictions <- 
  brc_predictions |> 
  left_join(data |> select(date, lm_prediction = fit, lm_lower = lwr, lm_upper = upr))

bind_rows(
  evaluate(brc_predictions$lm_prediction, brc_predictions$arrivals, "Linear regression"),
  evaluate(brc_predictions$brc_prediction, brc_predictions$arrivals, "BRC")
)

# Plot both models' predictions (and confidence intervals)
brc_predictions |> 
  ggplot(aes(x = date, y = arrivals)) +
  geom_line(colour = "black") +
  
  # geom_ribbon(aes(ymin = brc_lower, ymax = brc_upper), fill = "red", alpha = 0.2) +
  geom_line(aes(y = brc_prediction), colour = "red") +
  
  geom_ribbon(aes(ymin = lm_lower, ymax = lm_upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = lm_prediction), colour = "blue")

# To project the linear regression into the future, we'd first need to model 
# the future values of each independent variable (predictor). Plot the historical
# values to see whether they are linear or not.
data |> 
  select(date, visa_applications, visas_issued, arrivals, lag_arrivals) |> 
  pivot_longer(cols = -date) |> 
  ggplot(aes(x = date, y = value, group = name)) +
  geom_line(aes(colour = name))
