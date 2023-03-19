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

# ---- Compare to BRC simulation performance ----
# Use BRC predictions from the same point as `test_data` starts
brc_predictions_raw <- 
  read_csv("output-data/simulations/simulation-baseline-2023-01-23.csv")

brc_predictions <- 
  brc_predictions_raw |> 
  dplyr::slice(1:8) |> 
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
  evaluate(as.vector(arima_predictions), as.vector(arima_test), "arima"),
  evaluate(brc_predictions$brc_prediction, brc_predictions$arrivals, "BRC")
)
#--> BRC simulation gives the most accurate predictions of actual arrivals 8 weeks into the future

# Linear regression is the best-performing statistical model
# - let's compare it to the BRC simulation in more detail
lm_predictions_ci <- 
  bind_cols(
    data |> select(date), 
    predict(lm_model, data, interval = "confidence")
  )

model_predictions <- 
  brc_predictions |> 
  left_join(lm_predictions_ci |> select(date, lm_prediction = fit, lm_lower = lwr, lm_upper = upr))

bind_rows(
  evaluate(model_predictions$lm_prediction, model_predictions$arrivals, "Linear regression"),
  evaluate(model_predictions$brc_prediction, model_predictions$arrivals, "BRC")
)

# Plot both models' predictions (and confidence intervals)
model_predictions |> 
  ggplot(aes(x = date, y = arrivals)) +
  geom_line(colour = "black") +
  
  # geom_ribbon(aes(ymin = brc_lower, ymax = brc_upper), fill = "red", alpha = 0.2) +
  geom_line(aes(y = brc_prediction), colour = "red") +
  
  geom_ribbon(aes(ymin = lm_lower, ymax = lm_upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = lm_prediction), colour = "blue")

# ---- Compare 2-month-ahead predictions ----
# In practice, we wouldn't have information on future numbers of applications,
# visas issued, approval rates or arrival rates, so we would have to model them.
# Matt asked ChatGPT for some code to do this. ChatGPT generated most of the code 
# below; Matt has lightly edited and expanded it.

# Generate future dates (2 months into the future)
last_date <- max(train_data$date)
future_dates <- seq(from = last_date + days(7), by = "week", length.out = 8)

# Prepare future data
future_data <- data.frame(
  date = future_dates,
  visa_applications = rep(mean(train_data$visa_applications), length(future_dates)),
  visas_issued = rep(mean(train_data$visas_issued), length(future_dates)),
  visa_approval_rate = rep(mean(train_data$visa_approval_rate), length(future_dates)),
  arrival_rate = rep(mean(train_data$arrival_rate), length(future_dates)),
  lag_arrivals = c(tail(train_data$arrivals, 1), rep(mean(train_data$arrivals), length(future_dates) - 1))
)

# Predict future arrivals
predicted_arrivals <- predict(lm_model, future_data)

# Combine future dates with predictions
future_predictions <- data.frame(
  date = future_dates,
  lm_prediction = predicted_arrivals
)

future_predictions <- 
  future_predictions |> 
  left_join(brc_predictions |> select(date, brc_prediction)) |> 
  left_join(test_data |> select(date, arrivals))

bind_rows(
  evaluate(future_predictions$lm_prediction, future_predictions$arrivals, "Linear regression"),
  evaluate(future_predictions$brc_prediction, future_predictions$arrivals, "BRC")
)
#--> BRC simulation fares a *lot* better
