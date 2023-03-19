# Below is a simple R script that uses linear regression to predict the number 
# of arrivals using the given dataset. This example assumes that you have a 
# dataset named "data.csv" with columns "date", "visa_applications", 
# "visas_issued", and "arrivals".

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(caret)

# Read the dataset
data <- read.csv("data.csv")

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

# Model selection and training
model <- lm(arrivals ~ visa_applications + visas_issued + visa_approval_rate + arrival_rate + lag_arrivals, data = train_data)

# Model evaluation
predictions <- predict(model, test_data)
mae <- mean(abs(predictions - test_data$arrivals))
mse <- mean((predictions - test_data$arrivals)^2)
rmse <- sqrt(mse)

cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

# Predict future arrivals using the model
future_data <- data.frame(visa_applications = 1000, visas_issued = 800, visa_approval_rate = 0.8, arrival_rate = 0.75, lag_arrivals = 700)
predicted_arrivals <- predict(model, future_data)
cat("Predicted arrivals:", predicted_arrivals, "\n")
