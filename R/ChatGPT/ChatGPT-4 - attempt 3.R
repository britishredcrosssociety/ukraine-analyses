# If you would like to simulate the arrivals based on historical data and its 
# uncertainty, you can use a bootstrapping method to generate multiple possible 
# scenarios. Below is an R script demonstrating how to create a simple bootstrap 
# simulation for predicting arrivals based on the visa applications and visa approval rate.

# Load necessary libraries
library(tidyverse)

# Read the dataset
data <- read.csv("data.csv")

# Data preprocessing
data <- data %>% arrange(date)

# Feature engineering
data$visa_approval_rate <- data$visas_issued / data$visa_applications

# Bootstrap simulation
set.seed(123)

n_simulations <- 1000
future_visa_applications <- 1000
predictions <- numeric(n_simulations)

for (i in 1:n_simulations) {
  # Sample with replacement from the historical data
  sampled_data <- data %>% sample_n(size = nrow(data), replace = TRUE)
  
  # Calculate the average visa approval rate from the sampled data
  mean_approval_rate <- mean(sampled_data$visa_approval_rate)
  
  # Estimate the number of visas issued based on the future visa applications and the average visa approval rate
  estimated_visas_issued <- future_visa_applications * mean_approval_rate
  
  # Estimate the number of arrivals based on the historical ratio of arrivals to visas issued
  estimated_arrivals_to_visas_issued_ratio <- mean(sampled_data$arrivals / sampled_data$visas_issued)
  estimated_arrivals <- estimated_visas_issued * estimated_arrivals_to_visas_issued_ratio
  
  predictions[i] <- estimated_arrivals
}

# Analyze the simulation results
mean_prediction <- mean(predictions)
lower_bound <- quantile(predictions, 0.025)
upper_bound <- quantile(predictions, 0.975)

cat("Mean predicted arrivals:", mean_prediction, "\n")
cat("95% Confidence Interval:", lower_bound, "-", upper_bound, "\n")
