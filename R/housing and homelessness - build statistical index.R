library(tidyverse)
library(tidymodels)
library(poissonreg)

tidymodels_prefer()

set.seed(20220822)

# ---- Load data ----
england_indicators <- read_csv("data/housing/housing-england.csv")
homelessness_feb_july <- read_csv("data/homelessness/ukraine-homelessness-1-july.csv")
homelessness_feb_aug <- read_csv("data/homelessness/ukraine-homelessness-29-july.csv")

# Remove quintiles
england_indicators <- 
  england_indicators |> 
  select(-ltla21_name, -ends_with("_quintile"))

ukraine_data <- 
  homelessness_feb_aug |> 
  select(
    ltla21_code = lad_code, 
    Ukraine_homelessness = `Total Ukrainian households owed a prevention or relief duty`
    # Ukraine_homelessness = `% at risk of homelessness`
  ) |> 
  left_join(england_indicators)

# ---- Split data ----
ukraine_split <- initial_split(ukraine_data, prop = 0.7)

ukraine_train <- training(ukraine_split)
ukraine_test <- testing(ukraine_split)

ukraine_train <- na.omit(ukraine_train)

# ukraine_val <- validation_split(ukraine_train, prop = 0.80)

ukraine_cv <- vfold_cv(ukraine_train)

# Also use homelessness data from the end of July for out-of-sample testing
ukraine_test_future <- 
  homelessness_feb_july |> 
  select(
    ltla21_code = lad_code, 
    Ukraine_homelessness = `Total Ukrainian households owed a prevention or relief duty`
    # Ukraine_homelessness = `% at risk of homelessness`
  ) |> 
  left_join(england_indicators)

# ---- Preprocess data ----
model_recipe <- 
  # recipe(
  #   `% at risk of homelessness` ~ `Households assessed as threatened with homelessness per (000s)` +
  #     `Households assessed as homeless per (000s)` +
  #     `Households in temporary accommodation per 1,000` +
  #     `Households on housing waiting list per 1,000` +
  #     `Social housing stock as a proportion of all households` +
  #     `Vacant dwellings per 1,000 units of social housing stock`,
  #   data = ukraine_train
  # ) |> 
  recipe(Ukraine_homelessness ~ ., data = ukraine_train) |> 
  update_role(ltla21_code, new_role = "ID") |> 
  step_normalize(all_predictors()) |> 
  step_poly(all_predictors()) |> 
  step_interact(~ all_predictors():all_predictors())
  
# ---- Fit models to training data ----
# Set up a lasso Poisson regression
poisson_mod <- 
  poisson_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")

# Construct a workflow combining the recipe and models
poisson_wflow <-
  workflow() |> 
  add_model(poisson_mod) |> 
  add_recipe(model_recipe)

reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

# Find best tuned model
res <- 
  poisson_wflow |> 
  tune_grid(
    resamples = ukraine_cv,
    grid = reg_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rmse)
  )

res |> 
  collect_metrics() |> 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("RMSE") +
  scale_x_log10(labels = scales::label_number())

res |> 
  show_best("rmse", n = 15) |> 
  arrange(penalty)

# ---- Validation ----
# Select best parameters
best_params <-
  res |> 
  select_best(metric = "rmse")

# Refit using the entire training data
reg_res <-
  poisson_wflow |> 
  finalize_workflow(best_params) |> 
  fit(data = ukraine_train)

model_perf <- 
  ukraine_test |> 
  select(Ukraine_homelessness) |> 
  bind_cols(predict(reg_res, ukraine_test))

model_perf |> 
  rmse(Ukraine_homelessness , .pred)

model_perf |> 
  ggplot(aes(x = Ukraine_homelessness, y = .pred)) +
  geom_abline(lty = 2) +
  geom_point() +
  coord_obs_pred()

# How well does the model predict observed homelessness in a whole other month?
ukraine_predictions <- 
  ukraine_test_future |> 
  select(ltla21_code, Ukraine_homelessness) |> 
  bind_cols(predict(reg_res, ukraine_test_future))

ukraine_predictions |> 
  ggplot(aes(x = Ukraine_homelessness, y = .pred)) +
  geom_abline(lty = 2) +
  geom_point() +
  #geom_smooth(method = "lm") +
  coord_obs_pred()

# ---- Compare to composite index ----
ukraine_predictions <- 
  ukraine_predictions |> 
  mutate(
    prediction_rank = rank(.pred),
    prediction_decile = as.integer(Hmisc::cut2(.pred, g = 10))
  )

index <- read_csv("data/housing/housing-index-england.csv")

index |> 
  select(ltla21_code, decile = housing_and_homelessness_composite_quantiles, rank = housing_and_homelessness_composite_rank) |> 
  left_join(ukraine_predictions) |> 
  
  ggplot(aes(x = rank, y = prediction_rank)) +
  geom_abline(lty = 2) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  coord_obs_pred()
