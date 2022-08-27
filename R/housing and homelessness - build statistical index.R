library(tidyverse)
library(tidymodels)
library(poissonreg)
library(geographr)

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
  left_join(england_indicators) |> 
  left_join(geographr::lookup_ltla21_region21 |> select(ltla21_code, region21_name))

# ---- Split data ----
ukraine_split <- initial_split(ukraine_data, prop = 0.7, strata = region21_name)

ukraine_train <- training(ukraine_split)
ukraine_test <- testing(ukraine_split)

ukraine_train <- na.omit(ukraine_train)

# ukraine_val <- validation_split(ukraine_train, prop = 0.80)

ukraine_cv <- vfold_cv(ukraine_train, v = 10)

# Also use homelessness data from the end of July for out-of-sample testing
ukraine_test_future <- 
  homelessness_feb_july |> 
  select(
    ltla21_code = lad_code, 
    Ukraine_homelessness = `Total Ukrainian households owed a prevention or relief duty`
    # Ukraine_homelessness = `% at risk of homelessness`
  ) |> 
  left_join(england_indicators) |> 
  left_join(geographr::lookup_ltla21_region21 |> select(ltla21_code, region21_name))

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
  update_role(ltla21_code, region21_name, new_role = "ID") |> 
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
poisson_fit <-
  poisson_wflow |> 
  finalize_workflow(best_params) |> 
  fit(data = ukraine_train)

model_perf <- 
  ukraine_test |> 
  select(Ukraine_homelessness) |> 
  bind_cols(predict(poisson_fit, ukraine_test))

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
  bind_cols(predict(poisson_fit, ukraine_test_future))

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

# ---- Fit random forest ----
rf_model <- 
  rand_forest(trees = 1000) |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_wflow <- 
  workflow() |> 
  add_formula(
    Ukraine_homelessness ~ .) |> 
  add_model(rf_model) 

rf_fit <- 
  rf_wflow |> 
  fit(data = ukraine_train)

# ---- Compare random forest and Poisson models ----
# Helper function from https://www.tmwr.org/resampling.html
estimate_perf <- function(model, dat) {
  # Capture the names of the `model` and `dat` objects
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ukraine_", "", data_name)
  
  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)
  
  model %>%
    predict(dat) %>%
    bind_cols(dat %>% select(Ukraine_homelessness)) %>%
    reg_metrics(Ukraine_homelessness, .pred) %>%
    select(-.estimator) %>%
    mutate(object = obj_name, data = data_name)
}

estimate_perf(rf_fit, ukraine_train)
estimate_perf(poisson_fit, ukraine_train)

estimate_perf(rf_fit, ukraine_test |> na.omit())
estimate_perf(poisson_fit, ukraine_test)

estimate_perf(rf_fit, ukraine_test_future |> na.omit())
estimate_perf(poisson_fit, ukraine_test_future |> na.omit())

# - Resampling -
rf_res <- 
  rf_wflow |> 
  fit_resamples(
    resamples = ukraine_cv,
    control = control_resamples(save_pred = TRUE, save_workflow = TRUE)
  )

rf_res |> 
  collect_metrics()

assess_res <- 
  rf_res |> 
  collect_predictions()

assess_res |> 
  ggplot(aes(x = Ukraine_homelessness, y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(color = "red") + 
  coord_obs_pred() + 
  ylab("Predicted")

over_predicted <- 
  assess_res %>% 
  mutate(residual = Ukraine_homelessness - .pred) %>% 
  arrange(desc(abs(residual))) |> 
  slice(1:10)

# Almost all of the largest prediction errors are in London
ukraine_train |> 
  slice(over_predicted$.row) |> 
  left_join(geographr::lookup_ltla21_region21 |> select(ltla21_code, region21_name))

#?? Maybe we instead should fit a multilevel model accounting for region?
stan_recipe <- 
  recipe(
    Ukraine_homelessness ~ `Households assessed as threatened with homelessness per (000s)` +
      `Households assessed as homeless per (000s)` +
      `Households in temporary accommodation per 1,000` +
      `Households on housing waiting list per 1,000` +
      `Social housing stock as a proportion of all households` +
      `Vacant dwellings per 1,000 units of social housing stock`, 
      data = ukraine_train
  ) |> 
  # update_role(ltla21_code, new_role = "ID") |> 
  step_dummy(region21_name) |> 
  step_normalize(all_predictors())

stan_model <- 
  poisson_reg() |> 
  set_engine(
    "stan",
    prior_intercept = rstanarm::student_t(df = 1),
    prior = rstanarm::student_t(df = 1)
  )

stan_wflow <- 
  workflow() |> 
  add_recipe(stan_recipe) |> 
  add_model(
    stan_model,
    formula = Ukraine_homelessness ~ `Households assessed as threatened with homelessness per (000s)` +
      `Households assessed as homeless per (000s)` +
      `Households in temporary accommodation per 1,000` +
      `Households on housing waiting list per 1,000` +
      `Social housing stock as a proportion of all households` +
      `Vacant dwellings per 1,000 units of social housing stock` +
      (1 | starts_with(region21_name))
  )

stan_fit <- 
  stan_wflow |> 
  fit(data = ukraine_train)

