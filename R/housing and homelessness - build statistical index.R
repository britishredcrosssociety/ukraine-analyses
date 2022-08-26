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
  homelessness_feb_july |> 
  select(ltla21_code = lad_code, `% at risk of homelessness`) |> 
  left_join(england_indicators)

# ---- Split data ----
ukraine_split <- initial_split(ukraine_data, prop = 0.7)

ukraine_train <- training(ukraine_split)
ukraine_test <- testing(ukraine_split)

ukraine_train <- na.omit(ukraine_train)

ukraine_val <- validation_split(ukraine_train, prop = 0.80)

ukraine_cv <- vfold_cv(ukraine_train)

# Also use homelessness data from the end of July for out-of-sample testing
ukraine_test_future <- 
  homelessness_feb_aug |> 
  select(ltla21_code = lad_code, `% at risk of homelessness`) |> 
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
  recipe(`% at risk of homelessness` ~ ., data = ukraine_train) |> 
  step_normalize(all_predictors()) |> 
  update_role(ltla21_code, new_role = "ID")

# ---- Interactions ----
# Source: https://github.com/topepo/FES/blob/master/07_Detecting_Interaction_Effects/7_04_The_Brute-Force_Approach_to_Identifying_Predictive_Interactions/ames_glmnet.R
int_vars <- 
  model_recipe |> 
  pluck("var_info") |> 
  dplyr::filter(role == "predictor") |> 
  pull(variable)

interactions <- t(combn(as.character(int_vars), 2))
colnames(interactions) <- c("var1", "var2")

interactions <- 
  interactions |> 
  as_tibble() |> 
  mutate(
    term = 
      paste0(
        "starts_with('",
        var1,
        "'):starts_with('",
        var2,
        "')"
      )
  ) |>  
  pull(term) |> 
  paste(collapse = "+")

interactions <- paste("~", interactions)
interactions <- as.formula(interactions)

interaction_recipe <- 
  recipe(
    `% at risk of homelessness` ~ `Households assessed as threatened with homelessness per (000s)` +
      `Households assessed as homeless per (000s)` +
      `Households in temporary accommodation per 1,000` +
      `Households on housing waiting list per 1,000` +
      `Social housing stock as a proportion of all households` +
      `Vacant dwellings per 1,000 units of social housing stock`,
    data = ukraine_train
  ) |> 
  step_normalize(all_predictors()) |> 
  step_interact(interactions)

# ---- Fit models to training data ----
# lm_mod <- 
#   linear_reg() |> 
#   set_engine("lm")

# Set up a lasso Poisson regression
poisson_mod <- 
  poisson_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")

# Define a regularized regression and explicitly leave the tuning parameters
# empty for later tuning.
# glmnet_mod <-
#   linear_reg(penalty = tune::tune(), mixture = tune::tune()) |> 
#   set_engine("glmnet")

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
    resamples = ukraine_val,
    grid = reg_grid,
    control = control_grid(save_pred = TRUE, verbose = T),
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

# Find best tuned model
res <-
  poisson_wflow |> 
  tune_grid(
    resamples = ukraine_cv,
    grid = 10,
    metrics = yardstick::metric_set(yardstick::rmse)
  )


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

reg_res |> 
  collect_predictions()

# reg_res |> 
#   predict(new_data = bake(model_recipe |> prep(), ukraine_test)) |> 
#   bind_cols(ukraine_test) |> 
#   select(`% at risk of homelessness`, .pred) |> 
#   rmse(`% at risk of homelessness`, .pred)

ukraine_test |> 
  select(`% at risk of homelessness`) |> 
  bind_cols(predict(reg_res, ukraine_test)) |> 
  rmse(`% at risk of homelessness`, .pred)

ukraine_predictions <- 
  ukraine_test_future |> 
  select(ltla21_code, `% at risk of homelessness`) |> 
  bind_cols(predict(reg_res, ukraine_test_future))

ukraine_predictions |> 
  ggplot(aes(x = `% at risk of homelessness`, y = .pred)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, .3)) +
  scale_y_continuous(limits = c(0, .3))

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
  geom_point() +
  geom_smooth(method = "lm")
