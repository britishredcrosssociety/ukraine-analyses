library(tidyverse)
library(tidymodels)

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

ukraine_cv <- vfold_cv(ukraine_train)

# Also use homelessness data from the end of July for out-of-sample testing
ukraine_test_future <- 
  homelessness_feb_aug |> 
  select(ltla21_code = lad_code, `% at risk of homelessness`) |> 
  left_join(england_indicators)

# ---- Preprocess data ----
model_recipe <- 
  recipe(
    `% at risk of homelessness` ~ `Households assessed as threatened with homelessness per (000s)` +
      `Households assessed as homeless per (000s)` +
      `Households in temporary accommodation per 1,000` +
      `Households on housing waiting list per 1,000` +
      `Social housing stock as a proportion of all households` +
      `Vacant dwellings per 1,000 units of social housing stock`,
    data = ukraine_train
  ) |> 
  step_normalize()

# ---- Fit models to training data ----
lm_mod <- 
  linear_reg() |> 
  set_engine("lm")

# Define a regularized regression and explicitly leave the tuning parameters
# empty for later tuning.
glmnet_mod <-
  linear_reg(penalty = tune::tune(), mixture = tune::tune()) |> 
  set_engine("glmnet")

# Construct a workflow combining the recipe and models
lm_wflow <-
  workflow() |> 
  add_recipe(model_recipe) |> 
  add_model(glmnet_mod)

# Find best tuned model
res <-
  lm_wflow |> 
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
  lm_wflow |> 
  finalize_workflow(best_params) |> 
  fit(data = ukraine_train)

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
