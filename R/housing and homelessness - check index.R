library(tidyverse)
library(broom)
library(IMD)

homelessness_trends <- read_csv("data/homelessness/ukraine-homelessness-trends.csv")
england_index <- read_csv("output-data/index-of-housing-insecurity/housing-index-england.csv")

# Ukraine homelessness rates and our index of housing insecurity
homelessness_trends |> 
  left_join(england_index, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_code, lad_name, starts_with("percent"), ends_with("rank")) |> 
  select(-percent_delta) |> 
  rename(`1 June` = percent_june, `3 July` = percent_july, `29 July` = percent_aug) |> 
  pivot_longer(cols = contains(" "), names_to = "month", values_to = "percent") |> 
  mutate(month = factor(month, levels = c("1 June", "3 July", "29 July"))) |> 
  
  ggplot(aes(x = housing_and_homelessness_composite_rank, y = percent)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", colour = "red", fill = "red") +
  facet_wrap(~month) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  theme_classic() +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = "The index of housing insecurity predicts rates of Ukrainian homelessness",
    x = "Housing insecurity rank (higher rank = greater insecurity)",
    y = "Percent of Ukrainian households at risk of homelessness"
  )

ggsave("images/homelessness - composite index predicts homelessness.png", width = 175, height = 130, units = "mm")

# Ukraine homelessness (counts) and our index of housing insecurity
homelessness_trends |> 
  left_join(england_index, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_code, lad_name, starts_with("total"), contains("composite")) |> 
  select(-total_delta) |> 
  rename(`1 June` = total_june, `3 July` = total_july, `29 July` = total_aug) |> 
  pivot_longer(cols = contains(" "), names_to = "month", values_to = "count") |> 
  mutate(month = factor(month, levels = c("1 June", "3 July", "29 July"))) |> 
  
  ggplot(aes(x = housing_and_homelessness_composite_rank, y = count)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", colour = "red", fill = "red") +
  facet_wrap(~month) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = rel(1))
  ) + 
  labs(
    title = "More Ukrainian families are at risk of homelessness in Local Authorities with greater housing insecurity",
    x = "Housing insecurity rank (higher rank = greater insecurity)",
    y = "Number of Ukrainian households at risk of homelessness",
    caption = "Source: British Red Cross Index of Housing Insecurity, and DLUHC data"
  )

ggsave("images/homelessness - composite index predicts homelessness - counts.png", width = 183, height = 130, units = "mm")

# Plot against index deciles
homelessness_trends |> 
  left_join(england_index, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_code, lad_name, starts_with("total"), contains("composite")) |> 
  select(-total_delta) |> 
  rename(`1 June` = total_june, `3 July` = total_july, `29 July` = total_aug) |> 
  pivot_longer(cols = contains(" "), names_to = "month", values_to = "count") |> 
  mutate(month = factor(month, levels = c("1 June", "3 July", "29 July"))) |> 
  
  group_by(month, housing_and_homelessness_composite_quantiles) |> 
  summarise(count = sum(count)) |> 
  
  ggplot(aes(x = factor(housing_and_homelessness_composite_quantiles), y = count)) +
  geom_col() +
  facet_wrap(~month)

# ---- Is the index of housing insecurity just recapitulating the index of multiple deprivation? ----
ihs_imd <- 
  england_index |> 
  
  left_join(IMD::imd_england_lad, by = c("ltla21_code" = "lad_code")) |> 
  left_join(IMD::imd_england_lad_subdomains, by = c("ltla21_code" = "lad_code")) |> 
  
  # Calculate ranks
  mutate(
    IMD_score_rank = rank(Score),
    IMD_proportion_rank = rank(Proportion), 
    Housing_and_Access_Score_rank = rank(Housing_and_Access_Score), 
    Housing_and_Access_Proportion_rank = rank(Housing_and_Access_Proportion),
    Wider_barriers_Proportion_rank = rank(Wider_barriers_Proportion),
    Wider_barriers_Extent_rank = rank(Wider_barriers_Extent)
  )

ihs_imd |> 
  select(ltla21_code, housing_and_homelessness_composite_rank, IMD_score_rank:Wider_barriers_Extent_rank) |> 
  pivot_longer(cols = IMD_score_rank:Wider_barriers_Extent_rank, names_to = "IMD", values_to = "rank") |> 

  ggplot(aes(x = housing_and_homelessness_composite_rank, y = rank)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~IMD)

#--> Not really. There's a strong-ish correlation with population-weighted scores but very messy relationship.
#--> And no association with the IMD's "housing and access" domain.

# ---- Compare our index and the IMD's Housing and Access domain at predicting Ukrainian homelessness ----
ihs_imd_homelessness <- 
  ihs_imd |> 
  select(
    ltla21_code, 
    ltla21_name,
    
    homeless_threatened = `Households assessed as threatened with homelessness per (000s)`, 
    homeless = `Households assessed as homeless per (000s)`, 
    temp_accom = `Households in temporary accommodation per 1,000`, 
    waiting = `Households on housing waiting list per 1,000`, 
    housing_stock = `Social housing stock as a proportion of all households`, 
    vacancies = `Vacant dwellings per 1,000 units of social housing stock`,
    
    `Index of Housing Insecurity` = housing_and_homelessness_composite_rank, 
    `IMD` = IMD_score_rank,
    `IMD Housing and Access domain` = Housing_and_Access_Score_rank,
    `IMD Wider Barriers subdomain` = Wider_barriers_Extent_rank
  ) |> 
  left_join(homelessness_trends |> select(ltla21_code = lad_code, total_aug, total_july, total_june))

ihs_imd_homelessness |> 
  pivot_longer(cols = c(`Index of Housing Insecurity`, `IMD Housing and Access domain`, `IMD Wider Barriers subdomain`), values_to = "rank") |> 
  
  ggplot(aes(x = rank, y = total_aug)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name)

# Which combination of housing insecurity indicators best predicts number of Ukrainian households at risk of homelessness?
homelessness_indicators <- 
  ihs_imd |> 
  select(
    ltla21_code,
    homeless_threatened = `Households assessed as threatened with homelessness per (000s)`, 
    homeless = `Households assessed as homeless per (000s)`, 
    temp_accom = `Households in temporary accommodation per 1,000`, 
    waiting = `Households on housing waiting list per 1,000`, 
    housing_stock = `Social housing stock as a proportion of all households`, 
    vacancies = `Vacant dwellings per 1,000 units of social housing stock`
  ) |> 
  left_join(homelessness_trends |> select(ltla21_code = lad_code, total_aug)) |> 
  select(-ltla21_code) |> 
  na.omit()

mod_null <- glm(total_aug ~ 1, data = homelessness_indicators, family = "poisson")
mod_full <- glm(total_aug ~ ., data = homelessness_indicators, family = "poisson")

mod_null <- lm(percent_aug ~ 1, data = homelessness_indicators)
mod_full <- lm(percent_aug ~ ., data = homelessness_indicators)

# Forward stepwise regression
forward <- step(mod_null, direction = "forward", scope = formula(mod_full), trace = 0)

forward$anova
forward$coefficients

#--> The model with rates of households assessed as homeless + households in temporary accommodation gives the best fit

# Compare models to see which index better fits observed risks of homelessness
bind_rows(
  glance(lm(total_aug ~ `Index of Housing Insecurity`, data = ihs_imd_homelessness)) |> mutate(predictor = "Index of Housing Insecurity") |> relocate(predictor),
  glance(lm(total_aug ~ IMD, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD") |> relocate(predictor),
  glance(lm(total_aug ~ `IMD Housing and Access domain`, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD Housing and Access domain") |> relocate(predictor),
  glance(lm(total_aug ~ `IMD Wider Barriers subdomain`, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD Wider Barriers subdomain") |> relocate(predictor),
  glance(lm(total_aug ~ temp_accom + homeless, data = ihs_imd_homelessness)) |> mutate(predictor = "Homelessness + temp accomm") |> relocate(predictor)
) |> 
  arrange(AIC)

bind_rows(
  glance(glm(total_aug ~ `Index of Housing Insecurity`, data = ihs_imd_homelessness, family = "poisson")) |> mutate(predictor = "Index of Housing Insecurity") |> relocate(predictor),
  glance(glm(total_aug ~ IMD, data = ihs_imd_homelessness, family = "poisson")) |> mutate(predictor = "IMD") |> relocate(predictor),
  glance(glm(total_aug ~ `IMD Housing and Access domain`, data = ihs_imd_homelessness, family = "poisson")) |> mutate(predictor = "IMD Housing and Access domain") |> relocate(predictor),
  glance(glm(total_aug ~ `IMD Wider Barriers subdomain`, data = ihs_imd_homelessness, family = "poisson")) |> mutate(predictor = "IMD Wider Barriers subdomain") |> relocate(predictor),
  glance(glm(total_aug ~ homeless, data = ihs_imd_homelessness, family = "poisson")) |> mutate(predictor = "Homelessness") |> relocate(predictor),
  glance(glm(total_aug ~ homeless + homeless_threatened, data = ihs_imd_homelessness, family = "poisson")) |> mutate(predictor = "Homelessness + threatened") |> relocate(predictor),
  glance(glm(total_aug ~ temp_accom + homeless, data = ihs_imd_homelessness, family = "poisson")) |> mutate(predictor = "Homelessness + temp accomm") |> relocate(predictor),
  glance(glm(total_aug ~ temp_accom + homeless + homeless_threatened + waiting + housing_stock + vacancies, data = ihs_imd_homelessness, family = "poisson")) |> mutate(predictor = "Full model") |> relocate(predictor)
) |> 
  arrange(AIC) |> 
  mutate(delta = lag(AIC))

# Check against historical data
bind_rows(
  glance(lm(total_july ~ `Index of Housing Insecurity`, data = ihs_imd_homelessness)) |> mutate(predictor = "Index of Housing Insecurity") |> relocate(predictor),
  glance(lm(total_july ~ IMD, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD") |> relocate(predictor),
  glance(lm(total_july ~ `IMD Housing and Access domain`, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD Housing and Access domain") |> relocate(predictor),
  glance(lm(total_july ~ `IMD Wider Barriers subdomain`, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD Wider Barriers subdomain") |> relocate(predictor),
  glance(lm(total_july ~ temp_accom + homeless, data = ihs_imd_homelessness)) |> mutate(predictor = "Homelessness + temp accomm") |> relocate(predictor)
) |> 
  arrange(AIC)

bind_rows(
  glance(lm(total_june ~ `Index of Housing Insecurity`, data = ihs_imd_homelessness)) |> mutate(predictor = "Index of Housing Insecurity") |> relocate(predictor),
  glance(lm(total_june ~ IMD, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD") |> relocate(predictor),
  glance(lm(total_june ~ `IMD Housing and Access domain`, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD Housing and Access domain") |> relocate(predictor),
  glance(lm(total_june ~ `IMD Wider Barriers subdomain`, data = ihs_imd_homelessness)) |> mutate(predictor = "IMD Wider Barriers subdomain") |> relocate(predictor),
  glance(lm(total_june ~ temp_accom + homeless, data = ihs_imd_homelessness)) |> mutate(predictor = "Homelessness + temp accomm") |> relocate(predictor)
) |> 
  arrange(AIC)

#--> Same findings in all cases: 
#--> - A statistical index predicting Ukraine homelessness based on wider homelessness + temporary accommodation rates gives the best fit
#--> - wider barriers subdomain is the next best fit, followed by Housing and Access domain
#--> - Our composite housing insecurity index fits the data better than the overall IMD, but isn't a great predictor of Ukraine homelessness
#--> - So, it'd be better to generalise from a model of homelessness + temp accomm, rather than use the composite index, to predict homelessness in the devolved nations.

# ---- Try a multivariate adaptive regression spline ----
library(vip)
library(earth)

# Fit a MARS model
poisson_mars <- earth(total_aug ~ ., data = homelessness_indicators, degree = 2, pmethod = "exhaustive", glm = list(family="poisson"))

# Variable importance
vip::vi(poisson_mars)
vip(poisson_mars)
