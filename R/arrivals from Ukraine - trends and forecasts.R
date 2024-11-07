# This is a deterministic simulation that models weekly arrivals based on:
# 1. A backlog of issued visas that results in arrivals
# 2. A backlog of applications resulting in visas being issued
# 3. New applications being submitted
#
# Applications --> Visa issued --> Arrive in the UK
#
# See this drawing for a visual explanation of the sim: https://docs.google.com/drawings/d/e/2PACX-1vQe4Vk2vEkw0R8hj1SbXIxxhG8oTI0kPaavDnK06okRRddhNxuKGvov-HW__8-0HZiWCs0Hwti0Kqxt/pub?w=1522&h=467
#
# ASSUMPTIONS:
# - We'll assume the Government super sponsor schemes won't re-open, so no new applications,
#   but they still have a backlog of issued visas where the people are yet to arrive - let's assume half do.
#
library(tidyverse)
library(lubridate)
library(patchwork)
library(broom)
library(earth)
library(zoo)

####
####
# Load and process data ----
####
####

## Load DLUHC data ----
source("R/load Ukraine visa data - Local Authorities.R")
source("R/load Ukraine visa data - scraped.R")

## Make a dataframe containing weekly visa data ----
# Containing weekly applications, visas issued, and arrivals for:
# - Family Scheme (from scraped data)
# - Homes for Ukraine / individual sponsorship (from LA-level data)
# - Super sponsor / government sponsorship (from LA-level data)

weekly_family_scheme_visas <-
  visas_scraped |>
  arrange(Date) |>
  filter(str_detect(Stage, "arrival|visas issued|visa applications received")) |>
  filter(Scheme == "Ukraine Family Scheme") |>
  select(-Date, -Visas) |>
  pivot_wider(names_from = Stage, values_from = Visas_imputed, values_fn = mean) |>
  # Calculate week-on-week changes
  mutate(
    `Weekly applications` = `visa applications received` - lag(`visa applications received`),
    `Weekly visas issued` = `visas issued` - lag(`visas issued`),
    `Weekly arrivals` = `arrivals of visa-holders in the UK` - lag(`arrivals of visa-holders in the UK`)
  ) |>
  select(Week, Scheme, starts_with("Weekly")) |> 
  # Turn NAs into zeros
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

weekly_sponsorship_scheme_visas <-
  visas_ltla21_summary |>
  arrange(Date) |>
  filter(str_detect(Type, "^Sponsored")) |>
  mutate(Scheme = if_else(str_detect(Type, "Government"), "Government 'super sponsored'", Type)) |>
  # Calculate UK weekly totals
  mutate(
    Week = week(Date) + (52 * (year(Date) - 2022))
  ) |>
  group_by(Week, Scheme) |>
  summarise(
    `Number of visa applications` = sum(`Number of visa applications`),
    `Number of visas issued` = sum(`Number of visas issued`),
    `Number of arrivals` = sum(`Number of arrivals in the UK by sponsor location`)
  ) |>
  ungroup() |>
  # Calculate week-on-week changes
  group_by(Scheme) |>
  mutate(
    `Weekly applications` = `Number of visa applications` - lag(`Number of visa applications`),
    `Weekly visas issued` = `Number of visas issued` - lag(`Number of visas issued`),
    `Weekly arrivals` = `Number of arrivals` - lag(`Number of arrivals`)
  ) |>
  ungroup() |>
  select(Week, Scheme, starts_with("Weekly")) |> 
  # Turn NAs into zeros
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

# Since March 2024, the LA-level data has been published fortnightly rather than weekly
# This creates gaps when forecasting, so create a dataset with the full set of weeks using `expand_grid()`,
# then impute the 'missing' weeks
weekly_sponsorship_scheme_visas <- 
  expand_grid(
    Week = min(weekly_sponsorship_scheme_visas$Week):max(visas_scraped$Week),
    Scheme = unique(weekly_sponsorship_scheme_visas$Scheme)
  ) |> 
  left_join(weekly_sponsorship_scheme_visas) |> 
  
  # Impute missing weeks
  group_by(Scheme) |>
  mutate(
    `Weekly applications` = zoo::na.approx(`Weekly applications`, na.rm = FALSE),
    `Weekly visas issued` = zoo::na.approx(`Weekly visas issued`, na.rm = FALSE),
    `Weekly arrivals` = zoo::na.approx(`Weekly arrivals`, na.rm = FALSE)
  ) |> 
  
  # Fill in any remaining NAs - which will be the latest weeks (and, so, couldn't have been imputed)
  fill(starts_with("Weekly"), .direction = "down") |> 
  
  ungroup()

weekly_visas_by_scheme <- bind_rows(weekly_sponsorship_scheme_visas, weekly_family_scheme_visas) |>
  arrange(Week, Scheme) |>
  mutate(
    `% applications --> issued` = `Weekly visas issued` / `Weekly applications`,
    `% issued --> arrivals` = `Weekly arrivals` / `Weekly visas issued`
  ) |>
  # Convert week number to date; source: https://stackoverflow.com/a/46183403
  mutate(Date = ymd("2022-01-03") + weeks(Week - 1)) |>
  relocate(Date) |> 
  
  # Fix NaNs and Infs
  mutate(
    `% applications --> issued` = if_else(
      is.na(`% applications --> issued`) | is.infinite(`% applications --> issued`) | is.null(`% applications --> issued`) | is.nan(`% applications --> issued`),
      0,
      `% applications --> issued`
    ),
    
    `% issued --> arrivals` = if_else(
      is.na(`% issued --> arrivals`) | is.infinite(`% issued --> arrivals`) | is.null(`% issued --> arrivals`) | is.nan(`% issued --> arrivals`),
      0,
      `% issued --> arrivals`
    )
  )

## Make a dataframe containing cumulative visa data ----
# Containing total applications, visas issued, and arrivals for:
# - Family Scheme (from scraped data)
# - Homes for Ukraine / individual sponsorship (from LA-level data)
# - Super sponsor / government sponsorship (from LA-level data)

cumulative_family_scheme_visas <-
  visas_scraped |>
  arrange(Date) |>
  filter(str_detect(Stage, "arrival|visas issued|visa applications received")) |>
  filter(Scheme == "Ukraine Family Scheme") |>
  select(-Date, -Visas) |>
  pivot_wider(names_from = Stage, values_from = Visas_imputed, values_fn = mean) |>
  rename(
    `Number of visa applications` = `visa applications received`,
    `Number of visas issued` = `visas issued`,
    `Number of arrivals` = `arrivals of visa-holders in the UK`,
  ) |>
  mutate(`Number of arrivals` = case_when(row_number() == 127 ~ 58400.00,
                                          row_number() == 115 ~ 57900.00,
                                          TRUE ~ `Number of arrivals`))

cumulative_sponsorship_scheme_visas <-
  visas_ltla21_summary |>
  arrange(Date) |>
  filter(str_detect(Type, "^Sponsored")) |>
  mutate(Scheme = if_else(str_detect(Type, "Government"), "Government 'super sponsored'", Type)) |>
  # Calculate UK weekly totals
  mutate(
    Week = week(Date) + (52 * (year(Date) - 2022))
  ) |>
  group_by(Week, Scheme) |>
  summarise(
    `Number of visa applications` = sum(`Number of visa applications`),
    `Number of visas issued` = sum(`Number of visas issued`),
    `Number of arrivals` = sum(`Number of arrivals in the UK by sponsor location`)
  ) |>
  ungroup()

# Since March 2024, the LA-level data has been published fortnightly rather than weekly
# This creates gaps when forecasting, so:
# 1. Split the dataset into separate tibbles for weekly and fortnightly data
# 2. For the fortnightly data, use `expand_grid()` to add rows for the 'missing' weeks, then impute values
#
# (Can't do this in one single block of data, unlike above, since there are too many missing values at the start of the dataset, meaning imputation fails)
cumulative_sponsorship_scheme_visas_weekly_data <- 
  cumulative_sponsorship_scheme_visas |> 
  filter(Week < 124)

cumulative_sponsorship_scheme_visas_fortnightly_data <- 
  expand_grid(
    Week = 124:max(visas_scraped$Week),  # use the latest week from `visas_scraped` so we have the latest week across all datasets
    Scheme = unique(cumulative_sponsorship_scheme_visas$Scheme)
  ) |> 
  left_join(cumulative_sponsorship_scheme_visas) |>
  
  # Impute missing weeks
  group_by(Scheme) |>
  mutate(
    `Number of visa applications` = zoo::na.approx(`Number of visa applications`, na.rm = FALSE),
    `Number of visas issued` = zoo::na.approx(`Number of visas issued`, na.rm = FALSE),
    `Number of arrivals` = zoo::na.approx(`Number of arrivals`, na.rm = FALSE)
  ) |> 
  
  # Fill in any remaining NAs - which will be the latest weeks (and, so, couldn't have been imputed)
  fill(starts_with("Number"), .direction = "down") |> 
  ungroup()

# Combine weekly and formerly-fortnightly data
cumulative_sponsorship_scheme_visas <- bind_rows(cumulative_sponsorship_scheme_visas_weekly_data, cumulative_sponsorship_scheme_visas_fortnightly_data)

cumulative_visas_by_scheme <- bind_rows(cumulative_family_scheme_visas, cumulative_sponsorship_scheme_visas) |>
  arrange(Week, Scheme) |>
  mutate(
    `% applications --> issued` = `Number of visas issued` / `Number of visa applications`,
    `% issued --> arrivals` = `Number of arrivals` / `Number of visas issued`
  ) |>
  # Convert week number to date; source: https://stackoverflow.com/a/46183403
  mutate(Date = ymd("2022-01-03") + weeks(Week - 1)) |>
  relocate(Date)

write_csv(cumulative_visas_by_scheme, glue::glue("data/cumulative-visas/cumulative-visas-{max(cumulative_visas_by_scheme$Date)}.csv"))

####
####
# Prepare simulation parameters ----
####
####

## Calculate historical weekly processing rates ----
historical_processing_rates <-
  visas_scraped |>
  select(-Visas, -Date) |>
  pivot_wider(names_from = Stage, values_from = Visas_imputed, values_fn = mean) |>
  group_by(Scheme) |>
  mutate(
    `New applications this week` = `visa applications received` - lag(`visa applications received`),
    `Additional visas issued this week` = `visas issued` - lag(`visas issued`),
    `Visas refused/withdrawn over the last week` = (`applications refused` + `applications withdrawn`) - (lag(`applications refused`) + lag(`applications withdrawn`)),
    `This week's application backlog` = lag(`applications awaiting conclusion`) + `New applications this week` - `Visas refused/withdrawn over the last week`,
    `New arrivals this week` = `arrivals of visa-holders in the UK` - lag(`arrivals of visa-holders in the UK`),
    `Issued visas yet to arrive` = `visas issued` - `arrivals of visa-holders in the UK`,
    `% backlog issued a visa this week` = `Additional visas issued this week` / `This week's application backlog`,
    `% issued visas arriving this week` = `New arrivals this week` / lag(`Issued visas yet to arrive`)
  ) |> 
  # Turn NAs into zeros
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

# Calculate weekly visa issued --> arrival rate for super sponsor and individual sponsor schemes
historical_processing_rates_sponsorship_schemes <-
  cumulative_sponsorship_scheme_visas |>
  group_by(Scheme) |>
  mutate(
    `New applications this week` = `Number of visa applications` - lag(`Number of visa applications`),
    `Additional visas issued this week` = `Number of visas issued` - lag(`Number of visas issued`),
    # `Visas refused/withdrawn over the last week` = (`applications refused` + `applications withdrawn`) - (lag(`applications refused`) + lag(`applications withdrawn`)),

    # `This week's application backlog` = lag(`applications awaiting conclusion`) + `New applications this week` - `Visas refused/withdrawn over the last week`,

    `New arrivals this week` = `Number of arrivals` - lag(`Number of arrivals`),
    `Issued visas yet to arrive` = `Number of visas issued` - `Number of arrivals`,

    # `% backlog issued a visa this week` = `Additional visas issued this week` / `This week's application backlog`,
    `% issued visas arriving this week` = `New arrivals this week` / lag(`Issued visas yet to arrive`)
  )

## Calculate rates of applications that get issued visas ----
historical_processing_rates |>
  group_by(Scheme) |>
  summarise(
    `Mean rate of applications issued visas` = mean(`% backlog issued a visa this week`, na.rm = TRUE),
    `Median rate of applications issued visas` = median(`% backlog issued a visa this week`, na.rm = TRUE),
    `Min rate of applications issued visas` = min(`% backlog issued a visa this week`, na.rm = TRUE),
    `Max rate of applications issued visas` = max(`% backlog issued a visa this week`, na.rm = TRUE)
  )

# The average conversion rates are pretty similar for family and homes schemes, so just take one rate for simplicity
mean_applications_issued_rate <- mean(historical_processing_rates$`% backlog issued a visa this week`, na.rm = TRUE)
min_applications_issued_rate <- max(0, min(historical_processing_rates$`% backlog issued a visa this week`, na.rm = TRUE))  # Don't let this go below zero
max_applications_issued_rate <- max(historical_processing_rates$`% backlog issued a visa this week`, na.rm = TRUE)

## Calculate arrival rates from the pool of visas issued ----
# historical_processing_rates |>
#   ggplot(aes(x = Week, y = `% issued visas arriving this week`)) +
#   geom_line(aes(colour = Scheme))

arrival_rates <-
  bind_rows(
    historical_processing_rates |> select(Week, Scheme, `% issued visas arriving this week`),
    historical_processing_rates_sponsorship_schemes |> select(Week, Scheme, `% issued visas arriving this week`)
  ) |>
  filter(Scheme != "Ukraine Sponsorship Scheme")

# historical_processing_rates |>
# group_by(Scheme) |>
# summarise(
#   `Mean arrival rate` = mean(`% issued visas arriving this week`, na.rm = TRUE),
#   `Min arrival rate` = min(`% issued visas arriving this week`, na.rm = TRUE),
#   `Max arrival rate` = max(`% issued visas arriving this week`, na.rm = TRUE)
# )

# mean_arrival_rate_homes_scheme <- arrival_rates |> filter(Scheme == "Ukraine Sponsorship Scheme") |> pull(`Mean arrival rate`)
# min_arrival_rate_homes_scheme <- arrival_rates |> filter(Scheme == "Ukraine Sponsorship Scheme") |> pull(`Min arrival rate`)
# max_arrival_rate_homes_scheme <- arrival_rates |> filter(Scheme == "Ukraine Sponsorship Scheme") |> pull(`Max arrival rate`)

# Maximum values for arrival rates can be the initial rates in the data
max_arrival_rate_family_scheme <- arrival_rates |>
  na.omit() |>
  filter(`% issued visas arriving this week` > 0) |> 
  filter(Scheme == "Ukraine Family Scheme") |>
  filter(Week == min(Week)) |>
  pull(`% issued visas arriving this week`)
max_arrival_rate_homes_scheme <- arrival_rates |>
  na.omit() |>
  filter(`% issued visas arriving this week` > 0) |> 
  filter(Scheme == "Sponsored by individuals") |>
  filter(Week == min(Week)) |>
  pull(`% issued visas arriving this week`)
max_arrival_rate_govt_scheme <- arrival_rates |>
  na.omit() |>
  filter(`% issued visas arriving this week` > 0) |> 
  filter(Scheme == "Government 'super sponsored'") |>
  filter(Week == min(Week)) |>
  pull(`% issued visas arriving this week`)

# Minimum arrival rates can be their most recent values
min_arrival_rate_family_scheme <- arrival_rates |>
  na.omit() |>
  filter(Scheme == "Ukraine Family Scheme") |>
  filter(Week == max(Week)) |>
  pull(`% issued visas arriving this week`)
min_arrival_rate_homes_scheme <- arrival_rates |>
  na.omit() |>
  filter(Scheme == "Sponsored by individuals") |>
  filter(Week == max(Week)) |>
  pull(`% issued visas arriving this week`)
min_arrival_rate_govt_scheme <- arrival_rates |>
  na.omit() |>
  filter(Scheme == "Government 'super sponsored'") |>
  filter(Week == max(Week)) |>
  pull(`% issued visas arriving this week`)

# Calculate mean arrival rates
arrival_rates_summary <-
  arrival_rates |>
  group_by(Scheme) |>
  summarise(
    `Mean arrival rate` = mean(`% issued visas arriving this week`, na.rm = TRUE),
    `Min arrival rate` = max(0, min(`% issued visas arriving this week`, na.rm = TRUE)),  # Don't let this go below zero
    `Max arrival rate` = max(`% issued visas arriving this week`, na.rm = TRUE)
  )

mean_arrival_rate_family_scheme <- arrival_rates_summary |>
  filter(Scheme == "Ukraine Family Scheme") |>
  pull(`Mean arrival rate`)
mean_arrival_rate_homes_scheme <- arrival_rates_summary |>
  filter(Scheme == "Sponsored by individuals") |>
  pull(`Mean arrival rate`)
mean_arrival_rate_govt_scheme <- arrival_rates_summary |>
  filter(Scheme == "Government 'super sponsored'") |>
  pull(`Mean arrival rate`)

# min_arrival_rate_homes_scheme  <- arrival_rates |> filter(Scheme == "Sponsored by individuals") |> pull(`Min arrival rate`)
# min_arrival_rate_family_scheme <- arrival_rates |> filter(Scheme == "Ukraine Family Scheme") |> pull(`Min arrival rate`)
# min_arrival_rate_govt_scheme  <- arrival_rates |> filter(Scheme == "Government 'super sponsored'") |> pull(`Min arrival rate`)

## Calculate visa refusal/withdrawal rates ----
refusal_withdrawal_rate_by_scheme <-
  visas_scraped |>
  filter(str_detect(Stage, "application")) |>
  select(-Visas) |>
  pivot_wider(names_from = Stage, values_from = Visas_imputed, values_fn = mean) |>
  mutate(
    `applications withdrawn` = replace_na(`applications withdrawn`, 0),
    `applications refused` = replace_na(`applications refused`, 0),
    `applications awaiting conclusion` = replace_na(`applications awaiting conclusion`, 0),
    `% withdrawn/refused` = (`applications withdrawn` + `applications refused`) / `visa applications received`,
    `% awaiting` = `applications awaiting conclusion` / `visa applications received`
  ) |>
  filter(`% withdrawn/refused` > 0) |>
  group_by(Scheme) |>
  summarise(`Mean withdrawal/refusal rate` = mean(`% withdrawn/refused`)) |>
  ungroup()

refusal_withdrawal_rate_homes_scheme <- refusal_withdrawal_rate_by_scheme |>
  filter(Scheme == "Ukraine Sponsorship Scheme") |>
  pull(`Mean withdrawal/refusal rate`)

refusal_withdrawal_rate_family_scheme <- refusal_withdrawal_rate_by_scheme |>
  filter(Scheme == "Ukraine Family Scheme") |>
  pull(`Mean withdrawal/refusal rate`)

####
####
# Simulation functions ----
####
####

## Function for initialising simulation data ----
init_sim <- function(sim_start_week, num_weeks_to_simulate) {
  tibble(
    Week = (sim_start_week + 1):(sim_start_week + num_weeks_to_simulate),
    `Backlog of visa applications - Homes for Ukraine` = NA,
    `Backlog of visa applications - Family Scheme` = NA,
    `Backlog of visa applications - Super Sponsor` = NA,
    `Backlog of visas issued - Homes for Ukraine` = NA,
    `Backlog of visas issued - Family Scheme` = NA,
    `Backlog of visas issued - Super Sponsor` = NA,
    `Backlog of visas issued - Homes for Ukraine (upper bound)` = NA,
    `Backlog of visas issued - Family Scheme (upper bound)` = NA,
    `Backlog of visas issued - Super Sponsor (upper bound)` = NA,
    `Backlog of visas issued - Homes for Ukraine (lower bound)` = NA,
    `Backlog of visas issued - Family Scheme (lower bound)` = NA,
    `Backlog of visas issued - Super Sponsor (lower bound)` = NA,
    `Weekly applications - Homes for Ukraine` = NA,
    `Weekly applications - Family Scheme` = NA,
    `Arrival rate - Homes for Ukraine` = NA,
    `Arrival rate - Family Scheme` = NA,
    `Arrival rate - Super Sponsor` = NA,
    `Arrival rate - Homes for Ukraine (upper bound)` = NA,
    `Arrival rate - Family Scheme (upper bound)` = NA,
    `Arrival rate - Super Sponsor (upper bound)` = NA,
    `Arrival rate - Homes for Ukraine (lower bound)` = NA,
    `Arrival rate - Family Scheme (lower bound)` = NA,
    `Arrival rate - Super Sponsor (lower bound)` = NA,
    `Weekly arrivals - Homes for Ukraine` = NA,
    `Weekly arrivals - Family Scheme` = NA,
    `Weekly arrivals - Super Sponsor` = NA,
    `Weekly arrivals - Homes for Ukraine (upper bound)` = NA,
    `Weekly arrivals - Family Scheme (upper bound)` = NA,
    `Weekly arrivals - Super Sponsor (upper bound)` = NA,
    `Weekly arrivals - Homes for Ukraine (lower bound)` = NA,
    `Weekly arrivals - Family Scheme (lower bound)` = NA,
    `Weekly arrivals - Super Sponsor (lower bound)` = NA,
    `Weekly arrivals` = NA,
    `Weekly arrivals (upper bound)` = NA,
    `Weekly arrivals (lower bound)` = NA,
    `Total arrivals` = NA,
    `Total arrivals (upper bound)` = NA,
    `Total arrivals (lower bound)` = NA
  )
}

## The simulation ----
run_visa_simulation <- function(simulated_visas) {
  for (week in (min(simulated_visas$Week) + 1):max(simulated_visas$Week)) {
    # New arrivals this week, from last week's pool of issued visas - central estimate, based on predicted arrival rate for each scheme
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Homes for Ukraine`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Family Scheme`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Super Sponsor`, 0)

    # ... calculate lower bound based on predicted arrival rate for each scheme
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Homes for Ukraine (upper bound)`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme (upper bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Family Scheme (upper bound)`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor (upper bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Super Sponsor (upper bound)`, 0)

    # ... calculate upper bound based on predicted arrival rate for each scheme
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (lower bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Homes for Ukraine (lower bound)`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (lower bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme (lower bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Family Scheme (lower bound)`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (lower bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor (lower bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Super Sponsor (lower bound)`, 0)

    # # New arrivals this week, from last week's pool of issued visas - central estimate, based on mean arrival rate for each scheme
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine` * mean_arrival_rate_homes_scheme, 0)
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme` * mean_arrival_rate_family_scheme, 0)
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor` * mean_arrival_rate_govt_scheme, 0)
    #
    # #... calculate lower bound based on historical minimum arrival rate for each scheme
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` * max_arrival_rate_homes_scheme, 0)
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme (upper bound)` * max_arrival_rate_family_scheme, 0)
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor (upper bound)` * max_arrival_rate_govt_scheme, 0)
    #
    # #... calculate upper bound based on historical maximum arrival rate for each scheme
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (lower bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` * min_arrival_rate_homes_scheme, 0)
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (lower bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme (lower bound)` * min_arrival_rate_family_scheme, 0)
    # simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (lower bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor (lower bound)` * min_arrival_rate_govt_scheme, 0)
    #
    # Make sure numbers of arrivals don't go below zero
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine`)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme`)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor`)

    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (lower bound)`)

    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (upper bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (upper bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (upper bound)`)

    # Total new arrivals this week
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals` <-
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine` +
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme` +
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor`

    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (lower bound)` <-
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (lower bound)` +
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (lower bound)` +
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (lower bound)`

    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (upper bound)` <-
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (upper bound)` +
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (upper bound)` +
      simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (upper bound)`

    # Remove these arrivals from the visa backlog
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine`
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme`
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor`

    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (upper bound)`
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (lower bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme (lower bound)` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (upper bound)`
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (lower bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor (lower bound)` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (upper bound)`

    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (lower bound)`
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (upper bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme (upper bound)` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (lower bound)`
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (upper bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor (upper bound)` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (lower bound)`

    # Make sure visa backlog doesn't go below zero
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor`)

    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (lower bound)`)

    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (upper bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (upper bound)`)

    # Calculate new applications for each scheme
    # (No new applications for super sponsor scheme)
    new_applications_homes_scheme <-
      simulated_visas |>
      filter(Week == (week)) |>
      pull(`Weekly applications - Homes for Ukraine`)

    new_applications_family_scheme <-
      simulated_visas |>
      filter(Week == (week)) |>
      pull(`Weekly applications - Family Scheme`)

    # Make sure the number of new applications don't go below zero
    # new_applications_homes_scheme <- ifelse(new_applications_homes_scheme < 0, 0, new_applications_homes_scheme)
    # new_applications_family_scheme <- ifelse(new_applications_family_scheme < 0, 0, new_applications_family_scheme)

    # Some of these applications will be withdrawn/refused
    new_applications_homes_scheme <- round(new_applications_homes_scheme * (1 - refusal_withdrawal_rate_homes_scheme), 0)
    new_applications_family_scheme <- round(new_applications_family_scheme * (1 - refusal_withdrawal_rate_family_scheme), 0)

    # Add the remaining applications to the backlog
    simulated_visas[simulated_visas$Week == week, ]$`Weekly applications - Homes for Ukraine` <- new_applications_homes_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Weekly applications - Family Scheme` <- new_applications_family_scheme

    # Convert a proportion of last week's applications backlog into issued visas
    applications_issued_homes_scheme <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Homes for Ukraine` * mean_applications_issued_rate, 0)
    applications_issued_family_scheme <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Family Scheme` * mean_applications_issued_rate, 0)
    applications_issued_super_sponsor <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Super Sponsor` * mean_applications_issued_rate, 0)

    # Add them to the visa backlogs
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` + applications_issued_homes_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme` + applications_issued_family_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor` + applications_issued_super_sponsor

    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` + applications_issued_homes_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (lower bound)` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (lower bound)` + applications_issued_family_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (lower bound)` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (lower bound)` + applications_issued_super_sponsor

    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` + applications_issued_homes_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (upper bound)` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (upper bound)` + applications_issued_family_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (upper bound)` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (upper bound)` + applications_issued_super_sponsor

    # Make sure visa backlog doesn't go below zero
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor`)

    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (lower bound)`)

    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Family Scheme (upper bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Super Sponsor (upper bound)`)

    # Remove the newly issued visas from the application backlogs and add the new applications
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visa applications - Homes for Ukraine` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Homes for Ukraine` - applications_issued_homes_scheme + new_applications_homes_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visa applications - Family Scheme` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Family Scheme` - applications_issued_family_scheme + new_applications_family_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visa applications - Super Sponsor` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Super Sponsor` - applications_issued_super_sponsor

    # Track total arrivals
    simulated_visas[simulated_visas$Week == week, ]$`Total arrivals` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Total arrivals` + simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals`
    simulated_visas[simulated_visas$Week == week, ]$`Total arrivals (lower bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Total arrivals (lower bound)` + simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (lower bound)`
    simulated_visas[simulated_visas$Week == week, ]$`Total arrivals (upper bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Total arrivals (upper bound)` + simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (upper bound)`
  }

  simulated_visas <-
    simulated_visas |>
    # Don't need the first week - that was the seed data
    filter(Week > min(Week)) |>
    # Convert week number to date; source: https://stackoverflow.com/a/46183403
    mutate(Date = ymd("2022-01-03") + weeks(Week - 1)) |>
    relocate(Date)

  simulated_visas
}


####
####
# Baseline scenarios for arrivals ####
# This scenario assumes the following:
# - New visa applications submitted each week will follow the (declining) historical trend
# - Weekly arrival rates will following the declining historical trend
####
####

## Choose week numbers that the sim will run from and to ----
# (must be a week for which we already have DLUHC data; most likely, this will be the most recent week)
sim_start_week <-
  weekly_visas_by_scheme |>
  filter(Week == max(Week)) |>
  distinct(Week) |>
  pull(Week)

num_weeks_to_simulate <- 4 * 6 # the next six months

### Testing the simulation against historical data ----
# Use the code in this section to check predicted arrivals against observed data
# sim_start_week <- 20  # This can go as far back as week 20; before that, we don't have enough data to initialise the simulation

# Get number of weeks to simulate based on start and end dates in the observed data
# num_weeks_to_simulate <-
#   weekly_visas_by_scheme |>
#   summarise(n_weeks = max(Week) - sim_start_week) |>
#   pull(n_weeks)

## Get the most recent backlogs in each visa scheme for the simulation start week ----
visa_backlogs <-
  cumulative_visas_by_scheme |>
  filter(Week == sim_start_week) |>
  mutate(
    `Backlog of visas issued but not arrived` = `Number of visas issued` - `Number of arrivals`,
    `Backlog of visa applications but not issued` = `Number of visa applications` - `Number of visas issued`
  ) |>
  select(Scheme, starts_with("Backlog"))

## Get total number of arrivals from the point at which the simulation started ----
total_arrivals <-
  cumulative_visas_by_scheme |>
  filter(Week == sim_start_week) |>
  ungroup() |>  # Just in case
  summarise(`Total arrivals` = sum(`Number of arrivals`)) |>
  pull(`Total arrivals`)

## Set up a tibble for the simulated data ----
simulated_visas_baseline <-
  init_sim(sim_start_week, num_weeks_to_simulate) |>
  # Add most recent backlogs
  add_row(
    Week = sim_start_week,
    `Backlog of visa applications - Homes for Ukraine` = visa_backlogs |> filter(Scheme == "Sponsored by individuals") |> pull(`Backlog of visa applications but not issued`),
    `Backlog of visa applications - Family Scheme` = visa_backlogs |> filter(Scheme == "Ukraine Family Scheme") |> pull(`Backlog of visa applications but not issued`),
    `Backlog of visa applications - Super Sponsor` = visa_backlogs |> filter(Scheme == "Government 'super sponsored'") |> pull(`Backlog of visa applications but not issued`),
    `Backlog of visas issued - Homes for Ukraine` = visa_backlogs |> filter(Scheme == "Sponsored by individuals") |> pull(`Backlog of visas issued but not arrived`),
    `Backlog of visas issued - Family Scheme` = visa_backlogs |> filter(Scheme == "Ukraine Family Scheme") |> pull(`Backlog of visas issued but not arrived`),
    `Backlog of visas issued - Super Sponsor` = visa_backlogs |> filter(Scheme == "Government 'super sponsored'") |> pull(`Backlog of visas issued but not arrived`),
    `Backlog of visas issued - Homes for Ukraine (upper bound)` = visa_backlogs |> filter(Scheme == "Sponsored by individuals") |> pull(`Backlog of visas issued but not arrived`),
    `Backlog of visas issued - Family Scheme (upper bound)` = visa_backlogs |> filter(Scheme == "Ukraine Family Scheme") |> pull(`Backlog of visas issued but not arrived`),
    `Backlog of visas issued - Super Sponsor (upper bound)` = visa_backlogs |> filter(Scheme == "Government 'super sponsored'") |> pull(`Backlog of visas issued but not arrived`),
    `Backlog of visas issued - Homes for Ukraine (lower bound)` = visa_backlogs |> filter(Scheme == "Sponsored by individuals") |> pull(`Backlog of visas issued but not arrived`),
    `Backlog of visas issued - Family Scheme (lower bound)` = visa_backlogs |> filter(Scheme == "Ukraine Family Scheme") |> pull(`Backlog of visas issued but not arrived`),
    `Backlog of visas issued - Super Sponsor (lower bound)` = visa_backlogs |> filter(Scheme == "Government 'super sponsored'") |> pull(`Backlog of visas issued but not arrived`),

    # Get most recent weekly application figures
    `Weekly applications - Homes for Ukraine` = weekly_visas_by_scheme |> filter(Week == sim_start_week & Scheme == "Sponsored by individuals") |> pull(`Weekly applications`),
    `Weekly applications - Family Scheme` = weekly_visas_by_scheme |> filter(Week == sim_start_week & Scheme == "Ukraine Family Scheme") |> pull(`Weekly applications`),
    `Arrival rate - Homes for Ukraine` = NA,
    `Arrival rate - Family Scheme` = NA,
    `Arrival rate - Super Sponsor` = NA,
    `Arrival rate - Homes for Ukraine (upper bound)` = NA,
    `Arrival rate - Family Scheme (upper bound)` = NA,
    `Arrival rate - Super Sponsor (upper bound)` = NA,
    `Arrival rate - Homes for Ukraine (lower bound)` = NA,
    `Arrival rate - Family Scheme (lower bound)` = NA,
    `Arrival rate - Super Sponsor (lower bound)` = NA,
    `Weekly arrivals - Homes for Ukraine` = NA,
    `Weekly arrivals - Family Scheme` = NA,
    `Weekly arrivals - Super Sponsor` = NA,
    `Weekly arrivals - Homes for Ukraine (upper bound)` = NA,
    `Weekly arrivals - Family Scheme (upper bound)` = NA,
    `Weekly arrivals - Super Sponsor (upper bound)` = NA,
    `Weekly arrivals - Homes for Ukraine (lower bound)` = NA,
    `Weekly arrivals - Family Scheme (lower bound)` = NA,
    `Weekly arrivals - Super Sponsor (lower bound)` = NA,
    `Weekly arrivals` = NA,
    `Weekly arrivals (upper bound)` = NA,
    `Weekly arrivals (lower bound)` = NA,
    `Total arrivals` = total_arrivals,
    `Total arrivals (upper bound)` = total_arrivals,
    `Total arrivals (lower bound)` = total_arrivals
  ) |>
  arrange(Week)

## Calculate trend lines for weekly applications in Homes and Family schemes ----
# !! This was the original approach I took, which calculated (negative) trend lines for numbers of new applications,
# !! which were used during the sim to iteratively calculate each week's number of new applications.
# !! However, I want to include a polynomial function for calculating new applications in the 'winter surge' scenario,
# !! so it's easier to predict application numbers before the simulation rather than iteratively during the sim

# trendline_homes_scheme <-
#   lm(`Weekly applications` ~ Week, data = weekly_visas_by_scheme |> filter(Scheme == "Sponsored by individuals")) |>
#   broom::tidy() |>
#   filter(term == "Week") |>
#   pull(estimate)
#
# trendline_family_scheme <-
#   lm(`Weekly applications` ~ Week, data = weekly_visas_by_scheme |> filter(Scheme == "Ukraine Family Scheme")) |>
#   broom::tidy() |>
#   filter(term == "Week") |>
#   pull(estimate)

## Predict number of new visa applications each week ----
# Set up a tibble containing the week numbers for which we want to predict numbers of applications
predicted_weeks <-
  tibble(Week = (sim_start_week + 1):(sim_start_week + num_weeks_to_simulate))

# Predict number of applications for the individual sponsorship scheme
predicted_applications_homes_scheme <-
  lm(`Weekly applications` ~ Week, data = weekly_visas_by_scheme |> filter(Scheme == "Sponsored by individuals")) |>
  broom::augment(newdata = predicted_weeks) |>
  mutate(.fitted = if_else(.fitted < 0, 0, .fitted))

# mod_applications_homes_scheme <- earth(`Weekly applications` ~ Week, data = weekly_visas_by_scheme |> filter(Scheme == "Sponsored by individuals") |> na.omit())
# predict(mod_applications_homes_scheme) |> as_tibble()
#
# weekly_visas_by_scheme |>
#   filter(Scheme == "Sponsored by individuals") |>
#   na.omit() |>
#   bind_cols(predict(mod_applications_homes_scheme) |> as_tibble()) |>
#
#   ggplot(aes(x = Week, y = `Weekly applications...4`)) +
#   geom_point() +
#   geom_line() +
#   geom_line(aes(y = `Weekly applications...9`), colour = "red")

# Predict number of applications for the family scheme
predicted_applications_family_scheme <-
  lm(`Weekly applications` ~ Week, data = weekly_visas_by_scheme |> filter(Scheme == "Ukraine Family Scheme")) |>
  broom::augment(newdata = predicted_weeks) |>
  mutate(.fitted = if_else(.fitted < 0, 0, .fitted))

# Put the predicted numbers of applications in the simulation tibble
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Weekly applications - Homes for Ukraine` <- predicted_applications_homes_scheme$.fitted
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Weekly applications - Family Scheme` <- predicted_applications_family_scheme$.fitted

## Predict weekly arrival rates from historical trends ----
# predicted_arrivals_homes_scheme <-
#   lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Sponsored by individuals")) |>
#   # tidy(conf.int = TRUE)
#   broom::augment(newdata = predicted_weeks, interval = "confidence") |>
#   mutate(
#     .fitted = if_else(.fitted < 0, 0, .fitted),
#     .lower = if_else(.lower < 0, 0, .lower),
#     .upper = if_else(.upper < 0, 0, .upper)
#   )

mod_arrivals_homes_scheme <-
  earth(
    `% issued visas arriving this week` ~ Week,
    varmod.method = "lm",
    nfold = 10,
    ncross = 30,
    data = arrival_rates |> filter(Scheme == "Sponsored by individuals") |> na.omit()
  )

# summary(mod_arrivals_homes_scheme)
# predict(mod_arrivals_homes_scheme, interval = "cint") |> as_tibble()
#
# arrival_rates |>
#   filter(Scheme == "Sponsored by individuals") |>
#   na.omit() |>
#   bind_cols(predict(mod_arrivals_homes_scheme, interval = "cint") |> as_tibble()) |>
#
#   ggplot(aes(x = Week, y = `% issued visas arriving this week`)) +
#   geom_point() +
#   geom_line() +
#   geom_line(aes(y = fit), colour = "red") +
#   geom_ribbon(aes(ymin = lwr, max = upr), fill = "red", alpha = 0.3)

predicted_arrivals_homes_scheme <-
  predict(
    mod_arrivals_homes_scheme,
    newdata = predicted_weeks,
    interval = "pint"
  ) |>
  as_tibble() |>
  mutate(
    .fitted = if_else(fit < 0, 0, fit),
    .lower = if_else(lwr < 0, 0, lwr),
    .upper = if_else(upr < 0, 0, upr)
  )

# predicted_arrivals_family_scheme <-
#   lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Ukraine Family Scheme")) |>
#   broom::augment(newdata = predicted_weeks, interval = "confidence") |>
#   mutate(
#     .fitted = if_else(.fitted < 0, 0, .fitted),
#     .lower = if_else(.lower < 0, 0, .lower),
#     .upper = if_else(.upper < 0, 0, .upper)
#   )

mod_arrivals_family_scheme <-
  earth(
    `% issued visas arriving this week` ~ Week,
    varmod.method = "lm",
    nfold = 10,
    ncross = 30,
    data = arrival_rates |> filter(Scheme == "Ukraine Family Scheme") |> na.omit()
  )

predicted_arrivals_family_scheme <-
  predict(
    mod_arrivals_family_scheme,
    newdata = predicted_weeks,
    interval = "pint"
  ) |>
  as_tibble() |>
  mutate(
    .fitted = if_else(fit < 0, 0, fit),
    .lower = if_else(lwr < 0, 0, lwr),
    .upper = if_else(upr < 0, 0, upr)
  )

# predicted_arrivals_govt_scheme <-
#   lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Government 'super sponsored'")) |>
#   broom::augment(newdata = predicted_weeks, interval = "confidence") |>
#   mutate(
#     .fitted = if_else(.fitted < 0, 0, .fitted),
#     .lower = if_else(.lower < 0, 0, .lower),
#     .upper = if_else(.upper < 0, 0, .upper)
#   )

mod_arrivals_govt_scheme <-
  earth(
    `% issued visas arriving this week` ~ Week,
    varmod.method = "lm",
    nfold = 10,
    ncross = 30,
    data = arrival_rates |> filter(Scheme == "Government 'super sponsored'") |> na.omit()
  )

predicted_arrivals_govt_scheme <-
  predict(
    mod_arrivals_govt_scheme,
    newdata = predicted_weeks,
    interval = "pint"
  ) |>
  as_tibble() |>
  mutate(
    .fitted = if_else(fit < 0, 0, fit),
    .lower = if_else(lwr < 0, 0, lwr),
    .upper = if_else(upr < 0, 0, upr)
  )

# Put the predicted numbers of applications in the simulation tibble
# We'll use the Family Scheme rates for the Government super sponsor schemes, since they closely align
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine` <- predicted_arrivals_homes_scheme$.fitted
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Family Scheme` <- predicted_arrivals_family_scheme$.fitted
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Super Sponsor` <- predicted_arrivals_family_scheme$.fitted

simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine (lower bound)` <- predicted_arrivals_homes_scheme$.lower
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Family Scheme (lower bound)` <- predicted_arrivals_family_scheme$.lower
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Super Sponsor (lower bound)` <- predicted_arrivals_family_scheme$.lower

simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine (upper bound)` <- predicted_arrivals_homes_scheme$.upper
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Family Scheme (upper bound)` <- predicted_arrivals_family_scheme$.upper
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Arrival rate - Super Sponsor (upper bound)` <- predicted_arrivals_family_scheme$.upper

## Make a copy of the sim dataset to use in the next scenario before running this one
simulated_visas_no_more_applications <- simulated_visas_baseline

## Run the simulation ----
simulated_visas_baseline <-
  run_visa_simulation(simulated_visas_baseline)

write_csv(simulated_visas_baseline, glue::glue("output-data/simulations/simulation-baseline-{min(simulated_visas_baseline$Date)}.csv"))

## Plot historical and simulated arrivals ----
plt_sim_baseline <-
  cumulative_visas_by_scheme |>
  ggplot(aes(x = Date, y = `Number of arrivals`)) +
  geom_area(aes(fill = Scheme), position = "stack", colour = "white") +

  # Add simulated arrivals
  geom_ribbon(
    data = simulated_visas_baseline,
    inherit.aes = FALSE,
    mapping = aes(x = Date, ymin = `Total arrivals (lower bound)`, ymax = `Total arrivals (upper bound)`),
    fill = "grey80",
    alpha = 0.4
  ) +
  geom_line(
    data = simulated_visas_baseline,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (lower bound)`),
    colour = "grey60",
    lty = 2
  ) +
  geom_line(
    data = simulated_visas_baseline,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (upper bound)`),
    colour = "grey60",
    lty = 2
  ) +

  # Central estimate
  geom_line(
    data = simulated_visas_baseline,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals`)
  ) +
  geom_vline(xintercept = min(simulated_visas_baseline$Date), lty = 2, colour = "grey") +
  annotate(
    "text",
    x = min(simulated_visas_baseline$Date) + 25, y = max(simulated_visas_baseline$`Total arrivals (upper bound)`),
    label = "→ Forecast", colour = "grey", size = 3.5
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, .1))) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot"
  ) +
  labs(
    title = "Historical and predicted arrivals from Ukraine",
    x = NULL,
    fill = "Visa scheme",
    caption = "Source: British Red Cross analysis and simulation of DLUHC data"
  )

ggsave(plot = plt_sim_baseline, filename = "images/forecast arrivals.png", height = 150, width = 180, units = "mm")

# If testing the sim but checking how it predicts already-observed data (i.e. you have set sim_start_week <- 29, or similar),
# replot the above but highlighting and zooming in on the prediction of observed data
# plt_sim_baseline +
#   labs(
#     title = "The simulation predicts previously observed numbers of arrivals",
#     # subtitle = str_wrap("By starting the simulation from an earlier week, we see it accurately predicts already-observed numbers of arrivals (yellow box). This plot is zoomed-in, so does not show the full range of historical and predicted arrivals.", 100)
#   )
#
# ggsave("images/forecast arrivals - testing the simulation.png", height = 150, width = 180, units = "mm")


####
####
# Scenario: No new applications ----
# Same as baseline scenario but without any new visa applications
####
####

## No new applications ----
simulated_visas_no_more_applications <-
  simulated_visas_no_more_applications |>
  mutate(
    `Weekly applications - Homes for Ukraine` = 0,
    `Weekly applications - Family Scheme` = 0
  )

## Run the simulation ----
simulated_visas_no_more_applications <-
  run_visa_simulation(simulated_visas_no_more_applications)

write_csv(simulated_visas_no_more_applications, glue::glue("output-data/simulations/simulation-no-more-applications-{min(simulated_visas_no_more_applications$Date)}.csv"))


####
####
# Scenario: Winter surge ----
# Two parameters are changed in this scenario:
# 1. The numbers of new visa applications match historical peaks in applications
# 2. The rate of arrivals is higher than in the baseline scenario
####
####

## Choose week numbers that the sim will run from and to ----
sim_start_week <- min(simulated_visas_baseline$Week)
num_weeks_to_simulate <- 6 # Run surge scenario over six weeks

## Set up a tibble for the simulated data ----
simulated_visas_surge <-
  init_sim(sim_start_week, num_weeks_to_simulate) |>
  # Seed with data from week 50 of the baseline scenario
  bind_rows(
    simulated_visas_baseline |> filter(Week == sim_start_week)
  ) |>
  arrange(Week)

## Number of new visa applications each week ----
# We can just use the surges from the historical data, rather than needing to fit a model
simulated_visas_surge[simulated_visas_surge$Week >= (sim_start_week + 1), ]$`Weekly applications - Homes for Ukraine` <-
  weekly_visas_by_scheme |>
  filter(Scheme == "Sponsored by individuals") |>
  filter(Week > 24) |>
  filter(Week <= 24 + num_weeks_to_simulate) |>
  pull(`Weekly applications`)

simulated_visas_surge[simulated_visas_surge$Week >= (sim_start_week + 1), ]$`Weekly applications - Family Scheme` <-
  weekly_visas_by_scheme |>
  filter(Scheme == "Ukraine Family Scheme") |>
  filter(Week >= 14) |>
  filter(Week <= 14 + num_weeks_to_simulate) |>
  filter(`Weekly applications` > 0) |>
  pull(`Weekly applications`)

## Arrival rates ----
# Use predicted arrival rates for the first week we have data for each Scheme
predicted_arrivals_homes_scheme <-
  lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Sponsored by individuals")) |>
  augment(
    newdata = tibble(Week = 20),
    interval = "confidence"
  ) |>
  mutate(
    .fitted = if_else(.fitted < 0, 0, .fitted),
    .lower = if_else(.lower < 0, 0, .lower),
    .upper = if_else(.upper < 0, 0, .upper)
  )

predicted_arrivals_family_scheme <-
  lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Ukraine Family Scheme")) |>
  augment(
    newdata = tibble(Week = 15),
    interval = "confidence"
  ) |>
  mutate(
    .fitted = if_else(.fitted < 0, 0, .fitted),
    .lower = if_else(.lower < 0, 0, .lower),
    .upper = if_else(.upper < 0, 0, .upper)
  )

predicted_arrivals_govt_scheme <-
  lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Government 'super sponsored'")) |>
  augment(
    newdata = tibble(Week = 20),
    interval = "confidence"
  ) |>
  mutate(
    .fitted = if_else(.fitted < 0, 0, .fitted),
    .lower = if_else(.lower < 0, 0, .lower),
    .upper = if_else(.upper < 0, 0, .upper)
  )

# Put the predicted numbers of applications in the simulation tibble
simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine` <- predicted_arrivals_homes_scheme$.fitted
simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Family Scheme` <- predicted_arrivals_family_scheme$.fitted
simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Super Sponsor` <- predicted_arrivals_govt_scheme$.fitted

simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine (lower bound)` <- predicted_arrivals_homes_scheme$.lower
simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Family Scheme (lower bound)` <- predicted_arrivals_family_scheme$.lower
simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Super Sponsor (lower bound)` <- predicted_arrivals_govt_scheme$.lower

simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine (upper bound)` <- predicted_arrivals_homes_scheme$.upper
simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Family Scheme (upper bound)` <- predicted_arrivals_family_scheme$.upper
simulated_visas_surge[simulated_visas_surge$Week > sim_start_week, ]$`Arrival rate - Super Sponsor (upper bound)` <- predicted_arrivals_govt_scheme$.upper

## Run the simulation ----
simulated_visas_surge <-
  run_visa_simulation(simulated_visas_surge)

write_csv(simulated_visas_surge, glue::glue("output-data/simulations/simulation-surge-{min(simulated_visas_baseline$Date)}.csv"))

# Plot historical and simulated arrivals for all scenarios ----
cumulative_visas_by_scheme |>
  ggplot(aes(x = Date, y = `Number of arrivals`)) +
  geom_area(aes(fill = Scheme), position = "stack", colour = "white") +

  # Add simulated arrivals - baseline scenario
  geom_ribbon(
    data = simulated_visas_baseline,
    inherit.aes = FALSE,
    mapping = aes(x = Date, ymin = `Total arrivals (lower bound)`, ymax = `Total arrivals (upper bound)`),
    fill = "grey80",
    alpha = 0.4
  ) +
  geom_line(
    data = simulated_visas_baseline,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (lower bound)`),
    colour = "grey60",
    lty = 2
  ) +
  geom_line(
    data = simulated_visas_baseline,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (upper bound)`),
    colour = "grey60",
    lty = 2
  ) +
  # Central estimate
  geom_line(
    data = simulated_visas_baseline,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals`),
    colour = "grey60"
  ) +

  # Add simulated arrivals - no new applications scenario
  # !! This scenario's forecasts are essentially the same as those of the baseline scenario - so we won't include this after all
  # geom_ribbon(
  #   data = simulated_visas_no_more_applications,
  #   inherit.aes = FALSE,
  #   mapping = aes(x = Date, ymin = `Total arrivals (lower bound)`, ymax = `Total arrivals (upper bound)`),
  #   fill = "#a6dba0",
  #   alpha = 0.4
  # ) +
  # geom_line(
  #   data = simulated_visas_no_more_applications,
  #   inherit.aes = FALSE,
  #   mapping = aes(x = Date, y = `Total arrivals (lower bound)`),
  #   colour = "#5aae61",
  #   lty = 2
  # ) +
  # geom_line(
  #   data = simulated_visas_no_more_applications,
  #   inherit.aes = FALSE,
  #   mapping = aes(x = Date, y = `Total arrivals (upper bound)`),
  #   colour = "#5aae61",
  #   lty = 2
  # ) +
  # geom_line(
  #   data = simulated_visas_no_more_applications,
  #   inherit.aes = FALSE,
  #   mapping = aes(x = Date, y = `Total arrivals`),
  #   colour = "#5aae61",
  #   lty = 2
  # ) +

  # Add simulated arrivals - winter surge scenario
  geom_ribbon(
    data = simulated_visas_surge,
    inherit.aes = FALSE,
    mapping = aes(x = Date, ymin = `Total arrivals (lower bound)`, ymax = `Total arrivals (upper bound)`),
    fill = "cornflowerblue",
    alpha = 0.4
  ) +
  geom_line(
    data = simulated_visas_surge,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (lower bound)`),
    colour = "royalblue4",
    lty = 2
  ) +
  geom_line(
    data = simulated_visas_surge,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (upper bound)`),
    colour = "royalblue4",
    lty = 2
  ) +
  geom_line(
    data = simulated_visas_surge,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals`),
    colour = "royalblue4",
    lty = 2
  ) +
  geom_vline(xintercept = min(simulated_visas_baseline$Date), lty = 2, colour = "grey") +
  annotate(
    "text",
    x = min(simulated_visas_baseline$Date) + 25, y = max(simulated_visas_surge$`Total arrivals (upper bound)`),
    label = "→ Forecast", colour = "grey", size = 3.5
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, .1))) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot"
  ) +
  labs(
    title = "Historical and predicted arrivals from Ukraine",
    subtitle = str_wrap("Bars show historical numbers of arrivals, coloured by visa scheme. Grey solid line and shaded area shows predicted arrivals in the baseline scenario. Blue dotted line and shaded area shows predicted arrivals during a 'winter surge' scenario", 100),
    x = NULL,
    y = "Total number of arrivals",
    fill = "Visa scheme",
    caption = "Source: British Red Cross analysis and simulation of DLUHC data"
  )

ggsave("images/forecast arrivals - winter surge.png", height = 150, width = 180, units = "mm")

##
##
# Analyse scenarios ----
##
##
# How many more people might arrive under a surge scenario compared to baseline?
# Fetch predictions in 3 and 6 months' time
simulated_totals <-
  left_join(
    simulated_visas_baseline |>
      filter(Week == max(Week) | Week == min(Week) + 12) |>
      select(
        Week,
        baseline_total = `Total arrivals`,
        baseline_upper = `Total arrivals (upper bound)`,
        baseline_lower = `Total arrivals (lower bound)`
      ),
    simulated_visas_surge |>
      filter(Week == max(Week) | Week == min(Week) + 12) |>
      select(
        Week,
        surge_total = `Total arrivals`,
        surge_upper = `Total arrivals (upper bound)`,
        surge_lower = `Total arrivals (lower bound)`
      )
  ) |>
  mutate(total_diff = surge_total - baseline_total)

simulated_totals <-
  left_join(
    simulated_visas_surge |>
      filter(Week == max(Week)) |>
      select(
        Week,
        surge_total = `Total arrivals`,
        surge_upper = `Total arrivals (upper bound)`,
        surge_lower = `Total arrivals (lower bound)`
      ),
    simulated_visas_baseline |>
      filter(Week == max(simulated_visas_surge$Week)) |>
      select(
        Week,
        baseline_total = `Total arrivals`,
        baseline_upper = `Total arrivals (upper bound)`,
        baseline_lower = `Total arrivals (lower bound)`
      )
  ) |>
  mutate(total_diff = surge_total - baseline_total)

# How many people have arrived in the UK, as of the most recently observed week?
observed_total <-
  cumulative_visas_by_scheme |>
  filter(Week == max(Week)) |>
  summarise(observed_total = sum(`Number of arrivals`)) |>
  pull(observed_total)

simulated_totals <-
  simulated_totals |>
  mutate(
    obs_sim_diff = baseline_total - observed_total,
    obs_sim_diff_upper = baseline_upper - observed_total
  )

# How many people might arrive by the end of the winter surge (in addition to the baseline scenario)?
simulated_totals |>
  filter(Week == max(Week)) |>
  pull(total_diff)

# How many additional people might arrive under the baseline scenario in three
# months time?
three_month_baseline <- simulated_visas_baseline |>
  filter(Week == min(Week) + 12) |>
  pull(`Total arrivals`)

three_month_baseline - observed_total

three_month_baseline_upper <- simulated_visas_baseline |>
  filter(Week == min(Week) + 12) |>
  pull(`Total arrivals (upper bound)`)

three_month_baseline_upper - observed_total

# How many additional people might arrive under the baseline scenario...
# ... in three months' time?
simulated_totals |>
  filter(Week == min(Week)) |>
  pull(obs_sim_diff) # Central estimate
simulated_totals |>
  filter(Week == min(Week)) |>
  pull(obs_sim_diff_upper) # Upper bound
# ... in six months' time?
simulated_totals |>
  filter(Week == max(Week)) |>
  pull(obs_sim_diff) # Central estimate
simulated_totals |>
  filter(Week == max(Week)) |>
  pull(obs_sim_diff_upper) # Upper bound

# Average weekly arrival rates
simulated_visas_baseline |>
  select(
    Week,
    baseline_weekly = `Weekly arrivals`,
    baseline_weekly_upper = `Weekly arrivals (upper bound)`,
    baseline_weekly_lower = `Weekly arrivals (lower bound)`
  ) |>
  mutate(
    Period = if_else(Week <= min(Week) + 12, "First three months", "Next three months")
  ) |>
  group_by(Period) |>
  summarise(
    `Mean number of arrivals` = mean(baseline_weekly),
    `Mean number of arrivals (upper)` = mean(baseline_weekly_upper),
    `Mean number of arrivals (lower)` = mean(baseline_weekly_lower),
  )

####
####
# Plot simulation assumptions and parameters ----
####
####

# Show mean and observed rates
# (rather than the commented-out boxplots above, which show the medians - but we're using mean rates in the simulation)
historical_processing_rates |>
  ggplot(aes(x = Scheme, y = `% backlog issued a visa this week`, colour = Scheme, fill = Scheme)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, alpha = 0.3, show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 8, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  theme_classic() +
  theme(
    plot.title = element_text(size = rel(1)),
    plot.title.position = "plot"
  ) +
  labs(
    title = "Proportion of applications that are issued a visa each week",
    subtitle = str_wrap("Dots are the observed rates of visa issuance; stars show the mean rates.", 60),
    x = NULL,
    y = NULL
  )

ggsave("images/simulation/Proportion of applications that are issued a visa each week.png", width = 110, height = 80, units = "mm")

## Plot weekly arrivals as a proportion of the backlog of issued visas ----
arrival_rates |>
  mutate(Scheme = factor(Scheme, levels = c("Sponsored by individuals", "Ukraine Family Scheme", "Government 'super sponsored'"))) |>
  ggplot(aes(x = Week, y = `% issued visas arriving this week`)) +
  geom_line(aes(colour = Scheme), size = 1.1) +
  geom_smooth(aes(colour = Scheme, fill = Scheme), method = "lm") +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  guides(
    colour = guide_legend(
      ncol = 1
    )
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = rel(1)),
    plot.title.position = "plot",
    legend.position = "top",
    # legend.box = "vertical",
    # legend.margin = margin()
  ) +
  labs(
    title = "Weekly arrivals as a proportion of the backlog of issued visas",
    colour = NULL,
    fill = NULL
  )

ggsave("images/simulation/Weekly arrivals as a proportion of the backlog of issued visas.png", width = 110, height = 100, units = "mm")

## Plot weekly arrivals as a proportion of the backlog of issued visas - using MARS models ----
# Combine predictions for each MARS model
arrival_rates_mars <-
  bind_rows(
    arrival_rates |>
      filter(Scheme == "Sponsored by individuals") |>
      na.omit() |>
      bind_cols(predict(mod_arrivals_homes_scheme, interval = "cint") |> as_tibble()),
    arrival_rates |>
      filter(Scheme == "Ukraine Family Scheme") |>
      na.omit() |>
      bind_cols(predict(mod_arrivals_family_scheme, interval = "cint") |> as_tibble()),
    arrival_rates |>
      filter(Scheme == "Government 'super sponsored'") |>
      na.omit() |>
      bind_cols(predict(mod_arrivals_govt_scheme, interval = "cint") |> as_tibble())
  )

# Plot the non-linear models
arrival_rates_mars |>
  mutate(Scheme = factor(Scheme, levels = c("Sponsored by individuals", "Ukraine Family Scheme", "Government 'super sponsored'"))) |>
  ggplot(aes(x = Week, y = `% issued visas arriving this week`)) +
  geom_line(aes(colour = Scheme), size = 1.1) +
  geom_line(aes(y = fit, colour = Scheme), lty = 2) +
  geom_ribbon(aes(ymin = lwr, max = upr, colour = Scheme, fill = Scheme), alpha = 0.3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  guides(
    colour = guide_legend(
      ncol = 1
    )
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = rel(1)),
    plot.title.position = "plot",
    legend.position = "top",
    # legend.box = "vertical",
    # legend.margin = margin()
  ) +
  labs(
    title = "Weekly arrivals as a proportion of the backlog of issued visas",
    colour = NULL,
    fill = NULL
  )

ggsave("images/simulation/Weekly arrivals as a proportion of the backlog of issued visas - nonlinear models.png", width = 110, height = 100, units = "mm")

## Plot weekly applications by scheme ----
weekly_visas_by_scheme |>
  filter(!str_detect(Scheme, "Government")) |>
  ggplot(aes(x = Date, y = `Weekly applications`)) +
  geom_line(aes(colour = Scheme), size = 1.1) +
  geom_smooth(aes(colour = Scheme, fill = Scheme), method = "lm") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    panel.grid.major.y = element_line(colour = "lightgrey", linetype = 2)
  ) +
  labs(
    title = "Declining numbers of visa applications each week",
    x = NULL,
    y = "Number of applications each week",
    colour = NULL,
    fill = NULL,
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/simulation/weekly applications.png", width = 115, height = 100, units = "mm")

## Plot weekly applications by scheme ----
weekly_visas_by_scheme |>
  mutate(Scheme = factor(Scheme, levels = c("Sponsored by individuals", "Ukraine Family Scheme", "Government 'super sponsored'"))) |>
  ggplot(aes(x = Date, y = `Weekly visas issued`)) +
  geom_line(aes(colour = Scheme), size = 1.1) +
  geom_smooth(aes(colour = Scheme, fill = Scheme), method = "lm") +
  facet_wrap(~Scheme) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    panel.grid.major.y = element_line(colour = "lightgrey", linetype = 2)
  ) +
  labs(
    title = "Visas issued each week",
    x = NULL,
    y = "Number of visas issued each week",
    colour = NULL,
    fill = NULL,
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/simulation/weekly visas issued.png", width = 150, height = 100, units = "mm")

## Plot weekly applications, issuance, and arrivals for whole UK
weekly_visas_by_scheme |>
  group_by(Date) |>
  summarise(
    `Weekly applications` = sum(`Weekly applications`, na.rm = TRUE),
    `Weekly visas issued` = sum(`Weekly visas issued`, na.rm = TRUE),
    `Weekly arrivals` = sum(`Weekly arrivals`, na.rm = TRUE),
  ) |>
  ungroup() |>
  mutate(`Weekly applications` = if_else(`Weekly applications` < 0, NA_real_, `Weekly applications`)) |>
  pivot_longer(cols = -Date) |>
  mutate(name = factor(name, levels = c("Weekly applications", "Weekly visas issued", "Weekly arrivals"))) |>
  ggplot(aes(x = Date, y = value)) +
  geom_line(aes(colour = name), size = 1.1) +
  facet_wrap(~name, ncol = 3) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    panel.grid.major.y = element_line(colour = "lightgrey", linetype = 2)
  ) +
  labs(
    title = "Visa applications, visas issued, and arrivals each week",
    x = NULL,
    y = "Number of applications, visas issued, and arrivals",
    colour = NULL,
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/simulation/weekly visas.png", width = 150, height = 100, units = "mm")

