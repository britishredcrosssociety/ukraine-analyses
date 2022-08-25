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
  
  pivot_wider(names_from = Stage, values_from = Visas_imputed) |> 
  
  # Calculate week-on-week changes
  mutate(
    `Weekly applications` = `visa applications received` - lag(`visa applications received`),
    `Weekly visas issued` = `visas issued` - lag(`visas issued`),
    `Weekly arrivals` = `arrivals of visa-holders in the UK` - lag(`arrivals of visa-holders in the UK`)
  ) |> 
  
  select(Week, Scheme, starts_with("Weekly"))

weekly_sponsorship_scheme_visas <- 
  visas_ltla21_summary |> 
  arrange(Date) |> 
  filter(str_detect(Type, "^Sponsored")) |> 
  mutate(Scheme = if_else(str_detect(Type, "Government"), "Government 'super sponsored'", Type)) |>
  
  # Calculate UK weekly totals
  mutate(Week = week(Date)) |> 
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
  
  select(Week, Scheme, starts_with("Weekly"))

weekly_visas_by_scheme <- bind_rows(weekly_sponsorship_scheme_visas, weekly_family_scheme_visas) |> 
  arrange(Week, Scheme) |> 
  
  mutate(
    `% applications --> issued` = `Weekly visas issued` / `Weekly applications`,
    `% issued --> arrivals` = `Weekly arrivals` / `Weekly visas issued`
  ) |> 
  
  # Convert week number to date; source: https://stackoverflow.com/a/46183403
  mutate(Date = ymd("2022-01-03") + weeks(Week - 1)) |> 
  relocate(Date)
  
# Scraped data goes further back in time, so use that until 17th May, then use LA data for the following weeks
# weekly_visas <- 
#   full_join(weekly_arrivals_from_LA_data, weekly_arrivals_from_scraped_data, by = "Week") |> 
#   
#   mutate(`Weekly arrivals` = if_else(Week <= 20, `Weekly arrivals (scraped)`, `Weekly arrivals (LA)`)) |> 
#   
#   # Convert week number to date; source: https://stackoverflow.com/a/46183403  
#   mutate(Date = ymd("2022-01-03") + weeks(Week - 1))

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
  
  pivot_wider(names_from = Stage, values_from = Visas_imputed) |> 
  rename(
    `Number of visa applications` = `visa applications received`,
    `Number of visas issued` = `visas issued`,
    `Number of arrivals` = `arrivals of visa-holders in the UK`,
  )

cumulative_sponsorship_scheme_visas <- 
  visas_ltla21_summary |> 
  arrange(Date) |> 
  filter(str_detect(Type, "^Sponsored")) |> 
  mutate(Scheme = if_else(str_detect(Type, "Government"), "Government 'super sponsored'", Type)) |>
  
  # Calculate UK weekly totals
  mutate(Week = week(Date)) |> 
  group_by(Week, Scheme) |> 
  summarise(
    `Number of visa applications` = sum(`Number of visa applications`),
    `Number of visas issued` = sum(`Number of visas issued`),
    `Number of arrivals` = sum(`Number of arrivals in the UK by sponsor location`)
  ) |> 
  ungroup()

cumulative_visas_by_scheme <- bind_rows(cumulative_sponsorship_scheme_visas, cumulative_family_scheme_visas) |> 
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
# Rates for Family scheme
historical_processing_rates <- 
  visas_scraped |> 
  select(-Visas, -Date) |> 
  pivot_wider(names_from = Stage, values_from = Visas_imputed) |> 
  
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
  )

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
    `Min rate of applications issued visas` = min(`% backlog issued a visa this week`, na.rm = TRUE),
    `Max rate of applications issued visas` = max(`% backlog issued a visa this week`, na.rm = TRUE)
  )

# The average conversion rates are pretty similar for family and homes schemes, so just take one rate for simplicity
mean_applications_issued_rate <- mean(historical_processing_rates$`% backlog issued a visa this week`, na.rm = TRUE)
min_applications_issued_rate <- min(historical_processing_rates$`% backlog issued a visa this week`, na.rm = TRUE)
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
max_arrival_rate_family_scheme <- arrival_rates |> na.omit() |> filter(Scheme == "Ukraine Family Scheme") |> filter(Week == min(Week)) |> pull(`% issued visas arriving this week`)
max_arrival_rate_homes_scheme  <- arrival_rates |> na.omit() |> filter(Scheme == "Sponsored by individuals") |> filter(Week == min(Week)) |> pull(`% issued visas arriving this week`)
max_arrival_rate_govt_scheme   <- arrival_rates |> na.omit() |> filter(Scheme == "Government 'super sponsored'") |> filter(Week == min(Week)) |> pull(`% issued visas arriving this week`)

# Minimum arrival rates can be their most recent values
min_arrival_rate_family_scheme <- arrival_rates |> na.omit() |> filter(Scheme == "Ukraine Family Scheme") |> filter(Week == max(Week)) |> pull(`% issued visas arriving this week`)
min_arrival_rate_homes_scheme  <- arrival_rates |> na.omit() |> filter(Scheme == "Sponsored by individuals") |> filter(Week == max(Week)) |> pull(`% issued visas arriving this week`)
min_arrival_rate_govt_scheme   <- arrival_rates |> na.omit() |> filter(Scheme == "Government 'super sponsored'") |> filter(Week == max(Week)) |> pull(`% issued visas arriving this week`)

# Calculate mean arrival rates
arrival_rates_summary <- 
  arrival_rates |> 
  group_by(Scheme) |> 
  summarise(
    `Mean arrival rate` = mean(`% issued visas arriving this week`, na.rm = TRUE),
    `Min arrival rate` = min(`% issued visas arriving this week`, na.rm = TRUE),
    `Max arrival rate` = max(`% issued visas arriving this week`, na.rm = TRUE)
  )

mean_arrival_rate_family_scheme <- arrival_rates_summary |> filter(Scheme == "Ukraine Family Scheme") |> pull(`Mean arrival rate`)
mean_arrival_rate_homes_scheme <- arrival_rates_summary |> filter(Scheme == "Sponsored by individuals") |> pull(`Mean arrival rate`)
mean_arrival_rate_govt_scheme <- arrival_rates_summary |> filter(Scheme == "Government 'super sponsored'") |> pull(`Mean arrival rate`)

# min_arrival_rate_homes_scheme  <- arrival_rates |> filter(Scheme == "Sponsored by individuals") |> pull(`Min arrival rate`)
# min_arrival_rate_family_scheme <- arrival_rates |> filter(Scheme == "Ukraine Family Scheme") |> pull(`Min arrival rate`)
# min_arrival_rate_govt_scheme  <- arrival_rates |> filter(Scheme == "Government 'super sponsored'") |> pull(`Min arrival rate`)

## Calculate visa refusal/withdrawal rates ----
refusal_withdrawal_rate_by_scheme <- 
  visas_scraped |> 
  filter(str_detect(Stage, "application")) |> 
  select(-Visas) |> 
  pivot_wider(names_from = Stage, values_from = Visas_imputed) |> 
  
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
    
    #... calculate lower bound based on predicted arrival rate for each scheme
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Homes for Ukraine (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Homes for Ukraine (upper bound)`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Family Scheme (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Family Scheme (upper bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Family Scheme (upper bound)`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals - Super Sponsor (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Super Sponsor (upper bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Super Sponsor (upper bound)`, 0)
    
    #... calculate upper bound based on predicted arrival rate for each scheme
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
    applications_issued_homes_scheme  <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Homes for Ukraine` * mean_applications_issued_rate, 0)
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

num_weeks_to_simulate <- 4 * 6  # the next six months

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
#!! This was the original approach I took, which calculated (negative) trend lines for numbers of new applications,
#!! which were used during the sim to iteratively calculate each week's number of new applications.
#!! However, I want to include a polynomial function for calculating new applications in the 'winter surge' scenario, 
#!! so it's easier to predict application numbers before the simulation rather than iteratively during the sim

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

# Predict number of applications for the family scheme
predicted_applications_family_scheme <- 
  lm(`Weekly applications` ~ Week, data = weekly_visas_by_scheme |> filter(Scheme == "Ukraine Family Scheme")) |> 
  broom::augment(newdata = predicted_weeks) |> 
  mutate(.fitted = if_else(.fitted < 0, 0, .fitted))

# Put the predicted numbers of applications in the simulation tibble
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Weekly applications - Homes for Ukraine` <- predicted_applications_homes_scheme$.fitted
simulated_visas_baseline[simulated_visas_baseline$Week > sim_start_week, ]$`Weekly applications - Family Scheme` <- predicted_applications_family_scheme$.fitted

## Predict weekly arrival rates from historical trends ----
predicted_arrivals_homes_scheme <- 
  lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Sponsored by individuals")) |> 
  # tidy(conf.int = TRUE)
  broom::augment(newdata = predicted_weeks, interval = "confidence") |> 
  mutate(
    .fitted = if_else(.fitted < 0, 0, .fitted),
    .lower = if_else(.lower < 0, 0, .lower),
    .upper = if_else(.upper < 0, 0, .upper)
  )

predicted_arrivals_family_scheme <- 
  lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Ukraine Family Scheme")) |> 
  broom::augment(newdata = predicted_weeks, interval = "confidence") |> 
  mutate(
    .fitted = if_else(.fitted < 0, 0, .fitted),
    .lower = if_else(.lower < 0, 0, .lower),
    .upper = if_else(.upper < 0, 0, .upper)
  )

predicted_arrivals_govt_scheme <- 
  lm(`% issued visas arriving this week` ~ Week, data = arrival_rates |> filter(Scheme == "Government 'super sponsored'")) |> 
  broom::augment(newdata = predicted_weeks, interval = "confidence") |> 
  mutate(
    .fitted = if_else(.fitted < 0, 0, .fitted),
    .lower = if_else(.lower < 0, 0, .lower),
    .upper = if_else(.upper < 0, 0, .upper)
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

## Run the simulation ----
simulated_visas_baseline <- 
  run_visa_simulation(simulated_visas_baseline)

write_csv(simulated_visas_baseline, glue::glue("output-data/simulations/simulation-baseline-{min(simulated_visas_baseline$Date)}.csv"))

## Plot historical and simulated arrivals ----
plt_sim_baseline <- 
  cumulative_visas_by_scheme |> 
  ggplot(aes(x = Date, y = `Number of arrivals`)) +
  geom_col(aes(fill = Scheme), position = "stack", colour = "white") +
  
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
    "text", x = min(simulated_visas_baseline$Date) + 25, y = max(simulated_visas_baseline$`Total arrivals (upper bound)`), 
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
plt_sim_baseline +
  # geom_rect(
  #   data = tibble(
  #     xmin = ymd("2022-07-21"),
  #     xmax = ymd("2022-08-13"),
  #     ymin = 100000,
  #     ymax = 120000
  #   ),
  #   inherit.aes = FALSE, 
  #   mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
  #   alpha = 0.2,
  #   size = 2,
  #   fill = NA,
  #   colour = "gold"
  # ) +
  # coord_cartesian(
  #   xlim = c(ymd("2022-07-01"), ymd("2022-08-18")),
  #   ylim = c(90000, 150000)
  # ) +
  labs(
    title = "The simulation predicts previously observed numbers of arrivals",
    # subtitle = str_wrap("By starting the simulation from an earlier week, we see it accurately predicts already-observed numbers of arrivals (yellow box). This plot is zoomed-in, so does not show the full range of historical and predicted arrivals.", 100)
  )

ggsave("images/forecast arrivals - testing the simulation.png", height = 150, width = 180, units = "mm")

####
####
# Scenario: Winter surge ----
# Two parameters are changed in this scenario:
# 1. The numbers of new visa applications match historical peaks in applications
# 2. The rate of arrivals is higher than in the baseline scenario
####
####

## Choose week numbers that the sim will run from and to ----
sim_start_week <- 51  # Assume the winter surge starts in the middle of December
num_weeks_to_simulate <- 6  # Run surge scenario over six weeks

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

## Plot historical and simulated arrivals for both scenarios ----
cumulative_visas_by_scheme |> 
  ggplot(aes(x = Date, y = `Number of arrivals`)) +
  geom_col(aes(fill = Scheme), position = "stack", colour = "white") +
  
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
    "text", x = min(simulated_visas_baseline$Date) + 25, y = max(simulated_visas_surge$`Total arrivals (upper bound)`), 
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
simulated_totals |> filter(Week == max(Week)) |> pull(total_diff)

# How many additional people might arrive under the baseline scenario...
#... in three months' time?
simulated_totals |> filter(Week == min(Week)) |> pull(obs_sim_diff)        # Central estimate
simulated_totals |> filter(Week == min(Week)) |> pull(obs_sim_diff_upper)  # Upper bound
#... in six months' time?
simulated_totals |> filter(Week == max(Week)) |> pull(obs_sim_diff)        # Central estimate
simulated_totals |> filter(Week == max(Week)) |> pull(obs_sim_diff_upper)  # Upper bound

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

## Plot rates of applications issued as visas ----
historical_processing_rates |> 
  ggplot(aes(x = Scheme, y = `% backlog issued a visa this week`)) +
  geom_boxplot(aes(colour = Scheme), show.legend = FALSE) +
  geom_jitter(aes(colour = Scheme), alpha = 0.3, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(
    # axis.line.x = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_blank(),
    plot.title = element_text(size = rel(1)),
    plot.title.position = "plot"
  ) +
  labs(
    title = "Proportion of applications that are issued a visa each week\n",
    x = NULL,
    y = NULL
  )

ggsave("images/simulation/Proportion of applications that are issued a visa each week.png", width = 110, height = 80, units = "mm")

## Plot weekly arrivals as a proportion of the backlog of issued visas ----
arrival_rates |> 
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

## Plot surges in applications ----
# weekly_visas_by_scheme |> 
#   filter(Scheme == "Sponsored by individuals") |> 
#   
#   ggplot(aes(x = Week, y = `Weekly applications`)) +
#   geom_line(aes(colour = Scheme)) +
#   geom_smooth()


####
####
# Old code lies beneath ----
####
####

# ---- Check weekly and cumulative conversion rates ----
weekly_visas_by_scheme |> 
  select(Week, Scheme, starts_with("%")) |> 
  filter(!str_detect(Scheme, "Government")) |> 
  pivot_longer(cols = starts_with("%")) |> 
  ggplot(aes(x = Week, y = value, group = name)) +
  geom_line(aes(colour = name)) +
  facet_wrap(~Scheme)

cumulative_visas_by_scheme |> 
  select(Week, Scheme, starts_with("%")) |> 
  filter(!str_detect(Scheme, "Government")) |> 
  pivot_longer(cols = starts_with("%")) |> 
  ggplot(aes(x = Week, y = value, group = name)) +
  geom_line(aes(colour = name)) +
  facet_wrap(~Scheme)

# ---- Plot weekly arrivals by scheme ----
# weekly_arrivals_by_scheme <- 
#   visas_scraped |> 
#   arrange(Date) |> 
#   filter(str_detect(Stage, "arrival")) |> 
#   
#   # Calculate week-on-week changes
#   group_by(Scheme) |> 
#   mutate(`Weekly arrivals` = Visas_imputed - lag(Visas_imputed)) |> 
#   ungroup()

# weekly_arrivals_by_scheme |> 

weekly_visas_by_scheme |> 
  ggplot(aes(x = Date, y = `Weekly arrivals`)) +
  geom_line(aes(colour = Scheme), size = 1.1) +
  
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    panel.grid.major.y = element_line(colour = "lightgrey", linetype = 2)
  ) +
  labs(
    title = "Weekly spikes in arrivals sponsored by individuals could lead to spikes in destitution 6 to 12 months later",
    subtitle = "Showing weekly number of arrivals on each visa scheme",
    x = NULL,
    y = "Number of people arriving each week",
    colour = NULL,
    lty = NULL,
    caption = "British Red Cross analysis of DLUHC data"
  )

ggsave("images/weekly arrivals - absolute numbers - family and homes schemes.png", width = 225, height = 100, units = "mm")

# ---- Calculate growth rates (of the scraped data) ----
# Get denominator for calculating indexed values, based on the date the first arrivals were published in the Home Office data
visas_denominator <- 
  visas_scraped |> 
  filter(Date %in% c(ymd("2022-04-05"), ymd("2022-04-07"))) |> 
  select(Stage, Scheme, Visas_denominator = Visas)

# Calculate growth rates (and acceleration rates) for all stages of the visa journey
visas_growth <-
  visas_scraped |>
  left_join(visas_denominator) |> 
  
  group_by(Stage, Scheme) |> 
  mutate(
    # Calculate growth rate
    delta = (Visas - lag(Visas)) / lag(Visas),
    
    # Calculate growth rate of growth rate
    delta2 = abs(delta - lag(delta)) / lag(delta),
    
    # Calculate value indexed to `visas_denominator`
    index = Visas / Visas_denominator * 100
  ) |> 
  ungroup()

# Plot growth (indexed)
# visas_growth |> 
#   filter(!str_detect(Stage, "withdrawn|refused|awaiting")) |> 
#   ggplot(aes(x = Date, y = index)) +
#   geom_line(aes(colour = Stage, lty = Scheme), size = 1.1) +
#   scale_y_log10()
  
# Plot growth and acceleration rates
visas_growth |> 
  select(-c(Visas, Visas_denominator, index)) |> 
  filter(!str_detect(Stage, "withdrawn|refused|awaiting")) |> 
  rename(`Growth rate` = delta, `Change in growth rate` = delta2) |> 
  pivot_longer(cols = contains("growth")) |> 
  mutate(name = factor(name, levels = c("Growth rate", "Change in growth rate"))) |> 
  
  ggplot(aes(x = Date, y = value)) +
  geom_line(aes(colour = Stage, lty = Scheme), size = 1.1) +
  facet_wrap(~name, scales = "free_y")

plotly::ggplotly()

# ---- Calculate growth rates (of the downloaded, LA-level data) ----
# Get denominator for calculating indexed values, based on the date the first arrivals were published in the Home Office data
visas_denominator <- 
  visas_summary |> 
  filter(Date == ymd("2022-05-10")) |> 
  select(Location, Stage, Visas_denominator = Visas)

# Calculate growth rates (and acceleration rates) for all stages of the visa journey
visas_growth <-
  visas_summary |>
  arrange(Date) |> 
  left_join(visas_denominator) |> 
  
  group_by(Location, Stage) |> 
  mutate(
    # Number of new visas, applications arrivals
    difference = Visas - lag(Visas),
    
    # Calculate growth rate
    delta = (Visas - lag(Visas)) / lag(Visas),
    
    # Calculate growth rate of growth rate
    delta2 = abs(delta - lag(delta)) / lag(delta),
    
    # Calculate value indexed to `visas_denominator`
    index = Visas / Visas_denominator * 100
  ) |> 
  ungroup()

# Highlight super sponsor scheme pauses
# Scotland's pause started on 13 July 2022 and will run for three months
# Wales's pause started on 10th June 2022, with no timeframe given for unpausing (it was originally going to be paused just for June, but this was extended in July)
super_sponsor_pause <- 
  tribble(
    ~Location, ~xmin, ~xmax, ~ymin, ~ymax, ~ymax_rate, ~ymax_diff,
    "Scotland", ymd("2022-07-13"), min(max(visas_growth$Date), ymd("2022-10-13")), 0, visas_growth |> filter(Location == "Scotland") |> filter(Visas == max(Visas, na.rm = TRUE)) |> pull(Visas), visas_growth |> filter(Location == "Scotland") |> filter(delta == max(delta, na.rm = TRUE)) |> pull(delta), visas_growth |> filter(Location == "Scotland") |> filter(difference == max(difference, na.rm = TRUE)) |> pull(difference),
    "Wales", ymd("2022-06-10"), max(visas_growth$Date), 0, visas_growth |> filter(Location == "Wales") |> filter(Visas == max(Visas, na.rm = TRUE)) |> pull(Visas), visas_growth |> filter(Location == "Wales") |> filter(delta == max(delta, na.rm = TRUE)) |> pull(delta), visas_growth |> filter(Location == "Wales") |> filter(difference == max(difference, na.rm = TRUE)) |> pull(difference)
  )

# Plot absolute numbers
plt_visas <- 
  visas_growth |> 
  ggplot(aes(x = Date, y = Visas)) +
  geom_line(aes(colour = Stage), size = 1.1) +
  
  # Highlight super sponsor pauses
  geom_rect(data = super_sponsor_pause, inherit.aes = FALSE, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2) +
  
  facet_wrap(~Location, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot"
  ) +
  labs(
    title = "Number of visas and arrivals on the Homes for Ukraine scheme",
    subtitle = "Grey areas show pauses in super sponsor schemes for Scotland and Wales",
    x = NULL,
    y = "Number of visas and arrivals",
    colour = NULL
  )

# Plot weekly change in numbers
plt_diff <- 
  visas_growth |> 
  ggplot(aes(x = Date, y = difference)) +
  geom_line(aes(colour = Stage), size = 1.1) +
  
  # Highlight super sponsor pauses
  geom_rect(data = super_sponsor_pause, inherit.aes = FALSE, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax_diff), alpha = 0.2) +
  
  facet_wrap(~Location, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot"
  ) +
  labs(
    title = "Weekly visa applications, visas issued, and arrivals on the Homes for Ukraine scheme",
    subtitle = "Grey areas show pauses in super sponsor schemes for Scotland and Wales",
    x = NULL,
    y = "Number of visas and arrivals",
    colour = NULL
  )

# Plot growth and acceleration rates
plt_growth <- 
  visas_growth |> 
  ggplot(aes(x = Date, y = delta)) +
  geom_line(aes(colour = Stage), size = 1.1) +
  
  # Highlight super sponsor pauses
  geom_rect(data = super_sponsor_pause, inherit.aes = FALSE, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax_rate), alpha = 0.2) +
  
  facet_wrap(~Location, scales = "free_y") +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot"
  ) +
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "Rate of change",
    colour = NULL,
    caption = "British Red Cross analysis of DLUHC data"
  )

# Save plots
ggsave(plot = plt_visas, filename = "images/cumulative visa numbers.png", width = 200, height = 175, units = "mm")

plt_diff + plt_growth
ggsave("images/weekly visa numbers.png", width = 400, height = 175, units = "mm")

# ---- Visualise trends in weekly arrivals ----
visas_growth |> 
  filter(str_detect(Stage, "arrivals")) |> 
  ggplot(aes(x = Date, y = difference)) +
  # geom_line(aes(colour = Location), size = 1.1) +
  # geom_smooth(aes(colour = Location, fill = Location), alpha = 0.3) +
  geom_smooth(aes(colour = Location, fill = Location), alpha = 0.3, method = "lm") +
  
  # Highlight super sponsor pauses
  geom_rect(data = super_sponsor_pause, inherit.aes = FALSE, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax_diff), alpha = 0.2) +
  
  facet_wrap(~Location, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot"
  ) +
  labs(
    title = "Trends in weekly arrivals on the Homes for Ukraine scheme",
    subtitle = "Grey areas show pauses in super sponsor schemes for Scotland and Wales",
    x = NULL,
    y = "Number of arrivals",
    colour = NULL
  )

ggsave(filename = "images/trends in weekly arrivals.png", width = 200, height = 175, units = "mm")

# ---- Trends in visas issued where the visa-holder hasn't yet arrived in the UK ----
# Calculate flow rates from applications to issuance to arrivals (and withdrawals/refusals)
# visas_flow <- 
#   visas_scraped |> 
#   pivot_wider(id_cols = c(Week, Scheme), names_from = Stage, values_from = Visas) |> 
#   mutate(
#     issued_rate = `visas issued` / `visa applications received`,
#     arrivals_rate = `arrivals of visa-holders in the UK` / `visas issued`,
#     withdraw_refuse_rate = (`applications withdrawn` + `applications refused`) / `visa applications received`
#   )
#   
# # Mean withdrawal/refusal rates by scheme
# visas_flow |> 
#   filter(!is.na(withdraw_refuse_rate)) |> 
#   group_by(Scheme) |> 
#   summarise(mean_rate = mean(withdraw_refuse_rate))
# 
# visas_flow |> 
#   select(Week, Scheme, `% of applications issued visas`= issued_rate, `% of issued visas arriving` = arrivals_rate) |> 
#   pivot_longer(cols = starts_with("%")) |> 
#   ggplot(aes(x = Week, y = value)) + 
#   geom_line(aes(colour = Scheme, lty = name), size = 1.1)
# 
# # Mean rates over the last four weeks
# visas_flow |> 
#   filter(Week >= 14) |> 
#   group_by(Scheme) |> 
#   summarise(
#     mean_issued_rate = mean(issued_rate),
#     mean_arrivals_rate = mean(arrivals_rate)
#   )

# Arrival rates return to May 2022’s highs
weekly_arrivals_total |>
  ggplot(aes(x = Date, y = `Weekly arrivals`)) +
  geom_line(size = 1.1) +
  
  scale_y_continuous(labels = scales::comma) +
  # scale_color_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    panel.grid.major.y = element_line(colour = "lightgrey", linetype = 2)
  ) +
  labs(
    title = "Weekly numbers of arrivals - UK totals",
    x = NULL,
    y = "Number of people arriving each week",
    colour = NULL,
    lty = NULL,
    caption = "British Red Cross analysis of DLUHC data"
  )

# - Calculate mean monthly arrivals -
weekly_arrivals_total |> 
  mutate(Month = month(Date)) |> 
  group_by(Month) |> 
  summarise(
    `Total monthly arrivals` = sum(`Weekly arrivals`, na.rm = TRUE),
    `Mean monthly arrivals` = mean(`Weekly arrivals`, na.rm = TRUE)
  )
