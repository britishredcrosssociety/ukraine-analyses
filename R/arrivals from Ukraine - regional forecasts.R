# - Arrival rates vary by region
#
# Assumptions:
# - Application processing rates do not vary by region (since they are centrally administered)
library(tidyverse)
library(lubridate)
library(geographr)
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

## Make a dataframe containing weekly visa data in each region ----
# Containing weekly applications, visas issued, and arrivals for the Homes for Ukraine / individual sponsorship scheme
# Note that regional Family Scheme and Super Sponsor Scheme data aren't available
weekly_visas_by_region <- 
  visas_ltla21_uk |> 
  
  # Drop counties
  filter(!str_detect(ltla21_code, "^E10")) |> 
  
  left_join(geographr::lookup_ltla21_brc) |> 
  
  arrange(Date) |> 

  # Calculate UK weekly totals
  mutate(Week = week(Date)) |> 
  group_by(Week, brc_area) |> 
  summarise(
    `Number of visa applications` = sum(`Number of visa applications`),
    `Number of visas issued` = sum(`Number of visas issued`),
    `Number of arrivals` = sum(`Number of arrivals`)
  ) |> 
  ungroup() |> 
  
  # Calculate week-on-week changes
  group_by(brc_area) |> 
  mutate(
    `Weekly applications` = `Number of visa applications` - lag(`Number of visa applications`),
    `Weekly visas issued` = `Number of visas issued` - lag(`Number of visas issued`),
    `Weekly arrivals` = `Number of arrivals` - lag(`Number of arrivals`)
  ) |> 
  ungroup() |> 
  
  select(Week, brc_area, starts_with("Weekly")) |> 
  
  arrange(brc_area, Week) |> 
  
  mutate(
    `% applications --> issued` = `Weekly visas issued` / `Weekly applications`,
    `% issued --> arrivals` = `Weekly arrivals` / `Weekly visas issued`
  ) |> 
  
  # Convert week number to date; source: https://stackoverflow.com/a/46183403
  mutate(Date = ymd("2022-01-03") + weeks(Week - 1)) |> 
  relocate(Date)

weekly_visas_by_region |> 
  ggplot(aes(x = Date, y = `Weekly applications`)) +
  geom_line(aes(colour = brc_area), size = 1.1) +
  geom_smooth(aes(colour = brc_area, fill = brc_area), method = "lm") +
  facet_wrap(~brc_area) +
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
    title = "Comparing weekly visa applications across regions",
    x = NULL,
    y = "Number of applications each week",
    colour = NULL,
    fill = NULL,
    caption = "Source: British Red Cross analysis of DLUHC data"
  )


## Make a dataframe containing cumulative visa data by region ----
# Containing total applications, visas issued, and arrivals for Homes for Ukraine / individual sponsorship scheme
# Note that regional Family Scheme and Super Sponsor Scheme data aren't available
cumulative_visas_by_region <- 
  visas_ltla21_uk |> 
  
  # Drop counties
  filter(!str_detect(ltla21_code, "^E10")) |> 
  
  left_join(geographr::lookup_ltla21_brc) |> 
  
  arrange(Date) |> 
  
  # Calculate UK weekly totals
  mutate(Week = week(Date)) |> 
  group_by(Week, brc_area) |> 
  summarise(
    `Number of visa applications` = sum(`Number of visa applications`),
    `Number of visas issued` = sum(`Number of visas issued`),
    `Number of arrivals` = sum(`Number of arrivals`)
  ) |> 
  ungroup() |> 

  arrange(brc_area, Week) |> 
  
  mutate(
    `% applications --> issued` = `Number of visas issued` / `Number of visa applications`,
    `% issued --> arrivals` = `Number of arrivals` / `Number of visas issued`
  ) |> 
  
  # Convert week number to date; source: https://stackoverflow.com/a/46183403
  mutate(Date = ymd("2022-01-03") + weeks(Week - 1)) |> 
  relocate(Date)

cumulative_visas_by_region |> 
  ggplot(aes(x = Date, y = `Number of visa applications`)) +
  geom_line(aes(colour = brc_area), size = 1.1) +
  facet_wrap(~brc_area) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    panel.grid.major.y = element_line(colour = "lightgrey", linetype = 2)
  ) +
  labs(
    title = "Comparing total visa applications across regions",
    x = NULL,
    y = "Number of applications each week",
    colour = NULL,
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

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

## Calculate historical weekly processing rates ----
historical_processing_rates <- 
  visas_scraped |> 
  select(-Visas, -Date) |> 
  filter(Scheme == "Ukraine Sponsorship Scheme") |> 
  
  pivot_wider(names_from = Stage, values_from = Visas_imputed) |> 
  
  # group_by(Scheme) |> 
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

# The average conversion rates are pretty similar for family and homes schemes, so just take one rate for simplicity
mean_applications_issued_rate <- mean(historical_processing_rates$`% backlog issued a visa this week`, na.rm = TRUE)
# min_applications_issued_rate <- min(historical_processing_rates$`% backlog issued a visa this week`, na.rm = TRUE)
# max_applications_issued_rate <- max(historical_processing_rates$`% backlog issued a visa this week`, na.rm = TRUE)

## Calculate regional arrival rates from the pool of visas issued ----
# Calculate weekly visa issued --> arrival rates individual sponsor scheme by region
arrival_rates_by_region <- 
  cumulative_visas_by_region |> 
  
  group_by(brc_area) |> 
  mutate(
    `New arrivals this week` = `Number of arrivals` - lag(`Number of arrivals`),
    `Issued visas yet to arrive` = `Number of visas issued` - `Number of arrivals`,
    
    `% issued visas arriving this week` = `New arrivals this week` / lag(`Issued visas yet to arrive`)
  ) |> 

  na.omit()  
  
  # group_by(brc_area) |>
  # summarise(
  #   max_arrival_rate = max(`% issued visas arriving this week`),
  #   min_arrival_rate = min(`% issued visas arriving this week`),
  #   mean_arrival_rate = mean(`% issued visas arriving this week`)
  # ) |> 
  # ungroup()


####
####
# Simulation functions ----
####
####

## Function for initialising simulation data ----
init_sim <- function(sim_start_week, num_weeks_to_simulate, brc_area) {
  tibble(
    Region = brc_area,
    Week = (sim_start_week + 1):(sim_start_week + num_weeks_to_simulate),
    
    `Backlog of visa applications - Homes for Ukraine` = NA,
    
    `Backlog of visas issued - Homes for Ukraine` = NA,
    `Backlog of visas issued - Homes for Ukraine (upper bound)` = NA,
    `Backlog of visas issued - Homes for Ukraine (lower bound)` = NA,
    
    `Weekly applications - Homes for Ukraine` = NA,
    
    `Arrival rate - Homes for Ukraine` = NA,
    `Arrival rate - Homes for Ukraine (upper bound)` = NA,
    `Arrival rate - Homes for Ukraine (lower bound)` = NA,
    
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
    
    # New arrivals this week, from last week's pool of issued visas
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Homes for Ukraine`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (upper bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Homes for Ukraine (upper bound)`, 0)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (lower bound)` <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` * simulated_visas[simulated_visas$Week == week, ]$`Arrival rate - Homes for Ukraine (lower bound)`, 0)

    # Make sure numbers of arrivals don't go below zero
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals`)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (upper bound)`)

    # Remove these arrivals from the visa backlog
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals`
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (upper bound)`
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` - simulated_visas[simulated_visas$Week == week, ]$`Weekly arrivals (lower bound)`

    # Make sure visa backlog doesn't go below zero
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)`)
    
    # Calculate new applications
    new_applications_homes_scheme <- 
      simulated_visas |> 
      filter(Week == (week)) |> 
      pull(`Weekly applications - Homes for Ukraine`)
    
    # Some of these applications will be withdrawn/refused
    new_applications_homes_scheme <- round(new_applications_homes_scheme * (1 - refusal_withdrawal_rate_homes_scheme), 0)
    
    # Add the remaining applications to the backlog
    simulated_visas[simulated_visas$Week == week, ]$`Weekly applications - Homes for Ukraine` <- new_applications_homes_scheme
    
    # Convert a proportion of last week's applications backlog into issued visas
    applications_issued_homes_scheme  <- round(simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Homes for Ukraine` * mean_applications_issued_rate, 0)
  
    # Add them to the visa backlogs
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` + applications_issued_homes_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` + applications_issued_homes_scheme
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` <- simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` + applications_issued_homes_scheme

    # Make sure visa backlog doesn't go below zero
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (lower bound)`)
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)` <- max(0, simulated_visas[simulated_visas$Week == week, ]$`Backlog of visas issued - Homes for Ukraine (upper bound)`)

    # Remove the newly issued visas from the application backlogs and add the new applications
    simulated_visas[simulated_visas$Week == week, ]$`Backlog of visa applications - Homes for Ukraine` <- simulated_visas[simulated_visas$Week == week - 1, ]$`Backlog of visa applications - Homes for Ukraine` - applications_issued_homes_scheme + new_applications_homes_scheme

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
# Baseline scenarios for arrivals by region ####
# This scenario assumes the following:
# - New visa applications submitted each week will follow the (declining) historical trend
# - Weekly arrival rates will following the declining historical trend
####
####

## Choose week numbers that the sim will run from and to ----
# (must be a week for which we already have DLUHC data; most likely, this will be the most recent week)
sim_start_week <-  
  weekly_visas_by_region |> 
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
  cumulative_visas_by_region |> 
  filter(Week == sim_start_week) |> 
  mutate(
    `Backlog of visas issued but not arrived` = `Number of visas issued` - `Number of arrivals`,
    `Backlog of visa applications but not issued` = `Number of visa applications` - `Number of visas issued`
  ) |> 
  select(brc_area, starts_with("Backlog"))

## Get total number of arrivals from the point at which the simulation started ----
total_arrivals <- 
  cumulative_visas_by_region |> 
  filter(Week == sim_start_week) |> 
  group_by(brc_area) |> 
  summarise(`Total arrivals` = sum(`Number of arrivals`))

## Loop over each region, calculate region-specific simulation parameters, and run the sim
simulated_visas_by_region <- list()  # A place to store all the simulation outputs

brc_areas <- geographr::lookup_ltla21_brc |> distinct(brc_area) |> pull(brc_area)

# current_region <- "Central"  # For testing
for (current_region in brc_areas) {

  ## Set up a tibble for the simulated data for current region ----
  simulated_visas_current_region <- 
    init_sim(sim_start_week, num_weeks_to_simulate, brc_area = current_region) |> 
    # Add most recent backlogs
    add_row(
      Region = current_region,
      Week = sim_start_week,
      
      `Backlog of visa applications - Homes for Ukraine` = visa_backlogs |> filter(brc_area == current_region) |> pull(`Backlog of visa applications but not issued`),
  
      `Backlog of visas issued - Homes for Ukraine` = visa_backlogs |> filter(brc_area == current_region) |> pull(`Backlog of visas issued but not arrived`),
      `Backlog of visas issued - Homes for Ukraine (upper bound)` = visa_backlogs |> filter(brc_area == current_region) |> pull(`Backlog of visas issued but not arrived`),
      `Backlog of visas issued - Homes for Ukraine (lower bound)` = visa_backlogs |> filter(brc_area == current_region) |> pull(`Backlog of visas issued but not arrived`),
  
      # Get most recent weekly application figures
      `Weekly applications - Homes for Ukraine` = weekly_visas_by_region |> filter(Week == sim_start_week & brc_area == current_region) |> pull(`Weekly applications`),
  
      `Arrival rate - Homes for Ukraine` = NA,
      `Arrival rate - Homes for Ukraine (upper bound)` = NA,
      `Arrival rate - Homes for Ukraine (lower bound)` = NA,
  
      `Weekly arrivals` = NA,
      `Weekly arrivals (upper bound)` = NA,
      `Weekly arrivals (lower bound)` = NA,
      
      `Total arrivals` = total_arrivals |> filter(brc_area == current_region) |> pull(`Total arrivals`),
      `Total arrivals (upper bound)` = total_arrivals |> filter(brc_area == current_region) |> pull(`Total arrivals`),
      `Total arrivals (lower bound)` = total_arrivals |> filter(brc_area == current_region) |> pull(`Total arrivals`)
    ) |> 
    arrange(Week)
  
  ## Predict number of new visa applications each week ----
  # Set up a tibble containing the week numbers for which we want to predict numbers of applications
  predicted_weeks <- 
    tibble(Week = (sim_start_week + 1):(sim_start_week + num_weeks_to_simulate))
  
  # Predict number of applications for the individual sponsorship scheme
  predicted_applications_homes_scheme <- 
    lm(`Weekly applications` ~ Week, data = weekly_visas_by_region |> filter(brc_area == current_region)) |> 
    broom::augment(newdata = predicted_weeks) |> 
    mutate(.fitted = if_else(.fitted < 0, 0, .fitted))
  
  # Put the predicted numbers of applications in the simulation tibble
  simulated_visas_current_region[simulated_visas_current_region$Week > sim_start_week, ]$`Weekly applications - Homes for Ukraine` <- predicted_applications_homes_scheme$.fitted
  
  ## Predict weekly arrival rates from historical trends ----
  predicted_arrivals_homes_scheme <- 
    lm(`% issued visas arriving this week` ~ Week, data = arrival_rates_by_region |> filter(brc_area == current_region)) |> 
    broom::augment(newdata = predicted_weeks, interval = "confidence") |> 
    mutate(
      .fitted = if_else(.fitted < 0, 0, .fitted),
      .lower = if_else(.lower < 0, 0, .lower),
      .upper = if_else(.upper < 0, 0, .upper)
    )
  
  # Put the predicted numbers of applications in the simulation tibble
  simulated_visas_current_region[simulated_visas_current_region$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine` <- predicted_arrivals_homes_scheme$.fitted
  simulated_visas_current_region[simulated_visas_current_region$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine (lower bound)` <- predicted_arrivals_homes_scheme$.lower
  simulated_visas_current_region[simulated_visas_current_region$Week > sim_start_week, ]$`Arrival rate - Homes for Ukraine (upper bound)` <- predicted_arrivals_homes_scheme$.upper
  
  ## Run the simulation ----
  simulated_visas_current_region <- 
    run_visa_simulation(simulated_visas_current_region)
  
  simulated_visas_by_region[[current_region]] <- simulated_visas_current_region

  print(paste0("Finished simulation for ", current_region))
}

# Combine all simulated data into one tibble
simulated_visas_by_region <- 
  bind_rows(simulated_visas_by_region) |> 
  select(Region, Date, Week, starts_with("Weekly arrivals"), starts_with("Total"))

# Plot forecasts
cumulative_visas_by_region |> 
  rename(Region = brc_area) |> 
  
  ggplot(aes(x = Date, y = `Number of arrivals`)) +
  geom_col(aes(fill = Region), colour = "white") +
  
  # Add simulated arrivals
  geom_ribbon(
    data = simulated_visas_by_region,
    inherit.aes = FALSE,
    mapping = aes(x = Date, ymin = `Total arrivals (lower bound)`, ymax = `Total arrivals (upper bound)`),
    fill = "grey80",
    alpha = 0.4
  ) +
  geom_line(
    data = simulated_visas_by_region,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (lower bound)`, group = Region),
    colour = "grey60",
    lty = 2
  ) +
  geom_line(
    data = simulated_visas_by_region,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (upper bound)`, group = Region),
    colour = "grey60",
    lty = 2
  ) +
  
  # Central estimate
  geom_line(
    data = simulated_visas_by_region,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals`, group = Region)
  ) +
  
  geom_vline(xintercept = min(simulated_visas_by_region$Date), lty = 2, colour = "grey") +
  annotate(
    "text", x = min(simulated_visas_by_region$Date) + 50, y = max(simulated_visas_by_region$`Total arrivals (upper bound)`),
    label = "→ Forecast", colour = "grey", size = 3.5
  ) +
  
  facet_wrap(~Region) +
  
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, .1))) +
  scale_fill_brewer(palette = "Set2") +
  
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot"
  ) +
  labs(
    title = "Historical and predicted arrivals from Ukraine by British Red Cross area",
    x = NULL,
    caption = "Source: British Red Cross analysis and simulation of DLUHC data"
  )

ggsave(filename = "images/forecast arrivals by region.png", height = 200, width = 230, units = "mm")

write_csv(simulated_visas_by_region, glue::glue("output-data/simulations/simulation-by-region-{min(simulated_visas_by_region$Date)}.csv"))

# Wales only
simulated_visas_by_region_wales <- 
  simulated_visas_by_region |> 
  filter(Region == "Wales")

cumulative_visas_by_region |> 
  rename(Region = brc_area) |> 
  filter(Region == "Wales") |> 
  
  ggplot(aes(x = Date, y = `Number of arrivals`)) +
  geom_col(aes(fill = Region), colour = "white") +
  
  # Add simulated arrivals
  geom_ribbon(
    data = simulated_visas_by_region_wales,
    inherit.aes = FALSE,
    mapping = aes(x = Date, ymin = `Total arrivals (lower bound)`, ymax = `Total arrivals (upper bound)`),
    fill = "grey80",
    alpha = 0.4
  ) +
  geom_line(
    data = simulated_visas_by_region_wales,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (lower bound)`, group = Region),
    colour = "grey60",
    lty = 2
  ) +
  geom_line(
    data = simulated_visas_by_region_wales,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals (upper bound)`, group = Region),
    colour = "grey60",
    lty = 2
  ) +
  
  # Central estimate
  geom_line(
    data = simulated_visas_by_region_wales,
    inherit.aes = FALSE,
    mapping = aes(x = Date, y = `Total arrivals`, group = Region)
  ) +
  
  geom_vline(xintercept = min(simulated_visas_by_region_wales$Date), lty = 2, colour = "grey") +
  annotate(
    "text", x = min(simulated_visas_by_region_wales$Date) + 50, y = max(simulated_visas_by_region_wales$`Total arrivals (upper bound)` + 100),
    label = "→ Forecast", colour = "grey", size = 3.5
  ) +
  
  facet_wrap(~Region) +
  
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, .1))) +
  scale_fill_brewer(palette = "Set2") +
  
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot"
  ) +
  labs(
    title = "Historical and predicted arrivals into Wales from Ukraine",
    x = NULL,
    caption = "Source: British Red Cross analysis and simulation of DLUHC data"
  )

ggsave(filename = "images/forecast arrivals by region - Wales.png", height = 100, width = 130, units = "mm")

simulated_visas_by_region_wales |> 
  filter(Week == max(Week) | Week == min(Week) + 12) |> 
  select(Week, starts_with("Total"))
