library(tidyverse)

# ---- Load latest DLUHC data ----
source("R/load Ukraine visa data - Local Authorities.R")
source("R/load Ukraine visa data - scraped.R")

# ---- Make a dataframe containing cumulative visa data ----
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

# ---- Make a dataframe containing weekly visa data ----
weekly_visas <- 
  visas_scraped |> 
  arrange(Date) |> 
  filter(str_detect(Stage, "arrivals")) |> 
  
  select(-Date, -Visas) |> 
  
  pivot_wider(names_from = Stage, values_from = Visas_imputed) |> 
  
  group_by(Week) |> 
  summarise(
    `arrivals of visa-holders in the UK` = sum(`arrivals of visa-holders in the UK`, na.rm = TRUE)
  ) |> 
  
  # Calculate week-on-week changes
  mutate(
    `Weekly arrivals` = `arrivals of visa-holders in the UK` - lag(`arrivals of visa-holders in the UK`)
  ) |> 
  
  select(Week, starts_with("Weekly"))

# ---- Load simulated data (baseline scenario) ----
# Load predictions for the most recently published DLUHC data
simulated_visas_baseline <- read_csv("output-data/simulations/simulation-baseline-2022-09-12.csv")

# Load all simulation data into a `sim_data` tibble
i <- 1
sim_data <- list()
sim_files <- list.files("output-data/simulations", pattern = "baseline", full.names = TRUE)

for (sim_file in sim_files) {
  # Extract date from filename
  # Regex taken from https://regexland.com/regex-dates/
  sim_date <- str_extract(sim_file, "\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])")
  
  # Load simulated data and pre-pend the simulation date
  sim_data[[i]] <- read_csv(sim_file) |> 
    mutate(`Simulation date` = ymd(sim_date)) |> 
    relocate(`Simulation date`)
  
  i <- i + 1
}

# Convert list of tibbles to one flat tibble
sim_data <- bind_rows(sim_data)

# ---- How closely did we predict the total number of most recent arrivals? ----
# predicted_arrivals <- 
#   simulated_visas_baseline |> 
#   filter(Date == max(cumulative_visas_by_scheme$Date)) |> 
#   pull(`Total arrivals`)

# Fetch total arrivals from the most recently run simulation that covers the most up-to-date visa data
predicted_arrivals <- 
  sim_data |> 
  filter(Date == max(cumulative_visas_by_scheme$Date)) |> 
  filter(`Simulation date` == max(`Simulation date`)) |> 
  pull(`Total arrivals`)

observed_arrivals <- 
  cumulative_visas_by_scheme |> 
  filter(Date == max(Date)) |> 
  summarise(total = sum(`Number of arrivals`)) |> 
  pull(total)

scales::comma(predicted_arrivals)
scales::comma(observed_arrivals)
abs(observed_arrivals - predicted_arrivals)

# ---- Plot historical and simulated arrivals ----
date_to_focus_on <- cumulative_visas_by_scheme |> filter(Date == max(Date)) |> distinct(Date) |> pull(Date)
date_text <- str_glue("{day(date_to_focus_on)} {month.name[month(date_to_focus_on)]} {year(date_to_focus_on)}")

cumulative_visas_by_scheme |> 
  ggplot(aes(x = Date, y = `Number of arrivals`)) +
  geom_col(position = "stack", colour = "white", fill = "grey60") +
  
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
  
  # Highlight prediction and observation
  geom_rect(
    data = tibble(
      xmin = date_to_focus_on - ddays(5),
      xmax = date_to_focus_on + ddays(5),
      ymin = observed_arrivals - 10000,
      ymax = observed_arrivals + 10000
      # ymin = 105000,
      # ymax = 125000
    ),
    inherit.aes = FALSE,
    mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha = 0.2,
    size = 2,
    fill = NA,
    colour = "gold"
  ) +
  coord_cartesian(
    # xlim = c(ymd("2022-07-28"), ymd("2022-08-25")),
    xlim = c(
      date_to_focus_on - ddays(15),
      date_to_focus_on + ddays(10)
    ),
    ylim = c(90000, 150000)
  ) +
  
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, .1))) +
  scale_fill_brewer(palette = "Set2") +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot"
  ) +
  labs(
    title = str_glue("Our simulation accurately predicted the observed numbers of arrivals as of {date_text}"),
    subtitle = str_wrap(str_glue("We predicted {scales::comma(predicted_arrivals)} people would have arrived (in total); {scales::comma(observed_arrivals)} did."), 100),
    x = NULL,
    caption = "Source: British Red Cross analysis and simulation of DLUHC data"
  )

ggsave("images/forecast arrivals - testing the simulation on newly observed data.png", height = 150, width = 200, units = "mm")

# ---- Plot predictions 1, 2, ...n weeks ahead against observed data ----
# When did our predictions start?
sim_start_week <- 
  sim_data |> 
  filter(month(`Simulation date`) >= 8) |> 
  filter(Date == min(Date)) |> 
  pull(Date)

# Get actual arrival figures, starting from when we made our first prediction
observed_arrivals <- 
  cumulative_visas_by_scheme |> 
  filter(Date >= sim_start_week) |> 
  group_by(Date) |> 
  summarise(`Actual arrivals` = sum(`Number of arrivals`))

# Get predicted arrivals over the same period
predicted_arrivals <- 
  sim_data |> 
  filter(Date %in% observed_arrivals$Date) |> 
  select(Date, `Simulation date`, `Total arrivals`, `Total arrivals (lower bound)`, `Total arrivals (upper bound)`)

# Plot series of predictions against observed data
predicted_arrivals |> 
  ggplot(aes(x = factor(Date), y = `Total arrivals`)) +
  geom_pointrange(aes(ymin = `Total arrivals (lower bound)`, ymax = `Total arrivals (upper bound)`, colour = factor(`Simulation date`)), position = position_dodge(width = 0.2)) +
  geom_point(
    data = observed_arrivals,
    inherit.aes = FALSE,
    mapping = aes(x = factor(Date), y = `Actual arrivals`),
    shape = 4,
    size = 1.5
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot"
  ) +
  labs(
    title = "How close were our predictions?",
    subtitle = str_wrap("Dots and lines show predictions with upper/lower bounds, coloured by when the prediction was made. 'X's are the actual number of arrivals in a given week.", 80),
    colour = "Date prediction made",
    x = NULL,
    y = "Total arrivals (actual and predicted)"
  )

ggsave("images/simulation/How close were our predictions.png", width = 150, height = 120, units = "mm")

# ---- Plot predictions 1, 2, ...n weeks ahead against observed data - using weekly data ----
# When did our predictions start?
sim_start_week <- 
  sim_data |> 
  filter(month(`Simulation date`) >= 8) |> 
  filter(Week == min(Week)) |> 
  pull(Week)

# Get actual arrival figures, starting from when we made our first prediction
observed_arrivals <- 
  weekly_visas |> 
  filter(Week >= sim_start_week) |> 
  group_by(Week) |> 
  summarise(`Actual arrivals` = sum(`Weekly arrivals`))

# Get predicted arrivals over the same period
predicted_arrivals <- 
  sim_data |> 
  filter(Week %in% observed_arrivals$Week) |> 
  select(Week, `Simulation date`, `Weekly arrivals`, `Weekly arrivals (lower bound)`, `Weekly arrivals (upper bound)`)

# Plot series of predictions against observed data
predicted_arrivals |> 
  ggplot(aes(x = factor(Week), y = `Weekly arrivals`)) +
  geom_pointrange(aes(ymin = `Weekly arrivals (lower bound)`, ymax = `Weekly arrivals (upper bound)`, colour = factor(`Simulation date`)), position = position_dodge(width = 0.2)) +
  geom_point(
    data = observed_arrivals,
    inherit.aes = FALSE,
    mapping = aes(x = factor(Week), y = `Actual arrivals`),
    shape = 4,
    size = 1.5
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot"
  ) +
  labs(
    title = "How close were our predictions?",
    subtitle = str_wrap("Dots and lines show predictions with upper/lower bounds, coloured by when the prediction was made. 'X's are the actual number of arrivals in a given week.", 80),
    colour = "Date prediction made",
    x = "Week number",
    y = "Weekly arrivals (actual and predicted)"
  )

ggsave("images/simulation/How close were our predictions - weekly arrivals.png", width = 150, height = 120, units = "mm")

# ---- How well do we predict arrivals one week ahead of time (calculate Root Mean Square Error)? ----
# Keep only simulations that are nearest to the observed data date (those are the sims we ran a week in advance)
predicted_arrivals |> 
  #filter(Date == `Simulation date`) |> 
  filter(month(`Simulation date`) >= 8) |> 
  left_join(observed_arrivals) |> 
  yardstick::rmse(`Actual arrivals`, `Weekly arrivals`)

# ---- How have our predictions changed over time? ----
# Which weeks do we have multiple predictions for?
sim_multiple_weeks <- 
  sim_data |> 
  count(Week) |> 
  filter(n > 1)

sim_data |> 
  filter(Week %in% sim_multiple_weeks$Week) |> 
  
  ggplot(aes(x = Date, y = `Total arrivals`)) +
  geom_pointrange(aes(ymin = `Total arrivals (lower bound)`, ymax = `Total arrivals (upper bound)`, colour = factor(`Simulation date`)), position = position_dodge(width = 4)) +
  
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot"
  ) +
  labs(
    title = "How do our predictions change over time?",
    subtitle = "Dots and lines show predictions with upper/lower bounds, coloured by when the prediction was made.",
    colour = "Date prediction made",
    x = NULL,
    y = "Predicted arrivals"
  )
