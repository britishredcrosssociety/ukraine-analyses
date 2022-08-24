library(tidyverse)
library(patchwork)

## Load DLUHC data ----
source("R/load Ukraine visa data - Local Authorities.R")
source("R/load Ukraine visa data - scraped.R")

# ---- Tidy up UK-wide summary data ----
visas_summary <- 
  visas_ltla21_summary |> 
  # Keep only totals for each nation
  filter(
    Location %in% c("England", "Northern Ireland") | Type == "Total"
  ) |> 
  select(-Type) |> 
  rename(`Number of arrivals` = `Number of arrivals in the UK by sponsor location`) |> 
  pivot_longer(cols = starts_with("Number"), names_to = "Stage", values_to = "Visas")

# Scenario: People already issued visas but yet to arrive in the UK ----
# - Same as above, but from the weekly summary data (so not including withdrawals/refusals) -
visas_flow <- 
  visas_summary |> 
  arrange(Date) |> 
  pivot_wider(names_from = Stage, values_from = Visas) |> 
  mutate(
    `% of visas issued resulting in arrivals` = `Number of arrivals` / `Number of visas issued`,
    `Number of people with visas issued who are yet to arrive` = `Number of visas issued` - `Number of arrivals`,
    `% of applications resulting in visas issued` = `Number of visas issued` / `Number of visa applications`,
    `Number of applications still to process` = `Number of visa applications` - `Number of visas issued`
  )

# - Plot numbers and proportions of visas issued yet to arrive -
# Plot number of applications still to be processed and number of people still to arrive
plt_number_arrivals <- 
  visas_flow |> 
  select(Date, Location, `Number of people with visas issued who are yet to arrive`) |> 
  ggplot(aes(x = Date, y = `Number of people with visas issued who are yet to arrive`)) + 
  geom_line(aes(colour = Location), size = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "top",
    # plot.title.position = "plot"
  ) +
  labs(
    title = "Number of people with visas issued who are yet to arrive",
    x = NULL,
    y = "Number of people",
    colour = NULL,
    caption = "British Red Cross analysis of DLUHC data"
  )

plt_percent_arrivals <- 
  visas_flow |> 
  select(Date, Location, `% of visas issued resulting in arrivals`) |> 
  ggplot(aes(x = Date, y = `% of visas issued resulting in arrivals`)) + 
  geom_line(aes(colour = Location), size = 1.1) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  theme(
    legend.position = "top",
    # plot.title.position = "plot"
  ) +
  labs(
    title = "% of visas issued resulting in arrivals",
    x = NULL,
    y = "Percentage of visas",
    colour = NULL
  )

guide_area() + 
  (plt_percent_arrivals + plt_number_arrivals) +
  plot_layout(
    guides = "collect", 
    nrow = 2,
    heights = c(1, 10)
  ) + 
  plot_annotation(title = "Not all people issued visas have arrived in the UK, and the number of potential arrivals varies from nation to nation") +
  theme(legend.position = "top")

ggsave("images/people issued visas but yet to arrive.png", width = 270, height = 150, units = "mm")

# - Project potential number of arrivals on already-issused visas... -
#... based on historical highs in arrival rates in the nation (for a lower bound) and highest rate in England (as an upper bound)

# England projection
england_visas_issued_yet_to_arrive <- 
  visas_flow |> 
  filter(Date == max(Date) & Location == "England") |> 
  pull(`Number of people with visas issued who are yet to arrive`)

england_highest_arrival_rate <- 
  visas_flow |> 
  filter(Location == "England") |> 
  filter(`% of visas issued resulting in arrivals` == max(`% of visas issued resulting in arrivals`, na.rm = TRUE)) |> 
  pull(`% of visas issued resulting in arrivals`)

england_min_arrival_rate <- 
  visas_flow |> 
  filter(Location == "England") |> 
  filter(`% of visas issued resulting in arrivals` == min(`% of visas issued resulting in arrivals`, na.rm = TRUE)) |> 
  pull(`% of visas issued resulting in arrivals`)

england_visas_issued_yet_to_arrive * england_min_arrival_rate     # Lower bound in English potential arrivals
england_visas_issued_yet_to_arrive * england_highest_arrival_rate  # Upper bound in English potential arrivals

# Scotland projection
scotland_visas_issued_yet_to_arrive <- 
  visas_flow |> 
  filter(Date == max(Date) & Location == "Scotland") |> 
  pull(`Number of people with visas issued who are yet to arrive`)

scotland_highest_arrival_rate <- 
  visas_flow |> 
  filter(Location == "Scotland") |> 
  filter(`% of visas issued resulting in arrivals` == max(`% of visas issued resulting in arrivals`, na.rm = TRUE)) |> 
  pull(`% of visas issued resulting in arrivals`)

scotland_visas_issued_yet_to_arrive * scotland_highest_arrival_rate  # Lower bound in Scottish potential arrivals
scotland_visas_issued_yet_to_arrive * england_highest_arrival_rate   # Upper bound in Scottish potential arrivals

# Wales projection
wales_visas_issued_yet_to_arrive <- 
  visas_flow |> 
  filter(Date == max(Date) & Location == "Wales") |> 
  pull(`Number of people with visas issued who are yet to arrive`)

wales_highest_arrival_rate <- 
  visas_flow |> 
  filter(Location == "Wales") |> 
  filter(`% of visas issued resulting in arrivals` == max(`% of visas issued resulting in arrivals`, na.rm = TRUE)) |> 
  pull(`% of visas issued resulting in arrivals`)

wales_visas_issued_yet_to_arrive * wales_highest_arrival_rate    # Lower bound in Welsh potential arrivals
wales_visas_issued_yet_to_arrive * england_highest_arrival_rate  # Upper bound in Welsh potential arrivals

# NI projection
northern_ireland_visas_issued_yet_to_arrive <- 
  visas_flow |> 
  filter(Date == max(Date) & Location == "Northern Ireland") |> 
  pull(`Number of people with visas issued who are yet to arrive`)

northern_ireland_highest_arrival_rate <- 
  visas_flow |> 
  filter(Location == "Northern Ireland") |> 
  filter(`% of visas issued resulting in arrivals` == max(`% of visas issued resulting in arrivals`, na.rm = TRUE)) |> 
  pull(`% of visas issued resulting in arrivals`)

northern_ireland_visas_issued_yet_to_arrive * northern_ireland_highest_arrival_rate  # Lower bound in NI potential arrivals
northern_ireland_visas_issued_yet_to_arrive * england_highest_arrival_rate           # Upper bound in NI potential arrivals
