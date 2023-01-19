# ---- Load libs ----
library(tidyverse)
library(readxl)
library(lubridate)
library(hrbrthemes)

# ---- Load arrivals data ----
# - Arrivals from Ukraine -
# Calculated in analysis/ukraine/arrivals from Ukraine - trends and forecasts.R

# Arrivals across all schemes
arrivals <- read_csv("data/cumulative-visas/cumulative-visas-2023-01-09.csv") |>
  select(Date, Scheme, `Number of arrivals`) |>
  group_by(Date) |>
  summarise(arrivals = sum(`Number of arrivals`, na.rm = TRUE)) |>
  rename(date = Date) |>
  filter(date > "2022-04-01") |>
  mutate(growth_rate = (arrivals - lag(arrivals)) / lag(arrivals) * 100)

# ---- Load Cash Based Assistance (CBA) -----
# To generate the correct subset of data from RedRose, the following steps have
# to be taken:
# 1. Hover on the beneficiaries icon on the left hand side (3rd symbol down,
#    symbol = two people) and click "List"
# 2. Set a filter of "LOCATION LIKE UKRAINE"
# 3. Set a filter of "STATUS IN CANDIATE APPROVED"
# 4. Export the data (admin permissions required)
cba_raw <- read_excel("data/RedRose/redrose-filtered-17-01-23.xlsx")

# ---- Calculate people requesting cash since arrival ----
# Count the number of beneficiaries who have either been approved (status ==
# "Approved"), or are awaiting approval (status == "Candidate"), irrespective
# of wether they have yet received cash.
cba <-
  cba_raw |>
  slice(-1) |>
  select(
    date = `Registration Date`,
    children = `# of Children (<18)`,
    adults = `# of Adults (18+)`
  ) |>
  mutate(
    date = as_date(date),
    children = as.numeric(children),
    adults = as.numeric(adults)
  ) |>
  mutate(people = children + adults) |>
  group_by(date) |>
  summarise(people = sum(people)) |>
  mutate(people = cumsum(people)) |>
  filter(date > "2022-04-01") |>
  mutate(growth_rate = (people - lag(people)) / lag(people) * 100)

# ---- EDA ----
# Plot arrivals against cba
ggplot() +
  geom_line(
    data = arrivals,
    aes(x = date, y = arrivals)
  ) +
  geom_line(
    data = cba,
    aes(x = date, y = people),
    color = "red"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = NULL,
    y = "People"
  ) +
  theme_ipsum()

# Compare growth rates
ggplot() +
  geom_line(
    data = arrivals |> filter(date > "2022-07-01"),
    aes(x = date, y = growth_rate)
  ) +
  geom_line(
    data = cba |> filter(date > "2022-07-01"),
    aes(x = date, y = growth_rate),
    color = "red"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = NULL,
    y = "People"
  ) +
  theme_ipsum()

# Calculate growth rate over past 3 month period
arrivals_3_months_prior <- arrivals |>
  filter(date == max(date) - days(91)) |>
  pull(arrivals)

arrivals_latest <- arrivals |>
  filter(date == max(date)) |>
  pull(arrivals)

(arrivals_latest - arrivals_3_months_prior) / arrivals_3_months_prior * 100

cba_3_months_prior <- cba |>
  filter(date == max(date) - days(91)) |>
  pull(people)

latest_cba <- cba |>
  filter(date == max(date)) |>
  pull(people)

(latest_cba - cba_3_months_prior) / cba_3_months_prior * 100
