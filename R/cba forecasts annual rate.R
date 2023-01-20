# ---- Notes ----
# CBA information:
# 1. People make a request for CBA via the National Support Line, or somebody
#    from the local crisis response teams makes a request on their behalf. This
#    means there are two official pathways.
# 2. People must make a request within 14 days of arrival. A very small number
#    are granted some discretion on this period, but they make up only a
#    handful of people and can be ignored for this analysis.
# 3. Most rejected applications are due to people either chasing an initial
#    request, trying their luck trying to duplicate a request, or are abusing
#    the system to try and get free cash. This therefore means most people who
#    do apply for CBA and are eligible, receive it. Quantifying this unkown
#    paramater is difficult.

# Simulation steps:
# 1. Estimate the period of time from people arriving to people making a request
#    using domain knowledge from colleagues. Answer: 14 days.
# 2. From the historic arrivals data, slice the weeks off from point one to
#    align the data sets. E.g., assume that all arrivals will have made a
#    request by this point in time.
# 3. Calculate the percentage of arrivals that make a request and get granted
#    cash. This number needs to be calculated as the number of people issued
#    cash, and not the number of applications. This is because one application
#    can be for more than one person. These beneficiary rates can be calculated
#    from the RedRose data. Think if the backlog affects this (I don't think
#    so, as the approval rate should probably be the same in the backlog). Make
#    sure the rejection rate is factored into this equation.
# 4. Apply the conversion rate to the lower bound, point estimate, and upper
#    bounds of the simulated arrivals data (baseline and surge).
# 5. Build extra simulations:
#       - We expect less people to apply for cash as number per application
#         drops as less families with young children arrive
#       - We expect more people to apply for cash as new arrivals have less cash
#         available than initial cohort

# ---- Load libs ----
library(tidyverse)
library(readxl)
library(lubridate)
library(hrbrthemes)

# ---- Load arrivals data ----
# - Arrivals from Ukraine -
# Calculated in analysis/ukraine/arrivals from Ukraine - trends and forecasts.R

# Historic data
historic_cumulative_data <- read_csv("data/cumulative-visas/cumulative-visas-2023-01-09.csv")

# Simulated data
simulation_baseline <-
  read_csv("output-data/simulations/simulation-baseline-2023-01-16.csv") |>
  select(Date, Week, `Weekly arrivals`, `Weekly arrivals (upper bound)`, `Weekly arrivals (lower bound)`)

simulation_surge <-
  read_csv("output-data/simulations/simulation-surge-2023-01-16.csv") |>
  select(Date, Week, `Weekly arrivals`, `Weekly arrivals (upper bound)`, `Weekly arrivals (lower bound)`)

# ---- Load Cash Based Assistance (CBA) -----
# To generate the correct subset of data from RedRose, the following steps have
# to be taken:
# 1. Hover on the beneficiaries icon on the left hand side (3rd symbol down,
#    symbol = two people) and click "List"
# 2. Set a filter of "LOCATION LIKE UKRAINE"
# 3. Set a filter of "STATUS IN CANDIATE APPROVED"
# 4. Export the data (admin permissions required)
cba_raw <-
  read_excel("data/RedRose/redrose-filtered-17-01-23.xlsx")

# ---- Calculate people requesting cash since arrival ----
# Count the number of beneficiaries who have either been approved (status ==
# "Approved"), or are awaiting approval (status == "Candidate"), irrespective
# of wether they have yet received cash.
cba_people <-
  cba_raw |>
  slice(-1) |>
  select(
    children = `# of Children (<18)`,
    adults = `# of Adults (18+)`,
    status = Status
  ) |>
  mutate(
    children = as.numeric(children),
    adults = as.numeric(adults)
  ) |>
  mutate(people = children + adults) |>
  relocate(people, .before = status) |>
  summarise(total_people = sum(people)) |>
  pull(total_people)

# Next, we need to estimate the number of people who have made an application
# for CBA via the National Support Line (NSL), but have not yet been added to
# RedRose. This data can be found on the Ukraine PowerBI "Cash Card applications
# pipline" dashboard:
# https://app.powerbi.com/groups/me/apps/cfa2f05f-4b8f-4a5a-8699-5ae17e52b399/reports/6cacaec6-508d-4ac2-80c8-2da3379b647b/ReportSectionc1a9b3b105a37c566358?ctid=fedc3cba-ca5e-4388-a837-b45c7f0d71b7

# To do this, we need to:
#   1. Calculate the rate of applicaions which get approved. This can be
#      estimated by taking the current number of applications submitted
#      (nsl_num_applications) to the NSL, subtracting those which are currently
#      pending ID checks (nsl_num_pending) or waiting to be added to RedRose
#      (nsl_num_waiting), and calculating this figure as a percentage of the number
#      of rejected applications (nsl_num_reject)
#   2. The approval rate can then be multiplied by the number pending
#      identify checks to calculate the percentage of this group expected to be
#      added to RedRose.
#   3. The number of applications waiting to be added to RedRose can then be
#      added to get the total number of applications that will be added to
#      RedRose.
#   4. The total number of applications can the multiplied by the average of
#      number of people per application, to get a total
#      number of expected people.

# Step one
nsl_num_rejected <- 1523
nsl_num_applications <- 26230
nsl_num_pending <- 88
nsl_num_waiting <- 0

nsl_approval_rate <-
  1 - (nsl_num_rejected / (nsl_num_applications - nsl_num_pending - nsl_num_waiting))

# Step two
nsl_num_pending_and_approved <- round(nsl_num_pending * nsl_approval_rate)

# Step three
nsl_total_applications <- nsl_num_pending_and_approved + nsl_num_waiting

# Step four
average_people_per_application <-
  cba_raw |>
  slice(-1) |>
  select(
    children = `# of Children (<18)`,
    adults = `# of Adults (18+)`,
    status = Status
  ) |>
  mutate(
    children = as.numeric(children),
    adults = as.numeric(adults)
  ) |>
  mutate(people = children + adults) |>
  summarise(average = mean(people)) |>
  pull(average)

nsl_people <- round(nsl_total_applications * average_people_per_application)

require_cash_count <- cba_people + nsl_people

# ---- Calculate % arrivals that have claimed cash ----
# Filter histroic arrivals data to two weeks before todays date. This is because
# people have 14 days to make a CBA application upon arrival. While some people
# from the previous two weeks may have already trickled into the system (both
# NSL + RedRose) setting a two week threshold will ensure all those who could
# have applied for CBA will be captured in the data. This may bias the data to
# overestimate the number who have applied for cash from the given time period
# (due to people trickling in), but this is preferred to under estimating cash.
num_arrivals <-
  historic_cumulative_data |>
  filter(Date < today() - days(14)) |>
  group_by(Week) |>
  summarise(arrivals = sum(`Number of arrivals`)) |>
  filter(arrivals == max(arrivals, na.rm = TRUE)) |>
  pull(arrivals)

# Calculate percentage of arrivals that have or will claim cash
require_cash_percentage <- require_cash_count / num_arrivals

# ---- Simulations ----
# Expected scenario
baseline_cash <-
  simulation_baseline |>
  select(
    Date,
    Week,
    `Arrivals` = `Weekly arrivals`,
    `Arrivals (lower bound)` = `Weekly arrivals (lower bound)`,
    `Arrivals (upper bound)` = `Weekly arrivals (upper bound)`
  ) |>
  mutate(
    `Arrivals requiring cash` = `Arrivals` * require_cash_percentage,
    `Arrivals requiring cash (lower bound)` = `Arrivals (lower bound)` * require_cash_percentage,
    `Arrivals requiring cash (upper bound)` = `Arrivals (upper bound)` * require_cash_percentage
  ) |>
  mutate(
    `Cash amount` = `Arrivals requiring cash` * 50,
    `Cash amount (lower bound)` = `Arrivals requiring cash (lower bound)` * 50,
    `Cash amount (upper bound)` = `Arrivals requiring cash (upper bound)` * 50
  ) |>
  mutate(
    `Cumulative cash amount` = cumsum(`Cash amount`),
    `Cumulative cash amount (lower bound)` = cumsum(`Cash amount (lower bound)`),
    `Cumulative cash amount (upper bound)` = cumsum(`Cash amount (upper bound)`)
  )

write_csv(
  baseline_cash,
  glue::glue("data/cba-simulations/simulation-baseline-cash-{min(baseline_cash$Date)}.csv")
)

# # For surge accumulated cash, the accumulation value from week 50 from the
# # baseline scenario must be added on (as surge starts at week 51)
# surge_cash <-
#   simulation_surge |>
#   select(
#     Date,
#     Week,
#     `Arrivals` = `Weekly arrivals`,
#     `Arrivals (lower bound)` = `Weekly arrivals (lower bound)`,
#     `Arrivals (upper bound)` = `Weekly arrivals (upper bound)`
#   ) |>
#   mutate(
#     `Arrivals requiring cash` = `Arrivals` * require_cash_percentage,
#     `Arrivals requiring cash (lower bound)` = `Arrivals (lower bound)` * require_cash_percentage,
#     `Arrivals requiring cash (upper bound)` = `Arrivals (upper bound)` * require_cash_percentage
#   ) |>
#   mutate(
#     `Cash amount` = `Arrivals requiring cash` * 50,
#     `Cash amount (lower bound)` = `Arrivals requiring cash (lower bound)` * 50,
#     `Cash amount (upper bound)` = `Arrivals requiring cash (upper bound)` * 50
#   ) |>
#   mutate(
#     `Cumulative cash amount` = cumsum(`Cash amount`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount`) |> pull()),
#     `Cumulative cash amount (lower bound)` = cumsum(`Cash amount (lower bound)`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount (lower bound)`) |> pull()),
#     `Cumulative cash amount (upper bound)` = cumsum(`Cash amount (upper bound)`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount (upper bound)`) |> pull())
#   )

surge_cash <-
  simulation_surge |>
  select(
    Date,
    Week,
    `Arrivals` = `Weekly arrivals`,
    `Arrivals (lower bound)` = `Weekly arrivals (lower bound)`,
    `Arrivals (upper bound)` = `Weekly arrivals (upper bound)`
  ) |>
  mutate(
    `Arrivals requiring cash` = `Arrivals` * require_cash_percentage,
    `Arrivals requiring cash (lower bound)` = `Arrivals (lower bound)` * require_cash_percentage,
    `Arrivals requiring cash (upper bound)` = `Arrivals (upper bound)` * require_cash_percentage
  ) |>
  mutate(
    `Cash amount` = `Arrivals requiring cash` * 50,
    `Cash amount (lower bound)` = `Arrivals requiring cash (lower bound)` * 50,
    `Cash amount (upper bound)` = `Arrivals requiring cash (upper bound)` * 50
  ) |>
  mutate(
    `Cumulative cash amount` = cumsum(`Cash amount`),
    `Cumulative cash amount (lower bound)` = cumsum(`Cash amount (lower bound)`),
    `Cumulative cash amount (upper bound)` = cumsum(`Cash amount (upper bound)`)
  )

write_csv(
  surge_cash,
  glue::glue("data/cba-simulations/simulation-surge-cash-{min(baseline_cash$Date)}.csv")
)

# # Less percentage of people arriving require cash
# baseline_cash_less <-
#   simulation_baseline |>
#   select(
#     Date,
#     Week,
#     `Arrivals` = `Weekly arrivals`,
#     `Arrivals (lower bound)` = `Weekly arrivals (lower bound)`,
#     `Arrivals (upper bound)` = `Weekly arrivals (upper bound)`
#   ) |>
#   mutate(
#     `Arrivals requiring cash` = `Arrivals` * (require_cash_percentage - 0.1),
#     `Arrivals requiring cash (lower bound)` = `Arrivals (lower bound)` * (require_cash_percentage - 0.1),
#     `Arrivals requiring cash (upper bound)` = `Arrivals (upper bound)` * (require_cash_percentage - 0.1)
#   ) |>
#   mutate(
#     `Cash amount` = `Arrivals requiring cash` * 50,
#     `Cash amount (lower bound)` = `Arrivals requiring cash (lower bound)` * 50,
#     `Cash amount (upper bound)` = `Arrivals requiring cash (upper bound)` * 50
#   ) |>
#   mutate(
#     `Cumulative cash amount` = cumsum(`Cash amount`),
#     `Cumulative cash amount (lower bound)` = cumsum(`Cash amount (lower bound)`),
#     `Cumulative cash amount (upper bound)` = cumsum(`Cash amount (upper bound)`)
#   )

# surge_cash_less <-
#   simulation_surge |>
#   select(
#     Date,
#     Week,
#     `Arrivals` = `Weekly arrivals`,
#     `Arrivals (lower bound)` = `Weekly arrivals (lower bound)`,
#     `Arrivals (upper bound)` = `Weekly arrivals (upper bound)`
#   ) |>
#   mutate(
#     `Arrivals requiring cash` = `Arrivals` * (require_cash_percentage - 0.1),
#     `Arrivals requiring cash (lower bound)` = `Arrivals (lower bound)` * (require_cash_percentage - 0.1),
#     `Arrivals requiring cash (upper bound)` = `Arrivals (upper bound)` * (require_cash_percentage - 0.1)
#   ) |>
#   mutate(
#     `Cash amount` = `Arrivals requiring cash` * 50,
#     `Cash amount (lower bound)` = `Arrivals requiring cash (lower bound)` * 50,
#     `Cash amount (upper bound)` = `Arrivals requiring cash (upper bound)` * 50
#   ) |>
#   mutate(
#     `Cumulative cash amount` = cumsum(`Cash amount`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount`) |> pull()),
#     `Cumulative cash amount (lower bound)` = cumsum(`Cash amount (lower bound)`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount (lower bound)`) |> pull()),
#     `Cumulative cash amount (upper bound)` = cumsum(`Cash amount (upper bound)`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount (upper bound)`) |> pull())
#   )

# # More percentage of people arriving require cash
# baseline_cash_more <-
#   simulation_baseline |>
#   select(
#     Date,
#     Week,
#     `Arrivals` = `Weekly arrivals`,
#     `Arrivals (lower bound)` = `Weekly arrivals (lower bound)`,
#     `Arrivals (upper bound)` = `Weekly arrivals (upper bound)`
#   ) |>
#   mutate(
#     `Arrivals requiring cash` = `Arrivals` * (require_cash_percentage + 0.1),
#     `Arrivals requiring cash (lower bound)` = `Arrivals (lower bound)` * (require_cash_percentage + 0.1),
#     `Arrivals requiring cash (upper bound)` = `Arrivals (upper bound)` * (require_cash_percentage + 0.1)
#   ) |>
#   mutate(
#     `Cash amount` = `Arrivals requiring cash` * 50,
#     `Cash amount (lower bound)` = `Arrivals requiring cash (lower bound)` * 50,
#     `Cash amount (upper bound)` = `Arrivals requiring cash (upper bound)` * 50
#   ) |>
#   mutate(
#     `Cumulative cash amount` = cumsum(`Cash amount`),
#     `Cumulative cash amount (lower bound)` = cumsum(`Cash amount (lower bound)`),
#     `Cumulative cash amount (upper bound)` = cumsum(`Cash amount (upper bound)`)
#   )

# surge_cash_more <-
#   simulation_surge |>
#   select(
#     Date,
#     Week,
#     `Arrivals` = `Weekly arrivals`,
#     `Arrivals (lower bound)` = `Weekly arrivals (lower bound)`,
#     `Arrivals (upper bound)` = `Weekly arrivals (upper bound)`
#   ) |>
#   mutate(
#     `Arrivals requiring cash` = `Arrivals` * (require_cash_percentage + 0.1),
#     `Arrivals requiring cash (lower bound)` = `Arrivals (lower bound)` * (require_cash_percentage + 0.1),
#     `Arrivals requiring cash (upper bound)` = `Arrivals (upper bound)` * (require_cash_percentage + 0.1)
#   ) |>
#   mutate(
#     `Cash amount` = `Arrivals requiring cash` * 50,
#     `Cash amount (lower bound)` = `Arrivals requiring cash (lower bound)` * 50,
#     `Cash amount (upper bound)` = `Arrivals requiring cash (upper bound)` * 50
#   ) |>
#   mutate(
#     `Cumulative cash amount` = cumsum(`Cash amount`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount`) |> pull()),
#     `Cumulative cash amount (lower bound)` = cumsum(`Cash amount (lower bound)`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount (lower bound)`) |> pull()),
#     `Cumulative cash amount (upper bound)` = cumsum(`Cash amount (upper bound)`) + (baseline_cash |> filter(Week == 50) |> select(`Cumulative cash amount (upper bound)`) |> pull())
#   )

# ---- Plot ----
# - Expected scenario -
baseline_cash |>
  ggplot(aes(x = Date)) +
  geom_ribbon(
    mapping = aes(
      x = Date,
      ymin = `Cumulative cash amount (lower bound)`,
      ymax = `Cumulative cash amount (upper bound)`
    ),
    fill = "#9CAAAE",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = `Cumulative cash amount`),
    color = "#717171"
  ) +
  geom_line(
    aes(y = `Cumulative cash amount (lower bound)`),
    color = "#717171",
    lty = 2
  ) +
  geom_line(
    aes(y = `Cumulative cash amount (upper bound)`),
    color = "#717171",
    lty = 2
  ) +
  geom_ribbon(
    data = surge_cash,
    mapping = aes(
      x = Date,
      ymin = `Cumulative cash amount (lower bound)`,
      ymax = `Cumulative cash amount (upper bound)`
    ),
    fill = "cornflowerblue",
    alpha = 0.4
  ) +
  geom_line(
    data = surge_cash,
    aes(y = `Cumulative cash amount`),
    color = "royalblue4"
  ) +
  geom_line(
    data = surge_cash,
    aes(y = `Cumulative cash amount (lower bound)`),
    color = "royalblue4",
    lty = 2
  ) +
  geom_line(
    data = surge_cash,
    aes(y = `Cumulative cash amount (upper bound)`),
    color = "royalblue4",
    lty = 2
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = NULL,
    y = "Additional cash required (£)",
    title = "Predicted cash required by new Ukraine arrivals into the UK",
    subtitle = "Grey area shows predicted cash in the baseline scenario. Blue area shows predicted cash during a 'winter surge' scenario. \nSolid lines show predictions. Dotted lines show the lower and upper bounds of predictions."
  ) +
  theme_ipsum()
