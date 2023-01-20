# ---- Load libs ----
library(tidyverse)
library(readxl)
library(hrbrthemes)
library(lubridate)

# PowerBI source:
# https://app.powerbi.com/groups/me/apps/cfa2f05f-4b8f-4a5a-8699-5ae17e52b399/reports/6cacaec6-508d-4ac2-80c8-2da3379b647b/ReportSectionfcf47adc9f63a161aee0?ctid=fedc3cba-ca5e-4388-a837-b45c7f0d71b7

# ---- Load BRC data ----
cr <- read_excel(
  "data/brc/cr.xlsx",
  skip = 2
) |>
  select(
    week = Week,
    people = `No. People Assisted`
  ) |>
  group_by(week) |>
  summarise(people = sum(people)) |>
  mutate(cumsum_people = cumsum(people)) |>
  mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
  relocate(date)

rsrflat <-
  read_excel(
    "data/brc/rsrflat.xlsx",
    skip = 2
  ) |>
  select(
    week = Week,
    clients = `Main Clients`,
    dependents = Dependents
  ) |>
  group_by(week) |>
  summarise(
    clients = sum(clients),
    dependents = sum(dependents)
  ) |>
  slice(-c(1:2)) |>
  mutate(
    cumsum_clients = cumsum(clients),
    cumsum_dependents = cumsum(dependents)
  ) |>
  mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
  relocate(date)

nsl <-
  read_excel(
    "data/brc/nsl.xlsx",
    skip = 2
  ) |>
  select(
    week = Week,
    calls_abandoned = `Calls Abandoned`,
    calls_answered = `Calls Answered`
  ) |>
  group_by(week) |>
  summarise(
    calls_abandoned = sum(calls_abandoned),
    calls_answered = sum(calls_answered)
  ) |>
  mutate(
    cumsum_calls_abandoned = cumsum(calls_abandoned),
    cumsum_calls_answered = cumsum(calls_answered)
  ) |>
  mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
  relocate(date)

# ---- Load visa data ----
historic <-
  read_csv("data/cumulative-visas/cumulative-visas-2023-01-09.csv") |>
  rename(week = Week) |>
  group_by(week) |>
  summarise(arrivals = sum(`Number of arrivals`, na.rm = TRUE))

simulated_baseline <-
  read_csv("output-data/simulations/simulation-baseline-2023-01-16.csv") |>
  select(
    week = Week,
    arrivals = `Weekly arrivals`,
    arrivals_lower = `Weekly arrivals (lower bound)`,
    arrivals_upper = `Weekly arrivals (upper bound)`
  )

simulated_surge <-
  read_csv("output-data/simulations/simulation-surge-2023-01-16.csv") |>
  select(
    week = Week,
    arrivals = `Weekly arrivals`,
    arrivals_lower = `Weekly arrivals (lower bound)`,
    arrivals_upper = `Weekly arrivals (upper bound)`
  )

# ---- Calculate cumulative conversion rates ----
# Total arrivals to date
total_arrivals <-
  historic |>
  filter(arrivals == max(arrivals)) |>
  pull(arrivals)

# BRC values to date
  cr_max <-
  cr |>
  filter(cumsum_people == max(cumsum_people)) |>
  pull(cumsum_people) |>
  unique()

rsrflat_clients_max <-
  rsrflat |>
  filter(cumsum_clients == max(cumsum_clients)) |>
  pull(cumsum_clients)

rsrflat_dependents_max <-
  rsrflat |>
  filter(cumsum_dependents == max(cumsum_dependents)) |>
  pull(cumsum_dependents)

nsl_abandoned_max <-
  nsl |>
  filter(cumsum_calls_abandoned == max(cumsum_calls_abandoned)) |>
  pull(cumsum_calls_abandoned)

nsl_answered_max <-
  nsl |>
  filter(cumsum_calls_answered == max(cumsum_calls_answered)) |>
  pull(cumsum_calls_answered)

# Conversation rates
cr_rate <-
  cr_max / total_arrivals

rsrflat_clients_rate <-
  rsrflat_clients_max / total_arrivals

rsrflat_dependents_rate <-
  rsrflat_dependents_max / total_arrivals

nsl_abandoned_rate <-
  nsl_abandoned_max / total_arrivals

nsl_answered_rate <-
  nsl_answered_max / total_arrivals

brc_rates <-
  tibble(
    cr_rate,
    rsrflat_clients_rate,
    rsrflat_dependents_rate,
    nsl_abandoned_rate,
    nsl_answered_rate
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "service",
    values_to = "rate"
  ) |>
  mutate(service = str_remove_all(service, "_rate$"))

# ---- Simulate data ----
# - CR -
cr_baseline <-
  simulated_baseline |>
  mutate(
    cr = arrivals * brc_rates$rate[1],
    cr_lower = arrivals_lower * brc_rates$rate[1],
    cr_upper = arrivals_upper * brc_rates$rate[1]
  ) |>
  mutate(
    cumsum_cr = cumsum(cr) + cr_max,
    cumsum_cr_lower = cumsum(cr_lower) + cr_max,
    cumsum_cr_upper = cumsum(cr_upper) + cr_max
  ) |>
  mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
  relocate(date)

# cr_surge <-
#   simulated_surge |>
#   mutate(
#     cr = arrivals * brc_rates$rate[1],
#     cr_lower = arrivals_lower * brc_rates$rate[1],
#     cr_upper = arrivals_upper * brc_rates$rate[1]
#   ) |>
#   mutate(
#     cumsum_cr = cumsum(cr) + (cr_baseline |> filter(week == 51) |> pull(cumsum_cr)),
#     cumsum_cr_lower = cumsum(cr_lower) + (cr_baseline |> filter(week == 51) |> pull(cumsum_cr_lower)),
#     cumsum_cr_upper = cumsum(cr_upper) + (cr_baseline |> filter(week == 51) |> pull(cumsum_cr_upper))
#   ) |>
#   mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
#   relocate(date)

cr_surge <-
  simulated_surge |> 
  mutate(
    cr = arrivals * brc_rates$rate[1],
    cr_lower = arrivals_lower * brc_rates$rate[1],
    cr_upper = arrivals_upper * brc_rates$rate[1]
  ) |>
  mutate(
    cumsum_cr = cumsum(cr) + cr_max,
    cumsum_cr_lower = cumsum(cr_lower) + cr_max,
    cumsum_cr_upper = cumsum(cr_upper) + cr_max
  ) |>
  mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
  relocate(date)

cr_baseline |>
  ggplot(aes(x = date)) +
  geom_line(
    data = cr,
    aes(y = cumsum_people)
  ) +
  geom_ribbon(
    data = cr_baseline,
    mapping = aes(
      x = date,
      ymin = cumsum_cr_lower,
      ymax = cumsum_cr_upper
    ),
    fill = "#9CAAAE",
    alpha = 0.4
  ) +
  geom_line(
    data = cr_baseline,
    aes(y = cumsum_cr),
    color = "#717171"
  ) +
  geom_line(
    aes(y = cumsum_cr_lower),
    color = "#717171",
    lty = 2
  ) +
  geom_line(
    aes(y = cumsum_cr_upper),
    color = "#717171",
    lty = 2
  ) +
  geom_ribbon(
    data = cr_surge,
    mapping = aes(
      x = date,
      ymin = cumsum_cr_lower,
      ymax = cumsum_cr_upper
    ),
    fill = "cornflowerblue",
    alpha = 0.4
  ) +
  geom_line(
    data = cr_surge,
    aes(y = cumsum_cr),
    color = "royalblue4"
  ) +
  geom_line(
    data = cr_surge,
    aes(y = cumsum_cr_lower),
    color = "royalblue4",
    lty = 2
  ) +
  geom_line(
    data = cr_surge,
    aes(y = cumsum_cr_upper),
    color = "royalblue4",
    lty = 2
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  labs(
    x = "Date",
    y = "Number of people helped",
    title = "Historic and predicted number of people supported by CR in the Ukraine reponse.",
    subtitle = "Grey area shows predicted people in the baseline scenario. Blue area shows predicted people during a 'winter surge' scenario. \nDotted lines show the lower and upper bounds of predictions."
  ) +
  theme_ipsum()

# - RSRFLAT -
rsrflat_baseline <-
  simulated_baseline |>
  mutate(
    clients = arrivals * brc_rates$rate[2],
    clients_lower = arrivals_lower * brc_rates$rate[2],
    clients_upper = arrivals_upper * brc_rates$rate[2],
    dependents = arrivals * brc_rates$rate[3],
    dependents_lower = arrivals_lower * brc_rates$rate[3],
    dependents_upper = arrivals_upper * brc_rates$rate[3]
  ) |>
  mutate(
    cumsum_clients = cumsum(clients) + rsrflat_clients_max,
    cumsum_clients_lower = cumsum(clients_lower) + rsrflat_clients_max,
    cumsum_clients_upper = cumsum(clients_upper) + rsrflat_clients_max,
    cumsum_dependents = cumsum(dependents) + rsrflat_dependents_max,
    cumsum_dependents_lower = cumsum(dependents_lower) + rsrflat_dependents_max,
    cumsum_dependents_upper = cumsum(dependents_upper) + rsrflat_dependents_max
  ) |>
  mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
  relocate(date)

# rsrflat_surge <-
#   simulated_surge |>
#   mutate(
#     clients = arrivals * brc_rates$rate[2],
#     clients_lower = arrivals_lower * brc_rates$rate[2],
#     clients_upper = arrivals_upper * brc_rates$rate[2],
#     dependents = arrivals * brc_rates$rate[3],
#     dependents_lower = arrivals_lower * brc_rates$rate[3],
#     dependents_upper = arrivals_upper * brc_rates$rate[3]
#   ) |>
#   mutate(
#     cumsum_clients = cumsum(clients) + (rsrflat_baseline |> filter(week == 51) |> pull(cumsum_clients)),
#     cumsum_clients_lower = cumsum(clients_lower) + (rsrflat_baseline |> filter(week == 51) |> pull(cumsum_clients_lower)),
#     cumsum_clients_upper = cumsum(clients_upper) + (rsrflat_baseline |> filter(week == 51) |> pull(cumsum_clients_upper)),
#     cumsum_dependents = cumsum(dependents) + (rsrflat_baseline |> filter(week == 51) |> pull(cumsum_dependents)),
#     cumsum_dependents_lower = cumsum(dependents_lower) + (rsrflat_baseline |> filter(week == 51) |> pull(cumsum_dependents_lower)),
#     cumsum_dependents_upper = cumsum(dependents_upper) + (rsrflat_baseline |> filter(week == 51) |> pull(cumsum_dependents_upper))
#   ) |>
#   mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
#   relocate(date)

rsrflat_surge <-
  simulated_surge |>
  mutate(
    clients = arrivals * brc_rates$rate[2],
    clients_lower = arrivals_lower * brc_rates$rate[2],
    clients_upper = arrivals_upper * brc_rates$rate[2],
    dependents = arrivals * brc_rates$rate[3],
    dependents_lower = arrivals_lower * brc_rates$rate[3],
    dependents_upper = arrivals_upper * brc_rates$rate[3]
  ) |>
  mutate(
    cumsum_clients = cumsum(clients) + rsrflat_clients_max,
    cumsum_clients_lower = cumsum(clients_lower) + rsrflat_clients_max,
    cumsum_clients_upper = cumsum(clients_upper) + rsrflat_clients_max,
    cumsum_dependents = cumsum(dependents) + rsrflat_dependents_max,
    cumsum_dependents_lower = cumsum(dependents_lower) + rsrflat_dependents_max,
    cumsum_dependents_upper = cumsum(dependents_upper) + rsrflat_dependents_max
  ) |>
  mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
  relocate(date)

# Main clients
rsrflat_baseline |>
  ggplot(aes(x = date)) +
  geom_line(
    data = rsrflat,
    aes(y = cumsum_clients)
  ) +
  geom_ribbon(
    data = rsrflat_baseline,
    mapping = aes(
      x = date,
      ymin = cumsum_clients_lower,
      ymax = cumsum_clients_upper
    ),
    fill = "#9CAAAE",
    alpha = 0.4
  ) +
  geom_line(
    data = rsrflat_baseline,
    aes(y = cumsum_clients),
    color = "#717171"
  ) +
  geom_line(
    aes(y = cumsum_clients_lower),
    color = "#717171",
    lty = 2
  ) +
  geom_line(
    aes(y = cumsum_clients_upper),
    color = "#717171",
    lty = 2
  ) +
  geom_ribbon(
    data = rsrflat_surge,
    mapping = aes(
      x = date,
      ymin = cumsum_clients_lower,
      ymax = cumsum_clients_upper
    ),
    fill = "cornflowerblue",
    alpha = 0.4
  ) +
  geom_line(
    data = rsrflat_surge,
    aes(y = cumsum_clients),
    color = "royalblue4"
  ) +
  geom_line(
    data = rsrflat_surge,
    aes(y = cumsum_clients_lower),
    color = "royalblue4",
    lty = 2
  ) +
  geom_line(
    data = rsrflat_surge,
    aes(y = cumsum_clients_upper),
    color = "royalblue4",
    lty = 2
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  labs(
    x = "Date",
    y = "Number of main clients",
    title = "Historic and predicted number of main clients supported by Refugee Support in the Ukraine reponse",
    subtitle = "Grey area shows predicted clients in the baseline scenario. Blue area shows predicted clients during a 'winter surge' scenario. \nDotted lines show the lower and upper bounds of predictions."
  ) +
  theme_ipsum()

# Dependents
rsrflat_baseline |>
  ggplot(aes(x = date)) +
  geom_line(
    data = rsrflat,
    aes(y = cumsum_dependents)
  ) +
  geom_ribbon(
    data = rsrflat_baseline,
    mapping = aes(
      x = date,
      ymin = cumsum_dependents_lower,
      ymax = cumsum_dependents_upper
    ),
    fill = "#9CAAAE",
    alpha = 0.4
  ) +
  geom_line(
    data = rsrflat_baseline,
    aes(y = cumsum_dependents),
    color = "#717171"
  ) +
  geom_line(
    aes(y = cumsum_dependents_lower),
    color = "#717171",
    lty = 2
  ) +
  geom_line(
    aes(y = cumsum_dependents_upper),
    color = "#717171",
    lty = 2
  ) +
  geom_ribbon(
    data = rsrflat_surge,
    mapping = aes(
      x = date,
      ymin = cumsum_dependents_lower,
      ymax = cumsum_dependents_upper
    ),
    fill = "cornflowerblue",
    alpha = 0.4
  ) +
  geom_line(
    data = rsrflat_surge,
    aes(y = cumsum_dependents),
    color = "royalblue4"
  ) +
  geom_line(
    data = rsrflat_surge,
    aes(y = cumsum_dependents_lower),
    color = "royalblue4",
    lty = 2
  ) +
  geom_line(
    data = rsrflat_surge,
    aes(y = cumsum_dependents_upper),
    color = "royalblue4",
    lty = 2
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  labs(
    x = "Date",
    y = "Number of dependents",
    title = "Historic and predicted number of dependents supported by Refugee Support in the Ukraine reponse",
    subtitle = "Grey area shows predicted dependents in the baseline scenario. Blue area shows predicted dependents during a 'winter surge' scenario. \nDotted lines show the lower and upper bounds of predictions."
  ) +
  theme_ipsum()

# # - NSL -
# nsl_baseline <-
#   simulated_baseline |>
#   mutate(
#     abandoned = arrivals * brc_rates$rate[4],
#     abandoned_lower = arrivals_lower * brc_rates$rate[4],
#     abandoned_upper = arrivals_upper * brc_rates$rate[4],
#     answered = arrivals * brc_rates$rate[5],
#     answered_lower = arrivals_lower * brc_rates$rate[5],
#     answered_upper = arrivals_upper * brc_rates$rate[5]
#   ) |>
#   mutate(
#     cumsum_abandoned = cumsum(abandoned) + nsl_abandoned_max,
#     cumsum_abandoned_lower = cumsum(abandoned_lower) + nsl_abandoned_max,
#     cumsum_abandoned_upper = cumsum(abandoned_upper) + nsl_abandoned_max,
#     cumsum_answered = cumsum(answered) + nsl_answered_max,
#     cumsum_answered_lower = cumsum(answered_lower) + nsl_answered_max,
#     cumsum_answered_upper = cumsum(answered_upper) + nsl_answered_max
#   ) |>
#   mutate(date = ymd("2022-01-01") + weeks(week - 1)) |>
#   relocate(date)

# nsl_surge <-
#   simulated_surge |>
#   mutate(
#     abandoned = arrivals * brc_rates$rate[4],
#     abandoned_lower = arrivals_lower * brc_rates$rate[4],
#     abandoned_upper = arrivals_upper * brc_rates$rate[4],
#     answered = arrivals * brc_rates$rate[5],
#     answered_lower = arrivals_lower * brc_rates$rate[5],
#     answered_upper = arrivals_upper * brc_rates$rate[5]
#   )|>
#   mutate(
#     cumsum_abandoned = cumsum(abandoned) + (nsl_baseline |> filter(week == 51) |> pull(cumsum_abandoned)),
#     cumsum_abandoned_lower = cumsum(abandoned_lower) + (nsl_baseline |> filter(week == 51) |> pull(cumsum_abandoned_lower)),
#     cumsum_abandoned_upper = cumsum(abandoned_upper) + (nsl_baseline |> filter(week == 51) |> pull(cumsum_abandoned_upper)),
#     cumsum_answered = cumsum(answered) + (nsl_baseline |> filter(week == 51) |> pull(cumsum_answered)),
#     cumsum_answered_lower = cumsum(answered_lower) + (nsl_baseline |> filter(week == 51) |> pull(cumsum_answered_lower)),
#     cumsum_answered_upper = cumsum(answered_upper) + (nsl_baseline |> filter(week == 51) |> pull(cumsum_answered_upper))
#   ) |>
#   mutate(date = ymd("2022-01-01") + weeks(week-1))  |>
#   relocate(date)

# nsl_baseline |>
#   ggplot(aes(x = date)) +
#   geom_line(
#     data = nsl,
#     aes(y = cumsum_calls_abandoned),
#     colour = "#D0021B"
#   ) +
#   geom_ribbon(
#     data = nsl_baseline,
#     mapping = aes(
#       x = date,
#       ymin = cumsum_abandoned_lower,
#       ymax = cumsum_abandoned_upper
#     ),
#     fill = "#9CAAAE",
#     alpha = 0.4
#   ) +
#   geom_line(
#     data = nsl_baseline,
#     aes(y = cumsum_abandoned),
#     color = "#717171"
#   ) +
#   geom_line(
#     aes(y =  cumsum_abandoned_lower),
#     color = "#717171",
#     lty = 2
#   ) +
#   geom_line(
#     aes(y =  cumsum_abandoned_upper),
#     color = "#717171",
#     lty = 2
#   ) +
#   geom_ribbon(
#     data = nsl_surge,
#     mapping = aes(
#       x = date,
#       ymin = cumsum_abandoned_lower,
#       ymax = cumsum_abandoned_upper
#     ),
#     fill = "cornflowerblue",
#     alpha = 0.4
#   ) +
#   geom_line(
#     data = nsl_surge,
#     aes(y = cumsum_abandoned),
#     color = "royalblue4"
#   ) +
#   geom_line(
#     data = nsl_surge,
#     aes(y = cumsum_abandoned_lower),
#     color = "royalblue4",
#     lty = 2
#   ) +
#   geom_line(
#     data = nsl_surge,
#     aes(y = cumsum_abandoned_upper),
#     color = "royalblue4",
#     lty = 2
#   ) +
#   geom_line(
#     data = nsl,
#     aes(y = cumsum_calls_answered),
#     colour = "#40A22A"
#   ) +
#   geom_ribbon(
#     data = nsl_baseline,
#     mapping = aes(
#       x = date,
#       ymin = cumsum_answered_lower,
#       ymax = cumsum_answered_upper
#     ),
#     fill = "#9CAAAE",
#     alpha = 0.4
#   ) +
#   geom_line(
#     data = nsl_baseline,
#     aes(y = cumsum_answered),
#     color = "#717171"
#   ) +
#   geom_line(
#     aes(y =  cumsum_answered_lower),
#     color = "#717171",
#     lty = 2
#   ) +
#   geom_line(
#     aes(y =  cumsum_answered_upper),
#     color = "#717171",
#     lty = 2
#   ) +
#   geom_ribbon(
#     data = nsl_surge,
#     mapping = aes(
#       x = date,
#       ymin = cumsum_answered_lower,
#       ymax = cumsum_answered_upper
#     ),
#     fill = "cornflowerblue",
#     alpha = 0.4
#   ) +
#   geom_line(
#     data = nsl_surge,
#     aes(y = cumsum_answered),
#     color = "royalblue4"
#   ) +
#   geom_line(
#     data = nsl_surge,
#     aes(y = cumsum_answered_lower),
#     color = "royalblue4",
#     lty = 2
#   ) +
#   geom_line(
#     data = nsl_surge,
#     aes(y = cumsum_answered_upper),
#     color = "royalblue4",
#     lty = 2
#   ) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_date(date_breaks = "3 month", date_labels =  "%b %y") +
#   labs(
#     x = "Date",
#     y = "Number of calls",
#     title = "Historic and predicted number of Ukraine National Support Line calls",
#     subtitle = "Grey areas show predicted arrivals in the baseline scenario. Blue areas show predicted arrivals during a 'winter surge' scenario. \nDotted lines show the lower and upper bounds of predictions."
#   ) +
#   theme_ipsum()
