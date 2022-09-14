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
weekly_arrivals_by_scheme <-
  visas_scraped |>
  arrange(Date) |>
  filter(str_detect(Stage, "arrival")) |>

  # Calculate week-on-week changes
  group_by(Scheme) |>
  mutate(`Weekly arrivals` = Visas_imputed - lag(Visas_imputed)) |>
  ungroup()

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
visas_flow <-
  visas_scraped |>
  pivot_wider(id_cols = c(Week, Scheme), names_from = Stage, values_from = Visas) |>
  mutate(
    issued_rate = `visas issued` / `visa applications received`,
    arrivals_rate = `arrivals of visa-holders in the UK` / `visas issued`,
    withdraw_refuse_rate = (`applications withdrawn` + `applications refused`) / `visa applications received`
  )

# Mean withdrawal/refusal rates by scheme
visas_flow |>
  filter(!is.na(withdraw_refuse_rate)) |>
  group_by(Scheme) |>
  summarise(mean_rate = mean(withdraw_refuse_rate))

visas_flow |>
  select(Week, Scheme, `% of applications issued visas`= issued_rate, `% of issued visas arriving` = arrivals_rate) |>
  pivot_longer(cols = starts_with("%")) |>
  ggplot(aes(x = Week, y = value)) +
  geom_line(aes(colour = Scheme, lty = name), size = 1.1)

# Mean rates over the last four weeks
visas_flow |>
  filter(Week >= 14) |>
  group_by(Scheme) |>
  summarise(
    mean_issued_rate = mean(issued_rate),
    mean_arrivals_rate = mean(arrivals_rate)
  )

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

# ---- Plot surges in applications ----
weekly_visas_by_scheme |>
  filter(Scheme == "Sponsored by individuals") |>
  
  ggplot(aes(x = Week, y = `Weekly applications`)) +
  geom_line(aes(colour = Scheme)) +
  geom_smooth()

## Plot rates of applications issued as visas ----
# historical_processing_rates |> 
#   ggplot(aes(x = Scheme, y = `% backlog issued a visa this week`)) +
#   geom_boxplot(aes(colour = Scheme), show.legend = FALSE) +
#   geom_jitter(aes(colour = Scheme), alpha = 0.3, show.legend = FALSE) +
#   scale_y_continuous(labels = scales::percent) +
#   scale_color_brewer(palette = "Set2", direction = -1) +
#   theme_classic() +
#   theme(
#     # axis.line.x = element_blank(),
#     # axis.text.x = element_blank(),
#     # axis.ticks.x = element_blank(),
#     plot.title = element_text(size = rel(1)),
#     plot.title.position = "plot"
#   ) +
#   labs(
#     title = "Proportion of applications that are issued a visa each week\n",
#     x = NULL,
#     y = NULL
#   )
