library(tidyverse)
library(readODS)
library(readxl)
library(httr)
library(IMD)
library(ggfittext)
library(patchwork)
library(ggrepel)
library(geographr)
library(sf)
library(viridis)
library(ggsflabel)

conflicted::conflict_prefer("lag", "dplyr")

# ---- Load data ----
source("R/load Ukraine visa data - Local Authorities.R")

homelessness_feb_june <- read_csv("data/homelessness/ukraine-homelessness-3-june.csv")
homelessness_feb_july <- read_csv("data/homelessness/ukraine-homelessness-1-july.csv")
homelessness_feb_aug <- read_csv("data/homelessness/ukraine-homelessness-29-july.csv")

homelessness_total <- read_csv("data/homelessness/ukraine-homelessness-summary.csv")

homelessness_trends <- read_csv("data/homelessness/ukraine-homelessness-trends.csv")

england_housing <- read_csv("data/housing/housing-england.csv")
england_index <- read_csv("output-data/index-of-housing-insecurity/housing-index-england.csv")

# ---- Homelessness and deprivation ----
homelessness_feb_aug |> 
  left_join(imd_england_lad |> select(lad_code, Proportion, Extent)) |> 
  
  ggplot(aes(x = Extent, y = `Temporary Accommodation Snapshot`)) +
  # ggplot(aes(x = Extent, y = `Total Ukrainian households owed a prevention or relief duty`)) +
  geom_point() +
  geom_smooth(method = "lm")

# Are places with the highest increases in homelessness also more deprived?
homelessness_trends |> 
  left_join(imd_england_lad |> select(lad_code, Score, Proportion, Extent)) |> 
  
  ggplot(aes(x = Score, y = total_delta)) +
  geom_point() +
  geom_smooth(method = "lm")
#--> No.

# ---- Headline stats ----
homelessness_feb_aug_total$`Total Ukrainian households owed a prevention or relief duty`
homelessness_feb_july_total$`Total Ukrainian households owed a prevention or relief duty`
homelessness_feb_june_total$`Total Ukrainian households owed a prevention or relief duty`

homelessness_feb_aug_total$`Temporary Accommodation Snapshot`
homelessness_feb_july_total$`Temporary Accommodation Snapshot`
homelessness_feb_june_total$`Temporary Accommodation Snapshot`

# Plot total numbers of households owed a duty
homelessness_total |> 
  mutate(
    point_axis = 1,
    text_axis = 0.99
  ) |> 
  ggplot(aes(x = Date_text, y = point_axis)) +
  geom_point(aes(size = `Total Ukrainian households owed a prevention or relief duty`), show.legend = FALSE, alpha = 0.4, colour = "red") +
  geom_text(aes(y = text_axis, label = scales::comma(`Total Ukrainian households owed a prevention or relief duty`)), show.legend = FALSE, size = rel(8)) +
  scale_size_area(max_size = 20) +
  scale_y_continuous(limits = c(0.98, 1.01)) +  #, expand = expansion(mult = 2, add = 0.0))
  theme_void() +
  theme(
    axis.text.x = element_text()
  )

ggsave("images/homelessness - totals.png", width = 70, height = 30, units = "mm")

# ---- Show top tens ----
homelessness_feb_aug |> 
  arrange(desc(`Total Ukrainian households owed a prevention or relief duty`)) |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_name, region21_name, `Total Ukrainian households owed a prevention or relief duty`) |> 
  slice(1:10)

homelessness_feb_aug |> 
  arrange(desc(`Temporary Accommodation Snapshot`)) |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_name, region21_name, `Temporary Accommodation Snapshot`) |> 
  slice(1:10)

# ---- Rates/proportions of homelessness ----
# Table, where more than 10% of arrivals are at risk of homelessness or in temp accommodation
homelessness_feb_july |> 
  filter(`% at risk of homelessness` >= 0.1 | `% in temporary accommodation` >= 0.1) |> 
  select(lad_name, `% at risk of homelessness`, `% in temporary accommodation`) |> 
  arrange(desc(`% at risk of homelessness`))

# Visualise homelessness rates
homelessness_feb_aug |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_name, region21_name, `% at risk of homelessness`, `% in temporary accommodation`) |> 
  arrange(desc(`% at risk of homelessness`), desc(`% in temporary accommodation`)) |> 
  slice(1:10) |> 
  
  ggplot(aes(x = reorder(lad_name, `% at risk of homelessness`, sum), y = `% at risk of homelessness`)) +
  geom_col(aes(fill = region21_name)) +
  geom_bar_text(aes(label = lad_name)) +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    breaks = c(0, 0.1, 0.2, 0.25),
    labels = c("0", "1 in 10", "1 in 5", "1 in 4"),
    position = "right"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    title = str_wrap("Risk of homelessness for people from Ukraine is highest in London, the South East, and East of England", 60),
    x = NULL,
    fill = NULL,
    caption = "BRC/I&I analysis of DLUHC data"
  )

ggsave("images/risk of homelessness.png", width = 150, height = 120, units = "mm")

# - Are Local Authorities with higher rates of homelessness among arrivals also more deprived?
homelessness_feb_aug |> 
  left_join(imd_england_lad |> select(lad_code, Score, Proportion, Extent)) |> 
  mutate(
    label = if_else(
      Extent > 0.5 | `% at risk of homelessness` > 0.2, 
      lad_name, 
      NA_character_
    )
  ) |> 
  
  ggplot(aes(x = Score, y = `% at risk of homelessness`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggrepel::geom_label_repel(aes(label = label))

homelessness_feb_aug |> 
  left_join(imd_england_lad |> select(lad_code, Proportion, Extent)) |> 
  mutate(
    label = if_else(
      Extent > 0.5 | `% in temporary accommodation` > 0.08, 
      lad_name, 
      NA_character_
    )
  ) |> 
  
  ggplot(aes(x = Extent, y = `% in temporary accommodation`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggrepel::geom_label_repel(aes(label = label))

# ---- Which places had the largest increases in homelessness? ----
# How many places and which had new homelessness between June and July?
new_homelessness <- 
  homelessness_trends |> 
  filter((is.na(total_june) | total_june == 0) & (total_aug > 0)) |> 
  arrange(desc(total_july))

new_homelessness

new_homelessness |> 
  count(region21_name, sort = TRUE)

# How many places and which had new temporary accommodation between June and July?
new_temp_accomm <- 
  homelessness_trends |> 
  filter((is.na(temp_june) | temp_june == 0) & (temp_aug > 0)) |> 
  arrange(desc(temp_july))

new_temp_accomm

new_temp_accomm |> 
  count(region21_name, sort = TRUE)

# Plot LAs with the largest increases from zero in June
new_homelessness |> 
  arrange(desc(total_delta)) |> 
  slice(1:10) |> 
  select(
    lad_name, 
    region21_name, 
    `Number newly at risk of homelessness since June 2022` = total_delta, 
    `Number newly in temporary accommodation since June 2022` = temp_delta
  ) |> 
  pivot_longer(cols = starts_with("Number")) |> 
  
  ggplot(aes(x = reorder(lad_name, value, sum), y = value, colour = region21_name)) +
  geom_segment(aes(xend = lad_name), yend = 0, size = 1) +
  geom_point(size = 3) + 
  coord_flip() +
  facet_wrap(~name) +
  
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_colour_brewer(palette = "Paired") +
  
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    panel.grid.major.x = element_line(linetype = 2)
  ) +
  labs(
    title = "New homelessness among Ukrainian families is particularly prevalent in London and South East England",
    subtitle = str_wrap("Showing Local Authorities with no Ukrainian families at risk of homelessness or in temporary accommodation as of 3 June 2022 but at least one homeless household as of 29 July 2022", 120),
    x = NULL,
    y = "Number of Ukrainian households",
    colour = NULL,
    caption = "BRC/I&I analysis of DLUHC data"
  )

ggsave("images/risk of new homelessness.png", width = 220, height = 150, units = "mm")

# Plot LAs with the largest increases from June to July
homelessness_trends |> 
  arrange(desc(total_delta)) |> 
  slice(1:10) |> 
  select(
    lad_name, 
    region21_name, 
    `February to 3 June 2022` = temp_june, 
    `As of 1 July 2022` = temp_july,
    `As of 29 July 2022` = temp_aug,
    Change = total_delta
  ) |> 
  pivot_longer(cols = contains("2022")) |> 
  
  # Link pairs of answers for plotting lines between the dots; source: https://datavizpyr.com/connect-paired-points-with-lines-in-scatterplot-in-ggplot2/
  mutate(paired = rep(1:(n()/3), each = 3)) |> 
  
  ggplot(aes(x = reorder(lad_name, Change, sum), y = value)) +
  geom_line(aes(group = paired), lty = 2, color = "grey", arrow = arrow(type = "closed", length = unit(.35, "cm"))) +
  geom_point(aes(colour = name), size = 3) + 
  
  coord_flip() +
  
  # scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_colour_brewer(palette = "Paired") +
  
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    panel.grid.major.x = element_line(linetype = 2)
  ) +
  labs(
    title = str_wrap("Recent rises in risk of homelessness among Ukrainian families were most prevalent in London and South East England", 70),
    x = NULL,
    y = "Number of Ukrainian households at risk of homelessness",
    colour = NULL,
    caption = "BRC/I&I analysis of DLUHC data"
  )

ggsave("images/risk of homelessness - trends.png", width = 160, height = 100, units = "mm")

# Plot LAs with the largest increases in use of temporary accommodation from June to July
homelessness_trends |> 
  arrange(desc(temp_delta)) |> 
  slice(1:10) |> 
  select(
    lad_name, 
    region21_name, 
    `February to 3 June 2022` = temp_june, 
    `As of 1 July 2022` = temp_july,
    `As of 29 July 2022` = temp_aug,
    Change = temp_delta
  ) |> 
  pivot_longer(cols = contains("2022")) |> 
  
  # Link pairs of answers for plotting lines between the dots; source: https://datavizpyr.com/connect-paired-points-with-lines-in-scatterplot-in-ggplot2/
  mutate(paired = rep(1:(n()/3), each = 3)) |> 
  
  ggplot(aes(x = reorder(lad_name, Change, sum), y = value)) +
  geom_line(aes(group = paired), lty = 2, color = "grey", arrow = arrow(type = "closed", length = unit(.35, "cm"))) +
  geom_point(aes(colour = name), size = 3) + 
  
  coord_flip() +
  
  scale_colour_brewer(palette = "Paired") +
  
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    panel.grid.major.x = element_line(linetype = 2)
  ) +
  labs(
    title = str_wrap("Rising numbers of Ukrainian families living in temporary accommodation", 70),
    subtitle = str_wrap("Showing the ten Local Authorities with the largest recent inceases in numbers of Ukrainian households in temporary accommodation", 70),
    x = NULL,
    y = "Number of Ukrainian households in temporary accommodation",
    colour = NULL,
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/temporary accommodation - trends.png", width = 160, height = 100, units = "mm")

# ---- Homelessness trends by LA ----
# - Number of households at risk of homelessness -
risk_trends <- 
  homelessness_trends |> 
  filter(total_june > 0 & total_july > 0 & total_aug > 0) |> 
  select(lad_name, region21_name, `3 June` = total_june, `1 July` = total_july, `29 July` = total_aug) |> 
  pivot_longer(cols = -contains("_name"), names_to = "date", values_to = "count") |> 
  mutate(date = factor(date, levels = c("3 June", "1 July", "29 July")))

# What regions are the LAs with the 10 highest risk of homelessness in?
risk_trends |> 
  slice_max(count, n = 10) |> 
  count(region21_name)

# Label the top three LAs in each region
risk_trends_labels <- 
  risk_trends |> 
  filter(date == "29 July") |> 
  group_by(region21_name) |>
  slice_max(count, n = 3) |> 
  ungroup() |> 
  distinct(lad_name) |> 
  mutate(label = lad_name)

risk_trends |> 
  left_join(risk_trends_labels) |> 
  mutate(
    colour = if_else(!is.na(label), "red", "grey"),
    label = if_else(date == "29 July", label, NA_character_)
  ) |> 
  
  ggplot(aes(x = date, y = count, group = lad_name)) +
  geom_line(aes(colour = colour), size = 1.1, show.legend = FALSE) +
  geom_text_repel(aes(label = label)) +
  facet_wrap(~region21_name) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = c("grey", "red")) +
  theme_classic() +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = str_wrap("Most of the Ukrainian households at risk of homelessness live in London", 90),
    subtitle = str_wrap("Showing trends in households at risk of homelessness across England. Red lines highlight LAs with the largest number of Ukrainian households at risk of homelessness in each region.", 100),
    x = NULL,
    y = "Number of Ukrainian households at risk of homelessness",
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness trends - at risk - count.png", width = 200, height = 180, units = "mm")

# - Percent of households at risk of homelessness -
risk_trends <- 
  homelessness_trends |> 
  filter(percent_june > 0 & percent_july > 0 & percent_aug > 0) |> 
  select(lad_name, region21_name, `3 June` = percent_june, `1 July` = percent_july, `29 July` = percent_aug) |> 
  pivot_longer(cols = -contains("_name"), names_to = "date", values_to = "percent") |> 
  mutate(date = factor(date, levels = c("3 June", "1 July", "29 July")))

# What regions are the LAs with the 10 highest rates in?
risk_trends |> 
  slice_max(percent, n = 10) |> 
  count(region21_name)

# Label the top three LAs in each region
risk_trends_labels <- 
  risk_trends |> 
  filter(date == "29 July") |> 
  group_by(region21_name) |>
  slice_max(percent, n = 3) |> 
  ungroup() |> 
  distinct(lad_name) |> 
  mutate(label = lad_name)
  
risk_trends |> 
  left_join(risk_trends_labels) |> 
  mutate(
    colour = if_else(!is.na(label), "red", "grey"),
    label = if_else(date == "29 July", label, NA_character_)
  ) |> 
  
  ggplot(aes(x = date, y = percent, group = lad_name)) +
  geom_line(aes(colour = colour), size = 1.1, show.legend = FALSE) +
  geom_text_repel(aes(label = label)) +
  facet_wrap(~region21_name) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("grey", "red")) +
  theme_classic() +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = str_wrap("The highest proportions of Ukrainian households at risk of homelessness are in London and the East of England", 90),
    subtitle = str_wrap("Showing trends in households at risk of homelessness as a proportion of all Ukrainian households within Local Authorities. Red lines highlight LAs with the highest proportions of Ukrainian households at risk of homelessness.", 100),
    x = NULL,
    y = "Percentage of Ukrainian households at risk of homelessness",
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness trends - at risk - proportion.png", width = 200, height = 180, units = "mm")

# - Number of households in temporary accommodation
temp_accom_trends <- 
  homelessness_trends |> 
  filter(temp_june > 0 & temp_july > 0 & temp_aug > 0) |> 
  select(lad_name, region21_name, `3 June` = temp_june, `1 July` = temp_july, `29 July` = temp_aug) |> 
  pivot_longer(cols = -contains("_name"), names_to = "date", values_to = "count") |> 
  mutate(date = factor(date, levels = c("3 June", "1 July", "29 July")))

# What regions are the LAs with the 10 highest rates in?
temp_accom_trends |> 
  slice_max(count, n = 10) |> 
  count(region21_name)

# Label the top three LAs in each region
temp_accom_trends_labels <- 
  temp_accom_trends |> 
  filter(date == "29 July") |> 
  group_by(region21_name) |>
  slice_max(count, n = 3) |> 
  ungroup() |> 
  distinct(lad_name) |> 
  mutate(label = lad_name)

temp_accom_trends |> 
  left_join(temp_accom_trends_labels) |> 
  mutate(
    colour = if_else(!is.na(label), "red", "grey"),
    label = if_else(date == "29 July", label, NA_character_)
  ) |> 
  
  ggplot(aes(x = date, y = count, group = lad_name)) +
  geom_line(aes(colour = colour), size = 1.1, show.legend = FALSE) +
  geom_text_repel(aes(label = label)) +
  facet_wrap(~region21_name) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = c("grey", "red")) +
  theme_classic() +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = str_wrap("Most Ukrainian households in temporary accommodation are in London and the North West", 90),
    subtitle = str_wrap("Showing trends in Ukrainian households in temporary accommodation within Local Authorities. Red lines highlight LAs with the largest numbers of Ukrainian households in temporary accommodation.", 100),
    x = NULL,
    y = "Number of Ukrainian households in temporary accommodation",
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness trends - temporary accommodation - count.png", width = 200, height = 180, units = "mm")

# - Percent of households in temporary accommodation
temp_accom_trends <- 
  homelessness_trends |> 
  filter(temp_percent_june > 0 & temp_percent_july > 0 & temp_percent_aug > 0) |> 
  select(lad_name, region21_name, `3 June` = temp_percent_june, `1 July` = temp_percent_july, `29 July` = temp_percent_aug) |> 
  pivot_longer(cols = -contains("_name"), names_to = "date", values_to = "percent") |> 
  mutate(date = factor(date, levels = c("3 June", "1 July", "29 July")))

# What regions are the LAs with the 10 highest rates in?
temp_accom_trends |> 
  slice_max(percent, n = 10) |> 
  count(region21_name)

# Label the top three LAs in each region
temp_accom_trends_labels <- 
  temp_accom_trends |> 
  filter(date == "29 July") |> 
  group_by(region21_name) |>
  slice_max(percent, n = 3) |> 
  ungroup() |> 
  distinct(lad_name) |> 
  mutate(label = lad_name)

temp_accom_trends |> 
  left_join(temp_accom_trends_labels) |> 
  mutate(
    colour = if_else(!is.na(label), "red", "grey"),
    label = if_else(date == "29 July", label, NA_character_)
  ) |> 
  
  ggplot(aes(x = date, y = percent, group = lad_name)) +
  geom_line(aes(colour = colour), size = 1.1, show.legend = FALSE) +
  geom_text_repel(aes(label = label)) +
  facet_wrap(~region21_name) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("grey", "red")) +
  theme_classic() +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = str_wrap("The highest rates of Ukrainian households in temporary accommodation are in London, the North West, and the East of England", 90),
    subtitle = str_wrap("Showing trends in households in temporary accommodation as a proportion of all Ukrainian households within Local Authorities. Red lines highlight LAs with the highest rates of Ukrainian households in temporary accommodation", 100),
    x = NULL,
    y = "Percentage of Ukrainian households in temporary accommodation",
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness trends - temporary accommodation - proportion.png", width = 200, height = 180, units = "mm")

# ---- Compare to broader statutory homelessness in Local Authorities ----
# Do LAs with high rates of statutory homelessness also have high rates of homelessness risks among people from Ukraine?
all_LA_homelessness <- 
  homelessness_trends |> 
  select(
    lad_code, 
    lad_name, 
    `Number of arrivals` = `Number of arrivals (August)`,
    `Number of Ukrainian households threatened with homelessness` = total_aug,
    `Percentage of Ukrainian households threatened with homelessness` = percent_aug,
    `Percentage of Ukrainian households in temporary accommodation` = temp_percent_aug
  ) |> 

  left_join(england_housing, by = c("lad_code" = "ltla21_code")) |> 

  # Keep only LAs with people from Ukraine already living there
  filter(`Number of arrivals` > 0)

plt_homelessness_threat <-
  all_LA_homelessness |> 
  mutate(label = if_else(`Households assessed as threatened with homelessness per (000s)` > 4, lad_name, NA_character_)) |> 
  ggplot(aes(x = `Households assessed as threatened with homelessness per (000s)`, y = `Percentage of Ukrainian households threatened with homelessness`)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_label_repel(aes(label = label))

plt_homelessness_actual <-
  all_LA_homelessness |> 
  mutate(label = if_else(`Households assessed as homeless per (000s)` > 4, lad_name, NA_character_)) |> 
  ggplot(aes(x = `Households assessed as homeless per (000s)`, y = `Percentage of Ukrainian households threatened with homelessness`)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_label_repel(aes(label = label))

plt_temp_accomm <- 
  all_LA_homelessness |> 
  mutate(label = if_else(`Households in temporary accommodation per 1,000` > 20, lad_name, NA_character_)) |> 
  ggplot(aes(x = `Households in temporary accommodation per 1,000`, y = `Percentage of Ukrainian households in temporary accommodation`)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_label_repel(aes(label = label))

plt_waiting_list <- 
  all_LA_homelessness |> 
  # mutate(label = if_else(`Households in temporary accommodation per 1,000` > 20, lad_name, NA_character_)) |> 
  ggplot(aes(x = `Households on housing waiting list per 1,000`, y = `Percentage of Ukrainian households in temporary accommodation`)) +
  geom_point() + 
  geom_smooth(method = "lm")
  # geom_label_repel(aes(label = label))

plt_social_housing <- 
  all_LA_homelessness |> 
  # mutate(label = if_else(`Households in temporary accommodation per 1,000` > 20, lad_name, NA_character_)) |> 
  ggplot(aes(x = `Social housing stock as a proportion of all households`, y = `Percentage of Ukrainian households in temporary accommodation`)) +
  geom_point() + 
  geom_smooth(method = "lm") 
  # geom_label_repel(aes(label = label))

(plt_homelessness_threat | plt_homelessness_actual | plt_temp_accomm) /
  (plt_waiting_list | plt_social_housing)

# Which LAs have highest rates of homelessness / risk of homelessness / households in TA, but none (yet) are Ukrainian?
all_LA_homelessness |> 
  filter(`Percentage of Ukrainian households threatened with homelessness` == 0) |> 
  # arrange(desc(`Households assessed as threatened with homelessness per (000s)`))
  arrange(desc(`Households assessed as homeless per (000s)`))

all_LA_homelessness |> 
  filter(`Percentage of Ukrainian households in temporary accommodation` == 0) |> 
  arrange(desc(`Total number of households in TA per (000s)`))

# Do LAs with long housing waiting lists also have higher risks of homelessness?
all_LA_homelessness |> 
  mutate(label = if_else(housing_waiting_list_size > 15000, lad_name, NA_character_)) |> 
  ggplot(aes(x = housing_waiting_list_size, y = `Number of Ukrainian households threatened with homelessness`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label_repel(aes(label = label))

# ---- Spikes in arrivals by nation ----
weekly_visas_by_nation <- 
  visas_ltla21_summary |> 
  arrange(Date) |> 
  filter(str_detect(Type, "^Sponsored")) |> 
  mutate(Type = if_else(str_detect(Type, "Government"), "Government 'super sponsored'", Type)) |> 
  mutate(Type = fct_rev(Type)) |> 
  
  # Calculate week-on-week changes
  group_by(Location, Type) |> 
  mutate(
    `Weekly applications` = `Number of visa applications` - lag(`Number of visa applications`),
    `Weekly visas issued` = `Number of visas issued` - lag(`Number of visas issued`),
    `Weekly arrivals` = `Number of arrivals in the UK by sponsor location` - lag(`Number of arrivals in the UK by sponsor location`)
  ) |> 
  ungroup()

# Highlight super sponsor scheme pauses
# Scotland's pause started on 13 July 2022 and will run for three months
# Wales's pause started on 10th June 2022, with no timeframe given for unpausing (it was originally going to be paused just for June, but this was extended in July)
super_sponsor_pause <- 
  tribble(
    ~Location, ~xmin, ~xmax, ~ymin, ~ymax,
    "Scotland", ymd("2022-07-13"), min(max(weekly_visas_by_nation$Date), ymd("2022-10-13")), 0, weekly_visas_by_nation |> filter(Location == "Scotland") |> filter(`Weekly arrivals` == max(`Weekly arrivals`, na.rm = TRUE)) |> pull(`Weekly arrivals`),
    "Wales", ymd("2022-06-10"), max(weekly_visas_by_nation$Date), 0, weekly_visas_by_nation |> filter(Location == "Wales") |> filter(`Weekly arrivals` == max(`Weekly arrivals`, na.rm = TRUE)) |> pull(`Weekly arrivals`)
  )

weekly_visas_by_nation |> 
  ggplot(aes(x = Date, y = `Weekly arrivals`)) +
  
  # Highlight super sponsor pauses
  geom_rect(data = super_sponsor_pause, inherit.aes = FALSE, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2) +
  
  geom_line(aes(colour = Location, lty = Type), size = 1.1, show.legend = FALSE) +
  
  facet_wrap(~Location, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2", guide = "none") +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    panel.grid.major.y = element_line(colour = "lightgrey", linetype = 2)
  ) +
  labs(
    title = "Spikes in arrivals could lead to spikes in destitution 6 to 12 months later",
    subtitle = "Showing weekly number of arrivals sponsored by individuals and by the Scottish and Welsh Governments\nGrey shaded areas show pauses in Government 'super sponsor' schemes.",
    x = NULL,
    y = "Number of people arriving each week",
    colour = NULL,
    lty = NULL,
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/weekly arrivals - absolute numbers.png", width = 225, height = 150, units = "mm")
ggsave("images/weekly arrivals - absolute numbers.svg", width = 225, height = 150, units = "mm")

plotly::ggplotly()  # Manually analyse where the spikes in arrivals are
