library(tidyverse)
library(ggfittext)
library(ggtext)
library(IMD)

source("https://github.com/matthewgthomas/brclib/raw/master/R/colours.R")

# ---- Load data ----
source("R/load Ukraine visa data - scraped.R")
source("R/load Ukraine visa data - Local Authorities.R")

homelessness_feb_june <- read_csv("data/homelessness/ukraine-homelessness-3-june.csv")
homelessness_feb_july <- read_csv("data/homelessness/ukraine-homelessness-1-july.csv")
homelessness_feb_aug <- read_csv("data/homelessness/ukraine-homelessness-29-july.csv")

homelessness_total <- read_csv("data/homelessness/ukraine-homelessness-summary.csv")

homelessness_trends <- read_csv("data/homelessness/ukraine-homelessness-trends.csv")

england_housing <- read_csv("data/housing/housing-england.csv")
england_index <- read_csv("output-data/index-of-housing-insecurity/housing-index-england.csv")

# ---- Wrangling ----
homelessness_total <- 
  homelessness_total |> 
  mutate(
    Date_text = factor(Date_text, levels = c("3 June", "1 July", "29 July")),
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Total Ukrainian households owed a prevention or relief duty`
  )

# ---- How many currently at risk of homelessness? ----
homelessness_total |> 
  filter(Date == max(Date)) |> 
  select(`Total Ukrainian households owed a prevention or relief duty`)

# % increase since first homelessness stats released?
max(homelessness_total$`Total Ukrainian households owed a prevention or relief duty`) / min(homelessness_total$`Total Ukrainian households owed a prevention or relief duty`)

# ---- How many months since first arrivals? ----
first_arrivals <- 
  visas_scraped |> 
  ungroup() |> 
  filter(str_detect(Stage, "arrivals")) |> 
  filter(Date == min(Date)) |> 
  distinct(Date) |> 
  pull(Date)

latest_homelessness_data <- ymd("2022-09-02")

# How many months in between?
latest_homelessness_data - first_arrivals
interval(first_arrivals, latest_homelessness_data) / dmonths(1)

# ---- What % of households arriving on each scheme face homelessness? ----
homelessness_total |> 
  select(Date_text, `% Homes for Ukraine`, `% Family Scheme`) |> 
  pivot_longer(cols = starts_with("%"), names_to = "Scheme", values_to = "Percent") |> 
  
  ggplot(aes(x = Date_text, y = Percent, group = Scheme, colour = Scheme)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.5) +
  
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_color_manual(values = c(get_brc_colours()$teal, get_brc_colours()$red)) +
  
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)
  ) +
  labs(
    title = str_glue(
      "People arriving on the <span style='color:{get_brc_colours()$red}'>Homes of Ukraine Scheme</span> increasingly face homelessness<br/>
      <span style='font-size:10pt'>% of Ukraine refugee housholds on the <span style='color:{get_brc_colours()$teal}'>Family Scheme</span> and the <span style='color:{get_brc_colours()$red}'>Homes for Ukraine Scheme</span></span>"
    ),
    # subtitle = str_glue("% of Ukraine refugee housholds on the <span style='color:{get_brc_colours()$teal}'>Family Scheme</span> and the <span style='color:{get_brc_colours()$red}'>Homes for Ukraine Scheme</span>"),
    x = NULL,
    y = NULL,
    caption = "British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness - percent by scheme.png", width = 100, height = 100, units = "mm")

# ---- Numbers and %s in temporary accommodation ----
homelessness_total |> 
  select(Date, `Temporary Accommodation Snapshot`, `% in temporary accommodation`)

# ---- %s of total homelessness in each region ----
total_homelessness <- sum(homelessness_feb_aug$`Total Ukrainian households owed a prevention or relief duty`, na.rm = TRUE)

homelessness_feb_aug |> 
  arrange(desc(`Total Ukrainian households owed a prevention or relief duty`)) |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_name, region21_name, homelessness = `Total Ukrainian households owed a prevention or relief duty`) |> 
  
  group_by(region21_name) |> 
  summarise(
    regional_homelessness = sum(homelessness, na.rm = TRUE),
    proportion = regional_homelessness / total_homelessness
  ) |> 
  arrange(desc(proportion))

# ---- %s of households in temporary accommodation in each region ----
total_temp_acc <- sum(homelessness_feb_aug$`Temporary Accommodation Snapshot`, na.rm = TRUE)

homelessness_feb_aug |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_name, region21_name, temp_acc = `Temporary Accommodation Snapshot`) |> 
  
  group_by(region21_name) |> 
  summarise(
    regional_temp_acc = sum(temp_acc, na.rm = TRUE),
    proportion = regional_temp_acc / total_temp_acc
  ) |> 
  arrange(desc(proportion))

# % in temporary accommodation in London, South East, and East
.426 + .158 + .0988

# ---- Where has experienced by biggest increases in homelessness? ----
homelessness_trends |> 
  select(region21_name, ltla21_name, total_delta) |> 
  arrange(desc(total_delta))

# ---- Which places had no homelessness in June but do now? ----
new_homelessness <- 
  homelessness_trends |> 
  filter((is.na(total_june) | total_june == 0) & (is.na(total_july) | total_july == 0) & (total_aug > 0)) |> 
  arrange(desc(total_aug)) |> 
  select(region21_name, lad_name, total_june, total_july, total_aug)

new_homelessness

new_homelessness |> 
  count(region21_name, sort = TRUE)

# ---- Local Authorities with the highest rates of homelessness risk ----
homelessness_feb_aug |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  select(lad_name, region21_name, `% at risk of homelessness`, `% in temporary accommodation`) |> 
  arrange(desc(`% at risk of homelessness`), desc(`% in temporary accommodation`)) |> 
  slice(1:10) |> 
  
  ggplot(aes(x = reorder(lad_name, `% at risk of homelessness`, sum), y = `% at risk of homelessness`)) +
  geom_col(aes(fill = region21_name)) +
  geom_bar_text(aes(label = lad_name)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(
    limits = c(0, 0.33),
    breaks = c(0, 0.1, 0.2, 0.25, 0.33),
    labels = c("0", "1 in 10", "1 in 5", "1 in 4", "1 in 3"),
    position = "right"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    title = str_wrap("Risk of homelessness for Ukraine refugees is highest in <span style='color:#fc8d62'>London</span>, the <span style='color:#8da0cb'>South East</span>, and <span style='color:#66c2a5'>East</span> of England", 60),
    subtitle = str_wrap("Percentages are based on individual arrivals, not households, so are likely to underestimate the true rate of homelessness risks", 60),
    x = NULL,
    fill = NULL,
    caption = "British Red Cross analysis of DLUHC data"
  )

ggsave("images/risk of homelessness.png", width = 150, height = 120, units = "mm")

# ---- How is housing insecurity linked to Ukrainian homelessness? ----
ihs_imd_homelessness <- 
  england_index |> 
  
  left_join(homelessness_trends |> select(ltla21_code = lad_code, total_aug, total_july, total_june)) |> 
  
  left_join(IMD::imd_england_lad, by = c("ltla21_code" = "lad_code")) |> 
  left_join(IMD::imd_england_lad_subdomains, by = c("ltla21_code" = "lad_code")) |> 
  
  # Calculate ranks
  mutate(
    IMD_score_rank = rank(Score),
    IMD_proportion_rank = rank(Proportion), 
    Housing_and_Access_Score_rank = rank(Housing_and_Access_Score), 
    Housing_and_Access_Proportion_rank = rank(Housing_and_Access_Proportion),
    Wider_barriers_Proportion_rank = rank(Wider_barriers_Proportion),
    Wider_barriers_Extent_rank = rank(Wider_barriers_Extent)
  ) |> 
  
  select(
    ltla21_code, 
    ltla21_name,
    
    total_aug,
    
    homeless_threatened = `Households assessed as threatened with homelessness per (000s)`, 
    homeless = `Households assessed as homeless per (000s)`, 
    temp_accom = `Households in temporary accommodation per 1,000`, 
    waiting = `Households on housing waiting list per 1,000`, 
    housing_stock = `Social housing stock as a proportion of all households`, 
    vacancies = `Vacant dwellings per 1,000 units of social housing stock`,
    
    `IMD` = IMD_score_rank,
    `IMD Housing and Access domain` = Housing_and_Access_Score_rank,
    `IMD Wider Barriers subdomain` = Wider_barriers_Extent_rank
  )

# Explore relationship between all indicators and numbers of homeless Ukrainian households
ihs_imd_homelessness |> 
  # pivot_longer(cols = c(`IMD`, `IMD Housing and Access domain`, `IMD Wider Barriers subdomain`), values_to = "rank") |> 
  pivot_longer(cols = homeless_threatened:`IMD Wider Barriers subdomain`, values_to = "rank") |> 
  
  ggplot(aes(x = rank, y = total_aug)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name, ncol = 3, scales = "free_x")

# Plot 'wider barriers' subdomain
ihs_imd_homelessness |> 
  ggplot(aes(x = `IMD Wider Barriers subdomain`, y = total_aug)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", colour = "red", fill = "red") +
  scale_x_continuous(
    breaks = c(min(ihs_imd_homelessness$`IMD Wider Barriers subdomain`), max(ihs_imd_homelessness$`IMD Wider Barriers subdomain`)), 
    labels = c("Fewer barriers", "Greater barriers")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1), add = 0)) +
  theme_classic() +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = str_wrap("Homelessness for Ukraine refugees tends to be higher in places with higher barriers to housing", 60),
    subtitle = str_wrap("Barriers to housing include affordability, overcrowding, and Local Authority housing assistance", 60),
    x = "Barriers to housing",
    y = "Number of Ukraine refugee households\n at risk of homelessness",
    caption = "British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness - barriers.png", width = 100, height = 100, units = "mm")
