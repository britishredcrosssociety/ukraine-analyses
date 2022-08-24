library(tidyverse)
library(geographr)
# library(ggsflabel)
library(viridis)
library(httr)
library(sf)

# ---- Load visas data ----
source("R/load Ukraine visa data - Local Authorities.R")

visas_aug <- 
  visas_ltla21_uk |> 
  filter(Date == ymd("2022-08-02")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

# ---- Load housing data ----
# Run the next line if you need to create the country-specific .csv files
# source("housing and homelessness - wrangle data.R")

england <- read_csv("data/housing/housing-england.csv")
wales <- read_csv("data/housing/housing-wales.csv")
scotland <- read_csv("data/housing/housing-scotland.csv")
ni <- read_csv("data/housing/housing-ni.csv")

england_index <- read_csv("output-data/index-of-housing-insecurity/housing-index-england.csv")
wales_index <- read_csv("output-data/index-of-housing-insecurity/housing-index-wales.csv")
scotland_index <- read_csv("output-data/index-of-housing-insecurity/housing-index-scotland.csv")
ni_index <- read_csv("output-data/index-of-housing-insecurity/housing-index-ni.csv")

lad_names <- 
  boundaries_ltla21 |> 
  st_drop_geometry()

housing <- 
  bind_rows(
    england |> select(ltla21_code, ends_with("_quintile")),
    wales |> select(ltla21_code, ends_with("_quintile")),
    scotland |> select(ltla21_code, ends_with("_quintile")),
    ni |> select(ltla21_code, ends_with("_quintile"))
  ) |> 
  left_join(lad_names) |> 
  left_join(visas_aug, by = "ltla21_code")

housing_index <- 
  bind_rows(
    england_index |> select(ltla21_code, ends_with("_quantiles")),
    wales_index |> select(ltla21_code, ends_with("_quantiles")),
    scotland_index |> select(ltla21_code, ends_with("_quantiles")),
    ni_index |> select(ltla21_code, ends_with("_quantiles"))
  ) |> 
  left_join(lad_names) |> 
  left_join(visas_aug, by = "ltla21_code")

# ---- Filter most pressured LAs ----
# Filter a subset of LAs where rates of homelessness and temporary accommodation are high, housing stock is low, and waiting lists are long
LAs_to_map <- 
  housing |> 
  filter(
    # England has all four indicators 
    (str_detect(ltla21_code, "E") & 
      temp_accom_quintile >= 4 & homeless_quintile >= 4 & waiting_quintile >= 4 & stock_quintile <= 2) |
      
      # Wales has three indicators
      (str_detect(ltla21_code, "W") & 
         temp_accom_quintile >= 4 & homeless_quintile >= 4 & stock_quintile <= 2) |
      
      # Scotland has all four indicators
      (str_detect(ltla21_code, "S") & 
         temp_accom_quintile >= 4 & homeless_quintile >= 4 & waiting_quintile >= 4 & stock_quintile <= 2) |
      
      # Northern Ireland has two indicators
      (str_detect(ltla21_code, "N") & 
         homeless_quintile >= 4 & waiting_quintile >= 4)
  )

LAs_to_map_from_index <- 
  housing_index |> 
  filter(housing_and_homelessness_composite_quantiles == 10)

# Compare the filter lists of LAs
dplyr::intersect(LAs_to_map$ltla21_code, LAs_to_map_from_index$ltla21_code)
dplyr::setdiff(LAs_to_map$ltla21_code, LAs_to_map_from_index$ltla21_code)  # Which LAs are in the quintiles approach by not in the composite index?

# ---- Draw map ----
geographr::boundaries_ltla21 |> 
  left_join(LAs_to_map_from_index, by = "ltla21_code") |> 
  
  ggplot() +
  geom_sf(
    aes(fill = `Number of arrivals`),
    colour = "grey80"
  ) +
  # geom_sf_label_repel(
  #   aes(label = ltla21_name),
  #   # nudge_y = .25,
  #   # direction = "x",
  #   force_pull = 0,
  #   force = 2,
  #   label.size = .4
  # ) +
  scale_fill_viridis(
    na.value = "transparent",
    option = "magma",
    alpha = 0.7,
    begin = 0.1,
    end = 0.9,
    discrete = FALSE,
    direction = -1
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(
        b = 0.3,
        t = 0.2,
        l = 2,
        unit = "cm"
      )
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Local Authorities potentially requiring deeper support with homelessness",
    subtitle = str_wrap("Showing the subset of Local Authorities where people arriving on the Ukraine visa schemes are living and rates of homelessness (including people in temporary accommodation) were already high, housing stock is low, and waiting lists for housing are long. Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people).\n", 100),
    fill = "Number of Ukrainian households on visa schemes",
    caption = "Source: British Red Cross analysis of DLUHC, Stats Wales, Scottish Government, and NI Department for Communities data"
  )

ggsave("images/homelessness map - whole UK.png", width = 180, height = 160, units = "mm")

# ---- Draw hex cartogram ----
# Load lower tier LA hex cartogram
# Source: https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous
GET(
  "https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg",
  write_disk(tf <- tempfile(fileext = ".gpkg"))
)

hex_background <- read_sf(tf, layer = "7 Background")

hex_ltla21 <- read_sf(tf, layer = "6 LTLA-2021")

hex_groups <- read_sf(tf, layer = "2 Groups")

hex_labels <- read_sf(tf, layer = "1 Group labels") |> 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

# Merge composite index and arrivals
hex_ltla21 <- 
  hex_ltla21 |> 
  left_join(LAs_to_map_from_index, by = c("Lacode" = "ltla21_code")) |> 
  mutate(arrivals_fill = if_else(housing_and_homelessness_composite_quantiles == 10, `Number of arrivals`, NA_real_))

ggplot() +
  geom_sf(
    data = hex_background, 
    aes(geometry = geom)
  )+
  geom_sf(
    data = hex_ltla21, 
    mapping = aes(
      geometry = geom, fill = `Number of arrivals`
    ), 
    colour = NA
  ) +
  geom_sf(
    data = hex_groups, 
    aes(geometry = geom), 
    fill = NA, 
    colour = "Black"
  ) +
  geom_sf_text(
    data = hex_labels, 
    mapping = aes(
      geometry = geom, 
      label = Group.labe,
      hjust = just
    ), 
    size = rel(2.4), 
    colour = "Black"
  ) +
  scale_fill_viridis(
    na.value = "white",
    option = "magma",
    alpha = 0.7,
    begin = 0.1,
    end = 0.9,
    discrete = FALSE,
    direction = -1
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(
        b = 0.3,
        t = 0.2,
        l = 2,
        unit = "cm"
      )
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Local Authorities potentially requiring deeper support with homelessness",
    # subtitle = str_wrap("Showing the subset of Local Authorities where people arriving on the Ukraine visa schemes are living and rates of homelessness (including people in temporary accommodation) were already high, housing stock is low, and waiting lists for housing are long. Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people).\n", 100),
    fill = "Number of Ukrainian people on visa schemes",
    caption = "Source: British Red Cross analysis of DLUHC, Stats Wales, Scottish Government, and NI Department for Communities data\nCartogram from the House of Commons Library"
  )

ggsave("images/homelessness map - cartogram.png", width = 250, height = 200, units = "mm")

# ---- Save list of highest-risk areas ----
LAs_to_map_from_index |> 
  select(ltla21_code, ltla21_name) |> 
  left_join(geographr::lookup_ltla21_region21 |> select(ltla21_code, region21_name)) |> 
  select(region21_name, ltla21_name) |> 
  arrange(region21_name, ltla21_name) |> 
  write_csv("output-data/local authorities with highest housing insecurity.csv")
