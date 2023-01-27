# Contents:
# - Map showing absolute numbers of Ukraine homelessness by Local Authority
# - Map showing Ukraine homelessness as a proportion of all households, by Local Authority
# - List the top 10 LAs with highest homelessness (absolute and proportional)
# - Are higher counts/proportions in places that have seen more arrivals from Ukraine?
# - Are higher counts/proportions in places with generally higher homelessness rates?
# - Which constituencies have the highest numbers/rates of Ukraine homelessness?

library(tidyverse)
library(demographr)
library(geographr)
library(viridis)
library(sf)

# ---- Load data ----
homelessness_24feb_30dec <- read_csv("data/homelessness/ukraine-homelessness-30-december.csv")
homelessness_total <- read_csv("data/homelessness/ukraine-homelessness-summary.csv")
homelessness_trends <- read_csv("data/homelessness/ukraine-homelessness-trends.csv")

homelessness_latest <- 
  homelessness_24feb_30dec |> 
  select(lad_code, homeless = `Total Ukrainian households owed a prevention or relief duty`) |> 
  left_join(demographr::households21_ltla21, by = c("lad_code" = "ltla21_code")) |> 
  mutate(homeless_per_100000 = homeless / households * 100000)

homelessness_latest_shp <- 
  geographr::boundaries_ltla21 |> 
  filter(str_detect(ltla21_code, "^E")) |> 
  left_join(homelessness_latest, by = c("ltla21_code" = "lad_code"))

# ---- Map showing absolute numbers of Ukraine homelessness by Local Authority ----
homelessness_latest_shp |> 
  ggplot() +
  geom_sf(
    aes(fill = homeless),
    colour = "#5c747a"
  ) +
  scale_fill_gradient(low = "#f6f6f6", high = "#ee2a24") +
  # scale_fill_viridis(
  #   na.value = "transparent",
  #   option = "magma",
  #   alpha = 0.7,
  #   begin = 0.1,
  #   end = 0.9,
  #   discrete = FALSE,
  #   direction = -1
  # ) +
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
    # title = "Local Authorities potentially requiring deeper support with homelessness",
    # subtitle = str_wrap("Showing the subset of Local Authorities where people arriving on the Ukraine visa schemes are living and rates of homelessness (including people in temporary accommodation) were already high, housing stock is low, and waiting lists for housing are long. Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people).\n", 100),
    fill = "Number of Ukrainian households at risk of homelessness",
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness map for PRA spotlight - absolute count.png", width = 180, height = 160, units = "mm")

# ---- Map showing Ukraine homelessness as a proportion of all households, by Local Authority ----
homelessness_latest_shp |> 
  # Drop Scilly - clearly there's some error in the data
  filter(ltla21_code != "E06000053") |> 
  
  ggplot() +
  geom_sf(
    aes(fill = homeless_per_100000),
    colour = "#5c747a"
  ) +
  scale_fill_gradient(low = "#f6f6f6", high = "#ee2a24") +
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
    # title = "Local Authorities potentially requiring deeper support with homelessness",
    # subtitle = str_wrap("Showing the subset of Local Authorities where people arriving on the Ukraine visa schemes are living and rates of homelessness (including people in temporary accommodation) were already high, housing stock is low, and waiting lists for housing are long. Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people).\n", 100),
    fill = "Proportion of Ukrainian households at risk of homelessness \n(per 100,000 households)",
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness map for PRA spotlight - proportion.png", width = 180, height = 160, units = "mm")

# ---- List the top 10 LAs with highest homelessness (absolute and proportional) ----
homelessness_latest |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  arrange(desc(homeless)) |> 
  select(Region = region21_name, `Local Authority` = ltla21_name.y, `No. Ukraine households at risk of homelessness` = homeless, `No. Ukraine households at risk of homelessness (per 100,000)` = homeless_per_100000) |> 
  write_csv("output-data/homelessness/ukraine-homelessness-latest.csv")

# ---- Are higher counts/proportions in places that have seen more arrivals from Ukraine? ----
source("R/load Ukraine visa data - Local Authorities.R")

visas_ltla21_england_latest <- 
  visas_ltla21_england |> 
  filter(Date == max(Date))

homelessness_latest |> 
  left_join(visas_ltla21_england_latest, by = c("lad_code" = "ltla21_code")) |> 
  
  ggplot(aes(x = `Number of arrivals`, y = homeless)) +
  geom_point() +
  geom_smooth()

#--> No statistical relationship between number of arrivals and number at risk of homelessness
#--> (Though note this is comparing households with individuals)

# ---- Are higher counts/proportions in places with generally higher homelessness rates? ----
england_homelessness <- read_csv("data/housing/housing-england.csv")

england_homelessness |> 
  select(ltla21_code, `Households assessed as threatened with homelessness per (000s)`, `Households assessed as homeless per (000s)`) |> 
  left_join(homelessness_latest, by = c("ltla21_code" = "lad_code")) |> 
  
  # Drop Scilly - clearly there's some error in the data
  filter(ltla21_code != "E06000053") |> 
  
  ggplot(aes(x = `Households assessed as homeless per (000s)`, y = homeless_per_100000)) +
  geom_point() +
  geom_smooth()

#--> No statistical relationship

# ---- Which constituencies have the highest numbers/rates of Ukraine homelessness? ----
# Ward to Westminster Parliamentary Constituency to Local Authority District to Upper Tier Local Authority (December 2022) Lookup in the United Kingdom
ward22_constituency22_ltla22_utla22 <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD22_PCON22_LAD22_UTLA22_UK_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

lookup_constituency22_ltla22 <- 
  ward22_constituency22_ltla22_utla22 |> 
  st_drop_geometry() |> 
  distinct(constituency22_code = PCON22CD, constituency22_name = PCON22NM, ltla22_code = LAD22CD)

homelessness_top_10 <- 
  homelessness_latest |> 
  arrange(desc(homeless)) |>
  slice(1:10) |> 
  pull(lad_code)

lookup_constituency22_ltla22 |> 
  left_join(homelessness_latest, by = c("ltla22_code" = "lad_code")) |> 
  filter(ltla22_code %in% homelessness_top_10) |> 
  arrange(desc(homeless)) |> 
  
  select(`Local Authority` = ltla21_name, Constituency = constituency22_name, `No. Ukraine households at risk of homelessness` = homeless, `No. Ukraine households at risk of homelessness (per 100,000)` = homeless_per_100000) |> 
  write_csv("output-data/homelessness/ukraine-homelessness-by-constituency.csv")
