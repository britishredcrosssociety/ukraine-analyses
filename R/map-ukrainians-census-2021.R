library(tidyverse)
library(geographr)
library(janitor)
library(readxl)
library(httr)
library(sf)

# ---- Load Census data ----
source("R/load-census-2021-ukraine.R")

# ---- Load boundaries ----
tc <- read_sf("C:/Users/040026704/Documents/Data science/Data/Boundaries/BRC_Areas_2017/IL CR/IL_CR_boundaries_20161006_v2.shp")

tc <- 
  tc %>% 
  filter(!str_detect(name, "Scotland|Ireland"))

boundaries_lad <- read_sf("https://opendata.arcgis.com/datasets/2fee7f7198744627b567da49d69b90d0_0.geojson")

# boundaries_tmp <- 
#   boundaries_lad %>% 
#   filter(LAD21CD %in% c("E06000060", "E06000061", "E06000062"))
# 
# ggplot() +
#   geom_sf(
#     data = tc,
#     fill = NA,
#     colour = "black"
#   ) +
#   
#   geom_sf(
#     data = boundaries_tmp,
#     fill = "red"
#   )

# ---- Maps ----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      # Add labs elements
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5,
        margin = margin(
          b = 0.2,
          t = 0.2,
          l = 2,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = margin(
          t = 0.2,
          b = 0,
          unit = "cm"
        ),
        color = "#939184"
      ),
      ...
    )
}

boundaries_lad %>% 
  filter(!str_detect(LAD21CD, "^S")) %>% 
  left_join(census, by = c("LAD21CD" = "Code")) %>% 
  
  ggplot() +
  
  geom_sf(
    mapping = aes(fill = Ukraine),
    colour = NA
  ) +
  
  geom_sf(
    data = tc,
    fill = NA,
    colour = "white"
  ) +
  
  theme_map() +
  labs(
    title = "People born in Ukraine living in English and Welsh Local Authorities",
    subtitle = "I&I analysis of preliminary Census 2021 data; white outlines show BRC areas",
    fill = "No. people born in Ukraine"
  )

ggsave("images/map Ukrainians in UK - Census 2021.png", width = 220, height = 200, units = "mm")

# - London blow-up map -
boundaries_lad %>% 
  filter(!str_detect(LAD21CD, "^S")) %>% 
  left_join(census, by = c("LAD21CD" = "Code")) %>% 
  
  filter(TacticalCell == "London") %>% 
  
  ggplot() +
  
  geom_sf(
    mapping = aes(fill = Ukraine),
    colour = NA
  ) +
  
  theme_map() +
  labs(
    title = "People born in Ukraine living in London",
    subtitle = "I&I analysis of preliminary Census 2021 data",
    fill = "No. people born in Ukraine"
  )

ggsave("images/map Ukrainians in UK - London - Census 2021.png", width = 220, height = 200, units = "mm")

# ---- Summary table, by Cell ----
census %>% 
  group_by(TacticalCell) %>% 
  summarise(Ukraine = sum(Ukraine, na.rm = TRUE)) %>% 
  na.omit() %>% 
  arrange(desc(Ukraine)) %>% 

  write_csv("output-data/Ukrainians in UK - Census 2021.csv")

# --- Correlation with deprivation and asylum/resettlement ----
library(IMD)
library(asylum)

imd_lad <- 
  bind_rows(
    imd_england_lad %>% select(lad_code, Proportion, Extent),
    imd_wales_lad %>% select(lad_code, Proportion, Extent)
  )

census_imd <- 
  census %>% 
  left_join(imd_lad, by = c("Code" = "lad_code"))

summary(lm(Proportion ~ Ukraine, data = census_imd))

census_imd %>% 
  select(Ukraine, Proportion) %>% 
  na.omit() %>% 
  
  ggplot(aes(x = Ukraine, y = Proportion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(
    x = "No. people from Ukraine",
    y = "Proportion of highly deprived neighbours in each LA",
    title = "The Ukrainian diaspora is not more likely to live in deprived areas",
    subtitle = "Points show Local Authorities in England and Wales",
    caption = "I&I analysis of Census 2021 and IMD data"
  )

ggsave("images/Ukrainian diaspora and deprivation.png", width = 160, height = 120, units = "mm")
