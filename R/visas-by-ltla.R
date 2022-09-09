# ---- Load libs ----
library(tidyverse)
library(geographr)
library(demographr)
library(readODS)
library(httr2)
library(compositr)
library(viridis)
library(readxl)
library(sf)
library(hrbrthemes)

# --- Load pop ----
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
pop_sco <-
  read_csv(
    "data/mid-year-pop-est-20-data/mid-year-pop-est-20-data_Table 2.csv",
    skip = 3
  ) |>
  slice(-1:-2) |>
  select(ltla21_code = `Area code`, pop = `All Ages`) |>
  slice(1:32) |>
  mutate(pop = str_remove_all(pop, ",")) |>
  mutate(pop = as.double(pop))

# https://www.nisra.gov.uk/publications/2020-mid-year-population-estimates-northern-ireland
pop_ni <-
  read_excel(
    "data/MYE20-POP-TOTAL.xlsx",
    "Flat"
  ) |>
  filter(year == 2020) |>
  filter(type == "Rounded") |>
  filter(str_detect(area_code, "^N09")) |>
  select(ltla21_code = area_code, pop = MYE)

pop_eng_wal <- population21_ltla21 |>
  select(ltla21_code, pop = population)

pop_total <-
  bind_rows(
    pop_eng_wal, pop_sco, pop_ni
  )

# ---- Load data ----
raw <-
  download_file(
    "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1103094/Ukraine_sponsorship_scheme_-_visa_data_06_September_2022.ods",
    ".ods"
  )

england <-
  raw |>
  read_ods(
    sheet = "LTLA_-_England",
    range = "B9:E319"
  ) |>
  as_tibble() |>
  slice(-1) |>
  rename(
    ltla21_code = `LTLA - ONS code`,
    applications = `Number of visa applications`,
    issues = `Number of visas issued`,
    arrivals = `Number of arrivals in the UK by sponsor location`
  ) |>
  mutate(across(!ltla21_code, ~ str_remove(.x, "<"))) |>
  mutate(across(!ltla21_code, as.integer))

scotland <-
  raw |>
  read_ods(
    sheet = "Scotland",
    range = "B11:E44"
  ) |>
  as_tibble() |>
  slice(-1) |>
  rename(
    ltla21_code = `ONS code`,
    applications = `Number of visa applications`,
    issues = `Number of visas issued`,
    arrivals = `Number of arrivals in the UK by sponsor location`
  ) |>
  mutate(across(!ltla21_code, ~ str_remove(.x, "<"))) |>
  mutate(across(!ltla21_code, as.integer))

wales <-
  raw |>
  read_ods(
    sheet = "Wales",
    range = "B11:E34"
  ) |>
  as_tibble() |>
  slice(-1) |>
  rename(
    ltla21_code = `ONS code`,
    applications = `Number of visa applications`,
    issues = `Number of visas issued`,
    arrivals = `Number of arrivals in the UK by sponsor location`
  ) |>
  mutate(across(!ltla21_code, ~ str_remove(.x, "<"))) |>
  mutate(across(!ltla21_code, as.integer))

northern_ireland <-
  raw |>
  read_ods(
    sheet = "Northern_Ireland",
    range = "B9:E21"
  ) |>
  as_tibble() |>
  slice(-1) |>
  rename(
    ltla21_code = `ONS code`,
    applications = `Numbers of visa applications`,
    issues = `Number of visas issued`,
    arrivals = `Number of arrivals in the UK by sponsor location`
  ) |>
  mutate(across(!ltla21_code, ~ str_remove(.x, "<"))) |>
  mutate(across(!ltla21_code, as.integer))

uk <- bind_rows(england, scotland, wales, northern_ireland) |>
  pivot_longer(cols = !ltla21_code, names_to = "type", values_to = "count") |>
  left_join(pop_total) |>
  mutate(per_1000_people = count / pop * 1000) |>
  select(-pop) |> 
  mutate(type = str_to_title(type)) |> 
  filter(type != "Issues")

# ---- Plot w/ facet ----
boundaries_ltla21 |>
  left_join(uk) |>
  ggplot() +
  geom_sf(
    mapping = aes(fill = count),
    colour = "black",
    size = .1,
  ) +
  facet_wrap(vars(type)) +
  scale_fill_viridis(
    name = "Total number",
    na.value = "transparent",
    option = "magma",
    alpha = 0.8,
    begin = 0.95,
    end = 0.05,
    discrete = FALSE,
    direction = 1
  ) +
  theme_ipsum()

boundaries_ltla21 |>
  left_join(uk) |>
  filter(ltla21_code != "E06000053") |> 
  ggplot() +
  geom_sf(
    mapping = aes(fill = per_1000_people),
    colour = "black",
    size = .1,
  ) +
  facet_wrap(vars(type)) +
  scale_fill_viridis(
    name = "Per 1000 people \n in local authority",
    na.value = "transparent",
    option = "E",
    alpha = 0.8,
    begin = 0.95,
    end = 0.05,
    discrete = FALSE,
    direction = 1
  ) +
  theme_ipsum()