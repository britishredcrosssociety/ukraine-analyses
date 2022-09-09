library(tidyverse)
library(geographr)
library(sf)
library(readODS)
library(httr2)
library(compositr)


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

uk <- bind_rows(england, scotland, wales, northern_ireland)

# ---- Plot w/ facet ----
boundaries_ltla21 |>
  left_join(uk) |> 
  
