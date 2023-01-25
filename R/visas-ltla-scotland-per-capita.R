library(tidyverse)
library(readODS)
library(httr)

# ---- Load DLUHC data ----
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1130112/Ukraine_sponsorship_scheme_-_visa_data_17_January_2023.ods"

visas <- compositr::download_file(url, ".ods") |> 
  read_ods(sheet = "Scotland", skip = 10) |> 
  slice(2:33) |> 
  as_tibble() |> 
  select(
     ltl21_name = `Upper tier local authority name`,
     ltla21_code = `ONS code`, 
     arrivals = `Number of arrivals in the UK by sponsor location`
  ) |> 
  mutate(arrivals = as.double(arrivals))

# --- Load pop ----
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
GET(
  "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-20/mid-year-pop-est-20-data.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

pop_sco <-
  read_csv(
    file.path(tempdir(), "mid-year-pop-est-20-data_Table 2.csv"),
    skip = 3
  ) |>
  slice(-1:-2) |>
  select(ltla21_code = `Area code`, population = `All Ages`) |>
  slice(1:32) |>
  mutate(population = str_remove_all(population, ",")) |>
  mutate(population = as.double(population))

# ---- Calculate per capita figures ----
arrivals_per_100k <- visas |> 
  left_join(pop_sco) |> 
  mutate(arrivals_per_100k = arrivals/population * 100000) |> 
  arrange(desc(arrivals_per_100k))
