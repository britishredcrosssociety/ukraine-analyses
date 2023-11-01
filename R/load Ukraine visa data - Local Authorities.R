library(tidyverse)
library(lubridate)
library(readODS)
library(rvest)
library(httr)
library(xml2)

visa_path <- "data-raw/visas"

# Create folder if it doesn't exist
if (!dir.exists("data-raw")) {
  dir.create("data-raw")
  dir.create("data-raw/visas")
}

# ---- Scrape URLs for visa data and download files ----
visa_url <- "https://www.gov.uk/guidance/ukraine-sponsorship-scheme-visa-data-by-country-upper-and-lower-tier-local-authority"

# Set user agent
ua <- httr::user_agent("https://redcross.org.uk")

# Get webpage data
req <- httr::GET(visa_url, ua)

# Load html
out <- content(req, "text")
doc <- read_html(out)

# Get URLs of each visa data file by scraping the href of any <a> tags within anything that has the class "attachment-inline"
visa_data_urls <-
  doc |>
  # html_nodes(xpath = "//*[contains(@class, 'attachment-inline')]//a/@href") |>
  html_nodes(xpath = "//*[contains(@class, 'gem-c-attachment-link')]//a/@href") |>
  html_text()

# Download each dataset if it doesn't already exist
for (url in visa_data_urls) {
  visa_filename <- str_extract(url, "Ukraine.*")
  visa_filepath <- file.path(visa_path, visa_filename)

  if (!file.exists(visa_filepath)) {
    GET(
      url,
      write_disk(visa_filepath)
    )
    print(paste0("Downloaded ", visa_filename))
    Sys.sleep(0.75) # Scrape responsibly with a pause between requests
  } else {
    print(paste0(visa_filename, " has already been downloaded - skipping"))
  }
}

# ---- Helper functions for wrangling visa data ----
wrangle_visa_summary <- function(d, visa_date) {
  d |>
    as_tibble(.name_repair = "unique") |>
    rename(Type = `...2`) |>
    fill(Location, .direction = "down") |>
    na.omit() |>
    mutate(Date = visa_date) |>
    relocate(Date)
}

wrangle_visa_data <- function(d, visa_date, country = "^E") {
  d |>
    as_tibble() |>
    rename_with(function(x) "ltla21_code", contains("ONS.code")) |>
    filter(str_detect(ltla21_code, country)) |>
    mutate(Date = visa_date) |>
    relocate(Date) |>
    mutate(across(starts_with("Number"), as.character)) %>%
    # Some visa data files are missing the 'applications' or 'arrivals' columns - only rename if they exist
    # Inline conditional logic - the {if(...)} statements - needs matrittr pipes rather than base pipes
    {if(sum(str_detect(names(tmp_england), "applications")) > 0) rename_with(., function(x) "Number of visa applications", contains("applications")) else .} %>%
    {if(sum(str_detect(names(tmp_england), "arrivals")) > 0) rename_with(., function(x) "Number of arrivals", contains("arrivals")) else .}
}

convert_number_columns <- function(d) {
  d |>
    mutate(across(starts_with("Number"), ~ as.integer(str_remove(.x, "<")) - 1))
}

# ---- Load visa data ----
# Get list of downloaded visa data files
visa_files <- list.files(visa_path, pattern = "^Ukraine")

# Set up lists to hold all the tibbles
visas_summary <- list()
visas_england <- list()
visas_wales <- list()
visas_scotland <- list()
visas_ni <- list()

# List counter
i <- 1

# Loop over each visa data file and load/clean the data
for (visa_file in visa_files) {
  # Get date the data was published
  visa_date <-
    visa_file |>
    str_extract("(\\d{1,2}|\\d\\w+)_\\w+_\\d{4}") |>
    dmy()

  # The number of rows to skip when running read_ods() differs depending on the date/format of the file
  summary_skip <-
    case_when(
      visa_date <= ymd("2022-05-01") ~ 6,
      visa_date == ymd("2022-05-03") ~ 7,
      visa_date == ymd("2022-11-22") ~ 10,
      visa_date == ymd("2022-11-29") ~ 10,
      visa_date == ymd("2022-12-06") ~ 10,
      visa_date == ymd("2022-12-13") ~ 10,
      visa_date == ymd("2022-12-20") ~ 10,
      visa_date == ymd("2023-01-03") ~ 10,
      TRUE ~ 8
    )

  england_skip <-
    case_when(
      visa_date <= ymd("2022-05-01") ~ 6,
      visa_date == ymd("2022-05-03") ~ 7,
      visa_date == ymd("2022-06-21") ~ 7,
      visa_date == ymd("2022-11-22") ~ 10,
      visa_date == ymd("2022-11-29") ~ 10,
      visa_date == ymd("2022-12-06") ~ 10,
      visa_date == ymd("2022-12-13") ~ 10,
      visa_date == ymd("2022-12-20") ~ 10,
      visa_date == ymd("2023-01-03") ~ 10,
      TRUE ~ 8
    )

  wales_scotland_skip <-
    case_when(
      visa_date <= ymd("2022-05-01") ~ 8,
      visa_date == ymd("2022-05-03") ~ 9,
      visa_date == ymd("2022-11-22") ~ 12,
      visa_date == ymd("2022-11-29") ~ 12,
      visa_date == ymd("2022-12-06") ~ 12,
      visa_date == ymd("2022-12-13") ~ 12,
      visa_date == ymd("2022-12-20") ~ 12,
      visa_date == ymd("2023-01-03") ~ 12,
      TRUE ~ 10
    )

  ni_skip <-
    case_when(
      visa_date <= ymd("2022-05-01") ~ 6,
      visa_date == ymd("2022-11-22") ~ 10,
      visa_date == ymd("2022-11-29") ~ 10,
      visa_date == ymd("2022-12-06") ~ 10,
      visa_date == ymd("2022-12-13") ~ 10,
      visa_date == ymd("2022-12-20") ~ 10,
      visa_date == ymd("2023-01-03") ~ 10,
      TRUE ~ 8
    )

  # UK summary
  tmp_summary <- read_ods(file.path(visa_path, visa_file), sheet = "Summary", skip = summary_skip)
  visas_summary[[i]] <- wrangle_visa_summary(tmp_summary, visa_date)

  # England
  tmp_england <- read_ods(file.path(visa_path, visa_file), sheet = "LTLA_-_England", skip = england_skip)
  if (ncol(tmp_england) > 5) tmp_england <- tmp_england |> select(1:5)
  visas_england[[i]] <- wrangle_visa_data(tmp_england, visa_date, country = "^E")

  # Wales
  tmp_wales <- read_ods(file.path(visa_path, visa_file), sheet = "Wales", skip = wales_scotland_skip)
  visas_wales[[i]] <- wrangle_visa_data(tmp_wales, visa_date, country = "^W")

  # Scotland
  tmp_scotland <- read_ods(file.path(visa_path, visa_file), sheet = "Scotland", skip = wales_scotland_skip)
  visas_scotland[[i]] <- wrangle_visa_data(tmp_scotland, visa_date, country = "^S")

  # Northern Ireland
  tmp_ni <- read_ods(file.path(visa_path, visa_file), sheet = "Northern_Ireland", skip = ni_skip)
  visas_ni[[i]] <- wrangle_visa_data(tmp_ni, visa_date, country = "^N")

  i <- i + 1
  print(paste0("Imported data from ", visa_date))
}

# Combine lists into single tibbles
visas_ltla21_summary <- bind_rows(visas_summary) |>
  rename_all(~ gsub("\\.", " ", .))
visas_ltla21_england <- bind_rows(visas_england) |>
  rename_all(~ gsub("\\.", " ", .))
visas_ltla21_wales <- bind_rows(visas_wales) |>
  rename_all(~ gsub("\\.", " ", .))
visas_ltla21_scotland <- bind_rows(visas_scotland) |>
  rename_all(~ gsub("\\.", " ", .))
visas_ltla21_ni <- bind_rows(visas_ni) |>
  rename_all(~ gsub("\\.", " ", .))

# ---- Impute missing values ----
# The week of 27th Dec 2022 was not published. No information for why this is
# the case can be found. Assume this weeks figures match the previous week:

# - Summary -
visas_ltla21_summary <- visas_ltla21_summary |>
  distinct(Location, Type) |>
  mutate(
    Date = ymd("2022-12-27"),
    `Number of visa applications` = NA,
    `Number of visas issued` = NA,
    `Number of arrivals in the UK by sponsor location` = NA
  ) |>
  relocate(Date) |>
  bind_rows(visas_ltla21_summary) |>
  arrange(Date) |>
  group_by(Location, Type) |>
  fill(
    `Number of visa applications`,
    `Number of visas issued`,
    `Number of arrivals in the UK by sponsor location`
  ) |>
  ungroup()

# - England -
visas_ltla21_england <- visas_ltla21_england |>
  distinct(`Lower tier local authority name`, ltla21_code) |>
  mutate(
    Date = ymd("2022-12-27"),
    `Number of visa applications` = NA,
    `Number of visas issued` = NA,
    `Number of arrivals` = NA
  ) |>
  relocate(Date) |>
  bind_rows(visas_ltla21_england) |>
  arrange(Date) |>
  group_by(`Lower tier local authority name`, ltla21_code) |>
  fill(`Number of visa applications`, `Number of visas issued`, `Number of arrivals`) |>
  ungroup()

# - Wales -
visas_ltla21_wales <- visas_ltla21_wales |>
  distinct(`Upper tier local authority name`, ltla21_code) |>
  mutate(
    Date = ymd("2022-12-27"),
    `Number of visa applications` = NA,
    `Number of visas issued` = NA,
    `Number of arrivals` = NA
  ) |>
  relocate(Date) |>
  bind_rows(visas_ltla21_wales) |>
  arrange(Date) |>
  group_by(`Upper tier local authority name`, ltla21_code) |>
  fill(`Number of visa applications`, `Number of visas issued`, `Number of arrivals`) |>
  ungroup()

# - Scotland -
visas_ltla21_scotland <- visas_ltla21_scotland |>
  distinct(`Upper tier local authority name`, ltla21_code) |>
  mutate(
    Date = ymd("2022-12-27"),
    `Number of visa applications` = NA,
    `Number of visas issued` = NA,
    `Number of arrivals` = NA
  ) |>
  relocate(Date) |>
  bind_rows(visas_ltla21_scotland) |>
  arrange(Date) |>
  group_by(`Upper tier local authority name`, ltla21_code) |>
  fill(`Number of visa applications`, `Number of visas issued`, `Number of arrivals`) |>
  ungroup()

# - NI -
visas_ltla21_ni <- visas_ltla21_ni |>
  distinct(`Local authority name`, ltla21_code) |>
  mutate(
    Date = ymd("2022-12-27"),
    `Number of visa applications` = NA,
    `Number of visas issued` = NA,
    `Number of arrivals` = NA
  ) |>
  relocate(Date) |>
  bind_rows(visas_ltla21_ni) |>
  arrange(Date) |>
  group_by(`Local authority name`, ltla21_code) |>
  fill(`Number of visa applications`, `Number of visas issued`, `Number of arrivals`) |>
  ungroup()

# Create UK dataframe
visas_ltla21_uk <-
  bind_rows(
    visas_ltla21_england |> rename(ltla21_name = `Lower tier local authority name`),
    visas_ltla21_wales |> rename(ltla21_name = `Upper tier local authority name`),
    visas_ltla21_scotland |> rename(ltla21_name = `Upper tier local authority name`),
    visas_ltla21_ni |> rename(ltla21_name = `Local authority name`)
  ) |>
  mutate(Country = case_when(
    str_detect(ltla21_code, "^E") ~ "England",
    str_detect(ltla21_code, "^W") ~ "Wales",
    str_detect(ltla21_code, "^S") ~ "Scotland",
    str_detect(ltla21_code, "^N") ~ "Northern Ireland"
  ))

# Convert 'Number' columns to numbers
visas_ltla21_uk <- convert_number_columns(visas_ltla21_uk)
visas_ltla21_england <- convert_number_columns(visas_ltla21_england)
visas_ltla21_wales <- convert_number_columns(visas_ltla21_wales)
visas_ltla21_scotland <- convert_number_columns(visas_ltla21_scotland)
visas_ltla21_ni <- convert_number_columns(visas_ltla21_ni)

# Remove everything except the wrangled visa data
# rm(list = ls()[!str_detect(ls(), "visas_ltla21")])
rm(
  visa_path, visa_url, ua, req, out, doc, visa_data_urls, url, visa_filename, visa_filepath,
  wrangle_visa_summary, wrangle_visa_data, visa_files, i, visa_file, visa_date,
  summary_skip, england_skip, wales_scotland_skip, ni_skip,
  tmp_summary, tmp_england, tmp_wales, tmp_scotland, tmp_ni,
  visas_summary, visas_england, visas_wales, visas_scotland, visas_ni
)
