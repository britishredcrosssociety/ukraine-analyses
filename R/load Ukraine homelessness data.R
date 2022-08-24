library(tidyverse)
library(readODS)
library(httr)

source("R/load Ukraine visa data - Local Authorities.R")

# ---- Load latest homelessness management info (24 February to 29 July 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1097490/Ukraine_Homelessness_Management_Information_20220808.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_feb_aug_raw <- read_ods(tf, skip = 3)

# Remove empty columns
homelessness_feb_aug_raw <- homelessness_feb_aug_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_feb_aug_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Homelessness Prevented or Relieved via Mediation",
  "Homelessness Prevented or Relieved via Rematch",
  "Homelessness Prevented or Relieved via Other",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_feb_aug_raw <- 
  homelessness_feb_aug_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_feb_aug_total <- 
  homelessness_feb_aug_raw |> 
  slice(3)

homelessness_feb_aug <- 
  homelessness_feb_aug_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load previous homelessness management info (24 February to 1 July 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1090431/Ukraine_Homelessness_Management_Information_20220708.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_feb_july_raw <- read_ods(tf, skip = 3)

# homelessness_feb_july_raw <- 
#   homelessness_feb_july_raw |> 
#   janitor::remove_empty(which = "cols")

# Remove empty columns
homelessness_feb_july_raw <- homelessness_feb_july_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_feb_july_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Homelessness Prevented or Relieved via Mediation",
  "Homelessness Prevented or Relieved via Rematch",
  "Homelessness Prevented or Relieved via Other",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_feb_july_raw <- 
  homelessness_feb_july_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_feb_july_total <- 
  homelessness_feb_july_raw |> 
  slice(3)

homelessness_feb_july <- 
  homelessness_feb_july_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load previous homelessness management info (24 February to 3 June 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1083480/Ukraine_Homelessness_Pressures_16062022.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_feb_june_raw <- read_ods(tf, skip = 3)

# Remove empty columns
homelessness_feb_june_raw <- homelessness_feb_june_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_feb_june_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Homelessness Prevented or Relieved via Mediation",
  "Homelessness Prevented or Relieved via Rematch",
  "Homelessness Prevented or Relieved via Other",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_feb_june_raw <- 
  homelessness_feb_june_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_feb_june_total <- 
  homelessness_feb_june_raw |> 
  slice(3)

homelessness_feb_june <- 
  homelessness_feb_june_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Summary of homelessness ----
homelessness_total <- 
  bind_rows(
    homelessness_feb_aug_total  |> mutate(Date = ymd("2022-07-29"), Date_text = "29 July"),
    homelessness_feb_july_total |> mutate(Date = ymd("2022-07-01"), Date_text = "1 July"),
    homelessness_feb_june_total |> mutate(Date = ymd("2022-06-03"), Date_text = "3 June")
  ) |> 
  relocate(Date) |> 
  mutate(Date_text = factor(Date_text, levels = c("3 June", "1 July", "29 July")))

# Calculate proportions of households at risk of homelessness in each visa scheme
homelessness_total <- 
  homelessness_total |> 
  mutate(
    `Homes for Ukraine` = `Homes for Ukraine Scheme: Accommodation or arrangement broken down` + `Homes for Ukraine Scheme: Accommodation not available or suitable on arrival` + `Homes for Ukraine Scheme: Rejected sponsors offer`,
    `Family Scheme` = `Family Scheme: Accommodation or arrangement broken down` + `Family Scheme: Accommodation not available or suitable on arrival`,
    
    `% Homes for Ukraine` = `Homes for Ukraine` / `Total Ukrainian households owed a prevention or relief duty`,
    `% Family Scheme` = `Family Scheme` / `Total Ukrainian households owed a prevention or relief duty`,
  )

# ---- Homelessness as a proportion of all arrivals ----
# Wrangle number of arrivals for each date
visas_aug <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-08-02")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

visas_jul <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-07-05")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

visas_jun <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-06-07")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

# Calculate proportions
homelessness_feb_aug <- 
  homelessness_feb_aug |> 
  left_join(visas_aug, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

homelessness_feb_july <- 
  homelessness_feb_july |> 
  left_join(visas_jul, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

homelessness_feb_june <- 
  homelessness_feb_june |> 
  left_join(visas_jun, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

# ---- Make single tibble of longitudinal homelessness data ---- 
homelessness_trends <- 
  left_join(
    homelessness_feb_june |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (June)` = `Number of arrivals`,
        total_june = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_june = `% at risk of homelessness`,
        temp_june = `Temporary Accommodation Snapshot`, 
        temp_percent_june = `% in temporary accommodation`
      ),
    
    homelessness_feb_july |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (July)` = `Number of arrivals`,
        total_july = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_july = `% at risk of homelessness`,
        temp_july = `Temporary Accommodation Snapshot`, 
        temp_percent_july = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_feb_aug |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (August)` = `Number of arrivals`,
        total_aug = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_aug = `% at risk of homelessness`,
        temp_aug = `Temporary Accommodation Snapshot`, 
        temp_percent_aug = `% in temporary accommodation`
      )
  )

homelessness_trends <- 
  homelessness_trends |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  replace_na(list(
    total_june = 0, 
    percent_june = 0, 
    temp_june = 0, 
    temp_percent_june = 0, 
    
    total_july = 0, 
    percent_july = 0, 
    temp_july = 0,
    temp_percent_july = 0,
    
    total_aug = 0, 
    percent_aug = 0, 
    temp_aug = 0,
    temp_percent_aug = 0
  )) |> 
  mutate(
    total_delta = total_aug - total_june,
    percent_delta = percent_aug - percent_june,
    temp_delta = temp_aug - temp_june,
    temp_percent_delta = temp_percent_aug - temp_percent_june
  )

# ---- Save wrangled data ----
write_csv(homelessness_feb_june, "data/homelessness/ukraine-homelessness-3-june.csv")
write_csv(homelessness_feb_july, "data/homelessness/ukraine-homelessness-1-july.csv")
write_csv(homelessness_feb_aug, "data/homelessness/ukraine-homelessness-29-july.csv")

write_csv(homelessness_total, "data/homelessness/ukraine-homelessness-summary.csv")

write_csv(homelessness_trends, "data/homelessness/ukraine-homelessness-trends.csv")
