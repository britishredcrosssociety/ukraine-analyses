library(tidyverse)
library(readODS)
library(httr)

source("R/load Ukraine visa data - Local Authorities.R")

# ---- Load latest homelessness management info (24 February to 18 November 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1120984/Ukraine_Homelessness_Pressures_29112022.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_18nov_raw <- read_ods(tf, skip = 3, sheet = "Ukrainian_Homelessness")

# Remove empty columns
homelessness_24feb_18nov_raw <- homelessness_24feb_18nov_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_24feb_18nov_raw) <- c(
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

homelessness_24feb_18nov_raw <- 
  homelessness_24feb_18nov_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_18nov_total <- 
  homelessness_24feb_18nov_raw |> 
  slice(3)

homelessness_24feb_18nov <- 
  homelessness_24feb_18nov_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 21 October 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1115067/Ukraine_Homelessness_Pressures_31102022.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_21oct_raw <- read_ods(tf, skip = 3)

# Remove empty columns
homelessness_24feb_21oct_raw <- homelessness_24feb_21oct_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_24feb_21oct_raw) <- c(
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

homelessness_24feb_21oct_raw <- 
  homelessness_24feb_21oct_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_21oct_total <- 
  homelessness_24feb_21oct_raw |> 
  slice(3)

homelessness_24feb_21oct <- 
  homelessness_24feb_21oct_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 23 September 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1109392/Ukraine_Homelessness_management_information_03102022.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_23sep_raw <- read_ods(tf, skip = 3)

# Remove empty columns
homelessness_24feb_23sep_raw <- homelessness_24feb_23sep_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_24feb_23sep_raw) <- c(
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

homelessness_24feb_23sep_raw <- 
  homelessness_24feb_23sep_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_23sep_total <- 
  homelessness_24feb_23sep_raw |> 
  slice(3)

homelessness_24feb_23sep <- 
  homelessness_24feb_23sep_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 26 August 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1103040/Ukraine_Management_Information_Table_we-20220826.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_26aug_raw <- read_ods(tf, skip = 3)

# Remove empty columns
homelessness_24feb_26aug_raw <- homelessness_24feb_26aug_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_24feb_26aug_raw) <- c(
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

homelessness_24feb_26aug_raw <- 
  homelessness_24feb_26aug_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_26aug_total <- 
  homelessness_24feb_26aug_raw |> 
  slice(3)

homelessness_24feb_26aug <- 
  homelessness_24feb_26aug_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 29 July 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1097490/Ukraine_Homelessness_Management_Information_20220808.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_29jul_raw <- read_ods(tf, skip = 3)

# Remove empty columns
homelessness_24feb_29jul_raw <- homelessness_24feb_29jul_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_24feb_29jul_raw) <- c(
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

homelessness_24feb_29jul_raw <- 
  homelessness_24feb_29jul_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_29jul_total <- 
  homelessness_24feb_29jul_raw |> 
  slice(3)

homelessness_24feb_29jul <- 
  homelessness_24feb_29jul_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load previous homelessness management info (24 February to 1 July 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1090431/Ukraine_Homelessness_Management_Information_20220708.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_1jul_raw <- read_ods(tf, skip = 3)

# homelessness_24feb_1jul_raw <- 
#   homelessness_24feb_1jul_raw |> 
#   janitor::remove_empty(which = "cols")

# Remove empty columns
homelessness_24feb_1jul_raw <- homelessness_24feb_1jul_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_24feb_1jul_raw) <- c(
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

homelessness_24feb_1jul_raw <- 
  homelessness_24feb_1jul_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_1jul_total <- 
  homelessness_24feb_1jul_raw |> 
  slice(3)

homelessness_24feb_1jul <- 
  homelessness_24feb_1jul_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load previous homelessness management info (24 February to 3 June 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1083480/Ukraine_Homelessness_Pressures_16062022.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_3jun_raw <- read_ods(tf, skip = 3)

# Remove empty columns
homelessness_24feb_3jun_raw <- homelessness_24feb_3jun_raw[-c(3, 4, 10, 14, 21)]

names(homelessness_24feb_3jun_raw) <- c(
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

homelessness_24feb_3jun_raw <- 
  homelessness_24feb_3jun_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_3jun_total <- 
  homelessness_24feb_3jun_raw |> 
  slice(3)

homelessness_24feb_3jun <- 
  homelessness_24feb_3jun_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Summary of homelessness ----
homelessness_total <- 
  bind_rows(
    homelessness_24feb_18nov_total |> mutate(Date = ymd("2022-11-18"), Date_text = "18 November"),
    homelessness_24feb_21oct_total |> mutate(Date = ymd("2022-10-21"), Date_text = "21 October"),
    homelessness_24feb_23sep_total |> mutate(Date = ymd("2022-09-23"), Date_text = "23 September"),
    homelessness_24feb_26aug_total |> mutate(Date = ymd("2022-08-26"), Date_text = "26 August"),
    homelessness_24feb_29jul_total |> mutate(Date = ymd("2022-07-29"), Date_text = "29 July"),
    homelessness_24feb_1jul_total  |> mutate(Date = ymd("2022-07-01"), Date_text = "1 July"),
    homelessness_24feb_3jun_total  |> mutate(Date = ymd("2022-06-03"), Date_text = "3 June")
  ) |> 
  relocate(Date) |> 
  mutate(Date_text = factor(Date_text, levels = c("3 June", "1 July", "29 July", "26 August", "23 September", "21 October", "18 November")))

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
# Wrangle number of arrivals for each date - using nearest available arrivals figures

# Find most recent dates nearest to (but after) the homelessness data
visas_ltla21_england |> 
  arrange(Date) |> 
  distinct(Date) |> 
  print(n = 50)

visas_22nov <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-11-22")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

visas_25oct <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-10-25")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

visas_27sep <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-09-27")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

visas_30aug <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-08-30")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

visas_2aug <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-08-02")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

visas_5jul <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-07-05")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

visas_7jun <- 
  visas_ltla21_england |> 
  filter(Date == ymd("2022-06-07")) |> 
  mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
  select(ltla21_code, `Number of arrivals`)

# Calculate proportions
homelessness_24feb_18nov <- 
  homelessness_24feb_18nov |> 
  left_join(visas_22nov, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

homelessness_24feb_21oct <- 
  homelessness_24feb_21oct |> 
  left_join(visas_25oct, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

homelessness_24feb_23sep <- 
  homelessness_24feb_23sep |> 
  left_join(visas_27sep, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

homelessness_24feb_26aug <- 
  homelessness_24feb_26aug |> 
  left_join(visas_30aug, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

homelessness_24feb_29jul <- 
  homelessness_24feb_29jul |> 
  left_join(visas_2aug, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

homelessness_24feb_1jul <- 
  homelessness_24feb_1jul |> 
  left_join(visas_5jul, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

homelessness_24feb_3jun <- 
  homelessness_24feb_3jun |> 
  left_join(visas_7jun, by = c("lad_code" = "ltla21_code")) |> 
  mutate(
    `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
  )

# ---- Make single tibble of longitudinal homelessness data ---- 
homelessness_trends <- 
  left_join(
    homelessness_24feb_3jun |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (7 June)` = `Number of arrivals`,
        total_3jun = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_3jun = `% at risk of homelessness`,
        temp_3jun = `Temporary Accommodation Snapshot`, 
        temp_percent_3jun = `% in temporary accommodation`
      ),
    
    homelessness_24feb_1jul |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (5 July)` = `Number of arrivals`,
        total_1jul = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_1jul = `% at risk of homelessness`,
        temp_1jul = `Temporary Accommodation Snapshot`, 
        temp_percent_1jul = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_29jul |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (2 August)` = `Number of arrivals`,
        total_29jul = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_29jul = `% at risk of homelessness`,
        temp_29jul = `Temporary Accommodation Snapshot`, 
        temp_percent_29jul = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_26aug |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (30 August)` = `Number of arrivals`,
        total_26aug = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_26aug = `% at risk of homelessness`,
        temp_26aug = `Temporary Accommodation Snapshot`, 
        temp_percent_26aug = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_23sep |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (27 September)` = `Number of arrivals`,
        total_23sep = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_23sep = `% at risk of homelessness`,
        temp_23sep = `Temporary Accommodation Snapshot`, 
        temp_percent_23sep = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_21oct |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (25 October)` = `Number of arrivals`,
        total_21oct = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_21oct = `% at risk of homelessness`,
        temp_21oct = `Temporary Accommodation Snapshot`, 
        temp_percent_21oct = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_18nov |> 
      select(
        lad_code, 
        lad_name, 
        `Number of arrivals (22 November)` = `Number of arrivals`,
        total_18nov = `Total Ukrainian households owed a prevention or relief duty`, 
        percent_18nov = `% at risk of homelessness`,
        temp_18nov = `Temporary Accommodation Snapshot`, 
        temp_percent_18nov = `% in temporary accommodation`
      )
  )

homelessness_trends <- 
  homelessness_trends |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  replace_na(list(
    total_3jun = 0, 
    percent_3jun = 0, 
    temp_3jun = 0, 
    temp_percent_3jun = 0, 
    
    total_1jul = 0, 
    percent_1jul = 0, 
    temp_1jul = 0,
    temp_percent_1jul = 0,
    
    total_29jul = 0, 
    percent_29jul = 0, 
    temp_29jul = 0,
    temp_percent_29jul = 0,
    
    total_26aug = 0, 
    percent_26aug = 0, 
    temp_26aug = 0,
    temp_percent_26aug = 0,
    
    total_23sep = 0, 
    percent_23sep = 0, 
    temp_23sep = 0,
    temp_percent_23sep = 0,
    
    total_21oct = 0, 
    percent_21oct = 0, 
    temp_21oct = 0,
    temp_percent_21oct = 0,
    
    total_18nov = 0, 
    percent_18nov = 0, 
    temp_18nov = 0,
    temp_percent_18nov = 0
  )) |> 
  mutate(
    total_delta = total_18nov - total_3jun,
    percent_delta = percent_18nov - percent_3jun,
    temp_delta = temp_18nov - temp_3jun,
    temp_percent_delta = temp_percent_18nov - temp_percent_3jun
  )

# ---- Save wrangled data ----
write_csv(homelessness_24feb_3jun, "data/homelessness/ukraine-homelessness-3-june.csv")
write_csv(homelessness_24feb_1jul, "data/homelessness/ukraine-homelessness-1-july.csv")
write_csv(homelessness_24feb_29jul, "data/homelessness/ukraine-homelessness-29-july.csv")
write_csv(homelessness_24feb_26aug, "data/homelessness/ukraine-homelessness-26-august.csv")
write_csv(homelessness_24feb_23sep, "data/homelessness/ukraine-homelessness-23-september.csv")
write_csv(homelessness_24feb_21oct, "data/homelessness/ukraine-homelessness-21-october.csv")
write_csv(homelessness_24feb_18nov, "data/homelessness/ukraine-homelessness-18-november.csv")

write_csv(homelessness_total, "data/homelessness/ukraine-homelessness-summary.csv")
write_csv(homelessness_trends, "data/homelessness/ukraine-homelessness-trends.csv")

# Save a publicly accessible copy of totals and trends
write_csv(homelessness_total, "output-data/homelessness/ukraine-homelessness-summary.csv")
write_csv(homelessness_trends, "output-data/homelessness/ukraine-homelessness-trends.csv")
