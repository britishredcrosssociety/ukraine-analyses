library(tidyverse)
library(demographr)
library(geographr)
library(compositr)
library(readODS)
library(readxl)
library(httr)

# England ----
## Homelessness ----
# Load live tables on homelessness
# Source: https://www.gov.uk/government/statistical-data-sets/live-tables-on-homelessness
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1173277/Detailed_LA_202303.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

england_homeless_raw <- read_ods(tf, sheet = "A1", skip = 6)

england_homeless <- england_homeless_raw[, c(1, 2, 15, 16)]
names(england_homeless) <- c("ltla21_code", "ltla21_name", "Households assessed as threatened with homelessness per (000s)", "Households assessed as homeless per (000s)")

# Keep only Local Authority Districts
england_homeless <- 
  england_homeless |> 
  as_tibble() |> 
  filter(str_detect(ltla21_code, "^E0[6-9]")) |> 
  mutate(
    `Households assessed as threatened with homelessness per (000s)` = as.numeric(`Households assessed as threatened with homelessness per (000s)`),
    `Households assessed as homeless per (000s)` = as.numeric(`Households assessed as homeless per (000s)`)
  )

## Temporary accommodation ----
england_temp_accomm_raw <- read_ods(tf, sheet = "TA1", skip = 6)

england_temp_accomm <- england_temp_accomm_raw[, c(1, 2, 7)]
names(england_temp_accomm) <- c("ltla21_code", "ltla21_name", "Total number of households in TA per (000s)")

# Keep only Local Authority Districts
england_temp_accomm <- 
  england_temp_accomm |> 
  as_tibble() |> 
  filter(str_detect(ltla21_code, "^E0[6-9]")) |> 
  mutate(`Households in temporary accommodation per 1,000` = as.numeric(`Total number of households in TA per (000s)`)) |> 
  select(ltla21_code, `Households in temporary accommodation per 1,000`)

## Housing stock and waiting list data ----
# Load LA housing statistics
# Source: https://www.gov.uk/government/collections/local-authority-housing-data // https://www.gov.uk/government/statistical-data-sets/local-authority-housing-statistics-data-returns-for-2021-to-2022
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1166232/LAHS_21_22_accessible.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

england_stock_waits <- read_ods(tf, sheet = "Local_Authority_Data", skip = 2)

# Keep relevant columns
# Definitions of column codes are from: https://www.gov.uk/government/publications/completing-local-authority-housing-statistics-2020-to-2021-guidance-notes-and-bulk-upload
england_stock_waits <- 
  england_stock_waits |> 
  select(
    ltla21_code = LA_Code,
    # ltla21_name = organisation.name,
    
    # Total number of households on the waiting list at 31 March
    `Households on housing waiting list` = cc1a,
    
    # Total number of vacant dwellings owned by any local authority
    # (either your own or another local authority) within your district area at 31 March of the reporting year
    `Vacant dwellings` = e1a,
    
    # total_stock = a2ia,
    
    # Total social rent stock
    social_rent_stock = a2iaa,
    
    # Total affordable rent stock
    affordable_rent_stock = a2iab
  ) |> 
  mutate(social_rent_stock = as.integer(social_rent_stock), affordable_rent_stock = as.integer(affordable_rent_stock)) |> 
  mutate(`Housing stock` = social_rent_stock + affordable_rent_stock) |> 
  select(-social_rent_stock, -affordable_rent_stock)

# Calculate rates
england_stock_waits <- 
  england_stock_waits |> 
  left_join(demographr::households21_ltla21 |> select(-ltla21_name)) |> 
  mutate(
    `Households on housing waiting list per 1,000` = `Households on housing waiting list` / households * 1000,
    `Social housing stock as a proportion of all households` = `Housing stock` / households,
    `Vacant dwellings per 1,000 units of social housing stock` = if_else(is.na(`Housing stock`) | `Housing stock` == 0, `Vacant dwellings`, `Vacant dwellings` / `Housing stock` * 1000)
  ) |> 
  select(
    ltla21_code, 
    `Households on housing waiting list per 1,000`, 
    `Social housing stock as a proportion of all households`,
    # `Vacant dwellings per 1,000 units of social housing stock`
    `Vacant dwellings`
  )

## Combine datasets and calculate deciles ----
england <- 
  england_homeless |> 
  left_join(england_temp_accomm) |> 
  left_join(england_stock_waits) |> 
  
  mutate(
    stock_quintile = as.integer(Hmisc::cut2(`Social housing stock as a proportion of all households`, g = 5)),
    waiting_quintile = as.integer(Hmisc::cut2(`Households on housing waiting list per 1,000`, g = 5)),
    homeless_quintile = as.integer(Hmisc::cut2(`Households assessed as threatened with homelessness per (000s)`, g = 5)),
    temp_accom_quintile = as.integer(Hmisc::cut2(`Households in temporary accommodation per 1,000`, g = 5)),
    vacancies_quintile = as.integer(Hmisc::cut2(`Vacant dwellings`, g = 5))
  )

# Save
write_csv(england, "data/housing/housing-england.csv")

# Scotland ----
# Download latest homelessness data
# Source: https://www.gov.scot/publications/homelessness-in-scotland-2022-23/documents/
tf <- download_file("https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2023/08/homelessness-in-scotland-2022-23/documents/main-tables_homelessness-in-scotland-2022-23/main-tables_homelessness-in-scotland-2022-23/govscot%3Adocument/Main%2Btables_Homelessness%2Bin%2BScotland%2B2022-23.xlsx", ".xlsx")

## Local Authority names and codes ----
scottish_ltla_names_codes <- 
  geographr::boundaries_ltla21 |> 
  sf::st_drop_geometry() |> 
  filter(str_detect(ltla21_code, "^S")) |> 
  
  # Match to LA names in the homelessness data
  mutate(ltla21_name = str_replace(ltla21_name, " and ", " & ")) |> 
  mutate(
    ltla21_name = case_match(
      ltla21_name,
      "Na h-Eileanan Siar" ~ "Eilean Siar", 
      "City of Edinburgh" ~ "Edinburgh",
      "Shetland Islands" ~ "Shetland",
      .default = ltla21_name
    )
  )

## Household estimates ----
# Source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates
scotland_households <- read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2021", skip = 7)

scotland_households <- 
  scotland_households |> 
  mutate(ltla21_code = str_extract(`http://purl.org/linked-data/sdmx/2009/dimension#refArea`, "S[0-9]+")) |> 
  filter(str_detect(ltla21_code, "^S12")) |> 
  select(
    ltla21_code, 
    `Number of households` = `Which Are Occupied`
  )
  
## Homelessness ----
# Source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhomelessness-applications
# scotland_homelessness <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhomelessness-applications")
# 
# scotland_homelessness <- 
#   scotland_homelessness |> 
#   filter(DateCode == "2020/2021" & FeatureType == "Council Area" & `Application Type` == "Assessed as homeless or threatened with homelessness") |> 
#   select(
#     ltla21_code = FeatureCode,
#     `Assessed as homeless or threatened with homelessness` = Value
#   ) |> 
#   
#   # Calculate rate per 1000 households
#   left_join(scotland_households) |> 
#   mutate(`Assessed as homeless or threatened with homelessness per 1,000` = `Assessed as homeless or threatened with homelessness` / `Number of households` * 1000) |> 
#   select(ltla21_code, `Assessed as homeless or threatened with homelessness per 1,000`)

# Homelessness applications by local authority
scotland_homelessness <- read_excel(tf, sheet = "T1", range = "A6:F39")

scotland_homelessness <- 
  scotland_homelessness |> 
  select(ltla21_name = `Local Authority`, applications = `2022-23`) |> 
  filter(ltla21_name != "Scotland") |> 
  
  left_join(scottish_ltla_names_codes) |> 
  
  # Calculate rate
  left_join(scotland_households) |> 
  mutate(`Assessed as homeless or threatened with homelessness per 1,000` = applications / `Number of households` * 1000) |> 
  select(ltla21_code, `Assessed as homeless or threatened with homelessness per 1,000`)

## Temporary accommodation ----
# Source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Ftemporary-accommodation-statistics
scotland_temp_accom <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Ftemporary-accommodation-statistics")

scotland_temp_accom <- 
  scotland_temp_accom |> 
  filter(DateCode == max(DateCode) & FeatureType == "Council Area") |> 
  select(
    ltla21_code = FeatureCode,
    `Households in temporary accommodation` = Value
  ) |> 
  
  # Calculate rate per 1000 households
  left_join(scotland_households) |> 
  mutate(`Households in temporary accommodation per 1,000` = `Households in temporary accommodation` / `Number of households` * 1000) |> 
  select(ltla21_code, `Households in temporary accommodation per 1,000`)

#
scotland_temp_accom <- read_excel(tf, sheet = "T26", range = "A6:F39")

scotland_temp_accom <- 
  scotland_temp_accom |> 
  select(ltla21_name = `Local Authority`, temp_accomm = `2023`) |> 
  filter(ltla21_name != "Scotland") |> 
  
  left_join(scottish_ltla_names_codes) |> 
  
  # Calculate rate
  left_join(scotland_households) |> 
  mutate(`Households in temporary accommodation per 1,000` = temp_accomm / `Number of households` * 1000) |> 
  select(ltla21_code, `Households in temporary accommodation per 1,000`)

## Waiting list ----
# Source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousing-lists
scotland_waiting <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousing-lists")

scotland_waiting <- 
  scotland_waiting |> 
  filter(DateCode == max(DateCode) & FeatureType == "Council Area" & `Type of Housing List` == "Waiting list") |> 
  select(
    ltla21_code = FeatureCode,
    `Households on housing waiting list` = Value
  ) |> 
  
  # Calculate rate per 1000 households
  left_join(scotland_households) |> 
  mutate(`Households on housing waiting list per 1,000` = `Households on housing waiting list` / `Number of households` * 1000) |> 
  select(ltla21_code, `Households on housing waiting list per 1,000`)

## Social housing stock ----
# Source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Flocal-authority-housing-stock-by-normal-use
scotland_stock_raw <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Flocal-authority-housing-stock-by-normal-use")

scotland_stock_raw <- 
  scotland_stock_raw |> 
  filter(DateCode == max(DateCode) & FeatureType == "Council Area" & `Normal use of Dwelling` == "All") |> 
  select(
    ltla21_code = FeatureCode,
    `Housing stock` = Value
  )

# Calculate rate
scotland_stock <- 
  scotland_stock_raw |> 
  left_join(scotland_households) |> 
  mutate(`Social housing stock as a proportion of all households` = `Housing stock` / `Number of households`) |> 
  select(ltla21_code, `Social housing stock as a proportion of all households`)

## Vacancies ----
# Source: https://statistics.gov.scot/data/local-authority-vacant-dwellings
scotland_vacancies <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Flocal-authority-vacant-dwellings")

scotland_vacancies <- 
  scotland_vacancies |> 
  filter(DateCode == max(DateCode) & FeatureType == "Council Area" & `Status of vacant dwellings` == "All vacant dwellings" & `Length of time vacant` == "All vacant dwellings") |> 
  select(
    ltla21_code = FeatureCode,
    `Vacant dwellings` = Value
  ) |> 
  
  left_join(scotland_stock_raw) |> 
  mutate(`Vacant dwellings per 1,000 units of social housing stock` = if_else(is.na(`Housing stock`) | `Housing stock` == 0, `Vacant dwellings`, `Vacant dwellings` / `Housing stock` * 1000)) |> 
  select(ltla21_code, `Vacant dwellings per 1,000 units of social housing stock`)

## Combine datasets and calculate deciles ----
scotland <- 
  scotland_homelessness |> 
  left_join(scotland_temp_accom) |> 
  left_join(scotland_waiting) |> 
  left_join(scotland_stock) |> 
  left_join(scotland_vacancies) |> 
  
  mutate(
    stock_quintile = as.integer(Hmisc::cut2(`Social housing stock as a proportion of all households`, g = 5)),
    waiting_quintile = as.integer(Hmisc::cut2(`Households on housing waiting list per 1,000`, g = 5)),
    homeless_quintile = as.integer(Hmisc::cut2(`Assessed as homeless or threatened with homelessness per 1,000`, g = 5)),
    temp_accom_quintile = as.integer(Hmisc::cut2(`Households in temporary accommodation per 1,000`, g = 5)),
    vacancies_quintile = as.integer(Hmisc::cut2(`Vacant dwellings per 1,000 units of social housing stock`, g = 5))
  )

# Save
write_csv(scotland, "data/housing/housing-scotland.csv")

# Wales ----
source("https://github.com/matthewgthomas/brclib/raw/master/R/download_wales.R")

## Household estimates ----
# Dwelling stock estimates by local authority and tenure
# Source: https://statswales.gov.wales/Catalogue/Housing/Dwelling-Stock-Estimates
wales_households_raw <- download_wales("http://open.statswales.gov.wales/en-gb/dataset/hous0501")

wales_households <- 
  wales_households_raw |> 
  mutate(Year_Code = as.integer(Year_Code)) |> 
  filter(
    Year_Code == max(Year_Code) & 
      !Area_Hierarchy %in% c("0", "596") &  # Keep only Local Authorities
      Tenure_ItemName_ENG == "All tenures (Number)"
  ) |> 
  select(
    Area_Code,
    ltla21_name = Area_ItemName_ENG,
    `Number of households` = Data
  )

## Homelessness ----
# Households for which assistance has been provided by outcome and household type
# Source: https://statswales.gov.wales/Catalogue/Housing/Homelessness/Statutory-Homelessness-Prevention-and-Relief
wales_homelessness_raw <- download_wales("http://open.statswales.gov.wales/en-gb/dataset/hous0413")

#!! I don't think `number of outcomes` is what we want - needs to be number of households
wales_homelessness <- 
  wales_homelessness_raw |> 
  filter(
    Period_Code == "2022230",
    str_detect(Area_AltCode1, "^W06"), 
    Household_ItemName_ENG == "Total",
    str_detect(Outcomes_ItemName_ENG, "Number of outcomes")
  ) |> 
  
  mutate(Data = if_else(Data >= 0, Data, NA_real_)) |> 
  
  group_by(Area_AltCode1, Area_Code) |> 
  summarise(`Homeless or threatened with homelessness` = sum(Data, na.rm = TRUE)) |> 
  ungroup()

# Calculate homelessness rate
wales_homelessness <- 
  wales_homelessness |> 
  left_join(wales_households) |> 
  mutate(`Homeless or threatened with homelessness per 1,000` = `Homeless or threatened with homelessness` / `Number of households` * 1000) |> 
  select(ltla21_code = Area_AltCode1, `Homeless or threatened with homelessness per 1,000`)

## Temporary accommodation ----
# Households accommodated temporarily by accommodation type and household type 
# Source: https://statswales.gov.wales/Catalogue/Housing/Homelessness/Temporary-Accommodation
wales_temp_accom_raw <- download_wales("http://open.statswales.gov.wales/en-gb/dataset/hous0420")

wales_temp_accom <- 
  wales_temp_accom_raw |> 
  filter(
    Period_Code == "202223Q4",
    str_detect(Area_AltCode1, "^W06"), 
    Household_ItemName_ENG == "Total",
    Time_ItemName_ENG == "Total"
  ) |> 
  
  mutate(Data = if_else(Data >= 0, Data, NA_real_)) |> 
  
  group_by(Area_AltCode1, Area_Code) |> 
  summarise(`Households in temporary accommodation` = sum(Data, na.rm = TRUE)) |> 
  ungroup()

# Calculate rate
wales_temp_accom <- 
  wales_temp_accom |> 
  left_join(wales_households) |> 
  mutate(`Households in temporary accommodation per 1,000` = `Households in temporary accommodation` / `Number of households` * 1000) |> 
  select(ltla21_code = Area_AltCode1, `Households in temporary accommodation per 1,000`)

## Waiting list ----
#!! Couldn't find data

## Social housing stock ----
# Total social housing stock by local authority area and provider type
# Source: https://statswales.gov.wales/Catalogue/Housing/Social-Housing-Stock-and-Rents
wales_stock_raw <- download_wales("http://open.statswales.gov.wales/en-gb/dataset/hous0602")

wales_stock_count <- 
  wales_stock_raw |> 
  filter(
    Year_Code == "202122",
    str_detect(Area_AltCode1, "^W06"),
    Accommodation_ItemName_ENG == "Total social housing stock including intermediate tenures, intermediate rents and other social housing",
    Provider_ItemName_ENG == "Wales"
  ) |> 
  
  select(
    Area_Code,
    ltla21_code = Area_AltCode1,
    # ltla21_name = Area_ItemName_ENG,
    `Housing stock` = Data
  ) |> 
  
  as_tibble()

# Calculate rate
wales_stock <- 
  wales_stock_count |> 
  left_join(wales_households) |> 
  mutate(`Social housing stock as a proportion of all households` = `Housing stock` / `Number of households`) |> 
  select(ltla21_code, `Social housing stock as a proportion of all households`)

## Vacancies ----
# Vacancies by local authority area, availability and duration
# Source: https://statswales.gov.wales/Catalogue/Housing/Social-Housing-Vacancies
wales_vacancies_raw <- download_wales("http://open.statswales.gov.wales/en-gb/dataset/hous1401")

wales_vacancies <- 
  wales_vacancies_raw |> 
  filter(
    Year_Code == "202122",
    Area_Hierarchy != 0,  # Keep only Local Authorities
    Vacancy_ItemName_ENG == "Total",
    Availability_ItemName_ENG == "Total",
    Duration_ItemName_ENG == "Total", 
    Provider_ItemName_ENG == "Wales"
  ) |> 
  select(
    Area_Code,
    ltla21_name = Area_ItemName_ENG,
    `Vacant dwellings` = Data
  ) |> 
  
  as_tibble()

# Calculate rates
wales_vacancies <- 
  wales_vacancies |> 
  left_join(wales_stock_count) |> 
  mutate(`Vacant dwellings per 1,000 units of social housing stock` = if_else(is.na(`Housing stock`) | `Housing stock` == 0, `Vacant dwellings`, `Vacant dwellings` / `Housing stock` * 1000)) |> 
  select(ltla21_code, `Vacant dwellings per 1,000 units of social housing stock`)

## Combine datasets and calculate deciles ----
wales <- 
  wales_homelessness |> 
  left_join(wales_temp_accom) |> 
  left_join(wales_stock) |> 
  left_join(wales_vacancies) |> 
  
  mutate(
    stock_quintile = as.integer(Hmisc::cut2(`Social housing stock as a proportion of all households`, g = 5)),
    homeless_quintile = as.integer(Hmisc::cut2(`Homeless or threatened with homelessness per 1,000`, g = 5)),
    temp_accom_quintile = as.integer(Hmisc::cut2(`Households in temporary accommodation per 1,000`, g = 5)),
    vacancies_quintile = as.integer(Hmisc::cut2(`Vacant dwellings per 1,000 units of social housing stock`, g = 5))
  )

# Save
write_csv(wales, "data/housing/housing-wales.csv")

# Northern Ireland ----

## Households estimates ----

# Northern Ireland Housing Statistics 2021-22 Section 1 Tables – Supply
# Source: https://www.communities-ni.gov.uk/publications/northern-ireland-housing-statistics-2021-22
tf <- download_file("https://www.communities-ni.gov.uk/system/files/publications/communities/ni-housing-stats-21-22-tables1.ods", ".ods")

# 1.2 Total housing stock in each of the 11 district council areas 2008-2021
ni_households_raw <- read_ods(tf, sheet = "T1_2", range = "A3:P15")

ni_households <- 
  ni_households_raw |> 
  na.omit() |> 
  as_tibble() |> 
  select(
    ltla21_name = District.council,
    `Number of households` = X2022
  )

## Homelessness ----
# Source: https://www.communities-ni.gov.uk/publications/northern-ireland-homelessness-bulletin-july-december-2022
tf <- download_file("https://www.communities-ni.gov.uk/system/files/publications/communities/ni-homelessness-bulletin-jul-dec-2022-tables.ods", ".ods")

# 2.3 Households accepted as homeless by local government district
# ni_homelessness_raw <- read_ods(tf, sheet = "2_3", range = "A3:I14")

# 1.3 Homeless presenters by local government district
ni_homelessness_raw <- read_ods(tf, sheet = "1_3", range = "A4:U15")

ni_homelessness <- 
  ni_homelessness_raw[, c(1, 21)]

ni_homelessness <-
  ni_homelessness |> 
  as_tibble() |> 
  rename(
    ltla21_name = Local.government.district..LGD.,
    `Homeless or threatened with homelessness per 1,000` = Presenters.per.1.000.population...21
  )

## Temporary accommodation ----
#!! Data not available by Local Government District

## Waiting list ----
# Northern Ireland Housing Statistics 2021-22 Section 3 Tables – Social Renting Sector
# Source: https://www.communities-ni.gov.uk/publications/northern-ireland-housing-statistics-2021-22
tf <- download_file("https://www.communities-ni.gov.uk/system/files/publications/communities/ni-housing-stats-21-22-tables3.ods", ".ods")

# 3.6  Social rented sector waiting lists by new local government district  2021-22
ni_waiting_raw <- read_ods(tf, sheet = "T3_6", range = "A3:B14")

ni_waiting <- 
  ni_waiting_raw |> 
  rename(
    ltla21_name = New.local.government.district,
    `Households on housing waiting list` = X2021.22
  ) |> 
  mutate(ltla21_name = str_replace(ltla21_name, "&", "and"))

# Rates
ni_waiting <- 
  ni_waiting |> 
  left_join(ni_households) |> 
  mutate(`Households on housing waiting list per 1,000` = `Households on housing waiting list` / `Number of households` * 1000) |> 
  select(ltla21_name, `Households on housing waiting list per 1,000`)

## Social housing stock ----
#!! Couldn't find this data

## Combine datasets and calculate deciles ----
ni_lads <- 
  geographr::boundaries_ltla21 |> 
  sf::st_drop_geometry() |> 
  filter(str_detect(ltla21_code, "^N"))

ni <- 
  ni_homelessness |> 
  left_join(ni_waiting) |> 
  
  left_join(ni_lads) |> 
  
  mutate(
    # stock_quintile = as.integer(Hmisc::cut2(`Housing stock`, g = 5)),
    waiting_quintile = as.integer(Hmisc::cut2(`Households on housing waiting list per 1,000`, g = 5)),
    homeless_quintile = as.integer(Hmisc::cut2(`Homeless or threatened with homelessness per 1,000`, g = 5)),
    # temp_accom_quintile = as.integer(Hmisc::cut2(`Households in temporary accommodation per 1,000`, g = 5))
  ) |> 
  
  relocate(ltla21_code)

# Save
write_csv(ni, "data/housing/housing-ni.csv")
