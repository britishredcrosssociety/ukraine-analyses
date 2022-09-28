library(tidyverse)
library(geographr)
library(readxl)
library(httr)
library(sf)
library(viridis)
library(ggsflabel)

# ---- Under-doctored areas in England ----
# Load and geocode GP practices in England
# Source: https://digital.nhs.uk/services/organisation-data-service/file-downloads/gp-and-gp-practice-related-data
GET(
  "https://files.digital.nhs.uk/assets/ods/current/epraccur.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

gp_locations <- 
  read_csv(
    file.path(tempdir(), "epraccur.csv"), 
    col_select = c(1, 2, 10),
    col_names = FALSE
  ) |> 
  rename(
    gp_code = X1,
    gp_name = X2,
    postcode = X10
  )

# Geocode
gp_locations <- 
  gp_locations |> 
  mutate(postcode = str_remove(postcode, " ")) |> 
  left_join(
    geographr::lookup_postcode_oa11_lsoa11_msoa11_ltla20 |> select(-oa11_code)
  ) |> 
  select(-postcode)

# - Calculate patients per GP in English Local Authorities -
# Load GP workforce and patient list data
# Latest available (as of 17 August 2022) = June 2022: https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/30-june-2022
GET(
  "https://files.digital.nhs.uk/DE/36D2B7/GPWPracticeCSV.062022.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

gp <- read_csv(file.path(tempdir(), "General Practice â€“ June 2022 Practice Level.csv"))

# Tidy up the data
gp <- 
  gp |> 
  select(
    gp_code = PRAC_CODE,
    total_patients = TOTAL_PATIENTS,
    gp_fte = TOTAL_GP_FTE  # Total GPs Full Time Equivalents
  ) |> 
  
  mutate(gp_fte = as.double(gp_fte)) |> 
  
  # Practices where gp_fte is zero or NA have missing data, so exclude them
  filter(gp_fte > 0)

# Aggregate into Local Authorities
gp_lad <- 
  gp |> 
  left_join(gp_locations) |> 
  
  group_by(ltla20_code) |> 
  summarise(
    total_patients = sum(total_patients, na.rm = TRUE),
    total_gp_fte = sum(gp_fte, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(patients_per_gp = total_patients / total_gp_fte)

# Calculate deciles
gp_lad <- 
  gp_lad |> 
  mutate(underdoctored_decile = as.integer(Hmisc::cut2(patients_per_gp, g = 10)))

# ---- Under-doctored areas in Scotland ----
# Source: https://publichealthscotland.scot/publications/general-practice-gp-workforce-and-practice-list-sizes/general-practice-gp-workforce-and-practice-list-sizes-2011-2021/

# Number of GPs by Local Authority
GET(
  "https://publichealthscotland.scot/media/10696/table3_number_of_gps_localauthority_by_designation_sex_2011_2021.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)
gp_scotland <- read_excel(tf, sheet = "Data", skip = 2)

# Number of patients
GET(
  "https://publichealthscotland.scot/media/10698/table5_practice_listsizes_by_localauthority_age_2011_2021.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

gp_patients_scotland <- read_excel(tf, sheet = "Data", skip = 2)

# Wrangle data
gp_scotland <- 
  gp_scotland |> 
  filter(Gender == "All" & Designation == "All GPs") |> 
  select(
    ltla21_name = `Local Authority`, 
    total_gp = `2021`
  )

gp_patients_scotland <- 
  gp_patients_scotland |> 
  filter(Year == max(Year) & `Practice type` == "All") |> 
  select(
    ltla21_name = `Local Authority`,
    total_patients = `All ages`
  )

# Calculate patients per GP
gp_scotland <- 
  gp_scotland |> 
  left_join(gp_patients_scotland) |> 
  mutate(patients_per_gp = total_patients / total_gp)

gp_scotland <- 
  gp_scotland |> 
  filter(ltla21_name != "Scotland")

# Lookup LA codes
lad_codes_scotland <- 
  geographr::boundaries_ltla21 |> 
  st_drop_geometry() |> 
  filter(str_detect(ltla21_code, "^S"))

gp_scotland <- 
  gp_scotland |> 
  left_join(lad_codes_scotland)

# Calculate deciles
gp_scotland <- 
  gp_scotland |> 
  mutate(underdoctored_decile = as.integer(Hmisc::cut2(patients_per_gp, g = 10)))

# ---- Under-doctored areas in Wales ----
# source("https://github.com/matthewgthomas/brclib/raw/master/R/download_wales.R")

# Patient registrations from Stats Wales
# Source: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/General-Medical-Services/General-practice-population
# gp_patients_wales <- download_wales("http://open.statswales.gov.wales/en-gb/dataset/hlth0426")

# GP Practice Analysis 2022
# Source: https://nwssp.nhs.wales/ourservices/primary-care-services/general-information/data-and-publications/prescribing-data-extracts/gp-practice-analysis/
GET(
  "https://nwssp.nhs.wales/ourservices/primary-care-services/primary-care-services-documents/gp-practice-analysis-docs/gp-practice-analysis-2022",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

gp_wales <- read_excel(tf)

# Geocode
gp_wales <- 
 gp_wales |> 
  mutate(postcode = str_remove(Postcode, " ")) |> 
  left_join(
    geographr::lookup_postcode_oa11_lsoa11_msoa11_ltla20 |> select(-oa11_code)
  ) |> 
  select(-postcode)

# Aggregate into Local Authorities
gp_wales <- 
  gp_wales |> 
  group_by(ltla20_code) |> 
  summarise(
    total_patients = sum(`Total Number of Patients (Including Temporary Residents)`, na.rm = TRUE),
    total_gp = sum(CurrentRegisteredGPsInPractice, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(patients_per_gp = total_patients / total_gp)

# Calculate deciles
gp_wales <- 
  gp_wales |> 
  mutate(underdoctored_decile = as.integer(Hmisc::cut2(patients_per_gp, g = 10)))

# ---- Under-doctored areas in Northern Ireland ----
# Source: https://www.gov.uk/government/statistics/family-practitioner-services-general-medical-services-statistics-for-northern-ireland-202122
# Download General Medical Statistics for NI - Tables 2021-22
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1085792/FPS_Annual_General_Medical_Services_for_NI_Tables_2021-22.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

# Load number of GPs per Local Authority
# Table 1.2b: GPs by Gender, Age Group and Local Government District by year
gp_ni <- read_excel(tf, sheet = "1.2b", range = "A108:V119")

# Load number of registered patients per Local Authority
# Table 1.1b: Registered Patients by Gender, Age Group and Local Government District by year
gp_patients_ni <- read_excel(tf, sheet = "1.1b", range = "A108:Y119")

# Wrangle data
gp_ni <- 
  gp_ni |> 
  select(
    ltla21_name = `2022`, 
    total_gp = `All GPs Total`
  )

gp_patients_ni <- 
  gp_patients_ni |> 
  select(
    ltla21_name = `2022`, 
    total_patients = `All Persons Total`
  )

# Calculate patients per GP
gp_ni <- 
  gp_ni |> 
  left_join(gp_patients_ni) |> 
  mutate(patients_per_gp = total_patients / total_gp)

# Calculate deciles
gp_ni <- 
  gp_ni |> 
  mutate(underdoctored_decile = as.integer(Hmisc::cut2(patients_per_gp, g = 10)))

# Lookup LA codes
lad_codes_ni <- 
  geographr::boundaries_ltla21 |> 
  st_drop_geometry() |> 
  filter(str_detect(ltla21_code, "^N"))

gp_ni <- 
  gp_ni |> 
  left_join(lad_codes_ni)

# ---- UK-wide under-doctored areas tibble ----
lad_names <- 
  boundaries_ltla21 |> 
  st_drop_geometry()

# Make UK-wide tibble with GP data
gp_uk <- 
  bind_rows(
    gp_lad |> select(ltla21_code = ltla20_code, patients_per_gp, underdoctored_decile),
    gp_wales |> select(ltla21_code = ltla20_code, patients_per_gp, underdoctored_decile),
    gp_scotland |> select(ltla21_code, patients_per_gp, underdoctored_decile),
    gp_ni |> select(ltla21_code, patients_per_gp, underdoctored_decile)
  ) |> 
  
  # Lookup regions
  left_join(lad_names) |> 
  left_join(lookup_ltla21_region21 |> select(ltla21_code, region21_name)) |> 
  mutate(region = case_when(
    str_detect(ltla21_code, "^W") ~ "Wales",
    str_detect(ltla21_code, "^S") ~ "Scotland",
    str_detect(ltla21_code, "^N") ~ "Northern Ireland",
    TRUE ~ region21_name
  )) |> 
  select(ltla21_code, ltla21_name, region, patients_per_gp, underdoctored_decile)

write_csv(gp_uk, "output-data/under-doctored-areas/patients-per-gp-local-authorities.csv")

# ---- Combine with Ukraine visa data ----
source("R/load Ukraine visa data - Local Authorities.R")

gp_uk <- read_csv("output-data/under-doctored-areas/patients-per-gp-local-authorities.csv")

gp_visa_lad <- 
  gp_uk |> 
  left_join(
    visas_ltla21_uk |> filter(Date == max(Date)) |> select(ltla21_code, starts_with("Number")),
    by = "ltla21_code"
  )

# Any relationships between number of arrivals from Ukraine and under-doctored areas?
gp_visa_lad |> 
  filter(!is.na(region)) |> 
  ggplot(aes(x = patients_per_gp, y = `Number of arrivals`)) +
  geom_point(aes(colour = region)) +
  geom_smooth(aes(colour = region, fill = region), method = "lm") +
  facet_wrap(~region, scales = "free") +
  theme_classic() +
  labs(
    title = "People arriving from Ukraine mostly do not live in under-doctored areas",
    x = "Number of patients per GP",
    y = "Number of arrivals from Ukraine",
    source = "British Red Cross analysis of DLUHC, NHS Digital, NHS Wales, Public Health Scotland, and NISRA data"
  )

ggsave("images/under-doctored areas and Ukraine arrivals.png", width = 230, height = 150, units = "mm")

# Filter underdoctored areas
# gp_visa_lad <- 
#   gp_visa_lad |> 
#   mutate(underdoctored_decile = as.integer(Hmisc::cut2(patients_per_gp, g = 10)))

underdoctored_lads <- 
  gp_visa_lad |> 
  filter(underdoctored_decile >= 9)

# What regions are the under-doctored areas mostly in?
underdoctored_lads |> 
  count(region, sort = TRUE)

# How many (and what proportion of) Ukrainian arrivals live in under-doctored areas?
sum(underdoctored_lads$`Number of arrivals`)
scales::percent(sum(underdoctored_lads$`Number of arrivals`) / sum(visas_ltla21_uk$`Number of arrivals`, na.rm = TRUE), accuracy = 0.1)

# Wales figures
underdoctored_lads_wales <- underdoctored_lads |> filter(str_detect(ltla21_code, "W"))
scales::percent(sum(underdoctored_lads_wales$`Number of arrivals`) / sum(visas_ltla21_wales$`Number of arrivals`, na.rm = TRUE), accuracy = 0.1)

# List LAs
underdoctored_lads |> 
  select(region, ltla21_name) |> 
  arrange(region, ltla21_name) |> 
  write_csv("output-data/under-doctored-areas/underdoctored-areas.csv")

# ---- Map under-doctored areas, shaded by number of arrivals from Ukraine ----
geographr::boundaries_ltla21 |> 
  filter(str_detect(ltla21_code, "^W")) |> 
  left_join(underdoctored_lads, by = "ltla21_code") |> 
  
  ggplot() +
  geom_sf(
    aes(fill = `Number of arrivals`),
    colour = "grey80"
  ) +
  # geom_sf_label_repel(
  #   aes(label = `Lower tier local authority name`),
  #   # nudge_y = .25,
  #   # direction = "both",
  #   force_pull = -2,
  #   force = 3,
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
    title = "Under-doctored areas and arrivals from Ukraine",
    subtitle = str_wrap("Showing where people arriving on the Ukraine visa schemes are living in under-doctored areas (Local Authorities with the highest number of patients per full-time equivalent GP). Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people).", 100),
    caption = "Source: British Red Cross analysis of DLUHC, NHS Digital, NHS Wales, Public Health Scotland, and NISRA data"
  )

ggsave("images/under-doctored areas.png", width = 180, height = 200, units = "mm")
ggsave("images/under-doctored areas - Wales.png", width = 180, height = 150, units = "mm")
