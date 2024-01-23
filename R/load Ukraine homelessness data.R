library(tidyverse)
library(readODS)
library(httr)

# source("R/load Ukraine visa data - Local Authorities.R")

# ---- Load latest homelessness management info (24 February to 31 Dec 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/media/65a9373694c997000daeba2d/Ukraine_Homelessness_Pressures_Publication_December_2023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_31dec_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_31dec_raw <- homelessness_24feb_31dec_raw[-c(8, 10, 18)]

names(homelessness_24feb_31dec_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_31dec_raw <- 
  homelessness_24feb_31dec_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_31dec_total <- 
  homelessness_24feb_31dec_raw |> 
  slice(3)

homelessness_24feb_31dec <- 
  homelessness_24feb_31dec_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 30 Nov 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/media/657aed5b254aaa000d050d3b/Ukraine_Homelessness_Pressures_Publication_November_2023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_30nov_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_30nov_raw <- homelessness_24feb_30nov_raw[-c(8, 10, 18)]

names(homelessness_24feb_30nov_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_30nov_raw <- 
  homelessness_24feb_30nov_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_30nov_total <- 
  homelessness_24feb_30nov_raw |> 
  slice(3)

homelessness_24feb_30nov <- 
  homelessness_24feb_30nov_raw |> 
  filter(str_detect(lad_code, "^E"))


# ---- Load latest homelessness management info (24 February to 31 Oct 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/media/6553904950475b0013c5b5bb/Ukraine_Homelessness_Pressures_Publication_October_2023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_31oct_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_31oct_raw <- homelessness_24feb_31oct_raw[-c(8, 10, 18)]

names(homelessness_24feb_31oct_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_31oct_raw <- 
  homelessness_24feb_31oct_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_31oct_total <- 
  homelessness_24feb_31oct_raw |> 
  slice(3)

homelessness_24feb_31oct <- 
  homelessness_24feb_31oct_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 30 Sept 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/media/652ff0cc92895c0010dcb9d8/Ukraine_Homelessness_Pressures_Publication_September_2023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_30sep_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_30sep_raw <- homelessness_24feb_30sep_raw[-c(8, 10, 18)]

names(homelessness_24feb_30sep_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_30sep_raw <- 
  homelessness_24feb_30sep_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_30sep_total <- 
  homelessness_24feb_30sep_raw |> 
  slice(3)

homelessness_24feb_30sep <- 
  homelessness_24feb_30sep_raw |> 
  filter(str_detect(lad_code, "^E"))


# ---- Load latest homelessness management info (24 February to 31 August 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/media/650bf9ed52e73c00139425a8/Ukraine_Homelessness_Pressures_Publication_August_2023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_31aug_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_31aug_raw <- homelessness_24feb_31aug_raw[-c(8, 10, 18)]

names(homelessness_24feb_31aug_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_31aug_raw <- 
  homelessness_24feb_31aug_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_31aug_total <- 
  homelessness_24feb_31aug_raw |> 
  slice(3)

homelessness_24feb_31aug <- 
  homelessness_24feb_31aug_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 31 July 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1178226/Ukraine_Homelessness_Pressures_Publication_July_2023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_31jul_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_31jul_raw <- homelessness_24feb_31jul_raw[-c(8, 10, 18)]

names(homelessness_24feb_31jul_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_31jul_raw <- 
  homelessness_24feb_31jul_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_31jul_total <- 
  homelessness_24feb_31jul_raw |> 
  slice(3)

homelessness_24feb_31jul <- 
  homelessness_24feb_31jul_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 16 June 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1166112/Ukraine_Homelessness_Pressures_Publication_16062023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_16jun_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_16jun_raw <- homelessness_24feb_16jun_raw[-c(3:6, 12, 14, 22)]

names(homelessness_24feb_16jun_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_16jun_raw <- 
  homelessness_24feb_16jun_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_16jun_total <- 
  homelessness_24feb_16jun_raw |> 
  slice(3)

homelessness_24feb_16jun <- 
  homelessness_24feb_16jun_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 19 May 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1159972/Ukraine_Homelessness_Pressures_Publication_19052023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_19may_raw <- read_ods(tf, skip = 3, sheet = "Publication_")

# Remove empty columns
homelessness_24feb_19may_raw <- homelessness_24feb_19may_raw[-c(3:6, 12, 14, 22)]

names(homelessness_24feb_19may_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_19may_raw <- 
  homelessness_24feb_19may_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_19may_total <- 
  homelessness_24feb_19may_raw |> 
  slice(3)

homelessness_24feb_19may <- 
  homelessness_24feb_19may_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 21 April 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1154505/Ukraine_Homelessness_Pressures_Publication_21042023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_21apr_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_21apr_raw <- homelessness_24feb_21apr_raw[-c(3, 4, 10, 12, 20)]

names(homelessness_24feb_21apr_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  # "Homelessness Prevented or Relieved via Mediation",
  # "Homelessness Prevented or Relieved via Rematch",
  # "Homelessness Prevented or Relieved via Other",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_21apr_raw <- 
  homelessness_24feb_21apr_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_21apr_total <- 
  homelessness_24feb_21apr_raw |> 
  slice(3)

homelessness_24feb_21apr <- 
  homelessness_24feb_21apr_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 24 March 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1149152/Ukraine_Homelessness_Pressures_06042023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_24mar_raw <- read_ods(tf, skip = 3, sheet = "Publication")

# Remove empty columns
homelessness_24feb_24mar_raw <- homelessness_24feb_24mar_raw[-c(3, 4, 10, 12, 20)]

names(homelessness_24feb_24mar_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Prevention and relief",
  # "Homelessness Prevented or Relieved via Mediation",
  # "Homelessness Prevented or Relieved via Rematch",
  # "Homelessness Prevented or Relieved via Other",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_24mar_raw <- 
  homelessness_24feb_24mar_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_24mar_total <- 
  homelessness_24feb_24mar_raw |> 
  slice(3)

homelessness_24feb_24mar <- 
  homelessness_24feb_24mar_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 24 February 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1141192/Ukraine_Homelessness_Pressures_09032023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_24feb_raw <- read_ods(tf, skip = 3, sheet = "Ukrainian_Homelessness")

# Remove empty columns
homelessness_24feb_24feb_raw <- homelessness_24feb_24feb_raw[-c(2, 3, 10, 12, 20)]

names(homelessness_24feb_24feb_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  "Homelessness prevented or relieved",
  # "Homelessness Prevented or Relieved via Mediation",
  # "Homelessness Prevented or Relieved via Rematch",
  # "Homelessness Prevented or Relieved via Other",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_24feb_raw <- 
  homelessness_24feb_24feb_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_24feb_total <- 
  homelessness_24feb_24feb_raw |> 
  slice(3)

homelessness_24feb_24feb <- 
  homelessness_24feb_24feb_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 27 January 2023) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1135405/Ukraine_Homelessness_Pressures_09022023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_27jan_raw <- read_ods(tf, skip = 3, sheet = "Ukrainian_Homelessness")

# Remove empty columns
homelessness_24feb_27jan_raw <- homelessness_24feb_27jan_raw[-c(3, 4, 10, 11, 19)]

names(homelessness_24feb_27jan_raw) <- c(
  "lad_code",
  "lad_name",
  "Total Ukrainian households owed a prevention or relief duty",
  "Single household (total)",
  "Single household (%)",
  "Household with dependent children (total)",
  "Household with dependent children (%)",
  # "Homelessness Prevented or Relieved via Mediation",
  # "Homelessness Prevented or Relieved via Rematch",
  # "Homelessness Prevented or Relieved via Other",
  "Family Scheme: Accommodation or arrangement broken down",
  "Family Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Accommodation or arrangement broken down",
  "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival",
  "Homes for Ukraine Scheme: Rejected sponsors offer",
  "Extension Scheme",
  "Other/Not Known",
  "Temporary Accommodation Snapshot",
  "Offer of Settled Accomodation"
)

homelessness_24feb_27jan_raw <- 
  homelessness_24feb_27jan_raw |> 
  as_tibble() |> 
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_27jan_total <- 
  homelessness_24feb_27jan_raw |> 
  slice(3)

homelessness_24feb_27jan <- 
  homelessness_24feb_27jan_raw |> 
  filter(str_detect(lad_code, "^E"))

# ---- Load latest homelessness management info (24 February to 30 December 2022) ----
# Source: https://www.gov.uk/government/publications/homelessness-management-information-ukrainian-nationals-england
GET(
  "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1141190/Ukraine_Homelessness_Pressures_10012023.ods",
  write_disk(tf <- tempfile(fileext = ".ods"))
)

homelessness_24feb_30dec_raw <- read_ods(tf, skip = 3, sheet = "Ukrainian_Homelessness")

# Remove empty columns
homelessness_24feb_30dec_raw <- homelessness_24feb_30dec_raw[-c(2, 3, 5, 6, 12, 16, 23)]

names(homelessness_24feb_30dec_raw) <- c(
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

homelessness_24feb_30dec_raw <-
  homelessness_24feb_30dec_raw |>
  as_tibble() |>
  mutate(across(-(lad_code:lad_name), as.numeric))

homelessness_24feb_30dec_total <-
  homelessness_24feb_30dec_raw |>
  slice(3)

homelessness_24feb_30dec <-
  homelessness_24feb_30dec_raw |>
  filter(str_detect(lad_code, "^E"))

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
    homelessness_24feb_31dec_total |> mutate(Date = ymd("2023-12-31"), Date_text = "31 December"),
    homelessness_24feb_30nov_total |> mutate(Date = ymd("2023-11-30"), Date_text = "30 November"),
    homelessness_24feb_31oct_total |> mutate(Date = ymd("2023-10-31"), Date_text = "31 October"),
    homelessness_24feb_30sep_total |> mutate(Date = ymd("2023-09-30"), Date_text = "30 September"),
    homelessness_24feb_31aug_total |> mutate(Date = ymd("2023-08-31"), Date_text = "31 August"),
    homelessness_24feb_31jul_total |> mutate(Date = ymd("2023-07-31"), Date_text = "31 July"),
    homelessness_24feb_16jun_total |> mutate(Date = ymd("2023-06-16"), Date_text = "16 June"),
    homelessness_24feb_19may_total |> mutate(Date = ymd("2023-05-19"), Date_text = "19 May"),
    homelessness_24feb_21apr_total |> mutate(Date = ymd("2023-04-21"), Date_text = "21 April"),
    homelessness_24feb_24mar_total |> mutate(Date = ymd("2023-03-24"), Date_text = "24 March"),
    homelessness_24feb_24feb_total |> mutate(Date = ymd("2023-02-24"), Date_text = "24 February"),
    homelessness_24feb_27jan_total |> mutate(Date = ymd("2023-01-27"), Date_text = "27 January"),
    homelessness_24feb_30dec_total |> mutate(Date = ymd("2022-12-30"), Date_text = "30 December"),
    homelessness_24feb_18nov_total |> mutate(Date = ymd("2022-11-18"), Date_text = "18 November"),
    homelessness_24feb_21oct_total |> mutate(Date = ymd("2022-10-21"), Date_text = "21 October"),
    homelessness_24feb_23sep_total |> mutate(Date = ymd("2022-09-23"), Date_text = "23 September"),
    homelessness_24feb_26aug_total |> mutate(Date = ymd("2022-08-26"), Date_text = "26 August"),
    homelessness_24feb_29jul_total |> mutate(Date = ymd("2022-07-29"), Date_text = "29 July"),
    homelessness_24feb_1jul_total  |> mutate(Date = ymd("2022-07-01"), Date_text = "1 July"),
    homelessness_24feb_3jun_total  |> mutate(Date = ymd("2022-06-03"), Date_text = "3 June")
  ) |> 
  relocate(Date) |> 
  mutate(Date_text = factor(Date_text, levels = c("3 June", "1 July", "29 July", "26 August", "23 September", "21 October", "18 November", "30 December", "27 January", "24 February", "24 March", "21 April", "19 May", "16 June", "31 July", "31 August", "30 September", "31 October", "30 November", "31 December")))

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
#! Households are not directly comparable to individuals, so stop doing this

# Wrangle number of arrivals for each date - using nearest available arrivals figures

# Find most recent dates nearest to (but after) the homelessness data
# visas_ltla21_england |> 
#   arrange(Date) |> 
#   distinct(Date) |> 
#   print(n = 60)
# 
# visas_28mar <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2023-03-28")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_28feb <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2023-02-28")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_31jan <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2023-01-31")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_3jan <-
#   visas_ltla21_england |>
#   filter(Date == ymd("2023-01-03")) |>
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |>
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_22nov <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2022-11-22")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_25oct <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2022-10-25")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_27sep <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2022-09-27")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_30aug <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2022-08-30")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_2aug <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2022-08-02")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_5jul <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2022-07-05")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# visas_7jun <- 
#   visas_ltla21_england |> 
#   filter(Date == ymd("2022-06-07")) |> 
#   mutate(`Number of arrivals` = as.integer(str_remove(`Number of arrivals`, "<")) - 1) |> 
#   select(ltla21_code, `Number of arrivals`)
# 
# # Calculate proportions
# homelessness_24feb_24mar <- 
#   homelessness_24feb_24mar |> 
#   left_join(visas_28mar, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_24feb <- 
#   homelessness_24feb_24feb |> 
#   left_join(visas_28feb, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_27jan <- 
#   homelessness_24feb_27jan |> 
#   left_join(visas_31jan, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_30dec <-
#   homelessness_24feb_30dec |>
#   left_join(visas_3jan, by = c("lad_code" = "ltla21_code")) |>
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_18nov <- 
#   homelessness_24feb_18nov |> 
#   left_join(visas_22nov, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_21oct <- 
#   homelessness_24feb_21oct |> 
#   left_join(visas_25oct, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_23sep <- 
#   homelessness_24feb_23sep |> 
#   left_join(visas_27sep, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_26aug <- 
#   homelessness_24feb_26aug |> 
#   left_join(visas_30aug, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_29jul <- 
#   homelessness_24feb_29jul |> 
#   left_join(visas_2aug, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_1jul <- 
#   homelessness_24feb_1jul |> 
#   left_join(visas_5jul, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )
# 
# homelessness_24feb_3jun <- 
#   homelessness_24feb_3jun |> 
#   left_join(visas_7jun, by = c("lad_code" = "ltla21_code")) |> 
#   mutate(
#     `% at risk of homelessness` = `Total Ukrainian households owed a prevention or relief duty` / `Number of arrivals`,
#     `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Number of arrivals`
#   )

# ---- Make single tibble of longitudinal homelessness data ---- 
homelessness_trends <- 
  left_join(
    homelessness_24feb_3jun |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (7 June)` = `Number of arrivals`,
        total_3jun = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_3jun = `% at risk of homelessness`,
        temp_3jun = `Temporary Accommodation Snapshot`, 
        # temp_percent_3jun = `% in temporary accommodation`
      ),
    
    homelessness_24feb_1jul |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (5 July)` = `Number of arrivals`,
        total_1jul = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_1jul = `% at risk of homelessness`,
        temp_1jul = `Temporary Accommodation Snapshot`, 
        # temp_percent_1jul = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_29jul |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (2 August)` = `Number of arrivals`,
        total_29jul = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_29jul = `% at risk of homelessness`,
        temp_29jul = `Temporary Accommodation Snapshot`, 
        # temp_percent_29jul = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_26aug |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (30 August)` = `Number of arrivals`,
        total_26aug = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_26aug = `% at risk of homelessness`,
        temp_26aug = `Temporary Accommodation Snapshot`, 
        # temp_percent_26aug = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_23sep |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (27 September)` = `Number of arrivals`,
        total_23sep = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_23sep = `% at risk of homelessness`,
        temp_23sep = `Temporary Accommodation Snapshot`, 
        # temp_percent_23sep = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_21oct |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (25 October)` = `Number of arrivals`,
        total_21oct = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_21oct = `% at risk of homelessness`,
        temp_21oct = `Temporary Accommodation Snapshot`, 
        # temp_percent_21oct = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_18nov |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (22 November)` = `Number of arrivals`,
        total_18nov = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_18nov = `% at risk of homelessness`,
        temp_18nov = `Temporary Accommodation Snapshot`, 
        # temp_percent_18nov = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_30dec |>
      select(
        lad_code,
        lad_name,
        # `Number of arrivals (3 January)` = `Number of arrivals`,
        total_30dec = `Total Ukrainian households owed a prevention or relief duty`,
        # percent_30dec = `% at risk of homelessness`,
        temp_30dec = `Temporary Accommodation Snapshot`,
        # temp_percent_30dec = `% in temporary accommodation`
      )
  ) |>
  left_join(
    homelessness_24feb_27jan |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (31 January)` = `Number of arrivals`,
        total_27jan = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_27jan = `% at risk of homelessness`,
        temp_27jan = `Temporary Accommodation Snapshot`, 
        # temp_percent_27jan = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_24feb |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (28 February)` = `Number of arrivals`,
        total_24feb = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_24feb = `% at risk of homelessness`,
        temp_24feb = `Temporary Accommodation Snapshot`, 
        # temp_percent_24feb = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_24mar |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (28 March)` = `Number of arrivals`,
        total_24mar = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_24mar = `% at risk of homelessness`,
        temp_24mar = `Temporary Accommodation Snapshot`, 
        # temp_percent_24mar = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_21apr |> 
      select(
        lad_code, 
        lad_name, 
        # `Number of arrivals (28 March)` = `Number of arrivals`,
        total_21apr = `Total Ukrainian households owed a prevention or relief duty`, 
        # percent_24mar = `% at risk of homelessness`,
        temp_21apr = `Temporary Accommodation Snapshot`, 
        # temp_percent_24mar = `% in temporary accommodation`
      )
  ) |> 
  left_join(
    homelessness_24feb_19may |> 
      select(
        lad_code, 
        lad_name, 
        total_19may = `Total Ukrainian households owed a prevention or relief duty`, 
        temp_19may = `Temporary Accommodation Snapshot`
      )
  ) |> 
  left_join(
    homelessness_24feb_16jun |> 
      select(
        lad_code, 
        lad_name, 
        total_16jun = `Total Ukrainian households owed a prevention or relief duty`, 
        temp_16jun = `Temporary Accommodation Snapshot`
      )
  ) |> 
  left_join(
    homelessness_24feb_31jul |> 
      select(
        lad_code, 
        lad_name, 
        total_31jul = `Total Ukrainian households owed a prevention or relief duty`, 
        temp_31jul = `Temporary Accommodation Snapshot`
      )
  ) |> 
  left_join(
    homelessness_24feb_31aug |> 
      select(
        lad_code, 
        lad_name, 
        total_31aug = `Total Ukrainian households owed a prevention or relief duty`, 
        temp_31aug = `Temporary Accommodation Snapshot`
      ) 
  ) |>
    left_join(
      homelessness_24feb_30sep |> 
        select(
          lad_code, 
          lad_name, 
          total_30sep = `Total Ukrainian households owed a prevention or relief duty`, 
          temp_30sep = `Temporary Accommodation Snapshot`
        ) 
    ) |>
      left_join(
        homelessness_24feb_31oct |> 
          select(
            lad_code, 
            lad_name, 
            total_31oct = `Total Ukrainian households owed a prevention or relief duty`, 
            temp_31oct = `Temporary Accommodation Snapshot`
            )
      ) |>
  left_join(
    homelessness_24feb_30nov |> 
      select(
        lad_code, 
        lad_name, 
        total_30nov = `Total Ukrainian households owed a prevention or relief duty`, 
        temp_30nov = `Temporary Accommodation Snapshot`
        )
      ) |>
  left_join(
    homelessness_24feb_31dec |> 
      select(
        lad_code, 
        lad_name, 
        total_31dec = `Total Ukrainian households owed a prevention or relief duty`, 
        temp_31dec = `Temporary Accommodation Snapshot`
      )
  )

homelessness_trends <- 
  homelessness_trends |> 
  left_join(geographr::lookup_ltla21_region21, by = c("lad_code" = "ltla21_code")) |> 
  replace_na(list(
    total_3jun = 0, 
    # percent_3jun = 0, 
    temp_3jun = 0, 
    # temp_percent_3jun = 0, 
    
    total_1jul = 0, 
    # percent_1jul = 0, 
    temp_1jul = 0,
    # temp_percent_1jul = 0,
    
    total_29jul = 0, 
    # percent_29jul = 0, 
    temp_29jul = 0,
    # temp_percent_29jul = 0,
    
    total_26aug = 0, 
    # percent_26aug = 0, 
    temp_26aug = 0,
    # temp_percent_26aug = 0,
    
    total_23sep = 0, 
    # percent_23sep = 0, 
    temp_23sep = 0,
    # temp_percent_23sep = 0,
    
    total_21oct = 0, 
    # percent_21oct = 0, 
    temp_21oct = 0,
    # temp_percent_21oct = 0,
    
    total_18nov = 0, 
    # percent_18nov = 0, 
    temp_18nov = 0,
    # temp_percent_18nov = 0,
    
    total_30dec = 0,
    # percent_30dec = 0,
    temp_30dec = 0,
    # temp_percent_30dec = 0,
    
    total_27jan = 0, 
    # percent_27jan = 0, 
    temp_27jan = 0,
    # temp_percent_27jan = 0,
    
    total_24feb = 0, 
    # percent_24feb = 0, 
    temp_24feb = 0,
    # temp_percent_24feb = 0,
    
    total_24mar = 0, 
    # percent_24mar = 0, 
    temp_24mar = 0,
    # temp_percent_24mar = 0,
    
    total_21apr = 0,
    temp_21apr = 0,
    
    total_19may = 0,
    temp_19may = 0,
    
    total_16jun = 0,
    temp_16jun = 0,
    
    total_31jul = 0,
    temp_31jul = 0,
    
    total_31aug = 0,
    temp_31aug = 0,
    
    total_30sep = 0,
    temp_30sep = 0,
    
    total_31oct = 0,
    temp_31oct = 0,
    
    total_30nov = 0,
    temp_30nov = 0,
    
    total_31dec = 0,
    temp_31dec = 0
    
  )) |> 
  mutate(
    total_delta = total_31dec - total_3jun,
    temp_delta = temp_31dec - temp_3jun
  )

# ---- Save wrangled data ----
# write_csv(homelessness_24feb_3jun, "data/homelessness/ukraine-homelessness-3-june.csv")
# write_csv(homelessness_24feb_1jul, "data/homelessness/ukraine-homelessness-1-july.csv")
# write_csv(homelessness_24feb_29jul, "data/homelessness/ukraine-homelessness-29-july.csv")
# write_csv(homelessness_24feb_26aug, "data/homelessness/ukraine-homelessness-26-august.csv")
# write_csv(homelessness_24feb_23sep, "data/homelessness/ukraine-homelessness-23-september.csv")
# write_csv(homelessness_24feb_21oct, "data/homelessness/ukraine-homelessness-21-october.csv")
# write_csv(homelessness_24feb_18nov, "data/homelessness/ukraine-homelessness-18-november.csv")
# write_csv(homelessness_24feb_30dec, "data/homelessness/ukraine-homelessness-30-december.csv")
# write_csv(homelessness_24feb_27jan, "data/homelessness/ukraine-homelessness-27-january.csv")
# write_csv(homelessness_24feb_24feb, "data/homelessness/ukraine-homelessness-24-february.csv")
# write_csv(homelessness_24feb_24mar, "data/homelessness/ukraine-homelessness-24-march.csv")
# write_csv(homelessness_24feb_21apr, "data/homelessness/ukraine-homelessness-21-april.csv")
# write_csv(homelessness_24feb_19may, "data/homelessness/ukraine-homelessness-19-may.csv")
# write_csv(homelessness_24feb_16jun, "data/homelessness/ukraine-homelessness-16-june.csv")
# write_csv(homelessness_24feb_31jul, "data/homelessness/ukraine-homelessness-31-july.csv")

write_csv(homelessness_total, "data/homelessness/ukraine-homelessness-summary.csv")
write_csv(homelessness_trends, "data/homelessness/ukraine-homelessness-trends.csv")

# Save a publicly accessible copy of totals and trends
write_csv(homelessness_total, "output-data/homelessness/ukraine-homelessness-summary.csv")
write_csv(homelessness_trends, "output-data/homelessness/ukraine-homelessness-trends.csv")

