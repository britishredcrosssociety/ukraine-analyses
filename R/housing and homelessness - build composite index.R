library(tidyverse)
library(geographr)
library(GGally)

source("https://github.com/britishredcrosssociety/resilience-index/raw/main/R/utils.R")

# Load housing data ----
# Run the next line if you need to create the country-specific .csv files
# source("housing and homelessness - wrangle data.R")

england <- read_csv("data/housing/housing-england.csv")
wales <- read_csv("data/housing/housing-wales.csv")
scotland <- read_csv("data/housing/housing-scotland.csv")
ni <- read_csv("data/housing/housing-ni.csv")

# Remove quintiles
england  <- select(england, -ends_with("_quintile"))
wales    <- select(wales, -ends_with("_quintile"))
scotland <- select(scotland, -ends_with("_quintile"))
ni       <- select(ni, -ends_with("_quintile"))

# Explore correlations between indicators (within each nation) ----
england |> 
  select(-ltla21_code, -ltla21_name) |> 
  ggpairs()

wales |> 
  select(-ltla21_code) |> 
  ggpairs()

scotland |> 
  select(-ltla21_code) |> 
  ggpairs()

ni |> 
  select(-ltla21_code, -ltla21_name) |> 
  ggpairs()

# Build composite index ----
# This approach is adapated from the methods used to develop the Resilience Index,
# and broadly mirrors the approach taken to build the Index of Multiple Deprivation.
#
# For further details, see: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833951/IoD2019_Technical_Report.pdf
# 
# Steps:
#   1. Align indicators so that higher value = greater homelessness/housing pressures.
#   2. Rank the indicators.
#   3. Normalise the rank to a range between 0 and 1.
#   4. Calculate composite index score by summing normalised ranks.
#   5. Rank and quantise the scores.

## England ----
england_index <- 
  england |> 
  
  # Align indicators - Higher value = more pressured housing situation.
  mutate(
    `Social housing stock as a proportion of all households` = `Social housing stock as a proportion of all households` * -1,
    `Vacant dwellings per 1,000 units of social housing stock` = `Vacant dwellings per 1,000 units of social housing stock` * -1
  ) |> 

  # Calculate composite index
  calculate_composite_score (
    index_name = "housing_and_homelessness",
    num_quantiles = 10
  )

england <- left_join(england, england_index)

## Wales ----
wales_index <- 
  wales |> 
  
  # Align indicators - Higher value = more pressured housing situation.
  mutate(
    `Social housing stock as a proportion of all households` = `Social housing stock as a proportion of all households` * -1,
    `Vacant dwellings per 1,000 units of social housing stock` = `Vacant dwellings per 1,000 units of social housing stock` * -1
  ) |> 
  
  # Calculate composite index
  calculate_composite_score (
    index_name = "housing_and_homelessness",
    num_quantiles = 10
  )

wales <- left_join(wales, wales_index)

## Scotland ----
scotland_index <- 
  scotland |> 
  
  # Align indicators - Higher value = more pressured housing situation.
  mutate(
    `Social housing stock as a proportion of all households` = `Social housing stock as a proportion of all households` * -1,
    `Vacant dwellings per 1,000 units of social housing stock` = `Vacant dwellings per 1,000 units of social housing stock` * -1
  ) |> 
  
  # Calculate composite index
  calculate_composite_score (
    index_name = "housing_and_homelessness",
    num_quantiles = 10
  )

scotland <- left_join(scotland, scotland_index)

## Northern Ireland ----
ni_index <- 
  ni |> 
  
  # (No indicators need aligning)
  
  # Calculate composite index
  calculate_composite_score (
    index_name = "housing_and_homelessness",
    num_quantiles = 10
  )

# NI only has 11 Local Authorities, so just use ranks as the deciles
ni_index <- 
  ni_index |> 
  mutate(
    housing_and_homelessness_composite_quantiles = floor(housing_and_homelessness_composite_rank)
  )

ni <- left_join(ni, ni_index)

## Save indices ----
write_csv(england, "output-data/index-of-housing-insecurity/housing-index-england.csv")
write_csv(wales, "output-data/index-of-housing-insecurity/housing-index-wales.csv")
write_csv(scotland, "output-data/index-of-housing-insecurity/housing-index-scotland.csv")
write_csv(ni, "output-data/index-of-housing-insecurity/housing-index-ni.csv")
