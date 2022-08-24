library(tidyverse)
library(readxl)
library(httr)

GET(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/14354ct210001/ct210001v2.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

census <- read_excel(tf, sheet = "CT21_0001", skip = 9)

lookup_lad_cell <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv") %>% 
  distinct(lad_code = LAD19CD, TacticalCell)

census <- 
  census %>% 
  filter(str_detect(Code, "[EWK][0-9]+")) %>% 
  
  left_join(lookup_lad_cell, by = c("Code" = "lad_code")) %>% 
  
  # Fill in missing LAs
  mutate(TacticalCell = case_when(
    str_detect(Area, "Northamptonshire") ~ "Central",
    str_detect(Area, "Buckinghamshire") ~ "South and the Channel Islands",
    TRUE ~ TacticalCell
  ))

census <- 
  census %>% 
  mutate(Ukraine = as.integer(Ukraine))
