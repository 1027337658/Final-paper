#### Preamble ####
# Purpose: Cleanning raw Trackinginjustice Website Data and store in a parquet file format
# Author: Yuanting Han
# Date:  29 November 2024
# Contact: q2900725357@gmail.com
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded 
  # - The `readxl` package must be installed and loaded
  # - The `arrow` package must be installed and loaded
  # - The `tidyr` package must be installed and loaded
  # - 02-download_data.R must have been run
# Any other information needed?  Make sure in the  `Injustice` rproj

#### Workspace setup ####
library(tidyverse)
library(tidyr)
library(readxl)
library(arrow)
#### Clean data ####
raw_data <- read_excel("data/01-raw_data/Trackinginjustice Website Data Sheet.xlsx", na = "Unknown")

cleaned_data <-
  raw_data |>
  select(AGE, GENDER, DATE, RACE, PROVINCE, `HIGHEST LEVEL FORCE`,`POLICE SERVICE`) |>
  rename(
    age = AGE,
    gender = GENDER,
    date = DATE,
    race = RACE,
    province = PROVINCE,
    highest    = `HIGHEST LEVEL FORCE`,
    police = `POLICE SERVICE`
  ) |>
  mutate(gunshot = ifelse(highest == "Gunshot", 1, 0),
         race = 
           case_when(
             race ==  'Black' ~  'Black',
             race ==  'White' ~ 'White'
           ),
         gender = ifelse(gender == "Man", 1, 0),
         date = as.Date(date, format = "%d/%m/%Y"),
         year = year(date),
         period = 
           case_when(
             year %in% 2000:2005 ~  '2000-2005',
             year %in% 2006:2010 ~  '2006-2010',
             year %in% 2011:2015 ~  '2011-2015',
             year %in% 2016:2020 ~  '2016-2020',
             year %in% 2021:2025 ~  '2021-2025'
           ),
         prname = 
           case_when(
             province == "AB" ~ "Alberta",
             province == "BC" ~ "British Columbia",
             province == "MB" ~ "Manitoba",
             province == "NB" ~ "New Brunswick",
             province == "NL" ~ "Newfoundland and Labrador",
             province == "NS" ~ "Nova Scotia",
             province == "ON" ~ "Ontario",
             province == "PE" ~ "Prince Edward Island",
             province == "QC" ~ "Quebec",
             province == "SK" ~ "Saskatchewan"
           )
  ) |> 
  select(-date, -highest) |>
  drop_na()


#### Save data ####
write_parquet(cleaned_data, "data/02-analysis_data/analysis_data.parquet")
