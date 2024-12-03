#### Preamble ####
# Purpose: Simulate Trackinginjustice Website Data 
# Author: Yuanting Han
# Date:  29 November 2024
# Contact: q2900725357@gmail.com
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded 
# Any other information needed?  Make sure in the `Injustice` rproj


#### Workspace setup ####
library(tidyverse)
set.seed(123)


#### Simulate data ####
# provinces names
provinces <- c('Ontario','Newfoundland and Labrador',
               'British Columbia',
               'Saskatchewan','Quebec','Alberta',
               'New Brunswick',
               'Nova Scotia','Manitoba',
               'Prince Edward Island')

provinces_abb <- c('ON','NL','BC','SK','QC','AB','NB','NS','MB','PE')

polices <- c('Toronto Police Service','RCMP','Ottawa Police Service','Royal Newfoundland Constabulary','Vancouver Police Department',
             'Amherstburg Police Service','Hamilton Police Service','OPP','South Simcoe Police',
             'Regina Police Service','Service de police de la Ville de Montreal','Port Moody Police Department','Calgary Police Service',
             'Service de police de la Ville de Gatineau','Saskatoon Police Service','London Police Service','Peel Regional Police',
             'Niagara Regional Police Service','Surete du Quebec','York Regional Police','Edmonton Police Service','Winnipeg Police Service',
             'Brantford Police Service','Windsor Police Service','New Westminster Police Service, RCMP','Greater Sudbury Police Service',
             'Service de police de la Ville de Quebec','Waterloo Regional Police Service','Halton Regional Police Service',
             'Kawartha Lakes Police Service','Barrie Police Services','Service de police de la Ville de Laval','RCMP, Surete du Quebec',
             'Lake Shore Regional Police Service, RCMP','Peterborough Lakefield Community Police Force','Service de securite publique de Saguenay',
             'Saugeen Shores Police Service','Durham Regional Police Service','Regie intermunicipale de police de Saint-Jean-sur-Richelieu',
             'Victoria Police Department','Bathurst Police Force','Peterborough Police Service','Guelph Police Service',
             'Regie intermunicipale de police Roussillon','Regie intermunicipale de police du Lac-des-Deux-Montagnes',
             'Securite publique de Saguenay','Service de police de la Ville de Levis, Surete du Quebec, Service de police de la Ville de Quebec',
             'Cobourg Police Service','North Bay Police','Summerside Police Department','Kingston Police Services',
             'Service de police de la Ville de Repentigny','Halton Regional Police Services, Hamilton Police Service','Waterloo Regional Police')
# Create a dataset by randomly assigning provinces to observed cases
analysis_data <- tibble(
  age = round(rnorm(301, 36.8, 12.3)),
  gender = sample(c(1,0), size = 301, replace = TRUE, prob = c(0.5, 0.5)),
  race = sample(c("Black", "White"), size = 301, replace = TRUE, prob = c(0.2, 0.8)),
  gunshot = sample(c(1,0), size = 301, replace = TRUE, prob = c(0.7, 0.3)),
  province = sample(provinces_abb, size = 301, replace = TRUE, prob = c(0.25, 0.05, 0.20, 0.05, 0.15, 0.10, 0.05, 0.05, 0.05, 0.05)), # Rough province case number distribution
  year = sample(c(2000:2024), size = 301, replace = TRUE), #equal distribution
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
    ),
  period = 
    case_when(
      year %in% 2000:2005 ~  '2000-2005',
      year %in% 2006:2010 ~  '2006-2010',
      year %in% 2011:2015 ~  '2011-2015',
      year %in% 2016:2020 ~  '2016-2020',
      year %in% 2021:2025 ~  '2021-2025'
    ),
  police = sample(polices, size = 301, replace = TRUE) #just simulate, hard to align with province
)
analysis_data <- analysis_data |> 
  select(age, gender, race, province, police, gunshot, year, period, prname)    

#### Save data ####
write_csv(analysis_data, "data/00-simulated_data/simulated_data.csv")
