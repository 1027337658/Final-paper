#### Preamble ####
# Purpose: Modeling of cleaned Trackinginjustice Website Data
# Author: Yuanting Han
# Date:  29 November 2024
# Contact: q2900725357@gmail.com
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded 
  # - The `rstanarm` package must be installed and loaded
  # - 03-clean_data.R must have been run
# Any other information needed?  Make sure in the  `Injustice` rproj


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

### Model data ####
 
#model for canada overall
data$race <- factor(data$race, levels = c("White","Black"))
model_canada <-
  stan_glm(
    formula = gunshot ~ age + gender + race + period,
    data = data,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 123
  )
#model for ON overall
model_ON <-
  stan_glm(
    formula = gunshot ~ age + gender + race + period,
    data = data[data$province== "ON",],
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 123
  )

#model for Toronto Police Service
model_Toronto <-
  stan_glm(
    formula = gunshot ~ age + gender + race + period,
    data = data[data$police == "Toronto Police Service",],
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 123
  )


#### Save model ####
saveRDS(
  model_canada, 
  file = "models/model_canada.rds"
)

saveRDS(
  model_ON, 
  file = "models/model_ON.rds"
)

saveRDS(
  model_Toronto, 
  file = "models/model_Toronto.rds"
)

