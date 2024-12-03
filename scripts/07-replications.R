#### Preamble ####
# Purpose: Replication of canada maps
# Author: Yuanting Han
# Date:  29 November 2024
# Contact: q2900725357@gmail.com
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded 
  # - The `arrow` package must be installed and loaded
  # - The `canadamaps` package must be installed and loaded
  # - The `RColorBrewer` package must be installed and loaded
  # - 03-clean_data.R must have been run
  # - 00-simulated_data.R must have been run
# Any other information needed?  Make sure in the  `Injustice` rproj


#### Workspace setup ####
library(tidyverse)
library(arrow) #for read parquet
library(canadamaps) #for map
library(RColorBrewer) #for nicely colors
#### Load data ####

data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

#### Replicate of canada map
#showing Percent of Gun shot overall in Canada map
map_df  <- data |> 
  select(gunshot, prname) |>
  group_by(prname) |> 
  summarize(rate = mean(gunshot)) |> #Compute percent of Gun shot
  right_join(get_provinces(), by = "prname") |>  #get_provinces()  get canada provinces map information
  mutate(
    info = ifelse(is.na(rate), "No info.", round(rate * 100,1)), #make labels in map, contain information of province and Percent of Gun shot
    label = ifelse(is.na(rate),
                   paste(gsub(" /.*", "", prname),
                         paste0(info),
                         sep = "\n"),
                   paste(gsub(" /.*", "", prname),
                         paste0(info, "%"),
                         sep = "\n"))
  ) 

map_overall <- map_df |>  ggplot() + 
  geom_sf(aes(fill = rate * 100, geometry = geometry)) +
  geom_sf_label(aes(label = label, geometry = geometry), alpha = 0.9) +
  scale_fill_gradientn(colours = colorRampPalette(brewer.pal(6, "Set2"))(6), name = "") + #make nice colors
  labs(title = "Percent of Gun shot overall in Canada") +
  theme_void() +        #make appropriate themes
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) 

map_overall 


#show a similar map using simulated data
data <- read_csv("data/00-simulated_data/simulated_data.csv")

map_df  <- data |> 
  select(gunshot, prname) |>
  group_by(prname) |> 
  summarize(rate = mean(gunshot)) |> #Compute percent of Gun shot
  right_join(get_provinces(), by = "prname") |>  #get_provinces()  get canada provinces map information
  mutate(
    info = ifelse(is.na(rate), "No info.", round(rate * 100,1)), #make labels in map, contain information of province and Percent of Gun shot
    label = ifelse(is.na(rate),
                   paste(gsub(" /.*", "", prname),
                         paste0(info),
                         sep = "\n"),
                   paste(gsub(" /.*", "", prname),
                         paste0(info, "%"),
                         sep = "\n"))
  ) 

map_overall <- map_df |>  ggplot() + 
  geom_sf(aes(fill = rate * 100, geometry = geometry)) +
  geom_sf_label(aes(label = label, geometry = geometry), alpha = 0.9) +
  scale_fill_gradientn(colours = colorRampPalette(brewer.pal(6, "Set2"))(6), name = "") + #make nice colors
  labs(title = "Percent of Gun shot overall in Canada") +
  theme_void() +        #make appropriate themes
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) 

map_overall 