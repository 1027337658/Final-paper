#### Preamble ####
# Purpose: Exploratory analysis of cleaned Trackinginjustice Website Data
# Author: Yuanting Han
# Date:  29 November 2024
# Contact: q2900725357@gmail.com
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded 
  # - The `arrow` package must be installed and loaded
  # - The `cowplot` package must be installed and loaded 
  # - The `canadamaps` package must be installed and loaded 
  # - The `gtsummary` package must be installed and loaded
  # - The `gt` package must be installed and loaded
  # - The `RColorBrewer` package must be installed and loaded
  # - The `ggthemes` package must be installed and loaded
  # - 03-clean_data.R must have been run
  # - 02-download_data.R must have been run
# Any other information needed?  Make sure in the  `Injustice` rproj


#### Workspace setup ####
library(tidyverse) 
library(arrow) #for read parquet
library(cowplot) #for layout
library(canadamaps) #for map
library(gtsummary) #for tables
library(RColorBrewer) #for nicely colors
library(ggthemes) #for nicely themes of ggplot2

#### Read data ####
data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

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

#show differences of races in Percent of Gun shot across different periods
#Some pieces of R codes frame for plotting can be referenced in:
#https://www.kaggle.com/code/janiobachmann/attrition-in-an-organization-why-workers-quit
race.diffs <- data |>
  select(period, race, gunshot) |> 
  group_by(period, race) |>
  summarize(rate = mean(gunshot) ) |> 
  ggplot(aes(x=period, y=rate, color=race)) + 
  scale_y_continuous(labels = scales::percent, limits = c( 0, 1)) + 
  theme_tufte() +  
  geom_segment(aes(x=period,  
                   xend=period, 
                   y = 0, 
                   yend=1), 
               linetype="dashed", 
               size=0.3,
               color="white") +  
  geom_point(size = 6 ) + 
  labs(
    subtitle="Gun shot difference between blacks and whites across different periods",
    y="Percent of gun shot",
    x="Period",
    color = "Race") +  
  coord_flip() + 
  scale_color_manual(values=c("#58FA58", "#FA5858")) +  
  theme(legend.position="top", 
        legend.background = element_rect(fill="#FFF9F5", 
                                         size=0.5, 
                                         linetype="solid", 
                                         colour ="black")) + 
  theme(
    plot.subtitle=element_text(hjust= 0.3, color="white"), 
    plot.background=element_rect(fill="#0E5682"),
    axis.text = element_text(colour="white"),
    axis.title = element_text(colour="white"))
race.diffs

#get averages of ages by race
data |>
  select(race, age) |>
  group_by(race) |> 
  summarise(mean(age))
#get averages of ages overall
data |>
  select(race, age) |>
  summarise(mean(age))

age_text <- data.frame(
  label = c("Mean = 32.8 \n Years Old", "Mean = 37.8 \n Years Old"),
  race   = c("Black", "White")
)
#show age distribution
dist.races <- data |>
  select(race, age) |> 
  group_by(race) |> 
  ggplot(aes(x=age)) +
  scale_x_continuous(limits = c(0, 100)) + 
  geom_density(aes(fill=race), 
               alpha = 0.6,
               show.legend=F) + 
  facet_wrap(~ race) + 
  theme_minimal() + 
  geom_vline(aes(xintercept = mean(age)),
             color="indianred", 
             linetype = "dashed",
             size=1.2) + 
  labs(title="Age Distribution", y = "Density", x= "Age") + 
  scale_fill_manual(values=c("#58FA58", "#FA5858")) + 
  geom_text(
    data    = age_text,
    mapping = aes(x = 65, y = 0.025, label = label)
  )


dist.overall <- data |> 
  select(race, age) |> 
  ggplot(aes(x=age)) +
  geom_density(fill="skyblue") + 
  scale_x_continuous(limits = c(0, 100)) + 
  geom_vline(aes(xintercept=mean(age)),
             color="indianred", 
             linetype="dashed", 
             size=1.2) + 
  theme_minimal() + 
  labs(x="Overall age", y = "Density") + 
  annotate("text", label = "Mean = 36.8 \n Years Old",
           x = 55, y = 0.025, color = "black")

plot_grid(dist.races, dist.overall, nrow=2)

#time series of total cases
data |>
  group_by(year) |>
  summarise(total = sum(gunshot)) |>
  ggplot(aes(year, total)) +
  geom_line(color = "skyblue", linewidth = 2) +
  geom_point(size = 6, color = "darkorange") +
  geom_label(aes(label = total), vjust = -1, fill = "#FFF9F5") +
  theme_economist() +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Year", y = "Total number of highest level Gun shot",
       title = "Total number of highest level Gun shot in Canada across years") +
  theme(
    axis.title = element_text(size = 12, face = "bold"))

#show Gun Shot Rates by Gender and Race 
gender_diff <- data |> 
  select(gender, race, gunshot) |>
  mutate(gender = ifelse(gender == 1, "Male","Female")) |> 
  group_by( gender, race) |> 
  summarize(rate = mean(gunshot)) |> 
  ggplot(aes(x=fct_reorder(gender,rate), 
             y=rate, fill= race )) +
  geom_bar(stat="identity") + 
  facet_wrap(~ race) + 
  coord_flip() + 
  scale_fill_manual(values=c("#58FA58", "#FA5858")) +
  geom_label(aes(label= paste0( round(rate * 100,2),"%"), fill = race), colour = "white") + 
  scale_y_continuous(labels = scales::percent, limits = c( 0, 1)) +  
  theme_economist() + 
  labs(y ="Percent of gun shot", x="Gender", title="Gun Shot Rates by Gender and Race") + 
  theme(legend.position="none",
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10, face = "bold"),
        plot.title=element_text(hjust=0.4, size=12))

gender_diff

#filter only using top 6 provinces which have most cases, thus, information could be enough to compute gun shot rate
gunshot_race <- data |>
  filter(province %in% names(sort(table(data$province),decreasing = TRUE))[1:6]) |>
  select(province, race, gunshot) |>
  group_by(race, province) |>
  summarise(rate = mean(gunshot)) |>
  ggplot(aes(x=province, y=rate, color=race)) + 
  facet_wrap(~race) + 
  coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent, limits = c( 0, 1)) +  
  theme(legend.position = "top",
        plot.background=element_rect(fill="#FFF2E4")) + 
  stat_summary(fun.y=mean, fun.ymin = min, fun.ymax = max) +
  scale_color_manual(values=c("#58FA58", "#FA5858")) + 
  labs(title="Gun Shot Rates by Race Across Top 6 Gun Shot Cases Provinces",
       color = "Race", 
       x = "Province",
       y = "Percent of gun shot")

#blacks gun shot rate
black_rate <- data |>
  filter(province %in% names(sort(table(data$province),decreasing = TRUE))[1:6]) |>
  select(province, race, gunshot) |>
  group_by(province) |>
  filter(race == "Black") |> 
  summarize(rate_black = mean(gunshot))
#whites gun shot rate
white_rate <-  data |>
  filter(province %in% names(sort(table(data$province),decreasing = TRUE))[1:6]) |>
  select(province, race, gunshot) |>
  group_by(province) |>
  filter(race == "White") |> 
  summarize(rate_white = mean(gunshot))
#compute difference and combine to show the plot
diff <- merge(black_rate, white_rate) |> 
  mutate(diff = round(((rate_black - rate_white)/rate_white), 2) * 100) |>
  ggplot(aes(x=reorder(province, diff), y = diff, fill = province)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(6, "Set2"))(6)) + 
  theme(plot.background=element_rect(fill="#FFF1E0"), 
        legend.position="none") + 
  labs(x="Province", 
       y="Percent Difference (%)", 
       title="Percent Difference of Gun Shot by Top 6 Gun Shot Cases Provinces") + 
  geom_label(aes(label=paste0(diff, "%")), 
             colour = "white", 
             hjust=0.3)

plot_grid(gunshot_race, 
          diff, 
          nrow=2)

#now, filter using top 3 police
gunshot_race <- data |>
  filter( police %in% names(sort(table(data$ police),decreasing = TRUE))[1:3]) |>
  select( police, race, gunshot) |>
  group_by(race,  police) |>
  summarise(rate = mean(gunshot)) |>
  ggplot(aes(x= police, y=rate, color=race)) + 
  facet_wrap(~race) + 
  coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent, limits = c( 0, 1)) +  
  theme(legend.position = "top",
        plot.background=element_rect(fill="#FFF2E4")) + 
  stat_summary(fun.y=mean, fun.ymin = min, fun.ymax = max) +
  scale_color_manual(values=c("#58FA58", "#FA5858")) + 
  labs(title="Gun Shot Rates by Race Across Top 3 Gun Shot Cases Polices",
       color = "Race", 
       x = "Police",
       y = "Percent of gun shot")

#similar for blacks and whites
black_rate <- data |>
  filter( police %in% names(sort(table(data$ police),decreasing = TRUE))[1:3]) |>
  select( police, race, gunshot) |>
  group_by( police) |>
  filter(race == "Black") |> 
  summarize(rate_black = mean(gunshot))

white_rate <-  data |>
  filter( police %in% names(sort(table(data$ police),decreasing = TRUE))[1:3]) |>
  select( police, race, gunshot) |>
  group_by( police) |>
  filter(race == "White") |> 
  summarize(rate_white = mean(gunshot))

diff <- merge(black_rate, white_rate) |> 
  mutate(diff = round(((rate_black - rate_white)/rate_white), 2) * 100) |>
  ggplot(aes(x=reorder( police, diff), y = diff, fill =  police)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(3, "Set2"))(3)) + 
  theme(plot.background=element_rect(fill="#FFF1E0"), 
        legend.position="none") + 
  labs(x="Police", 
       y="Percent Difference (%)", 
       title="Percent Difference of Gun Shot by Top 3 Gun Shot Cases Polices") + 
  geom_label(aes(label=paste0(diff, "%")), 
             colour = "white", 
             hjust=0.3)

plot_grid(gunshot_race, 
          diff, 
          nrow=2)

#A summary table for data used, not show province 
data_table <- data  |>
  mutate(gender = ifelse(gender == 1, "Male","Female"))  |>
  select(-police, -year, -prname,-province)  

data_table  |> 
  tbl_summary()  





