# Load required libraries
library(shiny) #for shiny
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2) #for plot
library(here) #for path
library(arrow) #for read parquet
library(cowplot) #for layout
library(canadamaps) #for map
library(gtsummary) #for tables
library(RColorBrewer) #for nicely colors
library(ggthemes) #for nicely themes of ggplot2

#please run Shiny app from the project, otherwise, paths needed to be modified
#### Read data ####
data <- read_parquet(here("data/02-analysis_data/analysis_data.parquet"))
df <-    get_provinces()  #get_provinces()  get canada provinces map information
       

ui <- dashboardPage(
  dashboardHeader(title = "Racial Disparities in Police-Involved Deaths", titleWidth = 1200),
  dashboardSidebar(
    width = "0px", minified=FALSE
  ),
  dashboardBody(
    box(title="Canada map",
           status="primary",
           solidHeader=TRUE,
           plotOutput("plotmap")
    ),
    box(
      title="",
      status="primary",
      solidHeader=TRUE,
      selectInput("x1", "Which characteristic do you interested in?", 
                  selected = "Gun shot", 
                  choices = c("Gun shot","Age","Gender"))
     
    ),
    box( title="Race difference",
           status="primary",
           solidHeader=TRUE,
           plotOutput("racediff")
    ),
    
    box( title="",
         status="primary",
         solidHeader=TRUE,
         selectInput("x2", "Which province do you interested in?", 
                     selected = "Ontario", 
                     choices = c('Alberta','British Columbia','Manitoba','New Brunswick','Newfoundland and Labrador',
                     'Nova Scotia','Ontario','Prince Edward Island','Quebec','Saskatchewan','All'))
    ),
    
    box( title="Age  distribution",
         status="primary",
         solidHeader=TRUE,
         plotOutput("age")
    ),
    
    box( title="",
         status="primary",
         solidHeader=TRUE,
         checkboxGroupInput("x3", "Which province(s) do you interested in?", 
                     selected = c("ON", 'BC','NL'),
                     choices = c('AB','BC','MB','NB','NL','NS','ON','PE','QC','SK'))
    ),
    
    box( title="Differences across provinces",
         status="primary",
         solidHeader=TRUE,
         plotOutput("pr")
    ),
    
    box( title="",
         status="primary",
         solidHeader=TRUE,
         checkboxGroupInput("x4", "Which police(s) do you interested in?", 
                            selected = c("Toronto Police Service", 'Surete du Quebec','RCMP'),
                            choices = c('RCMP','Toronto Police Service','Surete du Quebec',
                                        'Service de police de la Ville de Montreal','Calgary Police Service'))
    ),
    
    box( title="Differences across polices",
         status="primary",
         solidHeader=TRUE,
         plotOutput("po")
    ),
    tags$footer(strong("Github Repo: https://github.com/1027337658/Injustice"),
                br(),
                strong("Data source: Tracking (in)justice Website open database https://trackinginjustice.ca/explore-the-data."),
                align = "center")
  )
  
)

 
server <- function(input,output,session) {
  
  

  #convert map into shiny which switch to gender, age, gunshot
  output$plotmap <- renderPlot({ 
    if(input$x1 == "Age") {
   data |> 
      select(age, prname) |>
      group_by(prname) |> 
      summarize(rate = mean(age)) |> #Gender rate or mean age or gun shot percent
      right_join(df, by = "prname") |> 
      mutate(
        info = ifelse(is.na(rate), "No info.", round(rate,1)), #make labels in map, contain information of province and Percent of Gun shot
        label = ifelse(is.na(rate),
                       paste(gsub(" /.*", "", prname),
                             paste0(info),
                             sep = "\n"),
                       paste(gsub(" /.*", "", prname),
                             paste0(info, " Years"),
                             sep = "\n"))
      )  |>  ggplot() + 
      geom_sf(aes(fill = rate, geometry = geometry)) +
      geom_sf_label(aes(label = label, geometry = geometry), alpha = 0.9) +
      scale_fill_gradientn(colours = colorRampPalette(brewer.pal(6, "Set2"))(6), name = "") + #make nice colors
      labs(title = "Average age overall in Canada") +
      theme_void() +        #make appropriate themes
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)
      )  
    } else if  (input$x1 == "Gender") {
      data |> 
        select(gender, prname) |>
        group_by(prname) |> 
        summarize(rate = mean(gender)) |> #Gender rate or mean age or gun shot percent
        right_join(df, by = "prname") |>  
        mutate(
          info = ifelse(is.na(rate), "No info.", round(rate * 100,1)), #make labels in map, contain information of province and Percent of Gun shot
          label = ifelse(is.na(rate),
                         paste(gsub(" /.*", "", prname),
                               paste0(info),
                               sep = "\n"),
                         paste(gsub(" /.*", "", prname),
                               paste0(info, "%"),
                               sep = "\n"))
        )  |>  ggplot() + 
        geom_sf(aes(fill = rate * 100, geometry = geometry)) +
        geom_sf_label(aes(label = label, geometry = geometry), alpha = 0.9) +
        scale_fill_gradientn(colours = colorRampPalette(brewer.pal(6, "Set2"))(6), name = "") + #make nice colors
        labs(title = "Percent of males overall in Canada") +
        theme_void() +        #make appropriate themes
        theme(
          legend.position = "top",
          plot.title = element_text(hjust = 0.5)
        ) 
    } else {
      
      data |> 
        select(gunshot, prname) |>
        group_by(prname) |> 
        summarize(rate = mean(gunshot)) |> #Gender rate or mean age or gun shot percent
        right_join(df, by = "prname") |>  #get_provinces()  get canada provinces map information
        mutate(
          info = ifelse(is.na(rate), "No info.", round(rate * 100,1)), #make labels in map, contain information of province and Percent of Gun shot
          label = ifelse(is.na(rate),
                         paste(gsub(" /.*", "", prname),
                               paste0(info),
                               sep = "\n"),
                         paste(gsub(" /.*", "", prname),
                               paste0(info, "%"),
                               sep = "\n"))
        )  |>  ggplot() + 
        geom_sf(aes(fill = rate * 100, geometry = geometry)) +
        geom_sf_label(aes(label = label, geometry = geometry), alpha = 0.9) +
        scale_fill_gradientn(colours = colorRampPalette(brewer.pal(6, "Set2"))(6), name = "") + #make nice colors
        labs(title = "Percent of Gun shot overall in Canada") +
        theme_void() +        #make appropriate themes
        theme(
          legend.position = "top",
          plot.title = element_text(hjust = 0.5)
        ) 
    }
  })
  
  #convert gunshot difference by race into shiny which switch to gender, age, gunshot
  output$racediff <- renderPlot({
    
    if(input$x1 == "Age") {
    
     data |>
      select(period, race, age) |> 
      group_by(period, race) |>
      summarize(rate = mean(age) ) |> 
      ggplot(aes(x=period, y=rate, color=race)) + 
      scale_y_continuous(limits = c( 0, 100)) + 
      theme_tufte() +  
      geom_segment(aes(x=period,  
                       xend=period, 
                       y = 0, 
                       yend=100), 
                   linetype="dashed", 
                   size=0.3,
                   color="white") +  
      geom_point(size = 6 ) + 
      labs(
        subtitle="Average age difference between blacks and whites across different periods",
        y="Average age",
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
    } else if( input$x1 == "Gender"   ) {
      data |>
        select(period, race, gender) |> 
        group_by(period, race) |>
        summarize(rate = mean(gender) ) |> 
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
          subtitle="Proportion of males difference between blacks and whites across different periods",
          y="Proportion of males",
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
    } else {
      data |>
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
      
    }
    
  }) 
  
  output$age <- renderPlot({ 
    if(input$x2 != "All") {
    #show age distribution
    dist.races <- data |> 
      filter(prname == input$x2) |>
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
      labs(title= paste0( "Age Distribution", " ",input$x2), y = "Density", x= "Age") + 
      scale_fill_manual(values=c("#58FA58", "#FA5858")) 
    dist.overall <- data |> 
      filter(prname  == input$x2) |>
      select(race, age) |> 
      ggplot(aes(x=age)) +
      geom_density(fill="skyblue") + 
      scale_x_continuous(limits = c(0, 100)) + 
      geom_vline(aes(xintercept=mean(age)),
                 color="indianred", 
                 linetype="dashed", 
                 size=1.2) + 
      theme_minimal() + 
      labs(x="Overall age", y = "Density") 
    
    plot_grid(dist.races, dist.overall, nrow=2)
    } else {
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
        scale_fill_manual(values=c("#58FA58", "#FA5858")) 
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
        labs(x="Overall age", y = "Density") 
      
      plot_grid(dist.races, dist.overall, nrow=2)
    }
  })
  
  
  output$pr <- renderPlot({ 
  
    gunshot_race <- data |>
      filter(province %in% input$x3) |>
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
      labs(title="Gun Shot Rates by Race Across Provinces",
           color = "Race", 
           x = "Province",
           y = "Percent of gun shot")
    #blacks gun shot rate
    black_rate <- data |>
      filter(province %in% input$x3) |>
      select(province, race, gunshot) |>
      group_by(province) |>
      filter(race == "Black") |> 
      summarize(rate_black = mean(gunshot))
    #whites gun shot rate
    white_rate <-  data |>
      filter(province %in% input$x3) |>
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
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(length(input$x3))) + 
      theme(plot.background=element_rect(fill="#FFF1E0"), 
            legend.position="none") + 
      labs(x="Province", 
           y="Percent Difference (%)", 
           title="Percent Difference of Gun Shot by Provinces") + 
      geom_label(aes(label=paste0(diff, "%")), 
                 colour = "white", 
                 hjust=0.3)
    
    plot_grid(gunshot_race, 
              diff, 
              nrow=2)
    
    
  }) 
  
  output$po <- renderPlot({ 
    
    gunshot_race <- data |>
      filter( police %in% input$x4) |>
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
      labs(title="Gun Shot Rates by Race Across Polices",
           color = "Race", 
           x = "Police",
           y = "Percent of gun shot")
    
    #similar for blacks and whites
    black_rate <- data |>
      filter( police %in% input$x4) |>
      select( police, race, gunshot) |>
      group_by( police) |>
      filter(race == "Black") |> 
      summarize(rate_black = mean(gunshot))
    
    white_rate <-  data |>
      filter( police %in% input$x4) |>
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
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(length(input$x4))) + 
      theme(plot.background=element_rect(fill="#FFF1E0"), 
            legend.position="none") + 
      labs(x="Police", 
           y="Percent Difference (%)", 
           title="Percent Difference of Gun Shot by Polices") + 
      geom_label(aes(label=paste0(diff, "%")), 
                 colour = "white", 
                 hjust=0.3)
    
    plot_grid(gunshot_race, 
              diff, 
              nrow=2)
    
    
  })
}
  
  
# Run the application 
shinyApp(ui = ui, server = server)
  