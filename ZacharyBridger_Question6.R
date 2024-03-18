#
setwd("C:/Users/Owner/Documents/BASEBALL DATA")


# load packages

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(shiny)
library(ggpubr)

# Import data / CSV & Remove Extra Column "V1"

AnalyticsQuestionnaireHitData=fread("AnalyticsQuestionnaireHitData_Tigers.csv")
AnalyticsQuestionnaireHitData <- subset(AnalyticsQuestionnaireHitData, select = -c(V1))

# Round Data

AnalyticsQuestionnaireHitData$ReleaseSpeed <- round(AnalyticsQuestionnaireHitData$ReleaseSpeed, digits = 1)
AnalyticsQuestionnaireHitData$LaunchSpeed <- round(AnalyticsQuestionnaireHitData$LaunchSpeed, digits = 1)
AnalyticsQuestionnaireHitData$LaunchAngle <- round(AnalyticsQuestionnaireHitData$LaunchAngle, digits = 0)
AnalyticsQuestionnaireHitData$LandingPositionX <- round(AnalyticsQuestionnaireHitData$LandingPositionX, digits = 0)
AnalyticsQuestionnaireHitData$LandingPositionY <- round(AnalyticsQuestionnaireHitData$LandingPositionY, digits = 0)
AnalyticsQuestionnaireHitData$LandingPositionZ <- round(AnalyticsQuestionnaireHitData$LandingPositionZ, digits = 0)
AnalyticsQuestionnaireHitData$PlateX <- round(AnalyticsQuestionnaireHitData$PlateX, digits = 1)
AnalyticsQuestionnaireHitData$PlateZ <- round(AnalyticsQuestionnaireHitData$PlateZ, digits = 1)

# UI Definition 

ui <- navbarPage(
  
  "Hitting Analysis", theme = "flatly",
  
  tabPanel("Hitters",
           
           sidebarLayout(
             
             sidebarPanel(
            
               selectInput("Hitter", label = "Select Hitter(s):",
                           choices = levels(as.factor(AnalyticsQuestionnaireHitData$BatterId)),
                           multiple = TRUE, selected = c(1:25)),
               
               
               checkboxGroupInput("Pitch", label = "Select Pitch Type(s):", 
                                  choices = levels(as.factor(AnalyticsQuestionnaireHitData$PitchType)),
                                  selected = c("FF", "CH", "CU", "SI", "SL", "KC", "FC")),
               
               sliderInput("pointSize", "Point Size:", 
                           min = 1, max = 3, 
                           value = 3),
               
               width = 2),
             
             
             mainPanel(
               
               fluidRow(plotlyOutput("sprayChart")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(plotlyOutput("Strike_Zone")),
               br(),
               br(),
               br(),
               br(),
               br(),
             )
           )
  ),
  
  
  # New page for Ball/Strike selection and action button
  tabPanel("At-Bat Count",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectizeInput("selectedBatterIds", "Select Hitter(s):", choices = NULL, multiple = TRUE),
               
               selectInput("selectedBalls", "Balls:", choices = 0:3, selected = 0),
               
               selectInput("selectedStrikes", "Strikes:", choices = 0:2, selected = 0),
               
               actionButton("updateMetrics", "Update Metrics"),
               
               width = 2),
             
             mainPanel(
               
               fluidRow(DTOutput("metricsTable")),
             )
             
             
             
           )),
  
)



# Server Logic

server = function(input, output, session) {
  
  # Pitch Types Based on Pitcher
  
  observeEvent(
    input$Hitter,
    updateCheckboxGroupInput(session,
                             "Pitch", "Select Pitch Type(s):",
                             choices = levels(factor(filter(AnalyticsQuestionnaireHitData,
                                                            BatterId == isolate(input$Hitter))$PitchType))))
  
  # Dynamically update BatterId choices based on the dataset
  
  observe({
    data <- AnalyticsQuestionnaireHitData
    updateSelectizeInput(session, "selectedBatterIds", choices = unique(data$BatterId))
  })
  
  
  
  # Start of Outputs (Plots and Data Tables)
  
  # Strike_Zone
  
  output$Strike_Zone <- renderPlotly({
    
    
    Top <- 18.29 / 12
    Bottom <- 44.08 / 12
    Left <- -8.5 / 12
    Right <- 8.5 / 12
    
    Width <- (Right - Left) / 3
    Height <- (Top - Bottom) / 3
    
    
    AnalyticsQuestionnaireHitData%>%
      filter(BatterId %in% input$Hitter,
             PitchType %in% input$Pitch) %>%
      ggplot(AnalyticsQuestionnaireHitData, mapping = aes(x= PlateX, y= PlateZ,
                                                          label = LaunchSpeed,
                                                          label2 = ReleaseSpeed,
                                                          label3 = PitchCall,
                                                          label4 = Balls,
                                                          label5 = Strikes,
                                                          label6 = BatterId)) +
      
      geom_point(aes(color = PitchType), size = input$pointSize) +
      scale_color_manual(values = c(CH = "blue", FF = "black",
                                    SL = "orange", CU = "red",
                                    FC = "green",SI = "grey",
                                    KC = "purple"))+
      
      
      
      
      # The Box (Bottom, Top, Left, Right)
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      
      # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (Left), yend = (8.5/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (8.5/12)) +
      geom_segment(x = (0), y = (0), xend = (Right), yend = (4.25/12)) + 
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "right") + 
      xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Strike Zone")
    
    
    
  })
  
  # Render the spray chart based on selected BatterIds
  
  output$sprayChart <- renderPlotly({
    
    
    # Filter data based on selected BatterIds
    
    filteredData <- AnalyticsQuestionnaireHitData %>%
      
      filter(BatterId %in% input$Hitter,
             PitchType %in% input$Pitch)
    
    # Generate the spray chart
    AnalyticsQuestionnaireHitData$PitchCall <- AnalyticsQuestionnaireHitData$LaunchAngle
    AnalyticsQuestionnaireHitData$PitchCall[AnalyticsQuestionnaireHitData$PitchCall < 10] <- "GB"
    AnalyticsQuestionnaireHitData$PitchCall[AnalyticsQuestionnaireHitData$PitchCall >= 10 & AnalyticsQuestionnaireHitData$PitchCall <= 25] <- "LD"
    AnalyticsQuestionnaireHitData$PitchCall[AnalyticsQuestionnaireHitData$PitchCall > 25] <- "FB"
    
    ggplot(filteredData, aes(x = LandingPositionX, y = LandingPositionY,
                             label = PitchCall,
                             label2 = LaunchAngle,
                             label3 = Balls,
                             label4 = Strikes,
                             label5 = BatterId,
                             Label6 = PitchType)) +
      
      
      geom_point(aes(color = LaunchSpeed), size = input$pointSize) +
      scale_color_gradient2(low = "blue", high = "red", midpoint = mean(filteredData$LaunchSpeed, na.rm = TRUE)) +
      xlim(-295, 295) + ylim(0, 450) +
      geom_segment(x = 0, xend = -315, y = 0, yend = 315, linewidth = 1.2) +
      geom_segment(x = 0, xend = 315, y = 0, yend = 315, linewidth = 1.2) +
      
      # Outfield Wall Segments
      geom_segment(x = -63, y = 400, xend = 63, yend = 400, linewidth = 1.2) +
      geom_segment(x = -315, y = 315, xend = -220.5, yend = 358, linewidth = 1.2) +
      geom_segment(x = 315, y = 315, xend = 220.5, yend = 358, linewidth = 1.2) +
      geom_segment(x = 220.5, y = 358, xend = 63, yend = 400, linewidth = 1.2) +
      geom_segment(x = -63, y = 400, xend = -220.5, yend = 358, linewidth = 1.2) +
      
      # Infield Arc Segments
      geom_segment(x = -100, y = 100, xend = -73.3, yend = 127.3, linewidth = 1.2) +
      geom_segment(x = 100, y = 100, xend = 73.3, yend = 127.3, linewidth = 1.2) +
      geom_segment(x = -73.3, y = 127.3, xend = -26.6, yend = 150, linewidth = 1.2) +
      geom_segment(x = 73.3, y = 127.3, xend = 26.6, yend = 150, linewidth = 1.2) +
      geom_segment(x = -26.6, y = 150, xend = 26.6, yend = 150, linewidth = 1.2) +
      
      
      
      coord_fixed() +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) + 
      labs(title = "Batted Ball Spray Chart", color = "Exit Velo") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "darkgrey"))
    
  })
  
  # New logic for calculating and displaying average metrics per pitch count
  
  output$metricsTable <- renderDT({
    
    req(input$updateMetrics)
    
    filteredData <- AnalyticsQuestionnaireHitData %>%
      filter(BatterId %in% input$selectedBatterIds,
             Balls == input$selectedBalls,
             Strikes == input$selectedStrikes)
    
    # Calculating average metrics
    
    avgMetrics <- filteredData %>%
      
      group_by(BatterId, PitchCall, PitchType) %>%
      
      summarise(AvgLaunchSpeed = round(mean(LaunchSpeed, na.rm = TRUE), 1),
                
                MaxLaunchAngle = round(max(LaunchAngle, na.rm = TRUE), 1),
                
                MaxLaunchSpeed = round(max(LaunchSpeed, na.rm = TRUE), 1),
                
             
                ) %>%
      
      # Ensure that only rows with batted balls are displayed
      filter(!is.na(AvgLaunchSpeed) & !is.na(MaxLaunchAngle) & !is.na(MaxLaunchSpeed)) %>%
      
      # Order by highest Launch Speed
      
      arrange(desc(MaxLaunchSpeed))
    
    
    # Display as a DataTable
    
    datatable(avgMetrics, options = list(pageLength = 5))
    
  })
  
}

# Run the application

shinyApp(ui = ui, server = server)
