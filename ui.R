library(shiny)


# Define UI for IRIS application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("NBA Shooting Zones: By Jason Katz"),
  
  # Sidebar with controls to select the variables to plot 
  sidebarPanel(
  
    submitButton("Apply Changes"),
    
    radioButtons("Type", h2("Statistic"),
                 list("Field Goal Percentage"  = "field_goal_percentage",
                      "Effective Field Goal Percentage" = "eFG",
                      "Points Per Shot" = "points_per_shot"), selected = "field_goal_percentage"),

  sliderInput("slider", label = h3("Defender Distance"), min = 0, 
              max = 15, value = c(0, 15), step = .5),
  
  sliderInput("slider2", label = h3("Dribbles Before Shot"), min = 0, 
              max = 20, value = c(0, 20), step = 1),
  
  sliderInput("slider3", label = h3("Shooter Velocity"), min = 0, 
              max = 12, value = c(0, 12), step = .5),
  
  sliderInput("slider4", label = h3("Shooter Velocity Angle"), min = -180, 
              max = 180, value = c(-180, 180), step = 10),
  
  sliderInput("slider5", label = h3("Defender Angle"), min = -180, 
              max = 180, value = c(-180, 180), step = 10),
  
  sliderInput("slider6", label = h3("Defender Velocity"), min = 0, 
              max = 12, value = c(0, 12), step = .5),
  
  sliderInput("slider7", label = h3("Defender Velocity Angle"), min = -180, 
              max = 180, value = c(-180, 180), step = 10),
  
  sliderInput("slider8", label = h3("Shot Distance"), min = 0, 
              max = 50, value = c(0, 50), step = 1),
  
      submitButton("Apply Changes")),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",
               plotOutput("plot"))
  
    ))))