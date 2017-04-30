library(plyr)
library(dplyr)
library(data.table)
library(datasets)
library(ggplot2)
library(graphics)
library(grDevices)
library(lattice)
library(lubridate)
library(manipulate)
library(MASS)
library(Matrix)
library(methods)
library(mosaic)
library(mosaicData)
library(psych)
library(stats)
library(stringr)
library(utils)
library(zoo)
library(caret)
library(class)
library(rpart)
library(e1071)
library(klaR)
library(mlbench)
library(randomForest)
library(Boruta)
library(rjson)
library(grid)
library(jpeg)
library(RCurl)
library(jpeg)
library(RColorBrewer)
library(shiny)
library(gsheet)


data <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1BZwnqXGPS-cJW_J1gXK22lXHRmuyaeHcdMhiqFVFQWI/edit#gid=1869485836'))

data$made <- as.factor(data$made)

data$made_numeric <- as.numeric(data$made)-1

data$shot_x_abs <- abs(data$shot_x)

data$shot_distance <- (data$shot_x^2 + data$shot_y^2)^.5

data$shot_type <- as.factor(ifelse(data$shot_y < 9.25 & data$shot_x_abs > 22, 3, ifelse(data$shot_y > 9.25 & data$shot_distance > 23.75, 3, 2)))

data$points <- (as.numeric(data$made)-1)*(as.numeric(data$shot_type)+1)

data$dribble <- as.factor(ifelse(data$dribbles_before == 0, "yes", "no"))

data$shot_zone <- as.factor(ifelse(data$shot_type == 3 & data$shot_y < 9.25 & data$shot_x > 0, "Right Corner Three", ifelse(data$shot_type == 3 & data$shot_y < 9.25 & data$shot_x < 0, "Left Corner Three", ifelse(data$shot_type == 3 & data$shot_y > 9.25 & data$shot_y < 42.25, "Above the Break Three", ifelse(data$shot_x_abs > 8 & data$shot_type == 2 | data$shot_y > 14.25 & data$shot_type == 2, "Mid-Range", ifelse(data$shot_distance < 4, "Restricted Area", ifelse(data$shot_x_abs < 8 & data$shot_y < 14.25 & data$shot_distance > 4, "Paint (Not RA)", "Backcourt")))))))

courtImg.URL <- "http://www.sawyoo.com/postpic/2011/05/nba-basketball-court-dimensions_97440.jpg"

court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)), width=unit(1,"npc"), height=unit(1,"npc"))

data_half_court <- data[which(!data$shot_zone=='Backcourt'), ]  

# Define server logic required to plot variables
shinyServer(function(input, output) {

    
  # Generate a plot of the selected variables
  output$plot <- renderPlot({
   
    data_half_court <- filter(data_half_court, defender_distance >= min(input$slider) & defender_distance <= max(input$slider) & dribbles_before >= min(input$slider2) & dribbles_before <= max(input$slider2) & shooter_velocity_ft_sec >= min(input$slider3) & shooter_velocity_ft_sec <= max(input$slider3) & shooter_velocity_angle >= min(input$slider4) & shooter_velocity_angle <= max(input$slider4) & defender_angle >= min(input$slider5) & defender_angle <= max(input$slider5) & defender_velocity_ft_sec >= min(input$slider6) & defender_velocity_ft_sec <= max(input$slider6) & defender_velocity_angle >= min(input$slider7) & defender_velocity_angle <= max(input$slider7) & shot_distance >= min(input$slider8) & shot_distance <= max(input$slider8))
    
    data_shots <- ddply(data_half_court, .(shot_zone), summarize, shots_attempted = length(made), shots_made = sum(made_numeric))
    
    data_shots$field_goal_percentage <- (data_shots$shots_made / data_shots$shots_attempted)
    
    data_shots$field_goal_percentage <- paste(as.character(round(100 * data_shots$field_goal_percentage, 1)), "%", sep="")
    
    data_shots$type <- ifelse(data_shots$shot_zone == "Above the Break Three", 3, ifelse(data_shots$shot_zone == "Left Corner Three", 3, ifelse(data_shots$shot_zone == "Right Corner Three", 3, 2)))
    
    data_shots$points_per_shot <- round(data_shots$type*data_shots$shots_made/data_shots$shots_attempted, 2)
    
    data_shots$eFG <- ifelse(data_shots$type == 2, data_shots$shots_made/data_shots$shots_attempted, 1.5*data_shots$shots_made/data_shots$shots_attempted)
    
    data_shots$eFG <- paste(as.character(round(100 * data_shots$eFG, 1)), "%", sep="")
    
    data_shots$x <- ifelse(data_shots$shot_zone == "Above the Break Three", 12, ifelse(data_shots$shot_zone == "Left Corner Three", 21, ifelse(data_shots$shot_zone == "Right Corner Three", -21, ifelse(data_shots$shot_zone == "Mid-Range", -13, ifelse(data_shots$shot_zone == "Paint (Not RA)", 0, 0)))))
    
    data_shots$y <- ifelse(data_shots$shot_zone == "Above the Break Three", 25, ifelse(data_shots$shot_zone == "Left Corner Three", 0, ifelse(data_shots$shot_zone == "Right Corner Three", 0, ifelse(data_shots$shot_zone == "Mid-Range", 10, ifelse(data_shots$shot_zone == "Paint (Not RA)", 12, 3)))))
    
    par(xpd=TRUE)
    
    p <- ggplot(data_shots, aes(x=x, y=y)) + annotation_custom(court, -25, 25, -4.75, 42.25) + geom_point(aes(colour = shot_zone, alpha = .8), size = 50) + geom_text(aes(colour = shot_zone, label = get(input$Type)), vjust = .2, size = 12, color = "black", fontface = "bold") + geom_text(aes(colour = shot_zone, label = paste(data_shots$shots_made, "of", data_shots$shots_attempted, sep = " ")), vjust = 3, hjust = .5, size = 5, color = "black", fontface = "bold") + guides(alpha = FALSE, size = FALSE, colour = guide_legend(override.aes = list(size=12, alpha = 1))) + xlim(-25, 25) + ylim(-4.75, 42.25) + coord_fixed() + theme(line = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.title = element_blank(), legend.text=element_text(size = 16, face = "bold"), legend.position = c(0.21, 0.755), legend.background = element_rect(fill=0), legend.key.size = unit(1.5, "cm"), plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))
    
    print(p)
 
  }, height = 666.7, width = 1000)
  
})
