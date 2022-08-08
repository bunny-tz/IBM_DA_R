# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
library(lubridate)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike<-city_weather_bike_df %>% group_by(CITY_ASCII, LNG, LAT) %>% slice(which.max(BIKE_PREDICTION))

  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown == 'All') {
      #Render the city overview map
      output$city_bike_map <- renderLeaflet({
        # Complete this function to render a leaflet map
        leaflet(cities_max_bike) %>% addTiles() %>%
          addCircleMarkers(lng=cities_max_bike$LNG, lat=cities_max_bike$LAT,
                           popup=cities_max_bike$LABEL,
                           radius=~case_when(cities_max_bike$BIKE_PREDICTION_LEVEL=='small' ~ 6,
                                             cities_max_bike$BIKE_PREDICTION_LEVEL=='medium' ~ 10,
                                             cities_max_bike$BIKE_PREDICTION_LEVEL=='large' ~ 12),
                           color=~color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL))
      })
    }
    else {
      #Render the specific city map
      filtered_data<-cities_max_bike %>% filter(CITY_ASCII==input$city_dropdown)
      city_weather_bike_df_filter<- city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown)
      output$city_bike_map <- renderLeaflet({
        # Complete this function to render a leaflet map
        leaflet(filtered_data) %>% addTiles() %>%
          addCircleMarkers(lng=filtered_data$LNG, lat=filtered_data$LAT,
                           popup=filtered_data$DETAILED_LABEL,
                           radius=~case_when(filtered_data$BIKE_PREDICTION_LEVEL=='small' ~ 6,
                                             filtered_data$BIKE_PREDICTION_LEVEL=='medium' ~ 10,
                                             filtered_data$BIKE_PREDICTION_LEVEL=='large' ~ 12),
                           color=~color_levels(filtered_data$BIKE_PREDICTION_LEVEL))
      })
      output$temp_line <- renderPlot({
        ggplot(city_weather_bike_df_filter, aes(x=hour(FORECASTDATETIME), y=TEMPERATURE))+
          geom_line(color='yellow', size = 1)+
          labs(x="Time (3 hours ahead)",y="Temperature (℃)")+
          geom_point()+
          geom_text(aes(label=paste(TEMPERATURE, " ℃")), hjust=0, vjust=0)+
          ggtitle(paste('Temperature Chart of ', input$city_dropdown))
      })
      output$bike_line <- renderPlot({
        ggplot(city_weather_bike_df_filter, aes(x=hour(FORECASTDATETIME), y=BIKE_PREDICTION))+
          geom_line(linetype = "dashed", color='blue', size = 1)+
          labs(x="Time (3 hours ahead)",y="Bike Demand Prediction")+
          geom_point()+
          geom_text(aes(label=BIKE_PREDICTION), hjust=0, vjust=0)+
          ggtitle(paste('Bike Demand Prediction Trend of', input$city_dropdown))
      })
      output$bike_date_output <- renderText({
        paste("Time = ", city_weather_bike_df_filter[1,]$FORECASTDATETIME, "  ",
              'BikeCountPred = ', city_weather_bike_df_filter[1,]$BIKE_PREDICTION)
        
      })
      output$humidity_pred_chart <- renderPlot({
        ggplot(city_weather_bike_df_filter, aes(x=HUMIDITY, y=BIKE_PREDICTION))+
          labs(x="Humidity",y="Bike Demand Prediction", hjust=0, vjust=0)+
          geom_point()+
          geom_smooth(method = 'lm', formula = y ~ poly(x, 4), se=FALSE)+
          ggtitle(paste('Bike Demand Prediction vs Humidity of', input$city_dropdown))
      })  
    } 
  })
})