# scripts to put into shiny app
# last detection data
# H. Short
# 2026-09-15 08:48:42
# -------------------------------------------------------------------------------
# load packages
library(tidyverse)

# -------------------------------------------------------------------------------
# read in data
last_locs_2024 = readRDS("clean-data/calfishtrack/2024_last_locations.rds")

# last detections map -------------------------------------------------------------------------------
# testable version
# map last locations
leaflet(last_loc) %>% 
  addTiles() %>% 
  addProviderTiles("Stadia.StamenTerrain") %>% 
  setView(lng = -121.51, lat = 38.24, zoom = 10) %>% 
  addHeatmap(lng = ~receiver_general_longitude,
           lat = ~receiver_general_latitude,
           intensity = ~count,
           radius = 30,
           blur = 20,
           max = max(last_loc$count),  # now scaled to your actual data range
           minOpacity = 0.3) %>% 
  addMarkers(lng = ~receiver_general_longitude,
             lat = ~receiver_general_latitude,
             popup = ~last_location,
             label = ~paste("Count:", count))

leaflet(last_loc) %>% 
  addTiles()

# -------------------------------------------------------------------------------
# shiny ui

  selectInput("water_year_last_locs", "Water Year",
              choices = c(2024, 2025, 2026),
              selected = 2024),
  leafletOutput("last_detection_map", height = 600)

# -------------------------------------------------------------------------------
# shiny server
output$last_detection_map_or_message <- renderUI({

    if (input$water_year == 2024) {plotOutput("last_detection_map")} 

    else {div(style = "color: #888; font-style: italic; font-size: 20px; padding: 20px; text-align: center;", 
              "Sorry, data for this year is not available right now.")}
  
})

output$last_detection_map <- renderLeaflet({
    
    # require water year as an input
    req(input$water_year_last_locs)
    
    leaflet(last_locs_2024()) %>%
      addTiles() %>%
      addProviderTiles("Stadia.StamenTerrain") %>%
      setView(lng = -121.51, lat = 38.24, zoom = 10) %>%
      addHeatmap(lng = ~receiver_general_longitude,
                 lat = ~receiver_general_latitude,
                 intensity = ~count,
                 radius = 30,
                 blur = 20,
                 max = max(df$count),
                 minOpacity = 0.3) %>%
      addMarkers(lng = ~receiver_general_longitude,
                 lat = ~receiver_general_latitude,
                 popup = ~last_location,
                 label = ~paste("Count:", count))
    
  }) # end render map