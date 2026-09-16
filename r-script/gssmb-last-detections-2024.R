# GSSMB 2024 Last Detections
# H. Short
# 2026-09-03 10:03:34

# -----------------------------------------------------------------------------------------------------------
# load packages
library(tidyverse)
library(leaflet)
library(leaflet.extras)

# -----------------------------------------------------------------------------------------------------------
# read in data
df = read_csv("C:/Users/Hbell/Projects/GSSMB_shiny/raw-data/detections/2024_GSSMB_ERRDAP_detections.csv")

# -----------------------------------------------------------------------------------------------------------
# pull out fish info
fish = df %>% 
  distinct(tag_id_hex, fish_type, fish_length, fish_weight, fish_release_date, release_location)

# pull out receiver info
receivers = df %>% 
  distinct(receiver_location, receiver_general_latitude, receiver_general_longitude)

# pull out detections
detections = df %>% 
  select(tag_id_hex, receiver_location, detection_count, first_time, last_time)

# -----------------------------------------------------------------------------------------------------------
# create detection order df
detection_order = detections %>% 

  # order by tag_id_hex and the first time the fish were detected - keeps all detections from a fish together and in chronological order
  arrange(tag_id_hex, first_time) %>% 
  
  # group by tag_id_hex
  group_by(tag_id_hex) %>% 
  
  # add a count/order of visits to the unique receiver visits
  mutate(order_of_visits = as.integer(cumsum(receiver_location != lag(receiver_location, default = first(receiver_location)))),
          
         # change data type of detection_count
         detection_count = as.integer(detection_count)) %>% 
  
  # now group by these to keep the columns linked
  group_by(tag_id_hex, order_of_visits, receiver_location) %>% 
  
  # count how many detections occurred at each receiver_location/visit, pick the first and last time of each visit
  summarize(detection_count = sum(detection_count), 
            first_time = min(first_time), 
            last_time = max(last_time),

            # drop the groups so the final df isn't grouped
           .groups = "drop") %>% 
  
  # rearrange by tag_id_hex and first_time to keep detections from a fish together and in chronological order
  arrange(tag_id_hex, first_time)

# add last_location in detection_order df
detection_order = detection_order %>% 
  arrange(tag_id_hex, last_time) %>% 
  group_by(tag_id_hex) %>% 

  # pull last receiver_location and assign to new column
  mutate(last_location = last(receiver_location)) %>% 
  ungroup()

# -------------------------------------------------------------------------------
# create new df with one row per tag_id_hex and last_location
last_loc = detection_order %>% 

  # pull one row per tag_id_hex and last_location
  distinct(tag_id_hex, last_location) %>% 
  
  # merge the receivers df in to get lat/long of receiver locations
  merge(receivers, by.x = "last_location", by.y = "receiver_location") %>% 
  
  # group by the last_location
  group_by(last_location) %>% 
  
  # count how many fish ended at each last_location
  mutate(count = n()) %>% 
  ungroup()

# -------------------------------------------------------------------------------
# add water year 
last_loc = last_loc %>% 
  mutate(water_year = "2024") %>% 
  mutate(water_year = as.factor(water_year))

# -------------------------------------------------------------------------------
# save files
write_csv(last_loc, "clean-data/calfishtrack/2024_last_locations.csv")
saveRDS(last_loc, "clean-data/calfishtrack/2024_last_locations.rds")

# -------------------------------------------------------------------------------

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

