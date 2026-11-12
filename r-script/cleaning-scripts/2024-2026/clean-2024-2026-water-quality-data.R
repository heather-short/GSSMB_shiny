# Clean historical (2024-2026) water quality data for shiny app
# H. Short
# 2026-08-19 08:51:20
# -------------------------------------------------------------------------------

# load packages
library(dataRetrieval)
library(tidyverse)

# -------------------------------------------------------------------------------
# get discharge data from USGS
# -------------------------------------------------------------------------------
# create a lookup table for the stations and sensors of interest
stations_parameters = tibble::tribble(~station_id,         ~p_code,
                                      "USGS-11447650",     "00060", 
                                      "USGS-11447650",     "72137", 
                                      "USGS-11447905",     "00060", 
                                      "USGS-11447905",     "72137") 

# create a lookup table to give the p_code meaning
parameter_lookup = tibble::tribble(~p_code,      ~value_type,    ~units,
                                   "00060",      "Discharge",    "cfs",
                                   "72137", "Discharge, tidally filtered", "cfs")

# create a lookup table to give the station_id meaning
station_lookup = tibble::tribble(~station_id,      ~location,
                                 "USGS-11447650",  "Sacramento River at Freeport", 
                                 "USGS-11447905", "Sacramento River below Georgiana Slough")

# -------------------------------------------------------------------------------
# create USGS retrieve function
get_USGS_data <- function(monitoring_location_id, parameter_code, start_date, end_date) {
  
  df <- read_waterdata_continuous(monitoring_location_id = monitoring_location_id,
                                  parameter_code = parameter_code, 
                                  time = c(start_date, end_date)) %>% 
            mutate(station_id = monitoring_location_id,
                   p_code = parameter_code)
  
  return(df)
  
} # end function

# 2024 -------------------------------------------------------------------------------
# set start and end dates
start_date = "2023-12-01"
end_date = "2024-07-01"

# create df -- you should not need to edit this
usgs_2024 = stations_parameters %>% 
  mutate(data = map2(station_id, p_code, 
                     ~ possibly(get_USGS_data, otherwise = NULL)(.x, .y, start_date, end_date))) %>% 
  select(-station_id, -p_code) %>% 
  unnest(data)

# -------------------------------------------------------------------------------
# clean up the new df and add more info

# bind dataframe with lookup tables
usgs_2024 = usgs_2024 %>% 
  left_join(parameter_lookup, by = "p_code") %>% 
  left_join(station_lookup, by = "station_id")

# clean up df
usgs_2024_clean = usgs_2024 %>% 
  mutate(downloaded_from = "USGS", # makes a new column to show that this data was downloaded from USGS
         
         # edit this to reflect the dates you chose
         water_year = "2024") %>% 
  
  rename(datetime = time) %>% 
  
  # selects and reorders columns
  select(downloaded_from, station_id, location, water_year, datetime, value, value_type, units) 

# -------------------------------------------------------------------------------
# create a test plot
ggplot(usgs_2024_clean, aes(x = datetime, y = value, color = value_type)) + 
  facet_grid(rows = vars(value_type), cols = vars(location), scales = "free_y") +
  geom_line(linewidth = 1.2)

# 2025 -------------------------------------------------------------------------------
# set start and end dates
start_date = "2024-12-01"
end_date = "2025-07-01"

# create df -- you should not need to edit this
usgs_2025 = stations_parameters %>% 
  mutate(data = map2(station_id, p_code, 
                     ~ possibly(get_USGS_data, otherwise = NULL)(.x, .y, start_date, end_date))) %>% 
  select(-station_id, -p_code) %>% 
  unnest(data)

# -------------------------------------------------------------------------------
# clean up the new df and add more info

# bind dataframe with lookup tables
usgs_2025 = usgs_2025 %>% 
  left_join(parameter_lookup, by = "p_code") %>% 
  left_join(station_lookup, by = "station_id")

# clean up df
usgs_2025_clean = usgs_2025 %>% 
  mutate(downloaded_from = "USGS", # makes a new column to show that this data was downloaded from USGS
         
         # edit this to reflect the dates you chose
         water_year = "2025") %>% 
  
  rename(datetime = time) %>% 
  
  # selects and reorders columns
  select(downloaded_from, station_id, location, water_year, datetime, value, value_type, units) 

# -------------------------------------------------------------------------------
# create a test plot
ggplot(usgs_2025_clean, aes(x = datetime, y = value, color = value_type)) + 
  facet_grid(rows = vars(value_type), cols = vars(location), scales = "free_y") +
  geom_line(linewidth = 1.2)

# 2026 -------------------------------------------------------------------------------
# set start and end dates
start_date = "2025-12-01"
end_date = "2026-07-01"

# create df -- you should not need to edit this
usgs_2026 = stations_parameters %>% 
  mutate(data = map2(station_id, p_code, 
                     ~ possibly(get_USGS_data, otherwise = NULL)(.x, .y, start_date, end_date))) %>% 
  select(-station_id, -p_code) %>% 
  unnest(data)

# -------------------------------------------------------------------------------
# clean up the new df and add more info

# bind dataframe with lookup tables
usgs_2026 = usgs_2026 %>% 
  left_join(parameter_lookup, by = "p_code") %>% 
  left_join(station_lookup, by = "station_id")

# clean up df
usgs_2026_clean = usgs_2026 %>% 
  mutate(downloaded_from = "USGS", # makes a new column to show that this data was downloaded from USGS
         
         # edit this to reflect the dates you chose
         water_year = "2026") %>% 
  
  rename(datetime = time) %>% 
  
  # selects and reorders columns
  select(downloaded_from, station_id, location, water_year, datetime, value, value_type, units) 

# -------------------------------------------------------------------------------
# create a test plot
ggplot(usgs_2026_clean, aes(x = datetime, y = value, color = value_type)) + 
  facet_grid(rows = vars(value_type), cols = vars(location), scales = "free_y") +
  geom_line(linewidth = 1.2)

# -------------------------------------------------------------------------------
# bind all usgs_cleans together
usgs_clean = bind_rows(usgs_2024_clean, usgs_2025_clean, usgs_2026_clean)

# -------------------------------------------------------------------------------
# read in cdec data - Freeport and GS

# read in freeport historical data and remove discharge
cdec_clean_fp = readRDS("clean-data/water-quality/2024-2026/historical_fp_water_quality.rds") %>% 
  filter(value_type != "discharge") %>% 
  filter(value_type != "velocity") %>% 
  rename("downloaded_from" = "agency",
         "station_id" = "location_id")

# read in GS historical data and remove discharge
cdec_clean_gs = readRDS("clean-data/water-quality/2024-2026/historical_gs_water_quality.rds") %>% 
  filter(value_type != "discharge") %>% 
  filter(value_type != "velocity") %>% 
  rename("downloaded_from" = "agency",
         "station_id" = "location_id")

# -------------------------------------------------------------------------------
# bind cdec and usgs dfs together
cdec_usgs = bind_rows(usgs_clean, cdec_clean_fp, cdec_clean_gs) %>% 
  mutate(value_type = recode(value_type, 
                             temperature = "Temperature",
                             turbidity = "Turbidity"))

# test plot
ggplot(cdec_usgs, aes(x = datetime, y = value, color = value_type)) + 
  facet_grid(rows = vars(value_type), cols = vars(location), scales = "free_y") +
  geom_line(linewidth = 1.2)

# -------------------------------------------------------------------------------
# write csv
write_csv(cdec_usgs, "clean-data/water-quality/historical_water_quality.csv")

# write rds
saveRDS(cdec_usgs, "clean-data/water-quality/historical_water_quality.rds")
