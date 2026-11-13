# Water Quality for Releases
# H. Short
# 2026-11-12 14:18:47

# load packages
library(tidyverse)
library(lunar)
library(dataRetrieval)

# read in release data
release = readRDS("clean-data/releases/2027/release_data_updated_mm.dd.yy.rds")

release = release %>% 
  # add in lunar phase
  mutate(lunar_phase = lunar.phase(release_datetime, name = TRUE),  
         # add new column datetime that rounds release_datetime to the nearest 15 min
         datetime = ceiling_date(release_datetime, "15 minutes")) %>% 
  
  # select columns to include in final data frame
  select(datetime, water_year, species, run, release_datetime, release_date, release_time, lunar_phase, release_id, fish_released, block)

# -------------------------------------------------------------------------------
# create release water quality data

# create a lookup table for the stations and sensors of interest
stations_parameters = tibble::tribble(~station_id,         ~p_code,
                                      "USGS-11447650",     "00060", 
                                      "USGS-11447650",     "72137",
                                      "USGS-11447650",     "00010",
                                      "USGS-11447650",     "63680",) 

# create a lookup table to give the p_code meaning
parameter_lookup = tibble::tribble(~p_code,      ~value_type,    ~units,
                                   "00060",      "Discharge",    "cfs",
                                   "72137", "Discharge, tidally filtered", "cfs",
                                   "00010", "Temperature",       "deg C",
                                   "63680", "Turbidity",         "FNU")

# create a lookup table to give the station_id meaning
station_lookup = tibble::tribble(~station_id,      ~location,
                                 "USGS-11447650",  "Sacramento River at Freeport")

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

# run function -------------------------------------------------------------------------------
# set start and end dates
start_date = "2026-11-01"
end_date = "2026-11-13"

# create df -- you should not need to edit this
usgs_download = stations_parameters %>% 
  mutate(data = map2(station_id, p_code, 
                     ~ possibly(get_USGS_data, otherwise = NULL)(.x, .y, start_date, end_date))) %>% 
  select(-station_id, -p_code) %>% 
  unnest(data)

# -------------------------------------------------------------------------------
# clean up the new df and add more info

# bind dataframe with lookup tables
usgs_download = usgs_download %>% 
  left_join(parameter_lookup, by = "p_code") %>% 
  left_join(station_lookup, by = "station_id")

# clean up df
usgs_download_clean = usgs_download %>% 
  mutate(downloaded_from = "USGS", # makes a new column to show that this data was downloaded from USGS
         
         # edit this to reflect the dates you chose
         water_year = "2027") %>% 
  
  rename(datetime = time) %>% 
  
  # selects and reorders columns
  select(downloaded_from, station_id, location, water_year, datetime, value, value_type, units) 

# -------------------------------------------------------------------------------
# create a test plot
ggplot(usgs_download_clean, aes(x = datetime, y = value, color = value_type)) + 
  facet_grid(rows = vars(value_type), cols = vars(location), scales = "free_y") +
  geom_line(linewidth = 1.2)

# -------------------------------------------------------------------------------
# merge water quality and release data
merged = merge(usgs_download_clean, release, by = "datetime", type = "full")

# clean up
merged = merged %>% 
  # filter out rows where no release took place
  filter(!is.na(release_datetime)) %>% 
  rename(water_year = water_year.x) %>% 
  select(datetime, downloaded_from, station_id, location, water_year, value, value_type, units, 
         species, run, release_datetime, release_date, release_time, lunar_phase, release_id, fish_released, block)

# -------------------------------------------------------------------------------
# create new summary data frame for merged data
summary = merged %>% 
  
  # group by these columns
  group_by(water_year, block, release_id, release_date, release_time, lunar_phase, fish_released, value_type) %>% 
  
  # give value for each value_type
  summarize(value) %>% 
  
  # split release_id into date and id
  separate(release_id, into = c("block", "rel_id"), sep = " (?=RT)") %>%
  
  # take long data and make it wide
  # give dis, vel, temp, turb each their own column that will have one value 
  spread(key = value_type, value = value) %>% 
  
  # mutate data types and rearrange rel_id levels to be in numerical order
  mutate(rel_id = factor(rel_id, levels = c("RT-1", "RT-2", "RT-3", "RT-4", "RT-5", "RT-6", 
                                            "RT-7", "RT-8", "RT-9", "RT-10", "RT-11", "RT-12",
                                            "RT-13", "RT-14", "RT-15", "RT-16", "RT-17", "RT-18", 
                                            "RT-19", "RT-20", "RT-21", "RT-22", "RT-23", "RT-24", "point_rel")))

# -------------------------------------------------------------------------------
# write to csv
write_csv(summary, "clean-data/releases/historical_release_water_quality_summary.csv")

# write to rds
saveRDS(summary, "clean-data/releases/historical_release_water_quality_summary.rds")

# add to for-app
saveRDS(summary, "for-app/historical_release_water_quality_summary.rds")



