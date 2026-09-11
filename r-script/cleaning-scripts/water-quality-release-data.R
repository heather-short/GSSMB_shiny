# Water Quality for Releases
# H. Short
# Thu Jun  4 13:49:19 2026 ------------------------------

# load packages
library(tidyverse)
library(lubridate)
library(lunar)

# read in release data
release = readRDS("clean-data/releases/historical_release_data.rds")

release = release %>% 
  # add in lunar phase
  mutate(lunar_phase = lunar.phase(release_datetime, name = TRUE),  
         # add new column datetime that rounds release_datetime to the nearest 15 min
         datetime = ceiling_date(release_datetime, "15 minutes")) %>% 
  
  # select columns to include in final data frame
  select(datetime, water_year, species, run, release_datetime, release_date, release_time, lunar_phase, release_id, fish_released)

# -------------------------------------------------------------------------------
# read in water quality data
freeport = readRDS("clean-data/water-quality/historical_water_quality.rds") %>% 
  filter(location == "Sacramento River at Freeport")

# -------------------------------------------------------------------------------
# merge water quality and release data
merged = merge(freeport, release, by = "datetime", type = "full")

# clean up
merged = merged %>% 
  # filter out rows where no release took place
  filter(!is.na(release_datetime)) %>% 
  filter(value_type != "Discharge, tidally filtered") %>% 
  rename(water_year = water_year.x) %>% 
  select(datetime, downloaded_from, station_id, location, water_year, value, value_type, units, 
         species, run, release_datetime, release_date, release_time, lunar_phase, release_id, fish_released)

# test plot
ggplot(merged, aes(x = datetime, y = value, color = water_year)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_point()

# create new summary data frame for merged data---------------------------------

summary = merged %>% 
  
  # group by these columns
  group_by(water_year, release_id, release_date, release_time, lunar_phase, fish_released, value_type) %>% 
  
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

summary[264, 3] = "point_rel"
summary[265, 3] = "point_rel"
summary[266, 3] = "point_rel"


summary = summary %>% 
  
  # add new column that says what month the release took place
  mutate(month = month(release_date, label = TRUE)) %>% 
  
  # rearrange month levels to be in water_year order
  mutate(month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                          "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")))

# write to csv
write_csv(summary, "clean-data/releases/historical_release_water_quality_summary.csv")

# write to rds
saveRDS(summary, "clean-data/releases/historical_release_water_quality_summary.rds")



