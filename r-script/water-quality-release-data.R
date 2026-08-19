# Water Quality for Releases
# H. Short
# Thu Jun  4 13:49:19 2026 ------------------------------

# load packages
library(tidyverse)
library(lubridate)

# read in data for Freeport station from CDEC through the years
fp_24 = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/water-quality/cdec_2024_fp_water_quality.csv")
fp_25 = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/water-quality/cdec_2025_fp_water_quality.csv")
fp_26 = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/water-quality/cdec_2026_fp_water_quality.csv")

# bind all df together
fp_all = rbind(fp_24, fp_25, fp_26)

# mutate data types 
fp_all = fp_all %>% 
  mutate(agency = as.factor(agency), 
       location_id = as.factor(location_id), 
       location = as.factor(location), 
       water_year = as.factor(water_year), 
       datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"))

# test plot
ggplot(fp_all, aes(x = datetime, y = value, color = water_year)) + 
  facet_wrap(~value_type, scales = "free_y") +
  geom_point()

# merge release data with water quality data------------------------------------

# read in release data
rel = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/releases/release_data.csv")

rel = rel %>% 
  
  # mutate data types
  mutate(water_year = as.factor(water_year), 
         run = as.factor(run),
         release_datetime = as.POSIXct(release_datetime, format="%Y-%m-%d %H:%M:%S"),
         release_id = as.character(release_id),
         lunar_phase = lunar.phase(release_datetime, name = TRUE), 
         release_date = date(release_datetime)) %>% 
  
  # add new column datetime that rounds release_datetime to the nearest 15 min
  mutate(datetime = ceiling_date(release_datetime, "15 minutes")) %>% 
  
  # select columns to include in final data frame
  select(datetime, water_year, species, run, release_datetime, release_date, release_time, lunar_phase, release_id, fish_released)


# merge fp_all water quality data and rel release data
fp_rel = merge(fp_all, rel, by = "datetime", type = "full")

fp_rel = fp_rel %>% 
  
  # filter out rows where no release took place
  filter(!is.na(release_datetime)) %>% 
  
  # rename to water_year
  mutate(water_year = water_year.x) %>% 
  
  # select columns to include in final data frame
  select(datetime, agency, location_id, location, water_year, value, value_type, units, species, run, release_datetime, release_date, release_time, lunar_phase, release_id, fish_released)

# test plot only shows water quality for dates when releases occurred
ggplot(fp_rel, aes(x = datetime, y = value, color = water_year)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_point()

# create new summary data frame for merged data---------------------------------

fp_rel_sum = fp_rel %>% 
  
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

fp_rel_sum[259, 3] = "point_rel"
fp_rel_sum[260, 3] = "point_rel"
fp_rel_sum[261, 3] = "point_rel"


fp_rel_sum = fp_rel_sum %>% 
  
  # add new column that says what month the release took place
  mutate(month = month(release_date, label = TRUE)) %>% 
  
  # rearrange month levels to be in water_year order
  mutate(month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                          "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")))

# write to csv
write.csv(fp_rel_sum, "C:/Users/Hbell/Projects/data-project/data-clean/releases/release_water_quality_summary.csv")



