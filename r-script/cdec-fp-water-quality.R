# create the Sacramento River at Freeport data frame for merging with release data
# H. Short
# Thu Jun  4 14:24:20 2026 ------------------------------

# load packages
library(CDECRetrieve)
library(tidyverse)

# start and end dates for each season ------------------------------------------

start_date_24 <- "2023-12-01"
end_date_24 <- "2024-07-01"

start_date_25 <- "2024-12-01"
end_date_25 <- "2025-07-01"

start_date_26 <- "2025-12-01"
end_date_26 <- "2026-07-01"

# station ids and sensor codes for CDEC gauges ---------------------------------

# Sacramento River at Freeport, CA
fp = "FPT"

# sensor code for discharge cfs
dis = "20"

# sensor code for velocity ft/s
vel = "21"

# sensor code for temperature deg C
temp = "25"

# sensor code for turbidity FNU
turb = "221"

# ------------------------------------------------------------------------------
#create the function to get cdec data using our tables

get_cdec_data <- function(station, sensor, start_date, end_date) {
  df <- cdec_query(
    station = station, 
    sensor = sensor,
    start_date = start_date, 
    end_date = end_date, 
    dur_code = "E"
  ) %>% 
    mutate(
      station_id = station,
      sensor_num = sensor
    )
  
  return(df)
}

# 2024 -------------------------------------------------------------------------

# get_cdec_data for discharge at Freeport in 2024
fp_dis_24 = get_cdec_data(fp, dis, start_date_24, end_date_24)

# filter out funky values
fp_dis_24 = fp_dis_24 %>% 
  slice(-11197)

# mutate data types and select final df
fp_dis_24 = fp_dis_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Sacramento River at Freeport", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Freeport in 2024
fp_vel_24 = get_cdec_data(fp, vel, start_date_24, end_date_24)

# mutate data types and select final df
fp_vel_24 = fp_vel_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Sacramento River at Freeport", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for temperature at Freeport in 2024
fp_temp_24 = get_cdec_data(fp, temp, start_date_24, end_date_24)

# filter out funky values
fp_temp_24 = fp_temp_24 %>% 
  filter(!parameter_value == 73.2) %>% 
  slice(-c(10853, 10854, 10855))

# mutate data types and select final df
fp_temp_24 = fp_temp_24 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Sacramento River at Freeport", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Freeport in 2024
fp_turb_24 = get_cdec_data(fp, turb, start_date_24, end_date_24)

# filter out funky values 
fp_turb_24 = fp_turb_24 %>% 
  filter(!parameter_value == 0.5) %>% 
  filter(!parameter_value == 1) %>% 
  filter(!parameter_value == 94.4) %>% 
  filter(!parameter_value == 85)

# mutate data types and select final df
fp_turb_24 = fp_turb_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Sacramento River at Freeport", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_fp_24 = rbind(fp_dis_24, fp_temp_24, fp_turb_24, fp_vel_24)

# write to a file
write.csv(cdec_fp_24, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2024_fp_water_quality.csv")

# sample plot
ggplot(cdec_fp_24, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()

# 2025 -------------------------------------------------------------------------

# get_cdec_data for discharge at Freeport in 2025
fp_dis_25 = get_cdec_data(fp, dis, start_date_25, end_date_25)

# mutate data types and select final df
fp_dis_25 = fp_dis_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Sacramento River at Freeport", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Freeport in 2025
fp_vel_25 = get_cdec_data(fp, vel, start_date_25, end_date_25)

# mutate data types and select final df
fp_vel_25 = fp_vel_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Sacramento River at Freeport", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for temperature at Freeport in 2025
fp_temp_25 = get_cdec_data(fp, temp, start_date_25, end_date_25)

# mutate data types and select final df
fp_temp_25 = fp_temp_25 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Sacramento River at Freeport", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Freeport in 2025
fp_turb_25 = get_cdec_data(fp, turb, start_date_25, end_date_25)

# mutate data types and select final df
fp_turb_25 = fp_turb_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Sacramento River at Freeport", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_fp_25 = rbind(fp_dis_25, fp_temp_25, fp_turb_25, fp_vel_25)

# write to a file
write.csv(cdec_fp_25, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2025_fp_water_quality.csv")

# sample plot
ggplot(cdec_fp_25, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()

# 2026 -------------------------------------------------------------------------

# get_cdec_data for discharge at Freeport in 2026
fp_dis_26 = get_cdec_data(fp, dis, start_date_26, end_date_26)

# mutate data types and select final df
fp_dis_26 = fp_dis_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Sacramento River at Freeport", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Freeport in 2026
fp_vel_26 = get_cdec_data(fp, vel, start_date_26, end_date_26)

# filter out funky data
fp_vel_26 = fp_vel_26 %>% 
  filter(!parameter_value == 9.77)

# mutate data types and select final df
fp_vel_26 = fp_vel_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Sacramento River at Freeport", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------
  
# get_cdec_data for temperature at Freeport in 2026
fp_temp_26 = get_cdec_data(fp, temp, start_date_26, end_date_26)

# filter out funky data
fp_temp_26 = fp_temp_26 %>% 
  filter(parameter_value > -1000) %>% 
  filter(parameter_value < 100)
  
# mutate and select to tidy df
fp_temp_26 = fp_temp_26 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Sacramento River at Freeport", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Freeport in 2026
fp_turb_26 = get_cdec_data(fp, turb, start_date_26, end_date_26)

# filter out funky data
fp_turb_26 = fp_turb_26 %>% 
  slice(-15374)

# mutate data types and select final df
fp_turb_26 = fp_turb_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Sacramento River at Freeport", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_fp_26 = rbind(fp_dis_26, fp_temp_26, fp_turb_26, fp_vel_26)

# write to a file
write.csv(cdec_fp_26, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2026_fp_water_quality.csv")

# sample plot
ggplot(cdec_fp_26, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()

