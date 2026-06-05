# create the Sacramento River below Georgiana Slough data frame for merging with release data
# H. Short
# Fri Jun  5 09:33:37 2026 ------------------------------
 
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

# Sacramento River below Georgiana Slough
sac = "GES"

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

# 2024--------------------------------------------------------------------------

# get_cdec_data for discharge at Sac below GS in 2024
sac_dis_24 = get_cdec_data(sac, dis, start_date_24, end_date_24)

# mutate data types and select final df
sac_dis_24 = sac_dis_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Sac below GS in 2024
sac_vel_24 = get_cdec_data(sac, vel, start_date_24, end_date_24)

# mutate data types and select final df
sac_vel_24 = sac_vel_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for temperature at Sac below GS in 2024
sac_temp_24 = get_cdec_data(sac, temp, start_date_24, end_date_24)

# filter out funky values
sac_temp_24 = sac_temp_24 %>% 
  slice(-8030)

# mutate data types and select final df
sac_temp_24 = sac_temp_24 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Sac below GS in 2024
sac_turb_24 = get_cdec_data(sac, turb, start_date_24, end_date_24)

# filter out funky values
sac_turb_24 = sac_turb_24 %>% 
  slice(-8030)

# mutate data types and select final df
sac_turb_24 = sac_turb_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_sac_24 = rbind(sac_dis_24, sac_temp_24, sac_turb_24, sac_vel_24)

# write to a file
write.csv(cdec_sac_24, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2024_sac_water_quality.csv")

# sample plot
ggplot(cdec_sac_24, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()

# 2025--------------------------------------------------------------------------

# get_cdec_data for discharge at Sac below GS in 2025
sac_dis_25 = get_cdec_data(sac, dis, start_date_25, end_date_25)

# mutate data types and select final df
sac_dis_25 = sac_dis_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Sac below GS in 2025
sac_vel_25 = get_cdec_data(sac, vel, start_date_25, end_date_25)

# mutate data types and select final df
sac_vel_25 = sac_vel_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for temperature at Sac below GS in 2025
sac_temp_25 = get_cdec_data(sac, temp, start_date_25, end_date_25)

# mutate data types and select final df
sac_temp_25 = sac_temp_25 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Sac below GS in 2025
sac_turb_25 = get_cdec_data(sac, turb, start_date_25, end_date_25)

# mutate data types and select final df
sac_turb_25 = sac_turb_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_sac_25 = rbind(sac_dis_25, sac_temp_25, sac_turb_25, sac_vel_25)

# write to a file
write.csv(cdec_sac_25, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2025_sac_water_quality.csv")

# sample plot
ggplot(cdec_sac_25, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()

# 2026 -------------------------------------------------------------------------

# get_cdec_data for discharge at Sac below GS in 2026
sac_dis_26 = get_cdec_data(sac, dis, start_date_26, end_date_26)

# mutate data types and select final df
sac_dis_26 = sac_dis_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Sac below GS in 2026
sac_vel_26 = get_cdec_data(sac, vel, start_date_26, end_date_26)

# mutate data types and select final df
sac_vel_26 = sac_vel_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for temperature at Sac below GS in 2026
sac_temp_26 = get_cdec_data(sac, temp, start_date_26, end_date_26)

# mutate data types and select final df
sac_temp_26 = sac_temp_26 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Sac below GS in 2026
sac_turb_26 = get_cdec_data(sac, turb, start_date_26, end_date_26)

# mutate data types and select final df
sac_turb_26 = sac_turb_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Sacramento River below Georgiana Slough", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_sac_26 = rbind(sac_dis_26, sac_temp_26, sac_turb_26, sac_vel_26)

# write to a file
write.csv(cdec_sac_26, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2026_sac_water_quality.csv")

# sample plot
ggplot(cdec_sac_26, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()
