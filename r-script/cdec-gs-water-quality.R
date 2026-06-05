# create the Georgiana Slough at Sacramento River data frame using CDEC data
# H. Short 
# Thu Jun  4 15:02:11 2026 ------------------------------

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

# Georgiana Slough at Sacramento River
gs = "GES"

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

# get_cdec_data for discharge at Georgiana Slough in 2024
gs_dis_24 = get_cdec_data(gs, dis, start_date_24, end_date_24)

# mutate data types and select final df
gs_dis_24 = gs_dis_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Georgiana Slough in 2024
gs_vel_24 = get_cdec_data(gs, vel, start_date_24, end_date_24)

# mutate data types and select final df
gs_vel_24 = gs_vel_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for temperature at Georgiana Slough in 2024
gs_temp_24 = get_cdec_data(gs, temp, start_date_24, end_date_24)

# filter out funky values
gs_temp_24 = gs_temp_24 %>% 
  slice(-8030)

# mutate data types and select final df
gs_temp_24 = gs_temp_24 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Georgiana Slough in 2024
gs_turb_24 = get_cdec_data(gs, turb, start_date_24, end_date_24)

# filter out funky values
gs_turb_24 = gs_turb_24 %>% 
  slice(-c(2073, 2094, 2095, 2096, 2097, 2098, 2099, 2100, 2101, 2102, 
           8030))

# mutate data types and select final df
gs_turb_24 = gs_turb_24 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2024") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_gs_24 = rbind(gs_dis_24, gs_temp_24, gs_turb_24, gs_vel_24)

# write to a file
write.csv(cdec_gs_24, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2024_gs_water_quality.csv")

# sample plot
ggplot(cdec_gs_24, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()

# 2025 -------------------------------------------------------------------------

# get_cdec_data for discharge at Georgiana Slough in 2025
gs_dis_25 = get_cdec_data(gs, dis, start_date_25, end_date_25)

# mutate data types and select final df
gs_dis_25 = gs_dis_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Georgiana Slough in 2025
gs_vel_25 = get_cdec_data(gs, vel, start_date_25, end_date_25)

# mutate data types and select final df
gs_vel_25 = gs_vel_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for temperature at Georgiana Slough in 2025
gs_temp_25 = get_cdec_data(gs, temp, start_date_25, end_date_25)

# mutate data types and select final df
gs_temp_25 = gs_temp_25 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Georgiana Slough in 2025
gs_turb_25 = get_cdec_data(gs, turb, start_date_25, end_date_25)

# mutate data types and select final df
gs_turb_25 = gs_turb_25 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2025") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_gs_25 = rbind(gs_dis_25, gs_temp_25, gs_turb_25, gs_vel_25)

# write to a file
write.csv(cdec_gs_25, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2025_gs_water_quality.csv")

# sample plot
ggplot(cdec_gs_25, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()

# 2026 water quality at Georgiana Slough -----------------------------------------------

# get_cdec_data for discharge at Georgiana Slough in 2026
gs_dis_26 = get_cdec_data(gs, dis, start_date_26, end_date_26)

# mutate data types and select final df
gs_dis_26 = gs_dis_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "discharge",
         units = "cfs", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for velocity at Georgiana Slough in 2026
gs_vel_26 = get_cdec_data(gs, vel, start_date_26, end_date_26)

# mutate data types and select final df
gs_vel_26 = gs_vel_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "velocity",
         units = "ft/s", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for temperature at Georgiana Slough in 2026
gs_temp_26 = get_cdec_data(gs, temp, start_date_26, end_date_26)

# mutate data types and select final df
gs_temp_26 = gs_temp_26 %>% 
  mutate(agency = "CDEC",
         value = round((parameter_value-32)*(5/9), 1),
         value_type = "temperature",
         units = "°C", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# get_cdec_data for turbidity at Georgiana Slough in 2026
gs_turb_26 = get_cdec_data(gs, turb, start_date_26, end_date_26)

# mutate data types and select final df
gs_turb_26 = gs_turb_26 %>% 
  mutate(agency = "CDEC",
         value = parameter_value,
         value_type = "turbidity",
         units = "FNU", 
         location = "Georgiana Slough at Sacramento River", 
         water_year = "2026") %>% 
  select(agency, location_id, location, water_year, datetime, value, value_type, units)

# -----------------------------------------------------

# bind all into one df
cdec_gs_26 = rbind(gs_dis_26, gs_temp_26, gs_turb_26, gs_vel_26)

# write to a file
write.csv(cdec_gs_26, "C:/Users/Hbell/Projects/data-project/data-clean/cdec_2026_gs_water_quality.csv")

# sample plot
ggplot(cdec_gs_26, aes(x = datetime, y = value, color = value_type)) + 
  facet_wrap(~value_type, scales = "free_y") + 
  geom_line()
