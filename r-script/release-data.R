# Cleaning Release Data
# H. Short
# Mon Jun 22 15:43:46 2026 ------------------------------


# Load  packages
library(stringr)
library(tidyverse)
library(janitor)

# LFCS_2024 -----------------------------------------------------

lfcs_2024_rel = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/releases/2024_lfcs_release.csv")

lfcs_2024_rel = lfcs_2024_rel %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>% 
  
  # manually rename columns
  rename(release_container_id = container_id, 
         release_time = time_of_release, 
         morts = number_of_mortalities, 
         shed_tags = number_of_shed_tags, 
         fish_released = number_of_fish_released,
         qaqc_by = qaqc_ed_by) %>%
  
  # add in columns for water year, species, run, and datetime
  mutate(water_year = as.factor(2024),
         species = as.factor("Chinook Salmon"),
         run = as.factor("late-fall"),
         release_date = mdy(date),
         release_datetime = as.POSIXct(paste(release_date, release_time), format = "%Y-%m-%d %H:%M"),
         release_container_id = as.factor(release_container_id),
         release_id = paste(date, release_container_id)) %>% 
  
  # select columns to include in final data frame
  select(water_year, species, run, release_date, release_time, release_datetime, release_container_id, release_id,
         crew, morts, shed_tags, fish_released, comments, entered_by, qaqc_by, qaqc_comments)

# manually assign NAs to release_id column when releases did not take place
lfcs_2024_rel[8,7] <- NA
lfcs_2024_rel[8,8] <- NA
lfcs_2024_rel[8,5] <- NA
lfcs_2024_rel[8,9] <- NA

# write clean csv
write.csv(lfcs_2024_rel, "C:/Users/Hbell/Projects/data-project/data-clean/releases/2024_LFCS_release_clean.csv")

# FCS_2024 -----------------------------------------------------

fcs_2024_rel = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/releases/2024_fcs_release.csv")

fcs_2024_rel = fcs_2024_rel %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>% 
  
  # manually rename columns
  rename(release_container_id = container_id, 
         release_time = time_of_release, 
         morts = number_of_mortalities, 
         shed_tags = number_of_shed_tags, 
         fish_released = number_of_fish_released,
         qaqc_by = qaqc_ed_by) %>%
  
  # add in columns for water year, species, run, and datetime
  mutate(water_year = as.factor(2024),
         species = as.factor("Chinook Salmon"),
         run = as.factor("fall"),
         release_date = mdy(date),
         release_datetime = as.POSIXct(paste(release_date, release_time), format = "%Y-%m-%d %H:%M"),
         release_container_id = as.factor(release_container_id),
         release_id = paste(date, release_container_id)) %>% 
  
  # select columns to include in final data frame
  select(water_year, species, run, release_date, release_time, release_datetime, release_container_id, release_id,
         crew, morts, shed_tags, fish_released, comments, entered_by, qaqc_by, 
         qaqc_comments)

# write clean csv
write.csv(fcs_2024_rel, "C:/Users/Hbell/Projects/data-project/data-clean/releases/2024_FCS_release_clean.csv")

# LFCS_2025 -----------------------------------------------------

lfcs_2025_rel = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/releases/2025_lfcs_release.csv")

lfcs_2025_rel = lfcs_2025_rel %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>% 
  
  # manually rename columns
  rename(release_container_id = container_id,
         morts = number_mortalities, 
         shed_tags = number_shed_tags, 
         fish_released = number_fish_released) %>%
  
   # change fish haul trailer to point release
  mutate(release_container_id = case_when(release_container_id == "Fish Haul Trailer" ~ "point_rel", 
                                          TRUE ~ release_container_id)) %>% 
  
  # add in columns for water year, species, run, and datetime
  mutate(water_year = as.factor(2025),
         species = as.factor("Chinook Salmon"),
         run = as.factor("late-fall"),
         release_date = mdy(date),
         release_datetime = as.POSIXct(paste(release_date, release_time), format = "%Y-%m-%d %H:%M"),
         release_container_id = as.factor(release_container_id),
         release_id = paste(date, release_container_id)) %>%
  
  # select columns to include in final data frame
  select(water_year, species, run, release_date, release_time, release_datetime, release_container_id, release_id,
         crew, morts, shed_tags, fish_released, comments, entered_by, qaqc_by, 
         qaqc_comments)

# write clean csv
write.csv(lfcs_2025_rel, "C:/Users/Hbell/Projects/data-project/data-clean/releases/2025_LFCS_release_clean.csv")

# FCS_2025 -----------------------------------------------------

fcs_2025_rel = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/releases/2025_fcs_release.csv")

# manually fix N/A to NA
fcs_2025_rel[fcs_2025_rel == "N/A"] <- NA

fcs_2025_rel = fcs_2025_rel %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>% 
  
  # manually rename columns
  rename(release_container_id = container_id,
         morts = number_mortalities, 
         shed_tags = number_shed_tags, 
         fish_released = number_fish_released) %>%
  
  # add in columns for water year, species, run, and datetime
  mutate(water_year = as.factor(2025),
         species = as.factor("Chinook Salmon"),
         run = as.factor("fall"),
         release_date = mdy(date),
         release_datetime = as.POSIXct(paste(release_date, release_time), format = "%Y-%m-%d %H:%M"),
         release_container_id = as.factor(release_container_id),
         release_id = paste(date, release_container_id)) %>%
  
  # select columns to include in final data frame
  select(water_year, species, run, release_date, release_time, release_datetime, release_container_id, release_id,
         crew, morts, shed_tags, fish_released, comments, entered_by, qaqc_by, 
         qaqc_comments)


# manually assign NAs to release_id column when releases did not take place
fcs_2025_rel[8,7] = NA
fcs_2025_rel[8,8] = NA
fcs_2025_rel[24,7] = NA
fcs_2025_rel[24,8] = NA

# write final csv
write.csv(fcs_2025_rel, "C:/Users/Hbell/Projects/data-project/data-clean/releases/2025_FCS_release_clean.csv")

# LFCS_2026 -----------------------------------------------------

lfcs_2026_rel = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/releases/2026_lfcs_release.csv")

# add RT- to release container id since it was not typed like previous years
lfcs_2026_rel$RT = "RT-"

# paste RT- with container ids to match previous years release_id
lfcs_2026_rel$release_container_id = as.factor(paste(lfcs_2026_rel$RT, lfcs_2026_rel$containerID, sep = ""))

lfcs_2026_rel = lfcs_2026_rel %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>% 
  
  # manually rename columns
  rename( morts = number_mortalities, 
         shed_tags = number_shed_tags, 
         fish_released = number_fish_released) %>%
  
  # add in columns for water year, species, run, and datetime
  mutate(water_year = as.factor(2026),
         species = as.factor("Chinook Salmon"),
         run = as.factor("late-fall"),
         release_date = mdy(date),
         release_datetime = as.POSIXct(paste(release_date, release_time), format = "%Y-%m-%d %H:%M"),
         release_id = paste(date, release_container_id)) %>%
  
  # select columns to include in final data frame
  select(water_year, species, run, release_date, release_time, release_datetime, release_container_id, release_id,
         crew, morts, shed_tags, fish_released, comments, entered_by, qaqc_by, 
         qaqc_comments)

# manually assign NAs to release_id column when releases did not take place
lfcs_2026_rel[71,7] = NA
lfcs_2026_rel[71,8] = NA

write.csv(lfcs_2026_rel, "C:/Users/Hbell/Projects/data-project/data-clean/releases/2026_LFCS_release_clean.csv")

# FCS_2026 -----------------------------------------------------

fcs_2026_rel = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/releases/2026_fcs_release.csv")

# add RT- to release container id since it was not typed like previous years
fcs_2026_rel$RT = "RT-"

# paste RT- with container ids to match previous years release_id
fcs_2026_rel$release_container_id = as.factor(paste(fcs_2026_rel$RT, fcs_2026_rel$containerID, sep = ""))

fcs_2026_rel = fcs_2026_rel %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>% 
  
  # manually rename columns
  rename( morts = number_mortalities, 
          shed_tags = number_shed_tags, 
          fish_released = number_fish_released) %>%
  
  # add in columns for water year, species, run, and datetime
  mutate(water_year = as.factor(2026),
         species = as.factor("Chinook Salmon"),
         run = as.factor("fall"),
         release_date = mdy(date),
         release_datetime = as.POSIXct(paste(release_date, release_time), format = "%Y-%m-%d %H:%M"),
         release_id = paste(date, release_container_id)) %>%
  
  # select columns to include in final data frame
  select(water_year, species, run, release_date, release_time, release_datetime, release_container_id, release_id,
         crew, morts, shed_tags, fish_released, comments, entered_by, qaqc_by, 
         qaqc_comments)

write.csv(fcs_2026_rel, "C:/Users/Hbell/Projects/data-project/data-clean/releases/2026_fcs_release_clean.csv")

# -----------------------------------------------------
#bind all data together to create one release_data file
release_data = rbind(lfcs_2024_rel, fcs_2024_rel, lfcs_2025_rel, fcs_2025_rel, lfcs_2026_rel, fcs_2026_rel)

# write to csv
write.csv(release_data, "C:/Users/Hbell/Projects/data-project/data-clean/releases/release_data.csv")  
  
