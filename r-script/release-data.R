# Cleaning Release Data
# H. Short
# Tue Jul  7 11:23:39 2026 ------------------------------

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

# fix ids that were written as RT-01 instead of RT-1...etc
fcs_2024_rel[25, 7] = "RT-1"
fcs_2024_rel[26, 7] = "RT-2"
fcs_2024_rel[27, 7] = "RT-3"
fcs_2024_rel[28, 7] = "RT-4"
fcs_2024_rel[29, 7] = "RT-5"
fcs_2024_rel[30, 7] = "RT-6"
fcs_2024_rel[31, 7] = "RT-7"
fcs_2024_rel[32, 7] = "RT-8"
fcs_2024_rel[33, 7] = "RT-9"

# all_2024 ------------------------------------------------------------------------------

release_2024 = rbind(lfcs_2024_rel, fcs_2024_rel)

week_lookup_2024 <- tibble(
  release_date = ymd(c("2023-12-06", "2023-12-07", "2023-12-08", "2023-12-09", # Block 1 # 2024
               "2023-12-13", "2023-12-14", "2023-12-15", "2023-12-16", # Block 2
               "2024-01-10", "2024-01-11", "2024-01-12", "2024-01-13", # Block 3
               "2024-01-17", "2024-01-18", "2024-01-19", "2024-01-20", # Block 4
               "2024-02-14", "2024-02-15", "2024-02-16", "2024-02-17", # Block 5
               "2024-04-17", "2024-04-18", "2024-04-19", "2024-04-20", # Block 6
               "2024-04-24", "2024-04-25", "2024-04-26", "2024-04-27")), #Block 7, no block 8
  
  block = rep(c("Block 1", "Block 2", "Block 3", "Block 4",
                "Block 5", "Block 6", "Block 7"), each = 4)) 

release_2024 = release_2024 %>% 
  left_join(week_lookup_2024, by = "release_date")

release_2024 = release_2024 %>% 
  mutate(release_id = paste(block, release_container_id))

release_2024 = release_2024 %>% 
  slice(-8)

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

# fix ids that were written as RT-01 instead of RT-1...etc
fcs_2025_rel[25, 7] = "RT-1"

# all_2025 ------------------------------------------------------------------------------

release_2025 = rbind(lfcs_2025_rel, fcs_2025_rel)

week_lookup_2025 <- tibble(
  release_date = ymd(c("2024-12-04", "2024-12-05", "2024-12-06", "2024-12-07",   # Block 1 # 2025
                       "2024-12-11", "2024-12-12", "2024-12-13", "2024-12-14",   # Block 2
                       "2025-01-08", "2025-01-09", "2025-01-10", "2025-01-11",   # Block 3
                       "2025-01-15", "2025-01-16", "2025-01-17", "2025-01-18",   # Block 4
                       "2025-02-12", "2025-02-13", "2025-02-14", "2025-02-14",   # Block 5
                       "2025-04-09", "2025-04-10", "2025-04-11", "2025-04-12",   # Block 6
                       "2025-04-16", "2025-04-17", "2025-04-18", "2025-04-19",   # Block 7
                       "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-03")), # Block 8
  
  block = rep(c("Block 1", "Block 2", "Block 3", "Block 4",
                "Block 5", "Block 6", "Block 7", "Block 8"), each = 4)) 

release_2025 = release_2025 %>% 
  left_join(week_lookup_2025, by = "release_date")

release_2025 = release_2025 %>% 
  mutate(release_id = paste(block, release_container_id))

release_2025 = release_2025 %>% 
  slice(-c(100, 108, 124))

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

# all_2026 ------------------------------------------------------------------------------

release_2026 = rbind(lfcs_2026_rel, fcs_2026_rel)

week_lookup_2026 <- tibble(
  release_date = ymd(c("2025-12-02", "2025-12-03", "2025-12-04", "2025-12-05", # Week 1
                       "2025-12-09", "2025-12-10", "2025-12-11", "2025-12-12", # Week 2
                       "2026-01-06", "2026-01-07", "2026-01-08", "2026-01-09", # Week 3
                       "2026-01-13", "2026-01-14", "2026-01-15", "2026-01-16", # Week 4
                       "2026-02-03", "2026-02-04", "2026-02-05", "2026-02-06", # Week 5
                       "2026-02-10", "2026-02-11", "2026-02-12", "2026-02-13", # Week 6
                       "2026-04-21", "2026-04-22", "2026-04-23", "2026-04-24", # Week 7
                       "2026-04-28", "2026-04-29",  "2026-04-30", "2026-05-01")), # Block 8
  
  block = rep(c("Block 1", "Block 2", "Block 3", "Block 4",
                "Block 5", "Block 6", "Block 7", "Block 8"), each = 4)) 

release_2026 = release_2026 %>% 
  left_join(week_lookup_2026, by = "release_date")

release_2026 = release_2026 %>% 
  mutate(release_id = paste(block, release_container_id))

release_2026 = release_2026 %>% 
  slice(-71)

# -----------------------------------------------------
#bind all data together to create one release_data file
release_data = rbind(release_2024, release_2025, release_2026)

# write to csv
write.csv(release_data, "C:/Users/Hbell/Projects/data-project/data-clean/releases/release_data.csv")  
  
