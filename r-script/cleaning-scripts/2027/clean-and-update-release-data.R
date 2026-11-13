# script to clean current release data and bind with historical data
# H. Short
# 2026-11-12 09:59:09
# -------------------------------------------------------------------------------
# load packages
library(tidyverse)
library(janitor)
library(lubridate)

# -------------------------------------------------------------------------------
# read in data
hist = readRDS("clean-data/releases/2024-2026/historical_release_data.rds")

current = read_csv("raw-data/releases/2027/release_blockAA_test.csv") # update csv name when making updates

# -------------------------------------------------------------------------------
# format historical data
hist_clean = hist %>% 
       mutate(water_year = as.factor(water_year), 
              species = as.factor(species), 
              run = as.factor(run), 
              release_date = as.Date(release_date),
              release_time = as.character(release_time),
              release_datetime = parse_date_time(paste(release_date, release_time), 
                                                orders = c("ymd HMS", "ymd HM", "ymd")),
              release_datetime = format(release_datetime, "%Y-%m-%d %H:%M:%S"),
              release_container_id = as.factor(release_container_id), 
              release_id = as.character(release_id),
              crew = as.character(crew),
              morts = as.integer(morts),
              shed_tags = as.integer(shed_tags),
              fish_released = as.integer(fish_released),
              comments = as.character(comments),
              entered_by = as.character(entered_by),
              qaqc_by = as.character(qaqc_by), 
              qaqc_comments = as.character(qaqc_comments),
              block = as.factor(block))

# -------------------------------------------------------------------------------
# clean up new data
current_clean = current %>% 
        # remove empty rows from the bottom of the csv
        remove_empty("rows") %>% 
  
        # clean up names 
        clean_names() %>% 
  
        # mutate data types
        mutate(release_date = mdy(date),
               crew = as.character(crew),
               release_container_id = as.character(container_id),
               release_time = as.character(release_time),
               fish_released = as.integer(number_fish_released),
               morts = as.integer(number_morts),
               shed_tags = as.integer(number_shed_tags),
               comments = as.character(comments),
               entered_by = as.character(entered_by),
               qaqc_by = as.character(qaqc_by),
               qaqc_comments = as.character(qaqc_comments),

              # add in new columns
              water_year = as.factor(2027), 
              species = as.factor("Chinook Salmon"),
              run = as.factor("late-fall"), # make sure to change this for fall run fish
              month = as.character(month(date, label = TRUE)), 
              month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                                "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),

              # create a new column for block
              block = as.factor("Block 1A"), # make sure to change block id!!

              # create a new column for release id
              release_id = paste(block, release_container_id),
              release_datetime = parse_date_time(paste(release_date, release_time), 
                                                orders = c("ymd HMS", "ymd HM", "ymd")),
              release_datetime = format(release_datetime, "%Y-%m-%d %H:%M:%S"),

              # change all empty cells to NA and then take NAs out of comment columns
              across(where(is.character), ~na_if(., "")),
              comments = replace_na(comments, ""),
              qaqc_comments = replace_na(qaqc_comments, "")) %>% 
  
        # rearrange by date and vial to get back to tagging order
        arrange(date, release_id) %>% 
  
        # select columns to include - also rearranges the order of the columns
        select(water_year, species, run, release_date, release_time, release_datetime, release_container_id, release_id, 
               crew, morts, shed_tags, fish_released, comments, entered_by, qaqc_by, qaqc_comments, block)

# -------------------------------------------------------------------------------
# bind clean current data with historical data
release_data = bind_rows(hist_clean, current_clean)

# -------------------------------------------------------------------------------
# write to csv
write_csv(release_data, "clean-data/releases/2027/release_data_updated_mm.dd.yy.csv")

# write to .rds to keep data types in tact
saveRDS(release_data, "clean-data/releases/2027/release_data_updated_mm.dd.yy.rds")

# add to for-app
saveRDS(release_data, "for-app/release_data_updated_mm.dd.yy.rds")

# -------------------------------------------------------------------------------
write_csv(hist_clean, "clean-data/releases/2027/release_data_updated_11.13.2026.csv")
saveRDS(hist_clean, "clean-data/releases/2027/release_data_updated_11.13.2026.rds")

saveRDS(hist_clean, "C:/Users/Hbell/Projects/gssmb-shiny-app/data/release_data_updated_11.13.2026.rds")


