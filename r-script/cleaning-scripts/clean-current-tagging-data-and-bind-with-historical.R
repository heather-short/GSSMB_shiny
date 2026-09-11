# script to clean current tagging data and bind with historical data
# H. Short
# 2026-09-08 13:19:22
# -------------------------------------------------------------------------------
# load packages
library(tidyverse)
library(janitor)
library(lubridate)

# -------------------------------------------------------------------------------
# read in data
hist = read_csv("clean-data/tagging/historical_tagging_data.csv",
                col_types = cols(anesthetic_time_mmss = col_character(),
                                    surgery_time_mmss = col_character(),
                                    recovery_time_mmss = col_character()))

current = read_csv("sheet-templates/tagging-data-entry-test.csv", # update csv name
                   col_types = cols(anesthetic_time_mmss = col_character(),
                                    surgery_time_mmss = col_character(),
                                    recovery_time_mmss = col_character()))

# -------------------------------------------------------------------------------
# format historical data
hist_clean = hist %>% 
       mutate(water_year = as.factor(water_year), 
              date = ymd(date), 
              species = as.factor(species), 
              run = as.factor(run), 
              source_tank_id = as.factor(source_tank_id),
              surgeon = as.factor(surgeon), 
              vial_number = as.character(vial_number),
              weight_gr = as.numeric(weight_gr),
              fork_length_mm = as.integer(fork_length_mm),
              tag_id_hex = as.character(tag_id_hex), 
              tag_activation_date = ymd(tag_activation_date),
              anesthetic_time_sec = as.integer(anesthetic_time_sec),
              surgery_time_sec = as.integer(surgery_time_sec), 
              recovery_time_sec = as.integer(recovery_time_sec),
              comments = as.character(comments),
              release_container_id = as.character(release_container_id),
              release_id = as.character(release_id),
              post_tagging_tank = as.factor(post_tagging_tank),
              qaqc_comments = as.character(qaqc_comments),
              block = as.factor(block),
              month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                                "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")))

# -------------------------------------------------------------------------------
# clean up new data
current_clean = current %>% 
        # remove empty rows from the bottom of the csv
        remove_empty("rows") %>% 
  
        # mutate data types
        mutate(date = mdy(date),
              source_tank_id = as.factor(source_tank_id), 
              surgeon = as.factor(surgeon),
              vial_number = as.character(vial_number),
              weight_gr = as.numeric(weight_gr),
              fork_length_mm = as.integer(fork_length_mm), 
              tag_activation_date = mdy(tag_activation_date),
              post_tagging_tank = as.factor(post_tagging_tank),
              comments = as.character(comments),
              qaqc_comments = as.character(qaqc_comments),

              # add in new columns
              water_year = as.factor(2027), 
              species = as.factor("Chinook Salmon"),
              run = as.factor("late-fall"), # make sure to change this for fall run fish
              month = as.character(month(date, label = TRUE)), 
              month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                                "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),

              block = as.factor("Block 1"), # make sure to change block id!!

              # create new columns converting mm:ss to sec
              anesthetic_time_sec = period_to_seconds(ms(anesthetic_time_mmss)), 
              surgery_time_sec = period_to_seconds(ms(surgery_time_mmss)),
              recovery_time_sec = period_to_seconds(ms(recovery_time_mmss)),

              # create a new column for release id
              release_id = paste((date + 1), release_container_id),

              # change all empty cells to NA and then take NAs out of comment columns
              across(where(is.character), ~na_if(., "")),
              comments = replace_na(comments, ""),
              qaqc_comments = replace_na(qaqc_comments, "")) %>% 
  
        # rearrange by date and vial to get back to tagging order
        arrange(date, as.numeric(vial_number)) %>% 
  
        # select columns to include - also rearranges the order of the columns
        select(water_year, month, date, species, run, source_tank_id, surgeon, crew, vial_number, 
              weight_gr, fork_length_mm, tag_id_hex, tag_activation_date, anesthetic_time_mmss, anesthetic_time_sec,
              surgery_time_mmss, surgery_time_sec, recovery_time_mmss, recovery_time_sec, comments, release_container_id, 
              release_id, post_tagging_tank, entered_by, qaqc_by, qaqc_comments, block)

# -------------------------------------------------------------------------------
# bind clean current data with historical data
tagging_data = bind_rows(hist_clean, current_clean)

# -------------------------------------------------------------------------------
# write to csv
write_csv(tagging_data, "clean-data/tagging/hist_and_current_tagging_data.csv")

# -------------------------------------------------------------------------------
# write to .rds to keep data types in tact
saveRDS(tagging_data, "clean-data/tagging/hist_and_current_tagging_data.rds")


