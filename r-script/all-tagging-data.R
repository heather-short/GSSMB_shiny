# Cleaning Tagging Data
# H. Short
# Fri Jun  5 12:00:25 2026 ------------------------------

# Load  packages
library(stringr)
library(tidyverse)
library(janitor)
library(readr)

# LFCS_2024 --------------------------------------------------------------------

# read in raw data file
LFCS_2024 = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tagging/2024_LFCS.csv")

# clean up file
LFCS_2024_clean = LFCS_2024 %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>%
  
  # manually rename columns we want to keep
  rename(tag_id_hex = acoustic_tag_id,
         anesthetic_time_mmss = time_in_anesthetic_mm_ss,
         qaqc_by = qaqc_ed_by, 
         recovery_time_mmss = time_to_recover_mm_ss,
         surgery_time_mmss = time_in_surgery_mm_ss,
         weight_gr = weight_g) %>% 
  
  # format data to appropriate classes
  mutate(date = mdy(date),
         source_tank_id = as.factor(source_tank_id), 
         surgeon = as.factor(surgeon),
         vial_number = as.character(vial_number),
         weight_gr = as.numeric(weight_gr),
         fork_length_mm = as.integer(fork_length_mm), 
         tag_activation_date = mdy(tag_activation_date),
    
         # create new columns for water year, species, run, and month and assign 
         # their values and order
         water_year = as.factor(2024),
         species = as.factor("Chinook Salmon"),
         run = as.factor("late-fall"), 
         month = as.factor(month(date, label = TRUE)), 
         month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),
    
         # create new columns that convert stopwatch timers to seconds
         anesthetic_time_sec = period_to_seconds(ms(anesthetic_time_mmss)), 
         surgery_time_sec = period_to_seconds(ms(surgery_time_mmss)) ,
         recovery_time_sec = period_to_seconds(ms(recovery_time_mmss)),
    
         # create new columns for release id and caba tank id
         release_id = paste((date+1), release_container_id),
         post_tagging_tank = as.factor(NA),
     
         # convert empty cells to NA and then remove all NAs from comment columns
         across(where(is.character), ~na_if(., "")),
         comments = replace_na(comments, ""),
         qaqc_comments = replace_na(qaqc_comments, "")) %>% 
  
  # arrange by date and then vial_number to get back to the order in which fish 
  # were tagged
  arrange(date, as.numeric(vial_number)) %>% 
  
  # select columns to include and reorder
  select(water_year, month, date, species, run, source_tank_id, surgeon, crew, vial_number, 
         weight_gr, fork_length_mm, tag_id_hex, tag_activation_date, anesthetic_time_mmss, anesthetic_time_sec,
         surgery_time_mmss, surgery_time_sec, recovery_time_mmss, recovery_time_sec, comments, release_container_id, 
         release_id, post_tagging_tank, entered_by, qaqc_by, qaqc_comments)

write.csv(LFCS_2024_clean, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/2024_LFCS_tagging_clean.csv")

# FCS_2024 ---------------------------------------------------------------------

FCS_2024 = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tagging/2024_FCS.csv")

# clean up file
FCS_2024_clean = FCS_2024 %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>%
  
  # manually rename columns we want to keep
  rename(tag_id_hex = acoustic_tag_id,
         anesthetic_time_mmss = time_in_anesthetic_mm_ss,
         qaqc_by = qaqc_ed_by, 
         recovery_time_mmss = time_to_recover_mm_ss,
         surgery_time_mmss = time_in_surgery_mm_ss,
         weight_gr = weight_g) %>% 
  
  # format data to appropriate classes
  mutate(date = mdy(date),
         source_tank_id = as.factor(source_tank_id), 
         surgeon = as.factor(surgeon),
         vial_number = as.character(vial_number),
         weight_gr = as.numeric(weight_gr),
         fork_length_mm = as.integer(fork_length_mm), 
         tag_activation_date = mdy(tag_activation_date),
    
         # create new columns for water year, species, run, and month and assign 
         # their values and order
         water_year = as.factor(2024),
         species = as.factor("Chinook Salmon"),
         run = as.factor("fall"), 
         month = as.factor(month(date, label = TRUE)), 
         month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),
    
         # create new columns that convert stopwatch timers to seconds
         anesthetic_time_sec = period_to_seconds(ms(anesthetic_time_mmss)), 
         surgery_time_sec = period_to_seconds(ms(surgery_time_mmss)),
         recovery_time_sec = period_to_seconds(ms(recovery_time_mmss)),
         
         # create new columns for release id and caba tank id
         release_id = paste((date+1), release_container_id),
         post_tagging_tank = as.factor(NA),
        
         # convert empty cells to NA and then remove all NAs from comment columns
         across(where(is.character), ~na_if(., "")),
         comments = replace_na(comments, ""),
         qaqc_comments = replace_na(qaqc_comments, "")) %>% 
  
  # arrange by date and then vial_number to get back to the order in which fish 
  # were tagged
  arrange(date, as.numeric(vial_number)) %>% 
  
  # select columns to include and reorder
  select(water_year, month, date, species, run, source_tank_id, surgeon, crew, vial_number, 
         weight_gr, fork_length_mm, tag_id_hex, tag_activation_date, anesthetic_time_mmss, anesthetic_time_sec,
         surgery_time_mmss, surgery_time_sec, recovery_time_mmss, recovery_time_sec, comments, release_container_id, 
         release_id, post_tagging_tank, entered_by, qaqc_by, qaqc_comments)

write.csv(FCS_2024_clean, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/2024_FCS_tagging_clean.csv")

# LFCS_2025---------------------------------------------------------------------

# read in raw data file
LFCS_2025 = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tagging/2025_LFCS.csv")

# fix incorrect value
LFCS_2025[331,16] = "RT-B"

# add in post tagging tank placeholder
LFCS_2025$post_tagging_tank = ""

# write in tank ids for the post-tagging tanks
LFCS_2025 <- LFCS_2025 %>%
  mutate(post_tagging_tank = case_when(release_containerID == "O-7" ~ "O-7",
                                       release_containerID == "O-6" ~ "O-6",
                                       release_containerID == "RT-A" ~ "A",
                                       release_containerID == "RT-B" ~ "B",
                                       release_containerID == "H-4" ~ "H-4",
                                       TRUE             ~ post_tagging_tank))

# write in NAs for the release containers for tagger effects
LFCS_2025 <- LFCS_2025 %>%
  mutate(release_containerID = na_if(release_containerID, "RT-A")) %>%
  mutate(release_containerID = na_if(release_containerID, "RT-B")) %>% 
  mutate(release_containerID = na_if(release_containerID, "H-4"))

# change release_container_id to point releases
LFCS_2025 <- LFCS_2025 %>%
  mutate(release_containerID = case_when(release_containerID == "O-7" ~ "point_rel",
                                         release_containerID == "O-6" ~ "point_rel",
                                          TRUE             ~ release_containerID))

# clean up file
LFCS_2025_clean = LFCS_2025 %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>%
  
  # manually rename columns we want to keep
  rename(fork_length_mm = length_mm) %>% 
  
  # format data to appropriate classes
  mutate(date = mdy(date),
         source_tank_id = as.factor(source_tank_id), 
         surgeon = as.factor(surgeon),
         vial_number = as.character(vial_number),
         weight_gr = as.numeric(weight_gr),
         fork_length_mm = as.integer(fork_length_mm), 
         tag_activation_date = mdy(tag_activation_date),
        
         # create new columns for water year, species, run, and month and assign 
         # their values and order
         water_year = as.factor(2025),
         species = as.factor("Chinook Salmon"),
         run = as.factor("late-fall"), 
         month = as.factor(month(date, label = TRUE)), 
         month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),
        
         # create new columns that convert stopwatch timers to seconds
         anesthetic_time_sec = period_to_seconds(ms(anesthetic_time_mmss)), 
         surgery_time_sec = period_to_seconds(ms(surgery_time_mmss)),
         recovery_time_sec = period_to_seconds(ms(recovery_time_mmss)),
        
         # create new column for release id 
         release_id = paste((date+1), release_container_id),
         
         # convert empty cells to NA and then remove all NAs from comment columns
         across(where(is.character), ~na_if(., "")),
         comments = replace_na(comments, ""),
         qaqc_comments = replace_na(qaqc_comments, "")) %>% 
  
  # arrange by date and then vial_number to get back to the order in which fish 
  # were tagged
  
  arrange(date, as.numeric(vial_number)) %>% 
  
  # select columns to include and reorder
  select(water_year, month, date, species, run, source_tank_id, surgeon, crew, vial_number, 
         weight_gr, fork_length_mm, tag_id_hex, tag_activation_date, anesthetic_time_mmss, anesthetic_time_sec,
         surgery_time_mmss, surgery_time_sec, recovery_time_mmss, recovery_time_sec, comments, release_container_id, 
         release_id, post_tagging_tank, entered_by, qaqc_by, qaqc_comments)

# change the release ids to NA if there was no release
LFCS_2025_clean = LFCS_2025_clean %>% 
  mutate(release_id = if_else(is.na(release_container_id), NA_character_, release_id))

write.csv(LFCS_2025_clean, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/2025_LFCS_tagging_clean.csv")

# FCS_2025----------------------------------------------------------------------
  
# read in raw data file
FCS_2025 = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tagging/2025_FCS.csv")

# clean up file
FCS_2025_clean = FCS_2025 %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>%
  
  # manually rename columns we want to keep
  rename(fork_length_mm = length_mm) %>% 
  
  # format data to appropriate classes
  mutate(date = mdy(date),
         source_tank_id = as.factor(source_tank_id), 
         surgeon = as.factor(surgeon),
         vial_number = as.character(vial_number),
         weight_gr = as.numeric(weight_gr),
         fork_length_mm = as.integer(fork_length_mm), 
         tag_activation_date = mdy(tag_activation_date),
        
         # create new columns for water year, species, run, and month and assign 
         # their values and order
         water_year = as.factor(2025),
         species = as.factor("Chinook Salmon"),
         run = as.factor("fall"), 
         month = as.factor(month(date, label = TRUE)), 
         month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),
        
         # create new columns that convert stopwatch timers to seconds
         anesthetic_time_sec = period_to_seconds(ms(anesthetic_time_mmss)), 
         surgery_time_sec = period_to_seconds(ms(surgery_time_mmss)),
         recovery_time_sec = period_to_seconds(ms(recovery_time_mmss)),
        
         # create new columns for release id and caba tank id
         release_id = paste((date+1), release_container_id),
         post_tagging_tank = as.factor(NA),
        
         # convert empty cells to NA and then remove all NAs from comment columns
         across(where(is.character), ~na_if(., "")),
         comments = replace_na(comments, ""),
         qaqc_comments = replace_na(qaqc_comments, "")) %>% 
  
  # arrange by date and then vial_number to get back to the order in which fish 
  # were tagged
  arrange(date, as.numeric(vial_number)) %>% 
  
  # select columns to include and reorder
  select(water_year, month, date, species, run, source_tank_id, surgeon, crew, vial_number, 
         weight_gr, fork_length_mm, tag_id_hex, tag_activation_date, anesthetic_time_mmss, anesthetic_time_sec,
         surgery_time_mmss, surgery_time_sec, recovery_time_mmss, recovery_time_sec, comments, release_container_id, 
         release_id, post_tagging_tank, entered_by, qaqc_by, qaqc_comments)

# write in tank ids for the post-tagging tanks
FCS_2025_clean <- FCS_2025_clean %>%
  mutate(post_tagging_tank = case_when(release_container_id == "RT-A" ~ "A",
                                       release_container_id == "RT-B" ~ "B",
                                       TRUE             ~ post_tagging_tank)) 

# write in NAs for the release containers for tagger effects
FCS_2025_clean <- FCS_2025_clean %>%
  mutate(release_container_id = na_if(release_container_id, "RT-A")) %>% 
  mutate(release_container_id = na_if(release_container_id, "RT-B"))

# change the release ids to NA if there was no release
FCS_2025_clean = FCS_2025_clean %>% 
  mutate(release_id = if_else(is.na(release_container_id), NA_character_, release_id))


write.csv(FCS_2025_clean, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/2025_FCS_tagging_clean.csv")

# LFCS_2026-----------------------------------------------------

# read in raw data file

LFCS_2026 = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tagging/2026_LFCS.csv")

# replace all random NA values with true NAs
LFCS_2026[LFCS_2026 == ""] <- NA
LFCS_2026[LFCS_2026 == "N/A"] <- NA
LFCS_2026[LFCS_2026 == "na"] <- NA
LFCS_2026[LFCS_2026 == "n/a"] <- NA
LFCS_2026[LFCS_2026 == "NA"] <- NA

# add in placeholder column for RT-
LFCS_2026$RT = "RT-"

# paste RT- with release container id
LFCS_2026$release_container_id = paste(LFCS_2026$RT, LFCS_2026$release_containerID, sep = "")

# clean up the releases to be NA when the fish were not released
LFCS_2026[LFCS_2026 == "RT-NA"] <- NA

# filter out the old column so the code below doesn't get confused
LFCS_2026 = LFCS_2026 %>% 
  select(!release_containerID)

# clean up file
LFCS_2026_clean = LFCS_2026 %>% 

  # restructure the column headers to be lowercase and include _
  clean_names() %>%
  
  # manually rename columns we want to keep
  rename(fork_length_mm = length_mm) %>% 
  
  # format data to appropriate classes
  mutate(date = mdy(date),
         source_tank_id = as.factor(source_tank_id), 
         surgeon = as.factor(surgeon),
         vial_number = as.character(vial_number),
         weight_gr = as.numeric(weight_gr),
         fork_length_mm = as.integer(fork_length_mm), 
         tag_activation_date = mdy(tag_activation_date),
        
         # create new columns for water year, species, run, and month and assign 
         # their values and order
         water_year = as.factor(2026),
         species = as.factor("Chinook Salmon"),
         run = as.factor("late-fall"), 
         month = as.factor(month(date, label = TRUE)), 
         month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),
        
         # create new columns that convert stopwatch timers to seconds
         anesthetic_time_sec = period_to_seconds(ms(anesthetic_time_mmss)),
         surgery_time_sec = period_to_seconds(ms(surgery_time_mmss)),
         recovery_time_sec = period_to_seconds(ms(recovery_time_mmss)),
        
         # create new columns for release id and caba tank id
         release_id = paste((date+1), release_container_id),
         post_tagging_tank = as.factor(caba_tank_id)) %>% 
        
  # remove the NAs for comment columns 
  mutate(comments = replace_na(comments, ""), 
         qaqc_comments = replace_na(qaqc_comments, "")) %>% 
      
  # arrange by date and then vial_number to get back to the order in which fish 
  # were tagged
  arrange(date, as.numeric(vial_number)) %>% 
  
  # select columns to include and reorder
  select(water_year, month, date, species, run, source_tank_id, surgeon, crew, vial_number, 
         weight_gr, fork_length_mm, tag_id_hex, tag_activation_date, anesthetic_time_mmss, anesthetic_time_sec,
         surgery_time_mmss, surgery_time_sec, recovery_time_mmss, recovery_time_sec, comments, release_container_id, 
         release_id, post_tagging_tank, entered_by, qaqc_by, qaqc_comments)

# change the release ids to NA if there was no release
LFCS_2026_clean = LFCS_2026_clean %>% 
  mutate(release_id = if_else(is.na(release_container_id), NA_character_, release_id))

write.csv(LFCS_2026_clean, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/2026_LFCS_tagging_clean.csv")

# FCS_2026----------------------------------------------------------------------

# read in raw data file
FCS_2026 = read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tagging/2026_FCS.csv")

# replace all random NA values with true NAs
FCS_2026[FCS_2026 == ""] <- NA
FCS_2026[FCS_2026 == "N/A"] <- NA
FCS_2026[FCS_2026 == "na"] <- NA
FCS_2026[FCS_2026 == "n/a"] <- NA
FCS_2026[FCS_2026 == "NA"] <- NA

# add in placeholder column for RT-
FCS_2026$RT = "RT-"

# paste RT- with release container id
FCS_2026$release_container_id = paste(FCS_2026$RT, FCS_2026$release_containerID, sep = "")

# clean up the releases to be NA when the fish were not released
FCS_2026[FCS_2026 == "RT-NA"] <- NA

# filter out the old column so the code below doesn't get confused
FCS_2026 = FCS_2026 %>% 
  select(!release_containerID)

# change QAQC comments column to not be logical...
FCS_2026$QAQC_comments = as.character(FCS_2026$QAQC_comments)
  
# clean up file
FCS_2026_clean = FCS_2026 %>% 
  
  # restructure the column headers to be lowercase and include _
  clean_names() %>%
  
  # manually rename columns we want to keep
  rename(fork_length_mm = length_mm) %>% 
  
  # format data to appropriate classes
  mutate(date = mdy(date),
         source_tank_id = as.factor(source_tank_id), 
         surgeon = as.factor(surgeon),
         vial_number = as.character(vial_number),
         weight_gr = as.numeric(weight_gr),
         fork_length_mm = as.integer(fork_length_mm), 
         tag_activation_date = mdy(tag_activation_date),
        
         # create new columns for water year, species, run, and month and assign 
         # their values and order
         water_year = as.factor(2026),
         species = as.factor("Chinook Salmon"),
         run = as.factor("fall"), 
         month = as.factor(month(date, label = TRUE)), 
         month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                         "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")),
        
         # create new columns that convert stopwatch timers to seconds
         anesthetic_time_sec = period_to_seconds(ms(anesthetic_time_mmss)),
         surgery_time_sec = period_to_seconds(ms(surgery_time_mmss)),
         recovery_time_sec = period_to_seconds(ms(recovery_time_mmss)),
        
         # create new columns for release id and caba tank id
         release_id = paste((date+1), release_container_id),
         post_tagging_tank = as.factor(caba_tank_id)) %>% 
      
 
  # remove the NAs for comment columns 
  mutate(comments = replace_na(comments, ""), 
         qaqc_comments = replace_na(qaqc_comments, "")) %>% 
      
  # arrange by date and then vial_number to get back to the order in which fish 
  # were tagged
  arrange(date, as.numeric(vial_number)) %>% 
  
  # select columns to include and reorder
  select(water_year, month, date, species, run, source_tank_id, surgeon, crew, vial_number, 
         weight_gr, fork_length_mm, tag_id_hex, tag_activation_date, anesthetic_time_mmss, anesthetic_time_sec,
         surgery_time_mmss, surgery_time_sec, recovery_time_mmss, recovery_time_sec, comments, release_container_id, 
         release_id, post_tagging_tank, entered_by, qaqc_by, qaqc_comments)

# change the release ids to NA if there was no release
FCS_2026_clean = FCS_2026_clean %>% 
  mutate(release_id = if_else(is.na(release_container_id), NA_character_, release_id))

write.csv(FCS_2026_clean, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/2026_FCS_tagging_clean.csv")


# -----------------------------------------------------
# bind all files together
tagging_data = rbind(LFCS_2024_clean, FCS_2024_clean, LFCS_2025_clean, FCS_2025_clean, LFCS_2026_clean, FCS_2026_clean)

write.csv(tagging_data, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/tagging_data.csv")

# -----------------------------------------------------
# remove released fish so this includes only tagger effects fish
tagger_effects_tagging_data = tagging_data %>% 
  filter(is.na(release_id)) %>% 
  filter_out(is.na(anesthetic_time_mmss))

write.csv(tagger_effects_tagging_data, "C:/Users/Hbell/Projects/data-project/data-clean/tag-effects/tag_effects_tagging_data.csv")

# -----------------------------------------------------
# now the opposite, remove tagger effects so it includes only fish released
  
released_fish_tagging_data = tagging_data %>% 
  filter(!is.na(release_id))

write.csv(released_fish_tagging_data, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/released_fish_tagging_data.csv")

# ------------------------------------------------------------------------------
# when rerunning data, just use this bottom part

LFCS_2024_clean = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/tagging/2024_LFCS_tagging_clean.csv")
FCS_2024_clean = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/tagging/2024_FCS_tagging_clean.csv")
LFCS_2025_clean = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/tagging/2025_LFCS_tagging_clean.csv")
FCS_2025_clean = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/tagging/2025_FCS_tagging_clean.csv")
LFCS_2026_clean = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/tagging/2026_LFCS_tagging_clean_2.csv")
FCS_2026_clean = read.csv("C:/Users/Hbell/Projects/data-project/data-clean/tagging/2026_FCS_tagging_clean.csv")

# bind all files together
tagging_data = rbind(LFCS_2024_clean, FCS_2024_clean, LFCS_2025_clean, FCS_2025_clean, LFCS_2026_clean, FCS_2026_clean)

# write.csv(tagging_data, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/tagging_data_2.csv")
# 
# # now the opposite, remove tagger effects so it includes only fish released
# released_fish_tagging_data = tagging_data %>% 
#   filter(!is.na(release_id))
# 
# write.csv(released_fish_tagging_data, "C:/Users/Hbell/Projects/data-project/data-clean/tagging/released_fish_tagging_data_.csv")
