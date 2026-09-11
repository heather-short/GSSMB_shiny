# Tag Life
# H. Short 
# Fri Jul  3 11:03:55 2026 ------------------------------

#load packages
library(tidyverse) # for ggplot2
library(lubridate) # need this for date stuff
library(plotly)
library(janitor)

# ------------------------------------------------------------------------------
# read in WHS Host file
whs1 <- read_csv("C:/Users/Hbell/Projects/GSSMB_shiny/raw-data/tag-life/2026/august_2026_page0.csv")
whs2 <- read_csv("C:/Users/Hbell/Projects/GSSMB_shiny/raw-data/tag-life/2026/august_2026_page1.csv")
whs3 <- read_csv("C:/Users/Hbell/Projects/GSSMB_shiny/raw-data/tag-life/2026/august_2026_page2.csv")
whs4 <- read_csv("C:/Users/Hbell/Projects/GSSMB_shiny/raw-data/tag-life/2026/august_2026_page3.csv")

# bind all of the files together
whs = bind_rows(whs1, whs2, whs3, whs4)

# read in tag life file
# tag life file should have two columns (hex_id & date_activated)
tag_life_tags <- read.csv("raw-data/tag-life/2026/tag_life_tags_2026.csv")

# last download date
download_date = as.Date("2026-08-28")

# ------------------------------------------------------------------------------
# clean up whs file
whs_clean <- whs %>% 
  
  # clean up column headers
  clean_names() %>% 
  
  # rename columns 
  rename(hex_id = hexadecimal_id) %>% 
  
  # mutate and add columns 
  mutate(datetime = as.POSIXct(time*86400, origin = "1899-12-30", tz = "UTC"),
         date = date(datetime),
         
         # trim the white space around the hex_id
         hex_id = trimws(hex_id), 
         hex_id = as.factor(hex_id))

# ------------------------------------------------------------------------------
# create new data frame for ping counts
pingcount = whs_clean %>% 
  
  # group by hex_id so we have a row for each hex_id
  group_by(hex_id) %>% 
  
  # count the number of times a tag pinged on a each date 
  count(date) %>% 
  
  # rename the new column 
  rename(pings_per_day = n)

# ------------------------------------------------------------------------------
# clean up tag life tag data frame
tag_life_tags = tag_life_tags %>% 
  
  # clean up column headers
  clean_names() %>% 
  
  # mutate data types
  mutate(tagging_block = as.factor(tagging_block), 
         hex_id = as.character(hex_id),
         date_activated = mdy(date_activated), 
         in_caba_tank = mdy(in_caba_tank))

# ------------------------------------------------------------------------------
# merge the pingcount df and the tag_life_tags df by hex_id
df = merge(pingcount, tag_life_tags, by = "hex_id", type = "full")

# use this df to visually inspect the data and manually assign end dates for each tag (do this in an excel spreadsheet)
# when the tag dies, it can still continue to ping in very low amounts - around 8 times per day
# any rows with ping counts with very low multiples of 8 (16, 24) are dates that should be considered post-tag-death
# find the last high ping count before the run of low 8s and use that as your end date for the tag

# -------------------------------------------------------------------------------
# read in csv with end dates
# end should have two columns (hex_id & date_dead)
end = read_csv("raw-data/tag-life/2026/tag_life_start_and_end_dates_2026.csv")

# change the tags that did not die to NA
end = end %>%
  mutate(date_dead = na_if(date_dead, "8/28/2026"))

# ------------------------------------------------------------------
# merge the original df with end_dates
df = merge(df, end, by = "hex_id") %>% 
  
  # format data types
  mutate(date = ymd(date), 
         date_dead = mdy(date_dead),
         date_activated = ymd(date_activated), 
         days_since_activation = as.integer(date - date_activated), 
         total_days_active = as.integer(date_dead - date_activated))

# make a tag summary with one row per each tag
tag_summary = df %>% 
  distinct(hex_id, tagging_block, total_days_active)

# make a survival df
survival_df = tag_summary %>% 
  
  # count the number of tags in each tagging block
  group_by(tagging_block) %>% 
  summarize(n_tags = n(), 
            .groups = "drop") %>% 
  
  rowwise() %>% 
  mutate(days_since_activation = list(0:150)) %>% 
  unnest(days_since_activation) %>% 
  ungroup()

# then, join it with the tag summary
survival_df = survival_df %>% 
  left_join(tag_summary, by = "tagging_block") %>% 
  
  # count the number of tags still active 
  # (NA total_days_active = tag never died, so it's active at every timepoint)
  group_by(tagging_block, days_since_activation, n_tags) %>% 
  summarize(n_active = sum(is.na(total_days_active) | total_days_active >= days_since_activation), 
            .groups = "drop") %>% 
  
  # create the proportion of tags active
  mutate(prop_active = n_active / n_tags)

# -------------------------------------------------------------------------------
# save as csv
write_csv(tag_summary, "clean-data/tag-life/tag_life_summary_2026.csv")
write_csv(survival_df, "clean-data/tag-life/tag_life_survival_2026.csv")

# ------------------------------------------------------------------------------
# plot days since activation vs proportion of tags active
ggplot(survival_df, aes(x = days_since_activation, y = prop_active, color = factor(tagging_block))) + 
  geom_line(linewidth = 2) + 
  # facet_wrap(~tagging_block) +
  labs(x = "Days Since Activation", 
       y = "Proportion of Tags Active", 
       color = "Tagging Block") + 
  scale_color_manual(values = c("1" = "#2F4F4F",
                                "2" = "#53868B",
                                "3" = "#6E8B3D",
                                "4" = "#A2CD5A")) +
  geom_vline(xintercept = 71, linetype = "dashed") +
  theme(axis.title.x  = element_text(size = 16),
        axis.text.x   = element_text(size = 14),
        axis.title.y  = element_text(size = 16),
        axis.text.y   = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
