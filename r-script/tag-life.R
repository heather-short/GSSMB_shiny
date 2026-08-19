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
whs1 <- read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tag-life/july_2026_page1.csv")
whs2 <- read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tag-life/july_2026_page2.csv")
whs3 <- read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tag-life/july_2026_page3.csv")
whs4 <- read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tag-life/july_2026_page4.csv")

whs = rbind(whs1, whs2)
whs = rbind(whs, whs3)
whs = rbind(whs, whs4)

# read in tag life file
tag_life_tags <- read.csv("C:/Users/Hbell/Projects/data-project/data-raw/tag-life/tag_life_tags_2026.csv")


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

# ------------------------------------------------------------------------------
# this creates the function to find the end_date for the tags
find_tag_end_date <- function(df,
                              download_date,
                              ping_threshold = 8, # edit this number if you want to change the ping_threshold
                              censor_window_days = 0,
                              min_low_run = 2) {
  
  # the receiver download date
  download_date <- as.Date(download_date)
  
  censor_cutoff <- download_date - censor_window_days
  
  df %>%
    
    # format date as a date
    mutate(date = as.Date(date)) %>%
    
    # arrange the data first by hex_id and then in order by date
    arrange(hex_id, date) %>%
    
    # create a column for determining if the pings_per_day falls below the threshold set
    group_by(hex_id) %>%
    mutate(is_low   = pings_per_day < ping_threshold,
           run_id   = with(rle(is_low), rep(seq_along(lengths), lengths))) %>%
    
    # count the low numbers in a run
    group_by(hex_id, run_id) %>%
    mutate(run_start = min(date), run_length = n()) %>%
    ungroup() %>%
    
    # find the last observed date that the tag was pinging
    group_by(hex_id) %>%
    slice_max(date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    
    # add in new columns and define meanings
    mutate(is_censored   = date >= censor_cutoff,
           settled_low   = is_low & run_length >= min_low_run,
           status = if_else(is_censored, "alive_censored", "dead"),
           end_date = case_when(is_censored   ~ as.Date(NA),
                                settled_low   ~ run_start,
                                TRUE          ~ date),
           end_type = case_when(is_censored   ~ "still_pinging_at_data_pull",
                                settled_low   ~ "settled_at_low_pings",
                                TRUE          ~ "abrupt_stop_high_ping")) %>%
    
    # select final df
    select(hex_id, status, end_date, end_type,
           last_observed_date = date,
           last_observed_ping = pings_per_day,
           terminal_run_length = run_length)
  
} # end function

# ------------------------------------------------------------------
# use the function to create the end_date df
end_dates <- find_tag_end_date(df, download_date = as.Date("2026-07-24")) %>% 
  select(hex_id, end_date)

# ------------------------------------------------------------------
# merge the original df with end_dates
df = merge(df, end_dates, by = "hex_id") %>% 
  
  # format data types
  mutate(date = ymd(date), 
         date_activated = ymd(date_activated), 
         days_since_activation = as.integer(date - date_activated), 
         total_days_active = as.integer(end_date - date_activated))

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
  mutate(days_since_activation = list(0:225)) %>% 
  unnest(days_since_activation) %>% 
  ungroup()

# then, join it with the tag summary
survival_df = survival_df %>% 
  left_join(tag_summary, by = "tagging_block") %>% 
  
  # keep everything that doesn't have an NA for total_days_active
  filter(!is.na(total_days_active)) %>% 
  
  # count the number of tags still active 
  group_by(tagging_block, days_since_activation, n_tags) %>% 
  summarize(n_active = sum(total_days_active >= days_since_activation), 
            .groups = "drop") %>% 
  
  # create the proportion of tags active
  mutate(prop_active = n_active / n_tags) 

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
        axis.text.x   = element_text(size = 12),
        axis.title.y  = element_text(size = 16),
        axis.text.y   = element_text(size = 12))





