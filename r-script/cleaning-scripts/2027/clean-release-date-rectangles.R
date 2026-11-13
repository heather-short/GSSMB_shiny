# clean up and make modifications to release date rectangles file
# 2026-11-03 13:52:14
# -------------------------------------------------------------------------------
library(tidyverse)

# load current file
current = read.csv("clean-data/releases/release_date_rectangles.csv")

updated = current %>% 
  mutate(start = mdy(start),
         end = mdy(end), 
         water_year = as.factor(water_year), 
         block = as.factor(block)) %>% 
  
  # add in new row for updated block
  add_row(start = mdy("11-12-2026"), # update start release date
          end = mdy("11-15-2026"), # update end release date
          water_year = factor("2027"), 
          block = factor("Block 1A")) # update block

# write new rds file
saveRDS(updated, "clean-data/releases/release_date_rectangles.rds")

# check new file 
test = readRDS("clean-data/releases/release_date_rectangles.rds")

# save to for-app
saveRDS(updated, "for-app/release_date_rectangles.rds")
