# scripts to put into shiny app
# release data
# H. Short
# 2026-09-11 13:22:12
# -------------------------------------------------------------------------------
# load packages
library(tidyverse)

# -------------------------------------------------------------------------------
# read in data
release = readRDS("clean-data/releases/historical_release_data.rds")
water_qual_release = readRDS("clean-data/releases/historical_release_water_quality_summary.rds")

# release water quality data table -------------------------------------------------------------------------------
# testable version



# shiny version
observe({

  # require water year as an input
  req(input$water_year_wq_release)
  
  dates = water_qual_release %>% 
    filter(water_year == input$water_year_wq_release)
    # pull release dates as a vector
    pull(release_date) %>% 
    unique() %>% 
    sort()

  updateSelectInput(session, "release_date",
                    choices = dates,
                    selected = dates[1])

})


