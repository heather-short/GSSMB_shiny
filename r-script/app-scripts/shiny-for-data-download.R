# scripts to put into shiny app
# download handler
# H. Short
# 2026-09-15 08:06:27
# -------------------------------------------------------------------------------
# load packages
library(tidyverse)

# -------------------------------------------------------------------------------
# read in data
tagging = readRDS("clean-data/tagging/historical_tagging_data.rds")
release = readRDS("clean-data/releases/historical_release_data.rds")
calfishtrack = readRDS("clean-data/calfishtrack/calfishtrack_report.rds")

# -------------------------------------------------------------------------------
# testable version
tagging %>%       
      select(water_year, block, tag_activation_date, species, run, date, surgeon, crew, vial_number, 
             tag_id_hex, weight_gr, fork_length_mm, anesthetic_time_mmss, surgery_time_mmss, recovery_time_mmss, comments)

release %>%      
      select(water_year, block, species, run, release_datetime, release_container_id, crew, morts, fish_released, comments)

calfishtrack %>%   
      rename(Release_Datetime_PST = release_datetime,
             TagID_Hex = tag_id_hex,
             Weight_gr = weight_gr, 
             Length_mm = fork_length_mm) %>% 
      mutate(Row = row_number(),
             StudyID = paste0("Georg_SL_Barrier_2026_", month),
             Release_riverkm = "162.64",
             Release_location = "Sacramento Marina",
             PRI_nominal = "5", 
             tag_battery_life_days = "71", 
             Release_Latitude = "38.560291", 
             Release_Longitude = "-121.517384") %>% 
      
      select(Row, StudyID, Release_Datetime_PST, TagID_Hex, Weight_gr, Length_mm, Release_riverkm,
             Release_location, PRI_nominal, tag_battery_life_days, Release_Latitude, Release_Longitude)

# -------------------------------------------------------------------------------
# shiny version
# shiny ui

# -------------------------------------------------------------------------------
# shiny server
# no active report upon opening
  active_report <- reactiveVal(NULL)
  
  # ---- Reactive data for each report type ----
  # tagging report 
  tagging_report <- reactive({
    
    # require water year selection
    req(input$water_year_tagging)
    
    tagging() %>% 
      filter(water_year == input$water_year) %>% 
      
      select(water_year, block, tag_activation_date, species, run, date, surgeon, crew, vial_number, 
             tag_id_hex, weight_gr, fork_length_mm, anesthetic_time_mmss, surgery_time_mmss, recovery_time_mmss, comments)
    
  }) # end tagging report table
  
# release report
  release_report <- reactive({
    
    # require water year selection
    req(input$water_year)
    
    release() %>%
      filter(water_year == input$water_year) %>% 
      
      select(water_year, block, species, run, release_datetime, release_container_id, crew, morts, fish_released, comments)
    
  }) # end release report table
  
# calfishtrack report
  calfishtrack_report <- reactive({
    
    # require tagging year and block selection
    req(input$water_year, input$block)
    
    calfishtrack() %>% 
      filter(water_year == input$water_year_calfishtrack,
             block == input$block_calfishtrack) %>% 
    
      rename(Release_Datetime_PST = release_datetime,
             TagID_Hex = tag_id_hex,
             Weight_gr = weight_gr, 
             Length_mm = fork_length_mm) %>% 
      
      mutate(Row = row_number(),
             StudyID = paste0("Georg_SL_Barrier_2026_", month),
             Release_riverkm = "162.64",
             Release_location = "Sacramento Marina",
             PRI_nominal = "5", 
             tag_battery_life_days = "71", 
             Release_Latitude = "38.560291", 
             Release_Longitude = "-121.517384") %>% 
      
      select(Row, StudyID, Release_Datetime_PST, TagID_Hex, Weight_gr, Length_mm, Release_riverkm,
             Release_location, PRI_nominal, tag_battery_life_days, Release_Latitude, Release_Longitude)
  })
  
  # sets the active report when generate report is clicked
  observeEvent(input$tagging_gen_report, {
    active_report("tagging_report")
  })
  
  observeEvent(input$release_gen_report, {
    active_report("release_report")
  })
  
  observeEvent(input$calfishtrack_gen_report, {
    active_report("calfishtrack_report")
  })
  
  # title above the table
  output$active_report_title <- renderText({
    req(active_report())
    switch(active_report(),
           tagging_report = "Tagging Report",
           release_report = "Release Report",
           calfishtrack_report   = "CalFishTrack Report")
  })
  
  # only one space in the app for each table to render in to
  output$report_table <- renderDT({

    # require a report to be generated first
    req(active_report())
    
    df <- switch(active_report(),
                 tagging_report = tagging_report(),
                 release_report = release_report(),
                 calfishtrack_report   = calfishtrack_report()
    )
    
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # download handlers
  output$tagging_download <- downloadHandler(
    filename = function() paste0("tagging_report.csv"),
    content = function(file) write.csv(tagging_report(), file, row.names = FALSE)
  )
  
  output$release_download <- downloadHandler(
    filename = function() paste0("release_report.csv"),
    content = function(file) write.csv(release_report(), file, row.names = FALSE)
  )
  
  output$calfishtrack_download <- downloadHandler(
    filename = function() paste0("calfishtrack_report.csv"),
    content = function(file) write.csv(calfishtrack_report(), file, row.names = FALSE)
  )
} # end server