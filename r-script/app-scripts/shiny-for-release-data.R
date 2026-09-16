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
fp_water_qual = readRDS("clean-data/water-quality/2024-2026/historical_fp_water_quality.rds")
release_rectangles = readRDS("clean-data/releases/release_date_rectangles.rds")

# release water quality data table -------------------------------------------------------------------------------
# testable version
water_qual_release %>%
      clean_names() %>% 
      arrange(rel_id) %>%
      select(rel_id, release_time, lunar_phase, fish_released, temperature, turbidity, discharge) %>% 
      rename("Release ID"       = rel_id,
             "Time of Release" = release_time, 
             "Lunar Phase" = lunar_phase,
             "Fish Released" = fish_released,
             "Temperature (°C)" = temperature,
             "Turbidity (FNU)"  = turbidity,
             "Discharge (cfs)"  = discharge)

# -------------------------------------------------------------------------------
# shiny version
# shiny ui

# -------------------------------------------------------------------------------
# shiny server
observe({

  # require water year as an input
  req(input$water_year_wq_release)
  
  dates = water_qual_release %>% 
    filter(water_year == input$water_year_wq_release)
    # pull release dates as a vector
    pull(release_date) %>% 
    # choose only the unique release dates
    unique() %>% 
    # sort in chronological order
    sort()

  updateSelectInput(session, "release_date",
                    choices = dates,
                    selected = dates[1])

}) # end observe
  
  # create date selection input using "dates"
  output$release_date_ui <- renderUI({
    
    selectInput("release_date", "Release Date:", choices = NULL)
    
  })

# filtered water quality table data
  filtered_water_qual_release <- reactive({
    
    # require both water year and release date as an input
    req(input$water_year_wq_release, input$release_date)
    
    water_qual_release() %>%
      filter(water_year == input$water_year_wq_release,
             release_date == input$release_date) %>%
      mutate(rel_id = factor(rel_id, levels = c("RT-1", "RT-2", "RT-3", "RT-4", "RT-5", "RT-6", 
                                                "RT-7", "RT-8", "RT-9", "RT-10", "RT-11", "RT-12",
                                                "RT-13", "RT-14", "RT-15", "RT-16", "RT-17", "RT-18", 
                                                "RT-19", "RT-20", "RT-21", "RT-22", "RT-23", "RT-24", "point_rel"))) %>% 
      arrange(rel_id) %>%
      select(rel_id, release_time, lunar_phase, fish_released, temperature, turbidity, discharge, velocity) %>% 
      rename("Release ID"       = rel_id,
             "Time of Release" = release_time, 
             "Lunar Phase" = lunar_phase,
             "Fish Released" = fish_released,
             "Temperature (°C)" = temperature,
             "Turbidity (FNU)"  = turbidity,
             "Discharge (cfs)"  = discharge,
             "Velocity (ft/s)"  = velocity)
  })
  
  # Render DT
  output$water_qual_release_table <- renderDT({
    datatable(water_qual_release(),
              rownames  = FALSE,
              options = list(dom = "t",
                             server = FALSE))
    
  }) # end render data table

# release water quality plot -------------------------------------------------------------------------------
# testable version
ggplot(fp_water_qual, aes(x = datetime, y = value)) +
      facet_wrap(~value_type, scales = "free_y")  +
      geom_rect(data = release_rectangles, inherit.aes = FALSE,
                aes(xmin = as.POSIXct(start),
                    xmax = as.POSIXct(end),
                    ymin = -Inf, ymax = Inf),
                    fill = "darkgray", 
                    alpha = 0.40) +
      geom_line(color = "#2F4F4F", 
                linewidth = 1.2) +
      scale_x_datetime(name = "Month",
                       date_breaks = "1 month",
                       date_labels = "%b") +
      labs(y = y_labels) +
      theme_minimal(base_size = 14) +
      theme(axis.title = element_text(size = 16),
            axis.text  = element_text(size = 12))

# shiny version
# create y label lookup vector
  y_labels <- c(discharge   = "Discharge (cfs)",
                temperature = "Temperature (°C)",
                turbidity   = "Turbidity (NTU)")
  
  filtered_fp_water_qual <- reactive({
    fp_water_qual() %>% 
      # filter by selected water year and value type
      filter(water_year == input$water_year_wq_release,
             value_type == input$value_type)
  })
  
  filtered_release_rectangles <- reactive({
    release_rectangles() %>%
      # filter rectangles by water year selection
      filter(water_year == input$water_year_wq_release)
  })
  
  output$fp_water_quality_plot <- renderPlot({
    
    req(nrow(filtered_fp_water_qual()) > 0) # requires there to be data - no plot if no data
    
    p = ggplot(filtered_fp_water_qual(), aes(x = datetime, y = value)) +
      geom_rect(data = filtered_release_rectangles(), inherit.aes = FALSE,
                aes(xmin = as.POSIXct(start),
                    xmax = as.POSIXct(end),
                    ymin = -Inf, ymax = Inf),
                    fill = "darkgray", 
                    alpha = 0.40) +
      geom_line(color = "#2F4F4F", 
                linewidth = 1.2) +
      scale_x_datetime(name = "Month",
                       date_breaks = "1 month",
                       date_labels = "%b") +
      labs(y = y_labels[input$value_type]) +
      theme_minimal(base_size = 14) +
      theme(axis.title = element_text(size = 16),
            axis.text  = element_text(size = 12))
    
    # require release date selection
    req(input$release_date)
    
    # modify data type 
    target_dt = as.POSIXct(input$release_date)

    # find the row in filtered_fp_water_qual() whose datetime is nearest to target_dt (since release_date may not exactly match a logged reading)
    closest_idx <- which.min(abs(difftime(filtered_fp_water_qual()$datetime, target_dt))) 
    point_data  <- filtered_fp_water_qual()[closest_idx, ] # pull that single closest row
    
    p + geom_point(data = point_data,
                   aes(x = datetime, y = value),
                   shape = 21, color = "black", fill = "#25ff92ff", size = 4)
    
  }, res = 96) # end render plot