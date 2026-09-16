# scripts to put into shiny app
# tagging data
# H. Short
# 2026-09-11 10:49:51
# -------------------------------------------------------------------------------
# load packages
library(tidyverse)

# -------------------------------------------------------------------------------
# read in data
tagging = readRDS("clean-data/tagging/historical_tagging_data.rds")

# weight vs length plot -------------------------------------------------------------------------------
# testable version
tagging %>% 
  # remove tag effects fish
  filter(!grepl("D", vial_number)) %>% 
  # plot
  ggplot(aes(x = weight_gr, y = fork_length_mm, color = month)) +
  facet_grid(~month) +
  geom_jitter() +
  scale_color_manual(values = c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                                 "Apr" = "#A2CD5A", "May" = "#FFC125")) +
  labs(x = "Weight (g)",
       y = "Fork Length (mm)") +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        title = element_blank(), 
        legend.position = "none",
        plot.caption = element_text(size = 16, face = "italic", hjust = 0, margin = margin(t = 20)),
        strip.text = element_text(size = 16))

# -------------------------------------------------------------------------------
# shiny ui
nav_panel(title = "Tagging", 
            
            fluidRow(h2("Tagging Procedure"),
                           
                     column(6, 
                            br(),
                            h5("Methods: \n
                                To begin each surgical procedure, fish were placed in an anesthesia solution 
                                until they lost equilibrium and were no longer responsive to touch. The fish 
                                was removed from anesthesia and weighed to the nearest 0.1 g and fork length 
                                was measured to the nearest mm. Fish were then placed ventral side up on a foam 
                                block before an irrigation tube was placed in or near the mouth to allow for 
                                irrigation of the gills with a lower dose of the anesthetic solution. A small 
                                incision was made parallel to the mid-ventral line and an ss400 JSATS acoustic
                                transmitter was inserted into the coelomic cavity before the incision was closed 
                                with a single uninterrupted 2 x 2 x 2 surgical knot. The duration of each surgery 
                                was measured from the time the fish was removed from anesthesia to the time it was 
                                placed in recovery following surgery. Tagging methods were based on Liedke et al. (2012)."),
                            br(),

                            # selection box for water year 
                            wellPanel(h4("Select a water year to explore the tagging data."),
                                      selectInput("water_year_tagging", " ",
                                                  choices = sort(unique(tagging$water_year)),
                                                  selected = max(tagging$water_year)))), 

                     # tag picture and caption
                     column(6, 
                            br(), br(),
                            div(style = "text-align: center;",
                                tags$img(src = "tag.jpg")),
                            br(),
                            h5("Figure 1: JSATS ss400 acoustic transmitter", style = "text-align: center;"))),
             
            # add a line to separate from next row
            br(),
            hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
            
            ### morphometrics ------------------------------------------------------------------------------
            
            fluidRow(h2("Morphometrics"),
                     
                     # weight vs length plot & caption
                     column(11,   
                            br(),
                            plotOutput("weight_length_plot"), 
                            br(),
                            h5("Figure 2: Fish weight (g) as a function of fork length (mm) 
                               by month of tagging. Each point represents an individual fish.")),
                     
                     # legend image      
                     column(1, 
                            br(), br(),
                            tags$img(src = "legend2.png"), style = "text-align: left;")), # end row
            
            br(), 
            
            fluidRow(
              
              # weight table and histogram and caption
              column(6, 
                     h3("Weight (g)", class = "text-center"),
                     br(),
                     DTOutput("weight_table"),
                     br(),
                     plotOutput("weight_histogram"), 
                     br(),
                     h5("Figure 3: Distribution of Chinook salmon weight (g) by month of tagging.")), 
              
              # fork length table and histogram and caption
              column(6, 
                     h3("Fork Length (mm)", class = "text-center"),
                     br(),
                     DTOutput("fork_length_table"),
                     br(),
                     plotOutput("fork_length_histogram"),
                     br(),
                     h5("Figure 4: Distribution of Chinook salmon fork length (mm) by month of tagging."))), 
            
            # add a line to separate from next row
            br(), 
            hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
            
            ### surgery times ------------------------------------------------------------------------------
            
            fluidRow(h2("Anesthetic, Surgery, and Recovery Times"),
                     
                     # anesthetic table and boxplot       
                     column(4, 
                            br(),
                            h3("Anesthetic", class = "text-center"),
                            DTOutput("anesthetic_table"),
                            br(),
                            plotOutput("anesthetic_plot")),
                     
                     # surgery table and boxplot       
                     column(4, 
                            br(),
                            h3("Surgery", class = "text-center"),
                            DTOutput("surgery_table"),
                            br(),
                            plotOutput("surgery_plot")),
                     
                     # recovery table and boxplot       
                     column(4, 
                            br(),
                            h3("Recovery", class = "text-center"),
                            DTOutput("recovery_table"),
                            br(),
                            plotOutput("recovery_plot"))), 
            
            # row for figure caption
            fluidRow(br(), 
                     h5("Figure 5: Anesthetic, surgery, and recovery times for tagged Late-Fall and Fall Run juvenile Chinook salmon. The centerline of the boxplots 
                        represents the median, the box represents the interquartile range (IQR), the whiskers extend 1.5 times 
                        the IQR, and the black points represent values outside 1.5 times the IQR. 
                        The mean value for each boxplot is represented by a white point."), 
                     br(), br())
            
  ), # end nav panel - tagging

# -------------------------------------------------------------------------------
# shiny server
output$weight_length_plot <- renderPlot({
  
# require water year as an input
req(input$water_year_tagging)

tagging() %>% 
  # remove tag effects fish
  filter(!grepl("D", vial_number)) %>% 
  # filter by water year selection 
  filter(water_year == input$water_year_tagging)
  # plot
  ggplot(aes(x = weight_gr, y = fork_length_mm, color = month)) +
  facet_grid(~month) +
  geom_jitter() +
  scale_color_manual(values = c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                                 "Apr" = "#A2CD5A", "May" = "#FFC125")) +
  labs(x = "Weight (g)",
       y = "Fork Length (mm)") +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        title = element_blank(), 
        legend.position = "none",
        plot.caption = element_text(size = 16, face = "italic", hjust = 0, margin = margin(t = 20)),
        strip.text = element_text(size = 16))
  
}, res = 96) # end render weight vs length plot

# weight data table -------------------------------------------------------------------------------
# testable version
weight_df = tagging %>% 
    # remove tag effects fish
    filter(!grepl("D", vial_number)) %>% 
    group_by(water_year, month, run) %>% 
    summarize(n = n(),
              Minimum = round(min(weight_gr, na.rm = TRUE), 2),
              Maximum = round(max(weight_gr, na.rm = TRUE), 2),
              Mean = round(mean(weight_gr, na.rm = TRUE), 2),
              SD = round(sd(weight_gr, na.rm = TRUE), 2),
              .groups = "drop") %>%
    rename("Water Year" = water_year, 
           "Month" = month,
           "Run Type" = run)


# shiny version
output$weight_table <- renderDT({
  
  # require water year as an input
  req(input$water_year_tagging)

  weight_df = tagging() %>% 
    # remove tag effects fish
    filter(!grepl("D", vial_number)) %>% 
    # filter by water year selection
    filter(water_year == input$water_year_tagging) %>% 
    group_by(water_year, month, run) %>% 
    summarize(n = n(),
              Minimum = round(min(weight_gr, na.rm = TRUE), 2),
              Maximum = round(max(weight_gr, na.rm = TRUE), 2),
              Mean = round(mean(weight_gr, na.rm = TRUE), 2),
              SD = round(sd(weight_gr, na.rm = TRUE), 2),
              .groups = "drop") %>%
    rename("Water Year" = water_year, 
           "Month" = month,
           "Run Type" = run)
  
  # create the data table with selectable rows
  datatable(weight_df,
            rownames = FALSE,
            selection = list(mode = "single"),   
            options = list(dom = "t",
                           server = FALSE, 
                           columnDefs = list(list(targets = 0, 
                                                  visible = FALSE, 
                                                  width = "0px"))))
}) # end render weight data table

# weight histogram -------------------------------------------------------------------------------
# testable version
# not really testable here

# shiny version
output$weight_histogram <- renderPlot({
  
  # require water year as an input
  req(input$water_year_tagging)

  # when a row in the weight data table is clicked, highlight the corresponding data in the histogram
  selected_month = NULL
  row = input$weight_table_selected_row

  # if a row is selected, then...
  if (length(row) > 0) {

    weight_df_lookup = tagging() %>% 
        filter(water_year == input$water_year_tagging) %>% 
        group_by(month) %>% 
        summarize() %>% 
        ungroup()

    # map the clicked row number to the month's name
    selected_month = as.character(weight_df_lookup$month[row])

  # make the histogram data & highlight conditions
  weight_histogram = tagging() %>% 
    # remove tag effects fish
    filter(!grepl("D", vial_number)) %>% 
    # require water year as an input
    filter(water_year == input$water_year_tagging) %>%
    mutate(highlight = if (is.null(selected_month)) "all" # no selection
                       else ifelse(month == selected_month, "selected", "other"))
    
    if (is.null(selected_month)) {
      # no row is selected - normal plot
      ggplot(weight_histogram, aes(x = weight_gr, fill = month)) +
        geom_histogram(alpha = 0.7, position = "identity") +
        scale_fill_manual(values = c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                                     "Apr" = "#A2CD5A", "May" = "#FFC125")) +
        labs(x = "Weight (g)", 
             y = "Count") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 12),
              title = element_blank(),
              legend.position = "none",
              plot.caption = element_text(size = 18, face = "italic", hjust = 0, margin = margin(t = 20)))
      
    } # end if statement

    else {
      # row selected — highlight chosen month, grey out others
      ggplot(weight_histogram, aes(x = weight_gr,
                                   fill = month,
                                   alpha = highlight)) +
        geom_histogram(position = "identity") +
        scale_fill_manual(values = c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                                     "Apr" = "#A2CD5A", "May" = "#FFC125")) +
        scale_alpha_manual(values = c(selected = 0.85, other = 0.15),
                           guide = "none") +
        labs(x = "Weight (g)", 
             y = "Count") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 12),
              title = element_blank(),
              legend.position = "none")
      
    } # end else statement
    
}) # end weight histogram

# fork length data table ------------------------------------------------------------------------------
# testable version
fork_length_df = tagging %>% 
    group_by(water_year, month, run) %>% 
    summarize("n" = n(), 
              "Minimum" = round(min(fork_length_mm, na.rm = T), 2),
              "Maximum" = round(max(fork_length_mm, na.rm = T), 2),
              "Mean" = round(mean(fork_length_mm, na.rm = T), 2),
              "SD" = round(sd(fork_length_mm, na.rm = T), 2),
              .groups = "drop") %>%
      rename("Water Year" = water_year,
             "Month" = month,
             "Run Type" = run)
  
# shiny version
output$fork_length_table <- renderDT({
  
  # require water year as an input
  req(input$water_year_tagging)

  fork_length_df = tagging() %>% 
    filter(water_year == input$water_year_tagging) %>% 
    group_by(water_year, month, run) %>% 
    summarize("n" = n(), 
              "Minimum" = round(min(fork_length_mm, na.rm = T), 2),
              "Maximum" = round(max(fork_length_mm, na.rm = T), 2),
              "Mean" = round(mean(fork_length_mm, na.rm = T), 2),
              "SD" = round(sd(fork_length_mm, na.rm = T), 2),
              .groups = "drop") %>%
      rename("Water Year" = water_year,
             "Month" = month,
             "Run Type" = run)
    
    # creates data table with selectable row
    datatable(fork_length_df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
    
  }) # end render fork length data table 

# fork length histogram -------------------------------------------------------------------------------
# testable version
# not really testable here

# shiny version
output$fork_length_histogram <- renderPlot({
  
  # require water year as an input
  req(input$water_year_tagging)

  # when a row in the weight data table is clicked, highlight the corresponding data in the histogram
  selected_month = NULL
  row = input$weight_table_selected_row

  # if a row is selected, then...
  if (length(row) > 0) {

    fork_length_df_lookup = tagging() %>% 
        filter(water_year == input$water_year_tagging) %>% 
        group_by(month) %>% 
        summarize() %>% 
        ungroup()

    # map the clicked row number to the month's name
    selected_month = as.character(fork_length_df_lookup$month[row])

  # make the histogram data & highlight conditions
  fork_length_histogram = tagging() %>% 
    # remove tag effects fish
    filter(!grepl("D", vial_number)) %>% 
    filter(water_year == input$water_year_tagging) %>%
    mutate(highlight = if (is.null(selected_month)) "all" # no selection
                       else ifelse(month == selected_month, "selected", "other"))
    
    if (is.null(selected_month)) {
      # no row is selected - normal plot
      ggplot(fork_length_histogram, aes(x = fork_length_mm, fill = month)) +
        geom_histogram(alpha = 0.7, position = "identity") +
        scale_fill_manual(values = c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                                     "Apr" = "#A2CD5A", "May" = "#FFC125")) +
        labs(x = "Fork Length (mm)", 
             y = "Count") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 12),
              title = element_blank(),
              legend.position = "none",
              plot.caption = element_text(size = 18, face = "italic", hjust = 0, margin = margin(t = 20)))
      
    } # end if statement

    else {
      # row selected — highlight chosen month, grey out others
      ggplot(fork_length_histogram, aes(x = fork_length_mm,
                                        fill = month,
                                        alpha = highlight)) +
        geom_histogram(position = "identity") +
        scale_fill_manual(values = c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                                     "Apr" = "#A2CD5A", "May" = "#FFC125")) +
        scale_alpha_manual(values = c(selected = 0.85, other = 0.15),
                           guide = "none") +
        labs(x = "Fork Length (mm))", 
             y = "Count") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 12),
              title = element_blank(),
              legend.position = "none")
      
    } # end else statement
    
}) # end fork length histogram

# anesthetic times data table -------------------------------------------------------------------------------
# testable version
anesthetic_df = tagging %>% 
    # remove tag effects fish
    filter(!grepl("D", vial_number)) %>%
    group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_anes = round(min(anesthetic_time_sec, na.rm = TRUE)),
                max_anes = round(max(anesthetic_time_sec, na.rm = TRUE)),
                mean_anes = round(mean(anesthetic_time_sec, na.rm = TRUE)),
                sd_anes = round(sd(anesthetic_time_sec, na.rm = TRUE))) %>% 
      mutate(min_anes = sprintf("%02d:%02d", min_anes %/% 60, min_anes %% 60),
             max_anes = sprintf("%02d:%02d", max_anes %/% 60, max_anes %% 60),
             mean_anes = sprintf("%02d:%02d", mean_anes %/% 60, mean_anes %% 60),
             sd_anes = sprintf("%02d:%02d", sd_anes %/% 60, sd_anes %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_anes, 
             "Max" = max_anes, 
             "Mean" = mean_anes, 
             "SD" = sd_anes)

# shiny version
output$anesthetic_table <- renderDT({
  
  # require water year as an input
  req(input$water_year_tagging)

  anesthetic_df = tagging() %>% 
    # remove tag effects fish
    filter(!grepl("D", vial_number)) %>%
    filter(water_year == input$water_year_tagging) %>% 
    group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_anes = round(min(anesthetic_time_sec, na.rm = TRUE)),
                max_anes = round(max(anesthetic_time_sec, na.rm = TRUE)),
                mean_anes = round(mean(anesthetic_time_sec, na.rm = TRUE)),
                sd_anes = round(sd(anesthetic_time_sec, na.rm = TRUE))) %>% 
      mutate(min_anes = sprintf("%02d:%02d", min_anes %/% 60, min_anes %% 60),
             max_anes = sprintf("%02d:%02d", max_anes %/% 60, max_anes %% 60),
             mean_anes = sprintf("%02d:%02d", mean_anes %/% 60, mean_anes %% 60),
             sd_anes = sprintf("%02d:%02d", sd_anes %/% 60, sd_anes %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_anes, 
             "Max" = max_anes, 
             "Mean" = mean_anes, 
             "SD" = sd_anes)
  
  # creates data table with selectable row
    datatable(anesthetic_df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
  }) # end render anesthetic data table
 
# anesthetic times plot -------------------------------------------------------------------------------
# testable version
    tagging %>%
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      ggplot(aes(x = run, y = anesthetic_time_sec, fill = run))+
      geom_boxplot() +
      labs(x = "Run Type",
           y = "Time in Anesthetic (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D"))+
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")

# shiny version
output$anesthetic_plot <- renderPlot({
  
  # require water year as an input
  req(input$water_year_tagging)
    
    tagging() %>%
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      filter(water_year == input$water_year_tagging) %>%
      ggplot(aes(x = run, y = anesthetic_time_sec, fill = run))+
      geom_boxplot() +
      labs(x = "Run Type",
           y = "Time in Anesthetic (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D"))+
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
}) # end render anesthetic plot

# surgery times data table -------------------------------------------------------------------------------
# testable version
surgery_df <- tagging %>% 
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_surg = round(min(surgery_time_sec, na.rm = TRUE)),
                max_surg = round(max(surgery_time_sec, na.rm = TRUE)),
                mean_surg = round(mean(surgery_time_sec, na.rm = TRUE)),
                sd_surg = round(sd(surgery_time_sec, na.rm = TRUE))) %>% 
      mutate(min_surg = sprintf("%02d:%02d", min_surg %/% 60, min_surg %% 60),
             max_surg = sprintf("%02d:%02d", max_surg %/% 60, max_surg %% 60),
             mean_surg = sprintf("%02d:%02d", mean_surg %/% 60, mean_surg %% 60),
             sd_surg = sprintf("%02d:%02d", sd_surg %/% 60, sd_surg %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_surg, 
             "Max" = max_surg, 
             "Mean" = mean_surg, 
             "SD" = sd_surg)

# shiny version
output$surgery_table <- renderDT({
    
    # require water_year as an input  
    req(input$water_year_tagging)
    
    surgery_df <- tagging() %>% 
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      filter(water_year == input$water_year_tagging) %>% 
      group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_surg = round(min(surgery_time_sec, na.rm = TRUE)),
                max_surg = round(max(surgery_time_sec, na.rm = TRUE)),
                mean_surg = round(mean(surgery_time_sec, na.rm = TRUE)),
                sd_surg = round(sd(surgery_time_sec, na.rm = TRUE))) %>% 
      mutate(min_surg = sprintf("%02d:%02d", min_surg %/% 60, min_surg %% 60),
             max_surg = sprintf("%02d:%02d", max_surg %/% 60, max_surg %% 60),
             mean_surg = sprintf("%02d:%02d", mean_surg %/% 60, mean_surg %% 60),
             sd_surg = sprintf("%02d:%02d", sd_surg %/% 60, sd_surg %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_surg, 
             "Max" = max_surg, 
             "Mean" = mean_surg, 
             "SD" = sd_surg)
    
    # creates data table with selectable row
    datatable(surgery_df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
  }) # end render surgery data table

# surgery times plot -------------------------------------------------------------------------------
# testable version
 tagging %>%
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      ggplot(aes(x = run, y = surgery_time_sec, fill = run)) +
      geom_boxplot() +
      labs(x = "Run Type",
           y = "Time in Surgery (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D")) +
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")

# shiny version
 output$surgery_plot <- renderPlot({
   
   # require water year as an input
   req(input$water_year_tagging)
    
    tagging() %>%
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      filter(water_year == input$water_year_tagging) %>%
      ggplot(aes(x = run, y = surgery_time_sec, fill = run)) +
      geom_boxplot() +
      labs(x = "Run Type",
           y = "Time in Surgery (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D")) +
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
 }) # end render surgery plot

# recovery times data table -------------------------------------------------------------------------------
# testable version
recovery_df <- tagging %>% 
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_rec = round(min(recovery_time_sec, na.rm = TRUE)),
                max_rec = round(max(recovery_time_sec, na.rm = TRUE)),
                mean_rec = round(mean(recovery_time_sec, na.rm = TRUE)),
                sd_rec = round(sd(recovery_time_sec, na.rm = TRUE))) %>% 
      mutate(min_rec = sprintf("%02d:%02d", min_rec %/% 60, min_rec %% 60),
             max_rec = sprintf("%02d:%02d", max_rec %/% 60, max_rec %% 60),
             mean_rec = sprintf("%02d:%02d", mean_rec %/% 60, mean_rec %% 60),
             sd_rec = sprintf("%02d:%02d", sd_rec %/% 60, sd_rec %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_rec, 
             "Max" = max_rec, 
             "Mean" = mean_rec, 
             "SD" = sd_rec)

# shiny version
output$recovery_table <- renderDT({
    
    # require water_year as an input  
    req(input$water_year_tagging)
    
    recovery_df <- tagging() %>% 
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      filter(water_year == input$water_year_tagging) %>% 
      group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_rec = round(min(recovery_time_sec, na.rm = TRUE)),
                max_rec = round(max(recovery_time_sec, na.rm = TRUE)),
                mean_rec = round(mean(recovery_time_sec, na.rm = TRUE)),
                sd_rec = round(sd(recovery_time_sec, na.rm = TRUE))) %>% 
      mutate(min_rec = sprintf("%02d:%02d", min_rec %/% 60, min_rec %% 60),
             max_rec = sprintf("%02d:%02d", max_rec %/% 60, max_rec %% 60),
             mean_rec = sprintf("%02d:%02d", mean_rec %/% 60, mean_rec %% 60),
             sd_rec = sprintf("%02d:%02d", sd_rec %/% 60, sd_rec %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_rec, 
             "Max" = max_rec, 
             "Mean" = mean_rec, 
             "SD" = sd_rec)
    
    # creates data table with selectable row
    datatable(recovery_df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
  }) # end render recovery data table 

# recover times plot  -------------------------------------------------------------------------------
# testable version
tagging %>%
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      ggplot(aes(x = run, y = recovery_time_sec, fill = run)) +
      geom_boxplot() +
      labs(x = "Run Type",
           y = "Time in Recovery (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D")) +
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")

# shiny version
output$recovery_plot <- renderPlot({
  
  # require water year as an input
  req(input$water_year_tagging)
    
    tagging() %>%
      # remove tag effects fish
      filter(!grepl("D", vial_number)) %>%
      filter(water_year == input$water_year_tagging) %>%
      ggplot(aes(x = run, y = recovery_time_sec, fill = run)) +
      geom_boxplot() +
      labs(x = "Run Type",
           y = "Time in Recovery (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D")) +
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
}) # end render recovery plot