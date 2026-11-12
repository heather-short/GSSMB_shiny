# GSSMB Shiny App
# H. Short
# last update: 2026-11-09 15:04:31

# load required packages ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)  
library(tidyverse)
library(shinythemes)
library(thematic)
library(paletteer)

# load data ------------------------------------------------------------------------------

# tagging page
tagging = readRDS("data/historical_released_fish_tagging_data.rds") # for all tagging plots/tables

# release page
release_dates = readRDS("data/release_date_rectangles.rds") # for water quality plot on release page
water_quality = readRDS("data/historical_water_quality.rds") # for water quality plot on release page
release_wq = readRDS("data/historical_release_water_quality_summary.rds") # for wq/release data table

# tag life page
tag_life_2026 = readRDS("data/tag_life_survival_2026.rds") # for tag life plot

# report page
tagging_data = readRDS("data/historical_tagging_data.rds") # for tagging report
release_data = readRDS("data/historical_release_data.rds") # for release report
for_calfishtrack = readRDS("data/calfishtrack_report.rds") # for calfishtrack report

# begin user interface ------------------------------------------------------------------------------

ui <- page_navbar(
  
  # sets the color theme
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  
  # app title
  title = "Georgiana Slough Salmonid Migratory Barrier Project", 
  navbar_options = navbar_options(underline = T),
  
  # make navbar dark
  tags$head(
    tags$style(HTML(".navbar {background-color: #343a40 !important;}
                           .navbar .navbar-brand,
                           .navbar .nav-link {color: #ffffff !important;}
                           .navbar .nav-link.active {color: #dcdcdc !important;}"))),
  
  # change color of table selection
  tags$head(
    tags$style(HTML("table.dataTable tbody tr.selected td,
                           table.dataTable tbody td.selected 
                           {border-top-color: white !important;
                            box-shadow: inset 0 0 0 9999px #53868B !important;}
                           table.dataTable tbody tr:active td 
                               {background-color: #53868B !important;}
                           :root {--dt-row-selected: transparent !important;}
                           table.dataTable tbody tr:hover, 
                           table.dataTable tbody tr:hover td 
                           {background-color: #D1EEEE !important;}"))),
                            
 # change view to 90%
 # tags$head(
 #   tags$style(HTML("body {zoom: 90%;}"))),
  
  # home page ------------------------------------------------------------------------------
  
  nav_panel(title = "Background", 
            
            fluidRow(h2("Background"),
        
              column(5,
                     br(), br(),
                     h5("The Georgiana Slough Salmonid Migratory Barrier (GSSMB) is a 640-foot-long bio-acoustic fish fence (BAFF) 
                        that uses complementary lights, sounds, and a bubble curtain to deter out-migrating juvenile Chinook salmon 
                        away from the entrance of Georgiana Slough to remain in the mainstem Sacramento River as they make their way 
                        to the ocean. If entrained in Georgiana Slough, fish enter the complex and highly altered central and south Delta, 
                        increasing travel time and decreasing through-Delta survival following increased contact with sub-optimal habitat, 
                        predators, and agriculture and water-project pumps. Beginning in 2023, the California Department of Water Resources 
                        began the annual deployment and operation of the GSSMB from November to May. Additionally, using acoustic telemetry, 
                        seasonal routing and survival studies have been implemented to evaluate the efficiency of the Barrier on out-migrating 
                        juvenile Chinook salmon."),
                     br(),   
                     h5("Navigate through this Shiny App to explore the GSSMB Effectiveness Study data from water years 2024, 2025, and 2026."),
                     br(), br(),

                     # salmon image and caption
                     div(style = "text-align: center;",
                         tags$img(src = "salmon2.jpg")),
                     br(),
                     h5("Figure 1: Juvenile Chinook salmon being measured before tagging surgery.", style = "text-align: center;")),
                     
              
              column(7, 
                     br(), br(),

                     # barrier image and caption
                     div(style = "text-align: center;",
                         tags$img(src = "barrier.jpg", height = 700, width = 1000)),
                     br(),
                     h5("Figure 2: Aerial view of the Georgiana Slough Salmonid Migratory Barrier looking downstream at the Sacramento River/Georgiana Slough divergence. 
                        The Sacramento River continues (right) while Georgiana Slough branches off (left).", style = "text-align: center;"))),
            
  ), # end nav panel - background
  
  # tagging page ------------------------------------------------------------------------------
  
  nav_panel(title = "Tagging", 
            
            fluidRow(h2("Tagging Procedure"),
                           
                     column(6, 
                            br(),
                            h5("Methods: \n
                                To begin each surgical procedure, juvenile Chinook salmon were placed in an anesthesia solution 
                                until they lost equilibrium and were no longer responsive to touch. The fish 
                                was removed from anesthesia and weighed to the nearest 0.1 g and fork length 
                                was measured to the nearest mm. Fish were then placed ventral side up on a foam 
                                block before an irrigation tube was placed in or near the mouth to allow for 
                                irrigation of the gills with a lower dose of the anesthetic solution. A small 
                                incision was made parallel to the mid-ventral line and an ss400 JSATS acoustic
                                transmitter was inserted into the coelomic cavity before the incision was closed 
                                with a single uninterrupted 2 x 2 x 2 surgical knot. The duration of each surgery 
                                was measured from the time the fish was removed from anesthesia to the time it was 
                                placed in recovery following surgery. Tagging methods were based on Liedtke et al. (2012)."),
                            br(),

                            # selection box for water year 
                            wellPanel(h4("Select a water year to explore the tagging data."),
                                      selectInput("wy", " ",
                                                  choices = sort(unique(tagging$water_year)),
                                                  selected = "2026"))), 

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
                     column(12,   
                            br(),
                            plotOutput("plot_wl"), 
                            br(),
                            h5("Figure 2: Fish weight (g) as a function of fork length (mm) 
                               by tagging block. Each point represents an individual fish."))),
            
            br(), 
            
            fluidRow(
              
              # weight table and histogram and caption
              column(6, 
                     h3("Weight (g)", class = "text-center"),
                     br(),
                     DTOutput("table_weight"),
                     br(),
                     plotOutput("hist_weight"), 
                     br(),
                     h5("Figure 3: Distribution of Chinook salmon weight (g) by tagging block.")), 
              
              # fork length table and histogram and caption
              column(6, 
                     h3("Fork Length (mm)", class = "text-center"),
                     br(),
                     DTOutput("table_fl"),
                     br(),
                     plotOutput("hist_fl"),
                     br(),
                     h5("Figure 4: Distribution of Chinook salmon fork length (mm) by tagging block."))), 
            
            # add a line to separate from next row
            br(), 
            hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
            
            ### surgery times ------------------------------------------------------------------------------
            
            fluidRow(h2("Anesthetic, Surgery, and Recovery Times"),
                     
                     # anesthetic table and boxplot       
                     column(4, 
                            br(),
                            h3("Anesthetic", class = "text-center"),
                            DTOutput("anes_times"),
                            br(),
                            plotOutput("anesthetic_boxplot")),
                     
                     # surgery table and boxplot       
                     column(4, 
                            br(),
                            h3("Surgery", class = "text-center"),
                            DTOutput("surg_times"),
                            br(),
                            plotOutput("surgery_boxplot")),
                     
                     # recovery table and boxplot       
                     column(4, 
                            br(),
                            h3("Recovery", class = "text-center"),
                            DTOutput("rec_times"),
                            br(),
                            plotOutput("recovery_boxplot"))), 
            
            # row for figure caption
            fluidRow(br(), 
                     h5("Figure 5: Anesthetic, surgery, and recovery times for tagged Late-Fall and Fall Run juvenile Chinook salmon. The centerline of the boxplots 
                        represents the median, the box represents the interquartile range (IQR), the whiskers extend 1.5 times 
                        the IQR, and the black points represent values outside 1.5 times the IQR. 
                        The mean value for each boxplot is represented by a white point."), 
                     br(), br())
            
  ), # end nav panel - tagging
  
  # release page ------------------------------------------------------------------------------
  
  nav_panel(title = "Releases", 
            
            fluidRow(h2("Release Procedure"),
                         
                     column(6, 
                            br(),
                            h5("Methods: Tagged juvenile Chinook salmon were released over a two-week block, to cover a spring-neap tidal cycle, 
                                and at regular intervals throughout the diel cycle. Small sub-groups of ten tagged fish were 
                                released in the main channel of the Sacramento River near the Sacramento Marina (38.560, -121.517) 
                                every 3 hours on each study release date, resulting in eight subgroups daily 
                                (midnight, 3 a.m., 6 a.m., 9 a.m., noon, 3 p.m., 6 p.m., and 9 p.m.). 
                                Releases occurred over a 72-hour period during each week."),
                            br(),

                            # selection box for water year
                            wellPanel(h4("Select a water year and release block to explore the release data."),
                                      selectInput("water_year", "Water Year:",
                                                  choices  = sort(unique(release_wq$water_year)),
                                                  selected = "2026"),
                                      selectInput("block_id", "Release Block:", choices = NULL))),

                     # release picture and caption
                     column(6,
                            div(style = "text-align: center;",
                                tags$img(src = "release2.jpg", height = 500, width = 800)),
                            br(),
                            h5("Figure 1: Fish being released into the Sacramento River.", style = "text-align: center;"))),
            
            # add a line to separate from next row
            br(), 
            hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
            
            ### water quality ------------------------------------------------------------------------------
            
            fluidRow(h2("Water Quality during Fish Releases"),
                     
                     # water quality table       
                     column(6,   
                            br(),
                            DTOutput("release_wq_table")),
                
                     # water quality table       
                     column(6,   
                            br(),
                            plotOutput("water_quality_plot"))),

            # row for figure caption
            fluidRow(br(), 
                     h5("Figure 2: Water quality at Freeport, CA. Shaded rectangles represent 72-hour release periods for each block 
                     of the study. The highlighted rectangles represent the selected Block. Data source: CDEC and USGS."), 
                     br(), br())
            
  ), # end nav panel - releases

# tag life page ------------------------------------------------------------------------------

nav_panel(title = "Tag Life", 
          
          fluidRow(h2("Tag Life Procedure"),
                      
                   column(6, 
                          br(),
                          h5("Methods: Tag life was evaluated to ensure that the transmitters used throughout study met the 
                             manufacturer’s stated run time. Tag life tags were activated at the same time as the rest of the 
                             tags for the tagging block. For every week of tagging, eight additional tags were activated and 
                             deployed in a tank with an acoustic receiver. The receiver was downloaded monthly and tags were 
                             left in the tank until their battery failed. Using tag detection data, the plot below was created 
                             confirm tag batteries ran to the manufacturer’s expected run time (at least 71 days)."),
                          br(),

                          # selection box for water year
                          wellPanel(h4("Select a water year to explore the tag life data."),
                                    selectInput("water_year", " ",
                                                choices  = sort(unique(release_wq$water_year)),
                                                selected = "2026"))),

                   # tag vial picture and caption   
                   column(6,
                          div(style = "text-align: center;",
                              tags$img(src = "tags.jpg", height = 500, width = 800)),
                          br(),
                          h5("Figure 1: Box of numbered tag vials.", style = "text-align: center;"))),
          
          # add a line to separate from next row
            br(), 
            hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
          
          ### tag life plot ------------------------------------------------------------------------------
          
          fluidRow(h2("Tag Life Data"),
                   
                   # tag life plot or message for no data and figure caption 
                   column(12,   
                          br(),
                          uiOutput("plotOrMessage"),
                          br(),
                          h5("Figure 2: Proportion of tags active plotted over time (days since tag activation). Colored lines represent individual tagging blocks 
                             (Block 1 = December, Block 2 = January, Block 3 = February, Block 4 = April) and unique batches of tags. 
                             The dashed line represents the 71 day tag life expectancy for JSATS model ss400 transmitters.")))
          
), # end nav panel - tag life

# download data ------------------------------------------------------------------------------

nav_panel(title = "Download Data", 
          
          fluidPage(
            
            fluidRow(h5("Make Water Year and Tagging Block selections below to generate reports for 
                        tagging data, release data, and for the CalFishTrack upload template. 
                        Select Download CSV once you have generated the appropriate report."),
                     br(), br(), 
              
              column(4,

                     wellPanel(h4("Tagging Report"),

                               # selection box for water year
                               selectInput("water_year", "Water Year:",
                                           choices  = sort(unique(tagging_data$water_year)),
                                           selected = "2026"),

                               # buttons
                               actionButton("tagging_gen_report", "Generate Report", class = "btn-primary"),
                               br(), br(),
                               downloadButton("tag_download", "Download CSV"))),
              
              column(4,
                     wellPanel(h4("Release Report"),

                               # selection box for water year
                               selectInput("water_year", "Water Year:",
                                           choices  = sort(unique(release_data$water_year)),
                                           selected = "2026"),

                               # buttons
                               actionButton("release_gen_report", "Generate Report", class = "btn-primary"),
                               br(), br(),
                               downloadButton("rel_download", "Download CSV"))),
              
              column(4,
                     wellPanel(h4("CalFishTrack Report"),

                               # selection box for water year and block
                               selectInput("water_year", "Water Year:",
                                           choices  = sort(unique(for_calfishtrack$water_year)),
                                           selected = "2026"),
                               selectInput("block", "Tagging & Release Block:",
                                           choices  = sort(unique(for_calfishtrack$block)),
                                           selected = "Block 1A"),

                               # buttons
                               actionButton("calfishtrack_gen_report", "Generate Report", class = "btn-primary"),
                               br(), br(),
                               downloadButton("calfishtrack_download", "Download CSV")))),
            
            fluidRow(
              
              # output for generate report
              column(12,
                     h3(textOutput("active_report_title")),
                     DTOutput("report_table")))),
          
) # end nav panel - download data
          
) # end page nav bar - end ui ------------------------------------------------------------------------------

# server logic ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # weight and length plot ------------------------------------------------------------------------------
  
  output$plot_wl <- renderPlot({
    
    # require water_year as an input
    req(input$wy)
    
    tagging %>% 
      # filter by water year selection
      filter(water_year == input$wy) %>% 
      
      # plot
      ggplot(aes(x = weight_gr, y = fork_length_mm, color = block)) + 
      facet_grid(~block) + 
      geom_jitter() + 
      scale_color_paletteer_d("MoMAColors::Ernst") +
      labs(x = "Weight (g)",
           y = "Fork Length (mm)") +
      theme(axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            plot.caption = element_text(size = 16, face = "italic", hjust = 0, margin = margin(t = 20)),
            title = element_blank(), 
            strip.text = element_text(size = 16))
    
  }, res = 96) # end render weight vs length plot
  
  # weight data table ------------------------------------------------------------------------------
  
  output$table_weight <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df = tagging %>%
      select(water_year, block, run, weight_gr) %>%
      # filter by water year selection
      filter(water_year == input$wy) %>%
      # modify data types
      mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                              "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5"))) %>%
      group_by(water_year, block, run) %>%
      summarize(n = n(),
                Minimum = round(min(weight_gr, na.rm = TRUE), 2),
                Maximum = round(max(weight_gr, na.rm = TRUE), 2),
                Mean = round(mean(weight_gr, na.rm = TRUE), 2),
                SD = round(sd(weight_gr, na.rm = TRUE), 2)) %>%
      ungroup() %>%
      rename("Water Year" = water_year,
             "Block" = block,
             "Run Type" = run)
    
    # creates data table with selectable row
    datatable(df,
              rownames  = FALSE,
              selection = list(mode = "single"),
              options   = list(
                dom = "t",
                columnDefs = list(
                  list(targets = 0, visible = FALSE),
                  list(targets = "_all", className = "dt-center")
                )
              ))
  }, server = FALSE) # end render data table
  
  # weight histogram ------------------------------------------------------------------------------
  
  output$hist_weight <- renderPlot({
    
    # require water_year as an input
    req(input$wy)
    
    # get the selected block from the clicked row (NULL if none selected)
    selected_block <- NULL # default, nothing selected
    row <- input$table_weight_rows_selected # clicked row on the table

    # if a row is selected
    if (length(row) > 0) {
      df_lookup <- tagging %>%
        filter(water_year == input$wy) %>%
        mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                                "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5"))) %>%
        group_by(block) %>%
        summarize() %>% # collapse to one row per block
        ungroup()
      
      # map the clicked row number to the actual block name
      # if no row is selected, this code is skipped
      selected_block <- as.character(df_lookup$block[row])} # end row selection
    
    # make the histogram  
    plot_data <- tagging %>%
      
      # require water_year as an input
      filter(water_year == input$wy) %>%
      mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                              "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5")),
             
             highlight = if (is.null(selected_block)) "all" # no selection, everything is tagged as all
             else ifelse(block == selected_block, "selected", "other")) # selection = selected, everything else = other
    
    if (is.null(selected_block)) {
      # No row selected — normal plot, colored by block
      ggplot(plot_data, aes(x = weight_gr, fill = block)) +
        geom_histogram(alpha = 0.7, position = "identity") +
        scale_fill_paletteer_d("MoMAColors::Ernst") +
        labs(x = "Weight (g)", 
             y = "Count") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 12),
              legend.position = "none",
              plot.caption = element_text(size = 18, face = "italic", hjust = 0, margin = margin(t = 20)),
              title = element_blank())
      
    } # end if statement
    
    else {
      # row selected — highlight chosen month, grey out others
      ggplot(plot_data, aes(x = weight_gr,
                            fill = block,
                            alpha = highlight)) +
        geom_histogram(position = "identity") +
        scale_fill_paletteer_d("MoMAColors::Ernst") +
        scale_alpha_manual(values = c(selected = 0.90, other = 0.10),
                           guide = "none") +
        labs(x = "Weight (g)", 
             y = "Count") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 12),
              legend.position = "none",
              title = element_blank())
      
    } # end else statement
    
  }) # end weight histogram
  
  # fork length data table ------------------------------------------------------------------------------
  
  output$table_fl <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df = tagging %>%
      select(water_year, block, run, fork_length_mm) %>%
      filter(water_year == input$wy) %>%
      mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                              "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5"))) %>%
      group_by(water_year, block, run) %>%
      summarize("n" = n(), 
                "Minimum" = round(min(fork_length_mm, na.rm = T), 2),
                "Maximum" = round(max(fork_length_mm, na.rm = T), 2),
                "Mean" = round(mean(fork_length_mm, na.rm = T), 2),
                "SD" = round(sd(fork_length_mm, na.rm = T), 2)) %>%
      ungroup() %>%
      rename("Water Year" = water_year,
             "Block" = block,
             "Run Type" = run)
    
    # creates data table with selectable row
    datatable(df,
              rownames  = FALSE,
              selection = list(mode = "single"),
              options   = list(
                dom = "t",
                columnDefs = list(
                  list(targets = 0, visible = FALSE),
                  list(targets = "_all", className = "dt-center")
                )
              ))
  }, server = FALSE) # end render data table 
  
  # fork length histogram ------------------------------------------------------------------------------
  
  output$hist_fl <- renderPlot({
    
    # require water_year as an input
    req(input$wy)
    
    # get the selected block from the clicked row (NULL if none selected)
    selected_block <- NULL # default, nothing selected
    row <- input$table_fl_rows_selected # clicked row on the table

    # if a row is selected
    if (length(row) > 0) {
      df_lookup <- tagging %>%
        filter(water_year == input$wy) %>%
        mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                                "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5"))) %>%
        group_by(block) %>%
        summarize() %>% # collapse to one row per block
        ungroup()
      
      # map the clicked row number to actual block name
      # if no row is selected, this code is skipped
      selected_block <- as.character(df_lookup$block[row])} # end row selection
    
    # make the histogram  
    df = tagging %>%
      
      # require water_year as an input
      filter(water_year == input$wy) %>%
      mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                              "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5")),
             
             highlight = if (is.null(selected_block)) "all" # no selection, everything is tagged as all
             else ifelse(block == selected_block, "selected", "other")) # selection = selected, everything else = other
    
    if (is.null(selected_block)) {
      # no row selected — normal plot, colored by block
      ggplot(df, aes(x = fork_length_mm, fill = block)) +
        geom_histogram(alpha = 0.7, position = "identity") +
        scale_fill_paletteer_d("MoMAColors::Ernst") +
        scale_y_continuous(position = "right") +
        labs(x = "Fork Length (mm)", 
             y = "Count") +
        theme(axis.title.x  = element_text(size = 16),
              axis.text.x   = element_text(size = 12),
              axis.title.y  = element_text(size = 16),
              axis.text.y   = element_text(size = 12),
              legend.position = "none",
              title = element_blank()
        )} # end if statement
    
    else {
      # row selected — highlight chosen block, grey out others
      ggplot(df, aes(x = fork_length_mm,
                            fill = block,
                            alpha = highlight)) +
        geom_histogram(position = "identity") +
        scale_fill_paletteer_d("MoMAColors::Ernst") +
        scale_y_continuous(position = "right") +
        scale_alpha_manual(values = c(selected = 0.90, other = 0.10),
                           guide = "none") +
        labs(x = "Fork Length (mm)", 
             y = "Count") +
        theme(axis.title.x  = element_text(size = 16),
              axis.text.x   = element_text(size = 12),
              axis.title.y  = element_text(size = 16),
              axis.text.y   = element_text(size = 12),
              legend.position = "none",
              title = element_blank())
      
    } # end else statement
    
  }) # end length histogram
  
  # anesthetic table and boxplot ------------------------------------------------------------------------------
  
  # data table
  output$anes_times <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df <- tagging %>% 
      filter(water_year == input$wy) %>% 
      mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                              "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5"))) %>% 
      group_by(water_year, block, run) %>% 
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
             "Block" = block,
             "Run Type" = run, 
             "Min" = min_anes, 
             "Max" = max_anes, 
             "Mean" = mean_anes, 
             "SD" = sd_anes)
    
    # creates data table with selectable row
    datatable(df,
              rownames  = FALSE,
              selection = list(mode = "single"),
              options   = list(
                dom = "t",
                columnDefs = list(
                  list(targets = 0, visible = FALSE),
                  list(targets = "_all", className = "dt-center")
                )
              ))
  }, server = FALSE) # end render data table
  
  # boxplot
  output$anesthetic_boxplot <- renderPlot({req(input$wy)
    
    tagging %>%
      filter(water_year == input$wy) %>%
      ggplot(aes(x = run, y = anesthetic_time_sec, fill = run))+
      geom_boxplot()+
      labs(x = "Run Type",
           y = "Time in Anesthetic (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#8CBF9AFF", "fall" = "#315B88FF"))+
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
  }) # end render plot
  
  # surgery table and boxplot ------------------------------------------------------------------------------
  
  # data table
  output$surg_times <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df <- tagging %>% 
      filter(water_year == input$wy) %>% 
      mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                              "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5"))) %>% 
      group_by(water_year, block, run) %>% 
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
             "Block" = block,
             "Run Type" = run, 
             "Min" = min_surg, 
             "Max" = max_surg, 
             "Mean" = mean_surg, 
             "SD" = sd_surg)
    
    # creates data table with selectable row
    datatable(df,
              rownames  = FALSE,
              selection = list(mode = "single"),
              options   = list(
                dom = "t",
                columnDefs = list(
                  list(targets = 0, visible = FALSE),
                  list(targets = "_all", className = "dt-center")
                )
              ))
  }, server = FALSE) # end render data table
  
  # boxplot
  output$surgery_boxplot <- renderPlot({req(input$wy)
    
    tagging %>%
      filter(water_year == input$wy) %>%
      ggplot(aes(x = run, y = surgery_time_sec, fill = run))+
      geom_boxplot() +
      labs(x = "Run Type",
           y = "Time in Surgery (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#8CBF9AFF", "fall" = "#315B88FF"))+
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
  }) # end render boxplot
  
  # recovery table and boxplot ------------------------------------------------------------------------------
  
  # data table
  output$rec_times <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df <- tagging %>% 
      filter(water_year == input$wy) %>% 
      mutate(block = factor(block, levels = c("Block 1A", "Block 1B", "Block 2A", "Block 2B", 
                                              "Block 3A", "Block 3B", "Block 4A", "Block 4B", "Block 5"))) %>% 
      group_by(water_year, block, run) %>% 
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
             "Block" = block,
             "Run Type" = run, 
             "Min" = min_rec, 
             "Max" = max_rec, 
             "Mean" = mean_rec, 
             "SD" = sd_rec)
    
    # creates data table with selectable row
    datatable(df,
              rownames  = FALSE,
              selection = list(mode = "single"),
              options   = list(
                dom = "t",
                columnDefs = list(
                  list(targets = 0, visible = FALSE),
                  list(targets = "_all", className = "dt-center")
                )
              ))
  }, server = FALSE) # end render data table 
  
  # boxplot
  output$recovery_boxplot <- renderPlot({req(input$wy)
    
    tagging %>%
      filter(water_year == input$wy) %>%
      ggplot(aes(x = run, y = recovery_time_sec, fill = run))+
      geom_boxplot()+
      labs(x = "Run Type",
           y = "Time in Recovery (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#8CBF9AFF", "fall" = "#315B88FF"))+
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
  }) # end render boxplot
  
  # release water quality data table ------------------------------------------------------------------------------
  
  # update block_id choices when water_year changes
  observe({
    
    # require water year as an input
    req(input$water_year)
    
    blocks <- release_wq %>%
      # filter by water year selection
      filter(water_year == input$water_year) %>%
      # pull all of the blocks as a vector
      pull(block) %>%
      # choose only the unique blocks
      unique() %>%
      # sort in chronological order
      sort()
    
    updateSelectInput(session, "block_id",
                      choices  = blocks,
                      selected = blocks[1])
  })
  
  # filtered table data
  filtered_release_wq <- reactive({
    
    # require both water year and release date as an input
    req(input$water_year, input$block_id)
    
    release_wq %>%
      filter(water_year == input$water_year,
             block == input$block_id) %>%
      ungroup() %>% 
      mutate(rel_id = factor(rel_id, levels = c(paste0("RT-", 1:24), "point_rel"))) %>% 
      arrange(rel_id) %>%
      select(rel_id, release_date, release_time, lunar_phase, fish_released, Temperature, Turbidity, Discharge) %>% 
      rename("Release ID"       = rel_id,
             "Date of Release" = release_date,
             "Time of Release" = release_time, 
             "Lunar Phase" = lunar_phase,
             "Fish Released" = fish_released,
             "Temperature (°C)" = Temperature,
             "Turbidity (FNU)"  = Turbidity,
             "Discharge (cfs)"  = Discharge)
  })
  
  # Render DT
  output$release_wq_table <- renderDT({
    datatable(filtered_release_wq(),
              rownames = FALSE,
              options  = list(
                dom = "t",
                paging     = FALSE,  
                columnDefs = list(
                  list(className = "dt-center", targets = "_all")
                )
              ))
  }, server = FALSE) # end render data table
  
  # release water quality plot ------------------------------------------------------------------------------
  
  freeport = reactive({

    water_quality %>% 
    filter(water_year == input$water_year) %>% 
    filter(location == "Sacramento River at Freeport")})
  
    facet_titles = c("Temperature"       = "Temperature (°C)",
                     "Turbidity"         = "Turbidity (FNU)",
                     "Discharge"         = "Discharge (cfs)",
                     "Discharge, tidally filtered" = "Tidally Filtered Discharge (cfs)")
  
    facet_colors = c("Temperature"       = "#5FA2A4FF",
                     "Turbidity"         = "#8CBF9AFF",
                     "Discharge"         = "#191F40FF",
                     "Discharge, tidally filtered" = "#315B88FF")
    
    release_date_rects = reactive({
      
      release_dates %>%
        # filter rectangles by water year selection
        filter(water_year == input$water_year) %>%
        # parse start and end dates
        mutate(start = ymd_hm(paste(start, "21:00"), tz = "America/Los_Angeles"),
               end   = start + hours(69),
               # flag the rectangle that matches the selected block
               selected = block %in% input$block_id)
      
    })
      
  
  output$water_quality_plot <- renderPlot({

     rects <- release_date_rects()
    
    ggplot(freeport(), aes(x = datetime, y = value, color = value_type)) + 
      facet_wrap(vars(value_type), ncol = 1, scales = "free_y",
                 labeller = as_labeller(facet_titles)) +
      geom_rect(data = filter(rects, !selected), inherit.aes = FALSE,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              fill = "darkgray", alpha = 0.30) +
      # selected rectangle (highlighted)
      geom_rect(data = filter(rects, selected), inherit.aes = FALSE,
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                fill = "#dde83dff", alpha = 0.55,
                color = "#8f962bff", linewidth = 0.8) +
       geom_line(linewidth = 1.2) + 
      scale_color_manual(values = facet_colors) +
      scale_x_datetime(name = "Month",
                       date_breaks = "1 month",
                       date_labels = "%b") +
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            strip.text = element_text(size = 16, hjust = 0, face = "bold"),
            legend.position = "none",
            panel.spacing = unit(1, "lines"))
    
  }, height = 1000)
  
  # tag life plot ------------------------------------------------------------------------------
  
  output$plotOrMessage <- renderUI({
    if (input$water_year == 2026) {plotOutput("tag_life_plot")} 
    else {div(style = "color: #888; font-style: italic; font-size: 20px; padding: 20px; text-align: center;", 
              "Sorry, no data for this year.")}
  })
  
  # line plot
  output$tag_life_plot <- renderPlot({req(input$water_year)
    
    tag_life_2026 %>%
      filter(water_year == input$water_year) %>%
      ggplot(aes(x = days_since_activation, y = prop_active, color = factor(tagging_block)))+
      geom_line(linewidth = 2) + 
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
    
  }, res = 96) # end render plot
  
# reports
  active_report <- reactiveVal(NULL)
  
  # ---- Reactive data for each report type ----
  tagging_report <- reactive({
    
    # require water year and block selection
    req(input$water_year)
    
    tagging_data %>% 
      filter(water_year == input$water_year) %>% 
      
      select(water_year, block, tag_activation_date, species, run, date, surgeon, crew, vial_number, 
             tag_id_hex, weight_gr, fork_length_mm, anesthetic_time_mmss, surgery_time_mmss, recovery_time_mmss, comments)
  })
  
  release_report <- reactive({
    
    # require water year and block selection
    req(input$water_year)
    
    release_data %>%
      filter(water_year == input$water_year) %>% 
      
      mutate(release_datetime = parse_date_time(release_datetime, 
                                                orders = c("ymd HMS", "ymd HM", "ymd")),
             release_datetime = format(release_datetime, "%Y-%m-%d %H:%M:%S")) %>% 
      
      select(water_year, block, species, run, release_datetime, release_container_id, crew, morts, fish_released, comments)
    
  })
  
  calfishtrack_report <- reactive({
    
    # require tagging year and block selection
    req(input$water_year, input$block)
    
    for_calfishtrack %>% 
      filter(water_year == input$water_year,
             block == input$block) %>% 
      
      mutate(release_datetime = parse_date_time(release_datetime, 
                                                orders = c("ymd HMS", "ymd HM", "ymd")),
             release_datetime = format(release_datetime, "%Y-%m-%d %H:%M:%S")) %>% 
      
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
  
  # ---- Set active report when a "Generate" button is clicked ----
  observeEvent(input$tagging_gen_report, {
    active_report("tagging_report")
  })
  
  observeEvent(input$release_gen_report, {
    active_report("release_report")
  })
  
  observeEvent(input$calfishtrack_gen_report, {
    active_report("calfishtrack_report")
  })
  
  # ---- Title above the table ----
  output$active_report_title <- renderText({
    req(active_report())
    switch(active_report(),
           tagging_report = "Tagging Report",
           release_report = "Release Report",
           calfishtrack_report   = "CalFishTrack Report"
    )
  })
  
  # ---- Shared table, switches based on active_report() ----
  output$report_table <- renderDT({
    req(active_report())
    
    df <- switch(active_report(),
                 tagging_report = tagging_report(),
                 release_report = release_report(),
                 calfishtrack_report   = calfishtrack_report()
    )
    
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Download handlers (independent of which table is displayed) ----
  output$tag_download <- downloadHandler(
    filename = function() paste0("tagging_report.csv"),
    content = function(file) write.csv(tagging_report(), file, row.names = FALSE)
  )
  
  output$rel_download <- downloadHandler(
    filename = function() paste0("release_report.csv"),
    content = function(file) write.csv(release_report(), file, row.names = FALSE)
  )
  
  output$calfishtrack_download <- downloadHandler(
    filename = function() paste0("calfishtrack_report.csv"),
    content = function(file) write.csv(calfishtrack_report(), file, row.names = FALSE)
  )
} # end server

# run app-----------------------------------------------------------------------

shinyApp(ui = ui, server = server)