# scripts to put into shiny app
# tag life data
# H. Short
# 2026-09-14 12:59:52
# -------------------------------------------------------------------------------
# load packages
library(tidyverse)

# -------------------------------------------------------------------------------
# read in data
tag_life_2026 = readRDS("clean-data/tag-life/tag_life_survival_2026.rds")

# tag life plot -------------------------------------------------------------------------------
# testable version
tag_life_2026 %>%
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
    
# -------------------------------------------------------------------------------
# shiny ui
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
                                    selectInput("water_year_tag_life", " ",
                                                choices  = sort(unique(tagging$water_year)),
                                                selected = max(tagging$water_year)))),

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
                          uiOutput("tag_life_plot_or_message"),
                          br(),
                          h5("Figure 2: Proportion of tags active plotted over time (days since tag activation). Colored lines represent individual tagging blocks 
                             (Block 1 = December, Block 2 = January, Block 3 = February, Block 4 = April) and unique batches of tags. 
                             The dashed line represents the 71 day tag life expectancy for JSATS model ss400 transmitters.")))
          
), # end nav panel - tag life

# -------------------------------------------------------------------------------
# shiny version
# server
output$tag_life_plot_or_message <- renderUI({

    if (input$water_year_tag_life == 2026) {plotOutput("tag_life_plot")} 

    else {div(style = "color: #888; font-style: italic; font-size: 20px; padding: 20px; text-align: center;", 
              "Sorry, data for this year is not available right now.")}
  
})
  
  # line plot
  output$tag_life_plot <- renderPlot({
    
    # require water year as an input
    req(input$water_year_tag_life)
    
    tag_life_2026 %>%
      filter(water_year == input$water_year_tag_life) %>%
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
  