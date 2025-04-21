#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidytuesdayR)
library(tidyverse)
library(arsenal)
library(ggplot2)
library(shiny)
library(dplyr)
library(readr)
library(viridis)

# Define UI for application that draws a histogram
video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-07-30/video_games.csv")
video_games_deve <- video_games %>%
  group_by(developer) %>%
  summarize(
    num_games = n(),
    avg_score = mean(metascore, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE),
    avg_time = mean(average_playtime, na.rm = TRUE),
    med_time = mean(median_playtime, na.rm = TRUE)
  ) %>%
  filter(
    num_games >= 5,
    !is.nan(avg_score),
    !is.na(developer)
  )

# User Interface
ui <- fluidPage(
  titlePanel("Video Game Developer Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "x_var",
        label = "Select X-axis Variable:",
        choices = c(
          "Number of Games" = "num_games",
          "Average Score" = "avg_score",
          "Average Price" = "avg_price",
          "Average Playtime" = "avg_time",
          "Median Playtime" = "med_time"
        ),
        selected = "avg_score"
      ),
      selectInput(
        inputId = "y_var",
        label = "Select Y-axis Variable:",
        choices = c(
          "Number of Games" = "num_games",
          "Average Score" = "avg_score",
          "Average Price" = "avg_price",
          "Average Playtime" = "avg_time",
          "Median Playtime" = "med_time"
        ),
        selected = "avg_price"
      ),
      sliderInput(
        inputId = "size_var",
        label = "Filter by Number of Games:",
        min = min(video_games_deve$num_games),
        max = max(video_games_deve$num_games),
        value = c(min(video_games_deve$num_games), max(video_games_deve$num_games))
      ),
      checkboxInput(
        inputId = "show_smooth",
        label = "Show Smooth Trend Line",
        value = FALSE
      )
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

# Server Logic


server <- function(input, output, session) {
  subsetted_data <- reactive({
    video_games_deve %>%
      filter(num_games >= input$size_var[1] & num_games <= input$size_var[2])
  })
  
  output$scatterplot <- renderPlot({
    x_label <- switch(input$x_var,
                      "num_games" = "Number of Games",
                      "avg_score" = "Average Metascore",
                      "avg_price" = "Average Price",
                      "avg_time" = "Average Playtime",
                      "med_time" = "Median Playtime")
    
    y_label <- switch(input$y_var,
                      "num_games" = "Number of Games",
                      "avg_score" = "Average Metascore",
                      "avg_price" = "Average Price",
                      "avg_time" = "Average Playtime",
                      "med_time" = "Median Playtime")
    
    p <- ggplot(subsetted_data(), aes_string(x = input$x_var, y = input$y_var)) + list(
      theme_minimal(),
      labs(
        title = "Developer Analysis",
        x = x_label,
        y = y_label,
        color = "Number of Games",
        size = "Number of Games"
      ),
      geom_point(alpha = 0.7, aes(size = num_games, color = num_games)),
      scale_size_continuous(range = c(2, 10)),
      scale_color_viridis_c()
    )
    
    if (input$show_smooth) {
      p <- p + geom_smooth(se = FALSE, method = "lm", color = "red")
    }
    
    p
  }, res = 100)
}


# Run the Shiny App
shinyApp(ui = ui, server = server)
