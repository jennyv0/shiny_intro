library(tidyverse)
library(shiny)

data = tidyr::billboard %>% 
  gather(key = "week", value = "position", starts_with("wk")) %>% 
  mutate(week = as.numeric(gsub("wk", "", week)),
         month_entered = strftime(date.entered, "%B"))

ui = fluidPage(
  titlePanel("Billboard song ranking"),
  selectInput("artist_selector", "Select a Song:", choices = unique(data$artist)),
  selectInput("song_selector", "Select a Song:", choices = unique(data$track)),
  sliderInput("week_no", "Week", min = 0 , max = 52, value = c(0,52)),
  
  # Plot output
  plotOutput("position_plot")
  
)
server = function(input, output) {
  
  filtered_df <- reactive({
    req(input$song_selector)
    data %>% 
      filter(track == input$song_selector)
  })
  
  observe({
    songs = data %>% 
      filter(artist == input$artist_selector) %>% 
      pull(track) %>% 
      unique() %>% 
      sort()
    
    updateSelectInput(
      inputId = "song_selector", 
      choices = songs
    )
  })
  
  # Render the plot
  output$position_plot <- renderPlot({
    
    # Filter data based on user selection
    selected_song_data <- reactive({
      filter(filtered_df(), 
             week >= input$week_no[1] & week <= input$week_no[2])
    })
    
    ggplot(selected_song_data(), aes(x = week, y = position)) +
      geom_line() +
      labs(title = paste("Position of", input$song_selector, "over Weeks"),
           x = "Week",
           y = "Position") +
      scale_y_reverse(limits = c(100, 0))
  })
  
  
  # Render the plot
  output$table <- renderTable({
    
    filtered_df()
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
