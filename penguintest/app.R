#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(palmerpenguins)

data <- penguins

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Hello penguins!"),
    
    # Sidebar with a slider input for number of bins 
    
    sliderInput("mass",
                "Mass:",
                min = 2000,
                max = 8000,
                value = 6000),
    
    checkboxInput('trend', 'Add trendline'),
    
    # Show a plot of the generated distribution
    
    plotOutput("distPlot"),
    plotOutput('scatterPlot')
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        data %>% 
            filter(body_mass_g < input$mass)
    })
    
    output$distPlot <- renderPlot({
        data %>% 
            filter(body_mass_g < input$mass)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
