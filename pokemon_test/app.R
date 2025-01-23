library(shiny)
library(pokemon)
library(shinyWidgets)
library(DT)
library(dplyr)

pokemon_data <- pokemon::pokemon


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("The Shiny Pokedex"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("type_1_filter", "Filter by Type 1:", unique(pokemon_data$type_1), multiple = TRUE),
            selectInput("type_2_filter", "Filter by Type 2:", unique(pokemon_data$type_2), multiple = TRUE),
            sliderInput("height_filter", "Filter by Height:", min = min(pokemon_data$height), max = max(pokemon_data$height), value = c(min(pokemon_data$height), max(pokemon_data$height))),
            sliderInput("weight_filter", "Filter by Weight:", min = min(pokemon_data$weight), max = max(pokemon_data$weight), value = c(min(pokemon_data$weight), max(pokemon_data$weight))),
            br(),
            actionButton('reset', 'Clear filters')
        ),
        mainPanel(
            DTOutput("table"),
            DTOutput("pokemon_details")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$reset, {
        updateSelectInput(session, "type_2_filter", choices = unique(pokemon_data$type_2))
        
        updateSelectInput(session, "type_1_filter", choices = unique(pokemon_data$type_1))
        
        updateSliderInput(session, "height_filter", min = min(pokemon_data$height), max = max(pokemon_data$height), value = c(min(pokemon_data$height), max(pokemon_data$height)))
        
        updateSliderInput(session, "weight_filter", min = min(pokemon_data$weight), max = max(pokemon_data$weight), value = c(min(pokemon_data$weight), max(pokemon_data$weight)))
    })
    
    
    
    # Reactive expression for filtering data based on user inputs
    filtered_data <- reactive({
        data <- pokemon_data
        
        # Filter by Type 1
        if (!is.null(input$type_1_filter) && length(input$type_1_filter) > 0) {
            data <- data[data$type_1 %in% input$type_1_filter, ]
        }
        
        # Filter by Type 2
        if (!is.null(input$type_2_filter) && length(input$type_2_filter) > 0) {
            data <- data[data$type_2 %in% input$type_2_filter, ]
        }
        
        # Filter by Height
        data <- data[data$height >= input$height_filter[1] & data$height <= input$height_filter[2], ]
        
        # Filter by Weight
        data <- data[data$weight >= input$weight_filter[1] & data$weight <= input$weight_filter[2], ]
        
        data <- data %>% 
            select(pokemon)
        
        return(data)
    })
    
    # Update filter choices based on selected data
    observe({
        
        # Update Type 2 filter choices based on selected Type 1 values
        selected_type_1 <- input$type_1_filter
        type_2_choices <- unique(pokemon_data$pokemon_data$type_2[pokemon_data$type_1 %in% selected_type_1])
        updateSelectInput(session, "type_2_filter", choices = type_2_choices)
        
        # Update Type 1 filter choices based on selected Type 2 values
        selected_type_2 <- input$type_2_filter
        type_1_choices <- unique(pokemon_data$pokemon_data$type_1[pokemon_data$type_2 %in% selected_type_2])
        updateSelectInput(session, "type_1_filter", choices = type_1_choices)
    })
    
    # Render the DataTable
    output$table <- renderDT({
        datatable(filtered_data(), selection = list(mode = 'single'))
    })
    
    
    details <- eventReactive(input$table_rows_selected,{
        
        pokemon_name <- filtered_data()$pokemon[input$table_rows_selected]
        subset(pokemon_data, pokemon_data$pokemon == pokemon_name)
        
    })
    
    # Render the DataTable
    output$pokemon_details <- renderDT({
        datatable(details())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
