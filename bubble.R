library(shiny)
library(plotly)
library(readr)

# Load the data
imp <- read_csv("capstone_data.csv")

# Get column names, excluding columns 1, 3, 4, and 6
valid_columns <- colnames(imp)[6:200]

ui <- fluidPage(
  titlePanel("Bubble Chart for Country Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X-axis variable:", choices = valid_columns),  # Exclude specific columns
      selectInput("y_var", "Select Y-axis variable:", choices = valid_columns),
      selectInput("size_var", "Select Bubble Size variable:", choices = valid_columns),
      # Single year slider with animation
      sliderInput("year", "Select Year:",
                  min = min(imp$year), max = max(imp$year),
                  value = min(imp$year), step = 1, round = 0, animate = TRUE
      )
    ),
    mainPanel(
      plotlyOutput("bubbleChart")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to filter data based on the selected year
  filtered_data <- reactive({
    req(input$year)  # Ensure year is selected
    subset(imp, year == input$year)  # Filter data by selected year
  })
  
  # Render the bubble chart
  output$bubbleChart <- renderPlotly({
    data <- filtered_data()
    
    # Check if the filtered data is empty
    if (nrow(data) == 0) {
      return(NULL)  # If no data is found, return NULL
    }
    
    # Create the bubble chart with the selected variables
    plot_ly(data = data, 
            x = ~data[[input$x_var]],  # Use double brackets to reference the column dynamically
            y = ~data[[input$y_var]],
            size = ~data[[input$size_var]],
            color = ~data$country_name,  # Color by country_name
            type = "scatter",
            mode = "markers",
            text = ~paste("Country:", country_name, "<br>Year:", year),
            marker = list(sizemode = 'diameter', opacity = 0.5, line = list(width = 0.5, color = "white"))) %>%
      layout(title = "Bubble Chart of Country Data",
             xaxis = list(title = input$x_var),
             yaxis = list(title = input$y_var))
  })
}

shinyApp(ui, server)
