library(shiny)
library(plotly)
library(readr)

# Load the data
imp <- read_csv("imputed_cn.csv")
region_map <- read_csv("continents2.csv")

imp$region <- sapply(imp$country_name, function(x) {
  region_map$region[which(region_map$name == x)]
})
# Map regions
imp$region <- factor(imp$region, levels = c("Asia", "Europe", "Africa", "Americas", "Oceania"))

# List selections
valid_columns <- list("e_pop", "e_gdppc", "v2eldonate", "v2elpubfin", "v2elembaut", "v2elrgpwr", "v2ellocpwr", "v2psbars", "v2exrescon", "v2cltort", "v2caautmob")
names(valid_columns) <- c("Population", "GDP per Capita", "Disclosure of Campaign Donations", "Public Campaign Finance", "Electoral Management Body Autonomy", "Regional Offices Relative Power", "Local Offices Relative Power", "Barriers to Parties", "Executive Respects Constitution", "Freedom From Torture", "Mobilization for Autocracy")


ui <- fluidPage(
  titlePanel("Multi-Feature Relationships Animation Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X-axis variable:", choices = valid_columns[valid_columns != c("e_pop", "e_gdppc")]),  # Exclude specific columns
      selectInput("y_var", "Select Y-axis variable:", choices = valid_columns[valid_columns != c("e_pop", "e_gdppc")]),
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
            color = ~data$region,  # Color by region
            type = "scatter",
            mode = "markers",
            text = ~paste("Country:", country_name, "<br>Year:", year),
            marker = list(sizemode = 'diameter', opacity = 0.5, line = list(width = 0.5, color = "white"))) %>%
      layout(title = "Interactive Bubble Chart: Explore Relationships Between Highlighted Features",
             xaxis = list(title = input$x_var),
             yaxis = list(title = input$y_var))
  })
}

shinyApp(ui, server)
