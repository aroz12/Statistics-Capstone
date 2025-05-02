library(shiny)
library(plotly)
library(readr)

# Load the data
imp <- read_csv("imputed_cn.csv")
region_map <- read_csv("continents2.csv")
# Map Regions to Imputed data via Country Name
imp$region <- sapply(imp$country_name, function(x) {
  region_map$region[which(region_map$name == x)]
})
imp$region <- factor(imp$region, levels = c("Asia", "Europe", "Africa", "Americas", "Oceania"))

# Load and rename additional dataset with New Indices
agg_data <- read_csv("singleagwithnewindices.csv")
colnames(agg_data) <- sapply(colnames(agg_data), function(x) {
  if (x == "rights_scaled") return("Rights")
  else if (x == "parties_scaled") return("Parties")
  else if (x == "mediacso_scaled") return("Media")
  else if (grepl("rescaled", x)) return("Freedom")
  else return(x)
})

# Merge
imp <- merge(imp, agg_data[, c("country_id", "year", "Rights", "Parties", "Media", "Freedom")],
             by = c("country_id", "year"), all.x = TRUE)
# Scale Freedom -> 7 = Most Free & 1 = Least Free
imp$Freedom <- abs(imp$Freedom - 8)

# Valid columns
valid_columns <- list(
  "e_pop", "e_gdppc", "v2eldonate", "v2elpubfin", "v2elembaut", "v2elrgpwr", "v2ellocpwr",
  "v2psbars", "v2exrescon", "v2cltort", "v2caautmob", "v2caassemb", "v2psoppaut",
  "Rights", "Parties", "Media", "Freedom"
)
names(valid_columns) <- c(
  "Population", "GDP per Capita", "Disclosure of Campaign Donations", "Public Campaign Finance",
  "Electoral Management Body Autonomy", "Regional Offices Relative Power", "Local Offices Relative Power",
  "Barriers to Parties", "Executive Respects Constitution", "Freedom From Torture",
  "Mobilization for Autocracy", "Protection for Civil Assembly",
  "Opposition Party Independence",
  "Rights", "Parties", "Media", "Freedom"
)


# UI
ui <- fluidPage(
  titlePanel("Multi-Feature Relationships Animated Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X-axis variable:", choices = valid_columns[valid_columns != c("e_pop", "e_gdppc")]),
      selectInput("y_var", "Select Y-axis variable:", choices = valid_columns[valid_columns != c("e_pop", "e_gdppc")]),
      selectInput("size_var", "Select Bubble Size variable:", choices = valid_columns),
      sliderInput("year", "Select Year:",
                  min = min(imp$year), max = max(imp$year),
                  value = min(imp$year), step = 1, round = 0, animate = TRUE),
      # Scaling important for different resolutions (Presentation-Specific)
      sliderInput("min_size", "Minimum Bubble Size:", min = 1, max = 20, value = 7),
      sliderInput("max_size", "Maximum Bubble Size:", min = 20, max = 100, value = 40)
    ),
    mainPanel(
      plotlyOutput("bubbleChart")
    )
  )
)


# Server
server <- function(input, output, session) {
  # Selected data changes based on new inputs
  filtered_data <- reactive({
    req(input$year)
    data <- subset(imp, year == input$year)
    size_var <- input$size_var
    min_size <- input$min_size
    max_size <- input$max_size
    # Check for valid data
    raw_vals <- data[[size_var]]
    raw_vals[is.na(raw_vals)] <- 0
    # Rescale bubbles depending on selection
    if (!(size_var %in% c("e_pop", "e_gdppc"))) {
      min_val <- min(raw_vals, na.rm = TRUE)
      max_val <- max(raw_vals, na.rm = TRUE)
      if (max_val == min_val) {
        norm_vals <- rep(0.5, length(raw_vals))
      } else {
        norm_vals <- (raw_vals - min_val) / (max_val - min_val)
      }
      data$size_value <- norm_vals * (max_size - min_size) + min_size
    } else {
      data$size_value <- raw_vals
    }
    return(data)
  })
  # Render data
  output$bubbleChart <- renderPlotly({
    data <- filtered_data()
    size_var <- input$size_var
    min_size <- input$min_size
    max_size <- input$max_size
    size_scale <- if (!(size_var %in% c("e_pop", "e_gdppc"))) max_size else 100
    # Non-Case Studies
    main_data <- subset(data, !(country_name %in% c("Afghanistan", "Myanmar", "Venezuela", "Benin", "Latvia", "USSR")))
    # Plotting
    main_plot <- plot_ly(data = main_data,
                         x = ~main_data[[input$x_var]],
                         y = ~main_data[[input$y_var]],
                         size = ~main_data$size_value,
                         sizes = c(min_size, size_scale),
                         color = ~main_data$region,
                         type = "scatter",
                         mode = "markers",
                         text = ~paste("Country:", country_name, "<br>Year:", year),
                         marker = list(sizemode = 'diameter', opacity = 0.2, line = list(width = 0.5, color = "white")))
    
    # Function to plot Case Studies on top of main data
    add_highlighted_country <- function(plot, country, color) {
      country_data <- subset(data, country_name == country)
      # Remove duplicate data within years -> choose first
      country_data <- country_data[!duplicated(country_data[c("country_name", "year")]), ]
      if (nrow(country_data) > 0) {
        plot <- plot %>%
          # Adding traces for each Case Study
          add_trace(data = country_data,
                    x = ~country_data[[input$x_var]],
                    y = ~country_data[[input$y_var]],
                    size = ~country_data$size_value,
                    # User-Scaled Bubble Size [Ideally difference b/w min & max is multiple of size var scale]
                    sizes = c(min_size, size_scale),
                    color = I(color),
                    type = "scatter",
                    mode = "markers",
                    text = ~paste("Country:", country, "<br>Year:", year),
                    marker = list(
                      # Distinct Trace Marker for Case Studies
                      sizemode = 'diameter',
                      opacity = 1,
                      line = list(width = 1, color = "black"),
                      symbol = "triangle-up"
                    ),
                    name = country)
      }
      return(plot)
    }
    
    # Plot Case Studies
    main_plot <- add_highlighted_country(main_plot, "Burma/Myanmar", "green")
    main_plot <- add_highlighted_country(main_plot, "Russia", "orange")
    main_plot <- add_highlighted_country(main_plot, "Afghanistan", "red")
    main_plot <- add_highlighted_country(main_plot, "Benin", "blue")
    main_plot <- add_highlighted_country(main_plot, "Venezuela", "purple")
    main_plot <- add_highlighted_country(main_plot, "USSR", "orange")
    # Output Final Plot
    main_plot %>%
      layout(title = "Interactive Bubble Chart: Explore Relationships Between Highlighted Features",
             # Technical name for research... could change to explanation for presentation purposes
             xaxis = list(title = input$x_var),
             yaxis = list(title = input$y_var))
  })
}
# Launch the app
shinyApp(ui, server)
