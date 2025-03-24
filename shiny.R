library(shiny)

# Load your dataset here. For example:
imp <- read_csv("/Users/alecasillas/Desktop/COURSES/STA4930/datasets/imputed.csv")


# Load your dataset here. For example:
# imp <- read.csv("path/to/imp.csv")

# Fit a linear regression model predicting the rescaled score from the ten features.
model <- lm(rescaled ~ e_gdppc + v2eldonate + v2elpubfin + v2elembaut +
              v2elrgpwr + v2ellocpwr + v2psbars + v2exrescon +
              v2cltort + v2caautmob, data = imp)

ui <- fluidPage(
  titlePanel("Rescaled Feature Score Simulation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(imp$country_name)),
      selectInput("year", "Select Year:", choices = unique(imp$year)),
      br(),
      sliderInput("e_gdppc", "GDP per Capita (e_gdppc):",
                  min = min(imp$e_gdppc, na.rm = TRUE),
                  max = max(imp$e_gdppc, na.rm = TRUE),
                  value = mean(imp$e_gdppc, na.rm = TRUE)),
      sliderInput("v2eldonate", "v2eldonate:",
                  min = min(imp$v2eldonate, na.rm = TRUE),
                  max = max(imp$v2eldonate, na.rm = TRUE),
                  value = mean(imp$v2eldonate, na.rm = TRUE)),
      sliderInput("v2elpubfin", "v2elpubfin:",
                  min = min(imp$v2elpubfin, na.rm = TRUE),
                  max = max(imp$v2elpubfin, na.rm = TRUE),
                  value = mean(imp$v2elpubfin, na.rm = TRUE)),
      sliderInput("v2elembaut", "v2elembaut:",
                  min = min(imp$v2elembaut, na.rm = TRUE),
                  max = max(imp$v2elembaut, na.rm = TRUE),
                  value = mean(imp$v2elembaut, na.rm = TRUE)),
      sliderInput("v2elrgpwr", "v2elrgpwr:",
                  min = min(imp$v2elrgpwr, na.rm = TRUE),
                  max = max(imp$v2elrgpwr, na.rm = TRUE),
                  value = mean(imp$v2elrgpwr, na.rm = TRUE)),
      sliderInput("v2ellocpwr", "v2ellocpwr:",
                  min = min(imp$v2ellocpwr, na.rm = TRUE),
                  max = max(imp$v2ellocpwr, na.rm = TRUE),
                  value = mean(imp$v2ellocpwr, na.rm = TRUE)),
      sliderInput("v2psbars", "v2psbars:",
                  min = min(imp$v2psbars, na.rm = TRUE),
                  max = max(imp$v2psbars, na.rm = TRUE),
                  value = mean(imp$v2psbars, na.rm = TRUE)),
      sliderInput("v2exrescon", "v2exrescon:",
                  min = min(imp$v2exrescon, na.rm = TRUE),
                  max = max(imp$v2exrescon, na.rm = TRUE),
                  value = mean(imp$v2exrescon, na.rm = TRUE)),
      sliderInput("v2cltort", "v2cltort:",
                  min = min(imp$v2cltort, na.rm = TRUE),
                  max = max(imp$v2cltort, na.rm = TRUE),
                  value = mean(imp$v2cltort, na.rm = TRUE)),
      sliderInput("v2caautmob", "v2caautmob:",
                  min = min(imp$v2caautmob, na.rm = TRUE),
                  max = max(imp$v2caautmob, na.rm = TRUE),
                  value = mean(imp$v2caautmob, na.rm = TRUE))
    ),
    mainPanel(
      h3("Score Comparison"),
      verbatimTextOutput("predScore")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive to extract the original row based on the selected country and year.
  original_data <- reactive({
    req(input$country, input$year)
    selected <- subset(imp, country_name == input$country & year == input$year)
    if(nrow(selected) > 0) {
      return(selected[1, ])
    } else {
      return(NULL)
    }
  })
  
  # Update slider defaults based on the selected country and year.
  observeEvent({
    input$country
    input$year
  }, {
    req(original_data())
    updateSliderInput(session, "e_gdppc", value = original_data()$e_gdppc)
    updateSliderInput(session, "v2eldonate", value = original_data()$v2eldonate)
    updateSliderInput(session, "v2elpubfin", value = original_data()$v2elpubfin)
    updateSliderInput(session, "v2elembaut", value = original_data()$v2elembaut)
    updateSliderInput(session, "v2elrgpwr", value = original_data()$v2elrgpwr)
    updateSliderInput(session, "v2ellocpwr", value = original_data()$v2ellocpwr)
    updateSliderInput(session, "v2psbars", value = original_data()$v2psbars)
    updateSliderInput(session, "v2exrescon", value = original_data()$v2exrescon)
    updateSliderInput(session, "v2cltort", value = original_data()$v2cltort)
    updateSliderInput(session, "v2caautmob", value = original_data()$v2caautmob)
  })
  
  # Reactive data frame from the slider inputs.
  new_data <- reactive({
    data.frame(
      e_gdppc = input$e_gdppc,
      v2eldonate = input$v2eldonate,
      v2elpubfin = input$v2elpubfin,
      v2elembaut = input$v2elembaut,
      v2elrgpwr = input$v2elrgpwr,
      v2ellocpwr = input$v2ellocpwr,
      v2psbars = input$v2psbars,
      v2exrescon = input$v2exrescon,
      v2cltort = input$v2cltort,
      v2caautmob = input$v2caautmob
    )
  })
  
  # Reactive prediction using the current slider inputs.
  predicted_score <- reactive({
    predict(model, newdata = new_data())
  })
  
  # Display the original score from the dataset and the adjusted score.
  output$predScore <- renderPrint({
    req(original_data())
    original <- original_data()$rescaled
    raw_adjusted <- predicted_score()
    adjusted <- min(max(raw_adjusted, 1), 7)
    cat("Original Score:", original, "\nAdjusted Score:", adjusted)
  })
  
}

shinyApp(ui, server)



