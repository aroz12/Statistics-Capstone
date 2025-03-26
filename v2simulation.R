library(shiny)

imp <- read_csv("/Users/alecasillas/Desktop/COURSES/STA4930/datasets/imputed.csv")

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
      sliderInput("e_gdppc", "GDP per Capita:",
                  min = min(imp$e_gdppc, na.rm = TRUE),
                  max = max(imp$e_gdppc, na.rm = TRUE),
                  value = mean(imp$e_gdppc, na.rm = TRUE)),
      sliderInput("v2eldonate", "Disclosure of Campaign Donations:",
                  min = min(imp$v2eldonate, na.rm = TRUE),
                  max = max(imp$v2eldonate, na.rm = TRUE),
                  value = mean(imp$v2eldonate, na.rm = TRUE)),
      sliderInput("v2elpubfin", "Public Campaign Finance:",
                  min = min(imp$v2elpubfin, na.rm = TRUE),
                  max = max(imp$v2elpubfin, na.rm = TRUE),
                  value = mean(imp$v2elpubfin, na.rm = TRUE)),
      sliderInput("v2elembaut", "EMB Autonomy:",
                  min = min(imp$v2elembaut, na.rm = TRUE),
                  max = max(imp$v2elembaut, na.rm = TRUE),
                  value = mean(imp$v2elembaut, na.rm = TRUE)),
      sliderInput("v2elrgpwr", "Regional Offices Relative Power:",
                  min = min(imp$v2elrgpwr, na.rm = TRUE),
                  max = max(imp$v2elrgpwr, na.rm = TRUE),
                  value = mean(imp$v2elrgpwr, na.rm = TRUE)),
      sliderInput("v2ellocpwr", "Local Offices Relative Power:",
                  min = min(imp$v2ellocpwr, na.rm = TRUE),
                  max = max(imp$v2ellocpwr, na.rm = TRUE),
                  value = mean(imp$v2ellocpwr, na.rm = TRUE)),
      sliderInput("v2psbars", "Barriers to Parties:",
                  min = min(imp$v2psbars, na.rm = TRUE),
                  max = max(imp$v2psbars, na.rm = TRUE),
                  value = mean(imp$v2psbars, na.rm = TRUE)),
      sliderInput("v2exrescon", "Executive Respects Constitution:",
                  min = min(imp$v2exrescon, na.rm = TRUE),
                  max = max(imp$v2exrescon, na.rm = TRUE),
                  value = mean(imp$v2exrescon, na.rm = TRUE)),
      sliderInput("v2cltort", "Freedom From Torture:",
                  min = min(imp$v2cltort, na.rm = TRUE),
                  max = max(imp$v2cltort, na.rm = TRUE),
                  value = mean(imp$v2cltort, na.rm = TRUE)),
      sliderInput("v2caautmob", "Mobilization for Autocracy:",
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

  original_data <- reactive({
    req(input$country, input$year)
    selected <- subset(imp, country_name == input$country & year == input$year)
    if(nrow(selected) > 0) {
      return(selected[1, ])
    } else {
      return(NULL)
    }
  })
  

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

  predicted_score <- reactive({
    raw_pred <- predict(model, newdata = new_data())
    clipped <- min(max(raw_pred, 1), 7)
    round(clipped, 1)
  })
  

  get_status <- function(score) {
    if (score >= 1.0 && score <= 2.5) {
      "Free"
    } else if (score >= 3.0 && score <= 5.0) {
      "Partly Free"
    } else if (score >= 5.5 && score <= 7.0) {
      "Not Free"
    } else {
      "Undefined"
    }
  }
  
  output$predScore <- renderPrint({
    req(original_data())
    original <- original_data()$rescaled
    adjusted <- predicted_score()
    status <- get_status(adjusted)
    cat("Original Score:", original, "\nAdjusted Score:", adjusted, "\nStatus:", status)
  })
}

shinyApp(ui, server)



