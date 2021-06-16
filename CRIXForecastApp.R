library("shiny")
library("shinyjs")
library("tidyverse")
library("dplyr")
library("xts")
library("dygraphs")
library("lubridate")
library("tibbletime")
library("forecast")
library("zoo")
library("timetk")
library("reshape2")
library("keras")
library("htmltools")
source("r/get_data.R")
source("r/analysis.R")

ETS <-
  readRDS(file = "models/ets.rds")

coin <- get_crypto()

coin$date <- ymd(coin$date)

log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
log_returns <- xts(log_returns, order.by = coin$date)
log_returns <- na.omit(log_returns)

ui <- fluidPage(
  tags$head(tags$style(".rightAlign{float:right;}")), # Align right the plots from Analysis page
  useShinyjs(),
  theme = 'bootstrap.min.css',
  
  # Navigation Bar
  navbarPage(
    "Seiðr",
    
    # Forecast Tab
    tabPanel(
      "Forecast",
      sidebarPanel(
        textInput(
          "coin",
          "Entity:",
          "CRIX"
        ),
        numericInput("periods_to_forecast",
                     "Select Number of Days to Forecast:",
                     value = 1),
        uiOutput('no_value'), # No error caused by empty periods_to_forecast
        uiOutput('ptf_text'), # Prompt user to enter a value if periods_to_forecast is empty
        selectInput(
          "model",
          "Select Model:",
          c(
            "LSTM" = 'lstm',
            "Naive" = 'naive_lr',
            "Mean" = "mean_lr",
            "ETS" = "ets_lr"
          )
        ),
        uiOutput('lstm_loading_time_text'), # Note about LSTM loading times
        # Show ETS parameters. Hidden if model != ETS
        conditionalPanel(
          condition = "input.model == 'ets_lr'",
          textInput("spec_ets",
                    "Method",
                    value = ETS$method),
          textInput("error",
                    "Error",
                    value = ETS$components[1]),
          textInput("trend",
                    "Trend",
                    value = ETS$components[2]),
          textInput("seasonality",
                    "Seasonality",
                    value = ETS$components[3]),
          numericInput("alpha",
                       "Alpha",
                       signif(ETS[["par"]][["alpha"]], 3))
        ),
        actionButton("update_plot", "Plot") # Button to update plot.
      ),
      mainPanel(dygraphOutput("series")) # App Output
    ),
    
    # Analysis Tab
    tabPanel(
      "Analysis",
      sidebarPanel(
        textInput(
          "coin_analysis",
          "Entity:",
          "CRIX"
        ),
        selectInput(
          "analysis",
          "Select Analysis:",
          c(
            "Autocorrelation" = "acf",
            "Partial Autocorrelation" = "pacf",
            "Monthly Decomposition" = "mdecomp"
          )
        ),
        actionButton("update_analysis", "Plot")
      ),
      # Determines which analysis is shown based on user input.
      mainPanel(
        conditionalPanel(
          condition = "input.analysis == 'mdecomp'",
          dygraphOutput("seasonal",height='230px')
        ),
        conditionalPanel(
          condition =  "input.analysis == 'mdecomp'",
          dygraphOutput("trend",height='230px')
        ),
        conditionalPanel(
          condition =  "input.analysis == 'mdecomp'",
          dygraphOutput("remainder",height='230px')
        ),
        conditionalPanel(
          condition =  "input.analysis == 'acf' | input.analysis == 'pacf'",
          plotOutput("acfpacf")
        )
      
      
    )),
    tabPanel("References"),
    tabPanel("Terms of Use"),
    tabPanel("Contact"),
    tabPanel("About"),
    # Dark background for the NavBar 
    inverse = TRUE
  )
)

server <- function(input, output) {
  
  
  # Backend of "Plot" button
  observeEvent(input$update_plot, {
    if (is.na(input$periods_to_forecast)) {
      
    } else {
    output$series <- renderDygraph({
      interactive_graph(
        periods_to_forecast = isolate(floor(input$periods_to_forecast)),
        for_model = isolate(input$model)
      )
    })}
  })
  
  # Backend of "Prompt user to enter a value if periods_to_forecast is empty" text
  
  output$ptf_text <-
    renderUI({
      if (is.na(input$periods_to_forecast)){
        br()
      } else if(input$model == 'lstm' & input$periods_to_forecast > 1){
        return(p("Warning: The LSTM model was designed to forecast exactly one step into the future. The chosen forecasting horizon is greater than one.", style = "color:red"))
      }
      
    })
  
  # Backend of LSTM loading times text
  
  output$lstm_loading_time_text <-
    renderUI({
      if(input$model == 'lstm'){
        return(p("Note: Loading the necessary libraries for prediction with the LSTM model might take a few seconds.", style = "color:gray"))
      }
      
    })
  
  # Backend of empty periods_to_forecast
  result<-reactive({
    validate(
      need(input$periods_to_forecast, "Please input a value.")
    )
  })
  
  output$no_value <- renderPrint({
    
    result()
  })
  
  # Disable coin Input
  
  disable('coin')
  disable('coin_analysis')
  
  # Disable user Input on ETS parameters
  disable('spec_ets')
  disable('error')
  disable('trend')
  disable('seasonality')
  disable('alpha')
  
  # Analysis Backend
  observeEvent(input$update_analysis, {
    if (isolate(input$analysis) == "mdecomp") {
      output$seasonal <- renderDygraph({
        analysis(for_analysis = isolate(input$analysis), part="seasonal")
      })
      
      output$trend <- renderDygraph({
        analysis(for_analysis = isolate(input$analysis), part="trend")
      })
      
      output$remainder <- renderDygraph({
        analysis(for_analysis = isolate(input$analysis), part="remainder")
      }) 
      
    } else if (isolate(input$analysis) == "acf") {
      
      
      output$acfpacf <- renderPlot({
        analysis(for_analysis = isolate(input$analysis))
      })
    } else if (isolate(input$analysis) == "pacf") {
      
      
      output$acfpacf <- renderPlot({
        analysis(for_analysis = isolate(input$analysis))
      })
      }
    })
  
  
}

shinyApp(ui = ui, server = server)
