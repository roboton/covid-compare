library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(DT)

source("covidcomp_lib.R")
min_us <- 1
min_global <- 1

# pull in data
jhu <- fetchPrepJhuData()
covtrack <- fetchPrepCovTrackData()

last_update <- paste(now(), Sys.timezone())
 
# control over mouse over values in plotly plot
options(scipen = 999, digits = 1)

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("lumen"),
  titlePanel("Covid-19 comparisons"),
  tags$div(paste("Last updated:", last_update)),
  tags$a(
    href = "https://github.com/CSSEGISandData/COVID-19",
    target = "_blank", "[data]"),
  tags$a(
    href = "https://ond3.com/analysis.nb.html",
    target = "_blank", "[analysis]"),
  tags$a(
    href = "https://github.com/roboton/covid-19_meta/tree/master/covidcomp",
    target = "_blank", "[git]"),
  tags$a(
    href = "mailto:roberton@gmail.com",
    target = "_blank", "[contact]"),
  tags$a(
    href = "https://robon.shinyapps.io/covidcomp/covidcomp_static.html",
    target = "_blank", "[too slow?]"),
  sidebarLayout(
    sidebarPanel(
      # data options
      selectInput("min_stat", "metric to compare by:",
                  c("Deaths" = "deaths", "Confirmed cases" = "confirmed")),
      numericInput("min_thresh",
                   "initial number of deaths/cases:",
                   min = 0, value = min_global),
      numericInput("max_days_since",
                   "days since initial number of deaths/cases:",
                   min = 0, value = 30),
      # plot options
      checkboxInput("per_capita",
                    "Counts per capita", value = TRUE),
      checkboxInput("smooth_plots",
                    "Smooth plot values", value = TRUE), 
      checkboxInput("scale_to_fit",
                    "Scale to fit", value = TRUE), 
      width = 1),
    mainPanel(
      tabsetPanel(
        id = "plotTabs", type = "tabs",
        tabPanel(
          "Global", value = "Global",
          plotlyOutput(
            "compPlot", width = "100%", height = "1600px"),
            downloadButton("downloadGlobalData", "download")),
        tabPanel(
          "US", value = "US",
          plotlyOutput(
            "compPlotUS", width = "100%", height = "1600px"),
            downloadButton("downloadUSData", "download"))
        # tabPanel(
        #   "Data", dataTableOutput("compData"),
        #   downloadButton("downloadCompData", "Download"))
        )
    )
  )
)
 
server <- function(input, output, session) {
  shinyjs::runjs("toggleCodePosition();")
  # observe({
  #   updateNumericInput(
  #     session, "min_thresh",
  #     value = {if (input$plotTabs == "US") min_us else min_global})
  # })
  
  suppressWarnings(output$compPlot <- renderPlotly({
    jhu %>% genPlotComps(geo_level = "country", min_thresh = input$min_thresh,
                         per_capita = input$per_capita,
                         min_stat = input$min_stat,
                         max_days_since = input$max_days_since,
                         smooth_plots = input$smooth_plots,
                         scale_to_fit = input$scale_to_fit) }))
  suppressWarnings(output$compPlotUS <- renderPlotly({
    covtrack %>%
      genPlotComps(geo_level = "state", min_thresh = input$min_thresh,
                   per_capita = input$per_capita,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   scale_to_fit = input$scale_to_fit) }))
  # output$compData <- renderDataTable({
  #   joined %>% genCompData(min_thresh = input$min_thresh)
  # })
  output$downloadGlobalData <- downloadHandler(
    filename = function() {
      paste0("covid-global-comp-data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(jhu %>% genCompData(
        geo_level = "country", min_thresh = input$min_thresh,
        per_capita = input$per_capita, min_stat = input$min_stat), file)
    }
  )
  output$downloadUSData <- downloadHandler(
    filename = function() {
      paste0("covid-us-comp-data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(covtrack %>% genCompData(
        geo_level = "state", min_thresh = input$min_thresh,
        per_capita = input$per_capita, min_stat = input$min_stat), file)
    }
  )
}

shinyApp(ui = ui, server = server)