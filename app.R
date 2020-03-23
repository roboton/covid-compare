library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(DT)

source("covidcomp_lib.R")
min_us <- 5
min_global <- 10

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
  sidebarLayout(
    sidebarPanel(
      # data options
      sliderInput("min_total",
                  "initial number of deaths:",
                  min = 1,
                  max = 50,
                  value = 10),
      sliderInput("max_days_since",
                  "days since nth death",
                  min = 5,
                  max = 90,
                  value = 30),
      # plot options
      checkboxInput("smooth_plots",
                    "Smooth plot values", value = TRUE), 
      checkboxInput("scale_to_fit",
                    "Scale to fit", value = TRUE), 
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        id = "plotTabs", type = "tabs",
        tabPanel(
          "Global", value = "Global",
          plotlyOutput(
            "compPlot", width = "100%", height = "1600px")),
        tabPanel(
          "US", value = "US",
          plotlyOutput(
            "compPlotUS", width = "100%", height = "1600px"))
        # tabPanel(
        #   "Data", dataTableOutput("compData"),
        #   downloadButton("downloadCompData", "Download"))
        )
    )
  )
)
 
server <- function(input, output, session) {
  shinyjs::runjs("toggleCodePosition();")
  observe({
    updateSliderInput(
      session, "min_total",
      value = {if (input$plotTabs == "US") min_us else min_global})
  })
  
  output$compPlot <- renderPlotly({
    withProgress(message = 'Making plot', value = 0, {
      jhu %>% genCompData(min_total = input$min_total) %>%
        # filter plots
        filter(!(stat %in% c("cfr", "crr") & value_type == "double_days")) %>%
        plotComps(min_total = input$min_total,
                  max_days_since = input$max_days_since,
                  smooth_plots = input$smooth_plots,
                  scale_to_fit = input$scale_to_fit)
    })
  })
  output$compPlotUS <- renderPlotly({
    withProgress(message = 'Making plot', value = 0, {
      covtrack %>%
        genCompData(geo_level = "state", min_stat = "death",
                    min_total = input$min_total) %>%
        # filter plots
        filter(!stat %in% c("negative", "pending")) %>%
        filter(!(endsWith(stat, "r") & value_type == "double_days")) %>%
        plotComps(min_total = input$min_total,
                  max_days_since = input$max_days_since,
                  smooth_plots = input$smooth_plots,
                  scale_to_fit = input$scale_to_fit)
    })
  })
  # output$compData <- renderDataTable({
  #   joined %>% genCompData(min_total = input$min_total)
  # })
  # output$downloadCompData <- downloadHandler(
  #   filename = function() {
  #     paste0("covid-comp-data-", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     write_csv(joined %>% genCompData(min_total = input$min_total), file)
  #   }
  # )
}

shinyApp(ui = ui, server = server)