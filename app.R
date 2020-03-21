library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly) 
library(DT)

source("covidcomp_lib.R")

# pull in data
joined <- readJoinJhuData() %>%
  group_by(`Country/Region`) %>%
  filter(sum(deaths) >= 20) %>% ungroup()
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
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        id = "plotTabs", type = "tabs",
        tabPanel(
          "Global Plot", value = "Global",
          plotlyOutput(
            "compPlot", width = "100%", height = "1600px")),
        # tabPanel(
        #   "Data", dataTableOutput("compData"),
        #   downloadButton("downloadCompData", "Download")),
        tabPanel(
          "US Plot", value = "US",
          plotlyOutput(
            "compPlotUS", width = "100%", height = "1600px"))
        )
    )
  )
)
 
server <- function(input, output, session) {
  shinyjs::runjs("toggleCodePosition();")
  observe({
    updateSliderInput(session, "min_total",
                      value = {if (input$plotTabs == "US") 2 else 10})
  })
  
  output$compPlot <- renderPlotly({
    joined %>% genCompData(min_total = input$min_total) %>%
      plotComps(min_total = input$min_total,
                max_days_since = input$max_days_since,
                smooth_plots = input$smooth_plots)
  })
  output$compPlotUS <- renderPlotly({
    joined %>%
      filter(`Country/Region` == "US") %>%
      filter(!grepl(",", `Province/State`)) %>%
      genCompData(geo_level = "Province/State", min_total = input$min_total) %>%
      plotComps(min_total = input$min_total,
                max_days_since = input$max_days_since,
                smooth_plots = input$smooth_plots)
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