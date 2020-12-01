library(shiny)
#library(shinythemes)
library(shinyjs)
library(shinybusy)

source("covidcomp_lib.R", local = TRUE)

# set up global params
min_global <- 5
refresh_interval <- hours(24)

# pull in data
all_locs <- lazy_dt(fst::read_fst("data/goog.fst"), key_by = "location")

n_locs <- 5
loc_list <- all_locs %>% filter(stat == "deaths") %>%
  group_by(location) %>%
  summarise(severity = log(last(popM, order_by = date))) %>%
  filter(is.finite(severity) & severity > 0) %>%
  collect()

last_update <- now(tzone = "GMT")

# control over mouse over values in plotly plot
options(scipen = 999, digits = 1)

ui <- function(request) {
  fluidPage(
    textInput("txt", "Text"),
    checkboxInput("chk", "Checkbox"),
    bookmarkButton()
  )
}
ui <- function(request) {
  fluidPage(
    #tags$head(includeHTML(("www/google-analytics.html"))),
    useShinyjs(), # for moving showcase code to the bottom
    add_busy_bar(color = "CornflowerBlue", timeout = 1000),
    #theme = shinytheme("lumen"),
    titlePanel("Covid-19 comparisons"),
    tags$a(
      href = "https://github.com/roboton/covid-compare",
      target = "_blank", "[git]"),
    tags$a(
      href = "mailto:roberton@gmail.com",
      target = "_blank", "[contact]"),
    tags$span(paste(" Last updated",
                    round(as.numeric(now() - last_update, units = "hours"), 2),
                    "hours ago")),
    sidebarLayout(
      sidebarPanel(
        # data options
        selectizeInput(
          "location", "Location",
          choices = loc_list$location,
          selected = sample(loc_list$location, size = n_locs,
                            prob = loc_list$severity),
          options = list(
            placeholder = 'type to select a location'),
          multiple = TRUE),
        bookmarkButton(),
        selectInput("min_stat", "metric to compare by:",
                    c("Deaths" = "deaths", "Confirmed cases" = "confirmed")),
        numericInput("min_thresh",
                     "initial number of deaths/cases:",
                     min = 0, value = min_global),
        checkboxInput("per_million",
                      "Counts per million people", value = TRUE),
        numericInput("max_days_since",
                     "days since initial number of deaths/cases:",
                     min = 0, value = 360),
        # plot options
        checkboxInput("smooth_plots",
                      "Smooth plot values", value = TRUE), 
        checkboxInput("scale_to_fit",
                      "Scale to fit", value = TRUE),
        checkboxInput("double_days",
                      "Show double days", value = TRUE),
        checkboxInput("show_daily",
                      "Show daily", value = FALSE),
        width = 2),
      mainPanel(
        tabsetPanel(
          id = "plotTabs", type = "tabs",
          tabPanel(
            "Plots", value = "plots",
            plotlyOutput(
              "compPlot", width = "100%", height = "1400px"),
              downloadButton("downloadGlobalData", "download data (csv)")
          ),
          # tabPanel(
          #   "Severity", value = "severity",
          #   DT::dataTableOutput("severityTable")
          # ),
          tabPanel(
            "FAQ",
            h4("Why does my country, state, province, or county not show up at all?"),
            p("By default only locations that have at least one death per million people are compared. Setting initial deaths (per million) to zero, for example, will show all locations."),
            h4("How frequently does this update?"),
            p("At least every six hours. Last updated date is shown below the title."),
            h4("Where does the data come from?"),
            p(a(href = "https://github.com/GoogleCloudPlatform/covid-19-open-data",
                "Google")),
            p("Search as you type from the Location box on the left."),
            h4("How do I hide certain location in my plot?"),
            p("Unclick them from the legend on the right hand side."),
            h4("How do I show just one location in my plot?"),
            p("Double click that location from the legend on the right hand side."),
            h4("When I unclick a location, the plot moves. Why does that happen?"),
            p("The y-axis scales to the minimum and maximum values displayed on the plot."),
            h4("What does it mean for days to double to increase over time?"),
            p("It's taking longer and longer for your counts to double - this is good for things we don't want like deaths and cases."),
            h4("What does the Download button do?"),
            p("Downloads the data (in csv format) used for the set of plots displayed."),
            h4("How do I provide feedback?"),
            p("File an issue in github ", a(href = "https://github.com/roboton/covid-compare/issues/new", "here"),
              " or contact me by ", a(href = "mailto:roberton@gmail.com", "email"))
          )
        ),
        width = 10
      )
    )
  )
}
   
  server <- function(input, output, session) {
    shinyjs::runjs("toggleCodePosition();")
    output$compPlot <- renderPlotly({
      filter_locs <- all_locs %>%
        filter(location %in% !!input$location) %>% collect()
      if (nrow(filter_locs) == 0) {
        return(plotly_empty())
      }
      filter_locs %>%
        genPlotComps(geo_level = "location",
                     min_thresh = input$min_thresh,
                     per_million = input$per_million,
                     min_stat = input$min_stat,
                     max_days_since = input$max_days_since,
                     smooth_plots = input$smooth_plots,
                     scale_to_fit = input$scale_to_fit,
                     refresh_interval = refresh_interval,
                     double_days = input$double_days,
                     show_daily = input$show_daily)
      })
    # output$severityTable <- DT::renderDataTable({
    #   loc_list
    # })
    output$downloadGlobalData <- downloadHandler(
      filename = function() {
        paste0("covid-global-comp-data-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(all_locs %>% genCompData(
          geo_level = "location", min_thresh = input$min_thresh,
          per_million = input$per_million, min_stat = input$min_stat), file)
      }
    )
  }

shinyApp(ui = ui, server = server, enableBookmarking = "url")