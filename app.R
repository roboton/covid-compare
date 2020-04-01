if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinythemes, shinyjs, shinybusy)

source("covidcomp_lib.R", local = TRUE)

# set up global params
min_us <- 1
min_global <- 1
refresh_interval <- hours(6)

exclude_countries <- c("San Marino", "Guyana", "China", "Andorra", "Cabo Verde")
default_locations <- c("US", "Spain", "Korea, South", "Italy", "China", "Iran",
                       "CA", "LA", "NY", "NJ",
                       "King County, Washington",
                       "Los Angeles, California",
                       "Orleans, Louisiana", 
                       "Santa Clara, California",
                       "Wayne, Michigan",
                       "New York City, New York")

# pull in data
jhu <- fetchPrepJhuData()
covtrack <- fetchPrepCovTrackData()
nyt <- fetchPrepNyt()
last_update <- now(tzone = "GMT")

# control over mouse over values in plotly plot
options(scipen = 999, digits = 1)

ui <- fluidPage(
  useShinyjs(), # for moving showcase code to the bottom
  add_busy_bar(color = "CornflowerBlue", timeout = 800),
  theme = shinytheme("lumen"),
  titlePanel("Covid-19 comparisons"),
  tags$div(paste("Last updated:", paste(last_update, "GMT"))),
  tags$a(
    href = "https://github.com/CSSEGISandData/COVID-19",
    target = "_blank", "[data]"),
  # tags$a(
  #   href = "https://robon.shinyapps.io/covidcomp/?showcase=1",
  #   target = "_blank", "[showcase]"),
  tags$a(
    href = "https://github.com/roboton/covid-compare",
    target = "_blank", "[git]"),
  tags$a(
    href = "mailto:roberton@gmail.com",
    target = "_blank", "[contact]"),
  # tags$a(
  #   href = "https://robon.shinyapps.io/covidcomp/covidcomp_static.html",
  #   target = "_blank", "[too slow?]"),
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
      checkboxInput("per_million",
                    "Counts per million people", value = TRUE),
      checkboxInput("smooth_plots",
                    "Smooth plot values", value = TRUE), 
      checkboxInput("scale_to_fit",
                    "Scale to fit", value = TRUE),
      h3("Why another dashboard?"),
      p("Facts are useful, but in times like these they can also be overwhelming.  It can be helpful to organize these facts so they can provide perspective and focus our attention to the right places.  The plots on the right contain the same data that you've seen in other places but organizes them in two important ways:"),
      p("(1) By default, the time series for each country/state begins on the day where each location had at least one death per million people in that location.  This is meant to calibrate each country/state so they can be compared at the same initial severity.  We choose deaths (vs cases) as our measure of severity because they are less sensitive to testing capacity as we assume that patients in more critical conditions are tested more uniformly than the population at large."),
      p("(2) Days to double, the metric in the second column seeks to give us an intuitive measure of how quickly the measures in the first column are growing over time.  It is computed as the number of days it took to double the counts in the first column.  Pandemics are scary because of exponential growth, and days to double is relatively intuitive way of understanding that."),
      p("Data from ", a(href = "https://coronavirus.jhu.edu/map.html", "JHU CSSE"),
        ", ",  a(href = "https://github.com/nytimes/covid-19-data", "NY Times"), 
        " and ", a(href = "https://covidtracking.com/", "Covid Tracking Project"),
        ". Great ideas from ", a(href = "https://twitter.com/loeserjohn", "John.")),
      width = 2),
    mainPanel(
      tabsetPanel(
        id = "plotTabs", type = "tabs",
        tabPanel(
          "Global", value = "Global",
          plotlyOutput(
            "compPlot", width = "100%", height = "1200px"),
            downloadButton("downloadGlobalData", "download data (csv)")
        ),
        tabPanel(
          "US States", value = "US",
          plotlyOutput(
            "compPlotUS", width = "100%", height = "1600px"),
            downloadButton("downloadUSData", "download data (csv)")
        ),
        tabPanel(
          "US Counties", value = "County",
          plotlyOutput(
            "compPlotCounty", width = "100%", height = "1200px"),
            downloadButton("downloadCountyData", "download data (csv)")
        ),
        tabPanel(
          "FAQ",
          h4("Why does my country not show up here?"),
          p("By default only countries that have one death per million people are shown and have a population of at least 1M people. Setting initial deaths to zero, for example, will show all countries."),
          h4("Why does my US state not show up here?"),
          p("By default only states that have one death per million people are shown. Setting initial deaths to zero, for example, will show all states."),
          h4("Why does my US county not show up here?"),
          p("By default only counties that have one death per million people are shown.  Counties with less than eight deaths are also not shown. Setting initial deaths, for example, to zero will show all counties with five or more deaths."),
          h4("How frequently does this update?"),
          p("Daily. Last updated date is shown below the title."),
          h4("Where does the data come from?"),
          p(a(href = "https://coronavirus.jhu.edu/map.html", "John Hopkins CSSE"),
            " for country data, the ",
            a(href = "https://covidtracking.com/", "Covid Tracking Project"),
            " for US state data, and the ",
            a(href = "https://github.com/nytimes/covid-19-data",
              "New York Times"), " for US county data."),
          h4("When I unclick a location, the plot moves. Why does that happen?"),
          p("The y-axis scales to the minimum and maximum values displayed on the plot."),
          h4("What does it mean for days to double to increase over time?"),
          p("It's taking longer and longer for your counts to double - this is good for things we don't want like deaths and cases."),
          h4("Why did you choose these locations to show by default?"),
          p("The default locations are chosen for no good reason besides that they are often talked about in the press. Please explore by clicking and unclicking the locations on the right.  Double-clicking a selected location will display only that location's plot."),
          h4("What does the Download button do?"),
          p("Downloads the data (in csv format) used for the set of plots displayed.")
        )
        # tabPanel(
        #   "Data", dataTableOutput("compData"),
        #   downloadButton("downloadCompData", "Download"))
      ),
      width = 10
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
  
  output$compPlot <- renderPlotly({
    jhu %>%
      genPlotComps(geo_level = "country",
                   min_thresh = input$min_thresh,
                   per_million = input$per_million,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   scale_to_fit = input$scale_to_fit,
                   refresh_interval = refresh_interval) })
  output$compPlotUS <- renderPlotly({
    covtrack %>%
      genPlotComps(geo_level = "state",
                   min_thresh = input$min_thresh,
                   per_million = input$per_million,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   scale_to_fit = input$scale_to_fit,
                   refresh_interval = refresh_interval) })
  output$compPlotCounty <- renderPlotly({
    # cds %>% 
    #   genPlotComps(geo_level = "location",
    nyt %>% 
      genPlotComps(geo_level = "county",
                   min_thresh = input$min_thresh,
                   per_million = input$per_million,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   scale_to_fit = input$scale_to_fit,
                   refresh_interval = refresh_interval) }) 
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
        per_million = input$per_million, min_stat = input$min_stat), file)
    }
  )
  output$downloadUSData <- downloadHandler(
    filename = function() {
      paste0("covid-us-comp-data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(covtrack %>% genCompData(geo_level = "state",
                                         min_thresh = input$min_thresh,
                                         per_million = input$per_million,
                                         min_stat = input$min_stat), file)
    }
  )
  output$downloadCountyData <- downloadHandler(
    filename = function() {
      paste0("covid-county-comp-data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(nyt %>% genCompData(geo_level = "county",
                                    min_thresh = input$min_thresh,
                                    per_million = input$per_million,
                                    min_stat = input$min_stat), file)
    }
  )
}

shinyApp(ui = ui, server = server)