library(shiny)
library(shinythemes)
library(shinyjs)
library(shinybusy)

source("covidcomp_lib.R", local = TRUE)

# set up global params
min_global <- 1
refresh_interval <- hours(6)

default_locations <- c("US", "Spain", "Korea, South", "Italy", "China", "Iran",
                       "California, USA", "Louisiana, USA", "New York, USA",
                       "New Jersey, USA",
                       "King County, Washington",
                       "Los Angeles County, California",
                       "Orleans County, Louisiana", 
                       "Santa Clara County, California",
                       "Wayne County, Michigan",
                       "New York City, New York")

# pull in data
jhu <- fetchPrepJhuData()
covtrack <- fetchPrepCovTrackData()
nyt <- fetchPrepNyt()

all_locs <- jhu %>%
  rename(location = country) %>%
  mutate(aggregate = "country") %>%
  bind_rows(
  covtrack %>%
    rename(location = state) %>%
    mutate(aggregate = "state"),
  nyt %>%
    rename(location = county) %>%
    mutate(aggregate = "county")) %>%
  group_by(location) %>%
  filter(any(stat == "deaths" & (!is.nan(popM) & popM >= min_global))) %>%
  ungroup()
  

loc_list <- all_locs %>% select(location, aggregate) %>% unique()

last_update <- now(tzone = "GMT")

# control over mouse over values in plotly plot
options(scipen = 999, digits = 1)

ui <- fluidPage(
  tags$head(includeHTML(("www/google-analytics.html"))),
  useShinyjs(), # for moving showcase code to the bottom
  add_busy_bar(color = "CornflowerBlue", timeout = 800),
  theme = shinytheme("lumen"),
  titlePanel("Covid-19 comparisons"),
  tags$div(paste("Last updated:", paste(last_update, "GMT"))),
  tags$a(
    href = "https://robon.shinyapps.io/covidcomp/?showcase=1",
    target = "_blank", "[showcase]"),
  tags$a(
    href = "https://github.com/roboton/covid-compare",
    target = "_blank", "[git]"),
  tags$a(
    href = "mailto:roberton@gmail.com",
    target = "_blank", "[contact]"),
  sidebarLayout(
    sidebarPanel(
      # data options
      selectizeInput(
        "location", "Countries, US states or counties", choices = loc_list %>% select(location),
        selected = default_locations,
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
                   min = 0, value = 30),
      # plot options
      checkboxInput("smooth_plots",
                    "Smooth plot values", value = TRUE), 
      checkboxInput("scale_to_fit",
                    "Scale to fit", value = TRUE),
      checkboxInput("double_days",
                    "Show double days", value = TRUE),
      h3("Why another dashboard?"),
      p("Facts are useful, but in times like these they can also be overwhelming.  It can be helpful to organize these facts so they can provide perspective and focus our attention to the right places."),
      p("With Covid-19, it’s not enough to display frightening curves or list the death toll. That just tells you how many people died. It doesn’t explain which countries are hit the worst and which are doing relatively well."), 
      p("This dashboard sets each country to the same starting point: X deaths per million of population.  This way, you can compare China, with its huge population and early experience with Covid-19, and Australia, with its much smaller population and later onset."),
      p("In addition, the metric in the second column of plots seek to give us an intuitive measure of how quickly the measures in the first column are growing over time. It is computed as the number of days it took to double the counts in the first column.  Pandemics are scary because of exponential growth, and days to double is relatively intuitive way of understanding that."),
      p("Data from ", a(href = "https://coronavirus.jhu.edu/map.html", "JHU CSSE"),
        ", ",  a(href = "https://github.com/nytimes/covid-19-data", "NY Times"), 
        ", ",  a(href = "https://www.cdc.gov/flu/weekly/index.htm", "CDC FluView"), 
        " and ", a(href = "https://covidtracking.com/", "Covid Tracking Project"),
        ". Great ideas from ", a(href = "https://twitter.com/loeserjohn", "John.")),
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
        tabPanel(
          "FAQ",
          h4("Why does my country, state, or county not show up here?"),
          p("By default only locations that have at least one death per million people are compared. Setting initial deaths to zero, for example, will show all locations."),
          h4("How frequently does this update?"),
          p("At least every six hours. Last updated date is shown below the title."),
          h4("Where does the data come from?"),
          p(a(href = "https://coronavirus.jhu.edu/map.html", "John Hopkins CSSE"),
            " for country data, ",
            a(href = "https://www.cdc.gov/flu/weekly/index.htm", "CDC FluView"),
            " for US state flu data, ",
            a(href = "https://covidtracking.com/", "Covid Tracking Project"),
            " for US state data, and ",
            a(href = "https://github.com/nytimes/covid-19-data",
              "New York Times"), " for US county data."),
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
 
server <- function(input, output, session) {
  shinyjs::runjs("toggleCodePosition();")
  output$compPlot <- renderPlotly({
    all_locs %>%
      filter(location %in% input$location) %>%
      genPlotComps(geo_level = "location",
                   min_thresh = input$min_thresh,
                   per_million = input$per_million,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   scale_to_fit = input$scale_to_fit,
                   refresh_interval = refresh_interval,
                   double_days = input$double_days) })
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