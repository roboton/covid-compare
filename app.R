library(shiny)
library(shinythemes)
library(shinyjs)
library(shinybusy)

source("covidcomp_lib.R", local = TRUE)

# set up global params
min_global <- 1
refresh_interval <- hours(6)

# pull in data
# jhu <- fetchPrepJhuData()
# covtrack <- fetchPrepCovTrackData()
# nyt <- fetchPrepNyt()
# cds <- fetchPrepCorDataScrape()
# 
# all_locs <- jhu %>%
#   rename(location = country) %>%
#   mutate(location = paste(location, "[jhu]")) %>%
#   bind_rows(
#     covtrack %>%
#       rename(location = state) %>%
#       mutate(location = paste(location, "[ctp]")),
#     nyt %>%
#       rename(location = county) %>%
#       mutate(location = paste(location, "[nyt]")),
#     cds %>%
#       mutate(location = paste(location, "[cds]"))
#   ) %>%
#   group_by(location) %>%
#   filter(any(stat == "deaths" & (!is.nan(popM) & popM >= min_global))) %>%
#   ungroup()

all_locs <- fetchPrepCorDataScrape()

# location_severity <- getLocationSeverity(all_locs)
# loc_list <- location_severity %>% pull(location) %>% unique()
# default_locations <- sample(location_severity$location, 8,
#                             prob = pmax(location_severity$severity, 0))


# loc_list <- all_locs %>% pull(location) %>% unique()
# default_locations <- c("Hubei, China [cds]",
#                        "King County, Washington [nyt]",
#                        "Orleans County, Louisiana [nyt]",
#                        "Louisiana, USA [ctp]",
#                        "New York, USA [ctp]",
#                        "New York City, New York [nyt]",
#                        "Lombardy, ITA [cds]",
#                        "Korea, South [jhu]")

# default_locations <- c("Hubei, China",
#                        "Lombardy, Italy",
#                        "New York City, New York, United States")
loc_list <- getLocationList(all_locs, severity = "simple")
# default_locations <- c(default_locations, sample(loc_list$location, size = 3,
#                                                  prob = loc_list$severity))

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
    tags$head(includeHTML(("www/google-analytics.html"))),
    useShinyjs(), # for moving showcase code to the bottom
    add_busy_bar(color = "CornflowerBlue", timeout = 800),
    theme = shinytheme("lumen"),
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
          choices = loc_list,
          selected = sample(loc_list$location, size = 3,
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
                     min = 0, value = 45),
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
        p("This dashboard sets each country to the same starting point: X deaths per million of population.  Deaths are chosen because we believe it is less sensitive than cases to a locale's ability to test. This way, you can compare China, with its huge population and early experience with Covid-19, and Australia, with its much smaller population and later onset."),
        p("In addition, the metric in the second column of plots seek to give us an intuitive measure of how quickly the measures in the first column are growing over time. It is computed as the number of days it took to double the counts in the first column.  Pandemics are scary because of exponential growth, and days to double is relatively intuitive way of understanding that."),
        p("Data from ", a(href = "https://coronavirus.jhu.edu/map.html", "JHU CSSE"), "[jhu]",
          ", ",  a(href = "https://coronadatascraper.com", "Corona Data Scraper"),  "[cds]",
          ", ",  a(href = "https://github.com/nytimes/covid-19-data", "NY Times [nyt]"), "[nyt]",
          ", ",  a(href = "https://www.cdc.gov/flu/weekly/index.htm", "CDC FluView"), "[ctp]",
          " and ", a(href = "https://covidtracking.com/", "Covid Tracking Project"), "[ctp]",
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
            h4("Why does my country, state, province, or county not show up at all?"),
            p("By default only locations that have at least one death per million people are compared. Setting initial deaths (per million) to zero, for example, will show all locations."),
            h4("How frequently does this update?"),
            p("At least every six hours. Last updated date is shown below the title."),
            h4("Where does the data come from?"),
            p(
              a(href = "https://coronadatascraper.com/", "Corona Data Scraper"),
              "[cds], ",
              a(href = "https://coronavirus.jhu.edu/map.html", "John Hopkins CSSE"),
              "[jhu] for country data, ",
              a(href = "https://www.cdc.gov/flu/weekly/index.htm", "CDC FluView"),
              "[flu] for US state flu data, ",
              a(href = "https://covidtracking.com/", "Covid Tracking Project"),
              "[ctp] for US state data, and ",
              a(href = "https://github.com/nytimes/covid-19-data",
                "New York Times"), "[nyt] for US county data."),
            h4("How do I add more locations to my plot?"),
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
        filter(location %in% input$location)
      if (nrow(filter_locs) == 0) {
        return(plotly_empty())
      }
      filter_locs %>%
        filter(location %in% input$location) %>%
        genPlotComps(geo_level = "location",
                     min_thresh = input$min_thresh,
                     per_million = input$per_million,
                     min_stat = input$min_stat,
                     max_days_since = input$max_days_since,
                     smooth_plots = input$smooth_plots,
                     scale_to_fit = input$scale_to_fit,
                     refresh_interval = refresh_interval,
                     double_days = input$double_days)
      })
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