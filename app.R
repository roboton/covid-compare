library(shiny)

source("covidcomp_lib.R", local = TRUE)

# set up global params
min_global <- 1
refresh_interval <- hours(24)
data_file <- "data/goog_weekly.fst"
last_update <- file.info(data_file)$mtime
n_locs <- 3

# pull in data
all_locs <- lazy_dt(fst::read_fst(data_file), key_by = "location")

loc_list <- all_locs %>% filter(stat == "new_deceased") %>%
  filter(!is.na(value)) %>%
  group_by(location) %>%
  arrange(location, date) %>%
  summarise(
    severity_popM = last(value),
    severity_total = last(value) * last(population) / 1e6) %>%
  ungroup() %>%
  filter(is.finite(severity_total) & severity_total > 0) %>%
  mutate(level = str_count(location, ",")) %>%
         #location = as.character(location)) %>%
  arrange(level) %>% select(-level) %>%
  as_tibble()

compare_metrics <- c("deaths" = "total_deceased", "cases" = "total_confirmed",
                     "tests" = "total_tested")

# control over mouse over values in plotly plot
options(scipen = 999, digits = 1)

ui <- function(request) {
  fluidPage(
    #shinyjs::useShinyjs(), # for moving showcase code to the bottom
    shinybusy::add_busy_bar(color = "CornflowerBlue", timeout = 800),
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
          "location", "Location", choices = NULL, multiple = TRUE,
          options = list(placeholder = 'type to select a location')),
        bookmarkButton(),
        selectInput("min_stat", "metric to compare by:", compare_metrics),
        numericInput("min_thresh",
                     "initial number of deaths:", min = 0, value = min_global),
        checkboxInput("per_million",
                      "Counts per million people", value = TRUE),
        numericInput("max_days_since",
                     "days since initial number of deaths:", min = 0,
                     value = 365),
        # plot options
        checkboxInput("show_legend",
                      "Show legend", value = TRUE),
        checkboxInput("smooth_plots",
                      "Smooth plot values", value = FALSE), 
        width = 2),
      mainPanel(
        tabsetPanel(
          id = "plotTabs", type = "tabs",
          tabPanel(
            "Epidemiology", value = "epi_plots",
            plotlyOutput(
              "epiPlot", width = "100%", height = "1400px"),
              downloadButton("downloadData", "download data (csv)")
          ),
          tabPanel(
            "Hospitalization", value = "hosp_plots",
            plotlyOutput(
              "hospPlot", width = "100%", height = "1400px")
          ),
          tabPanel(
            "Severity", value = "severity",
            DT::dataTableOutput("severityTable")
          ),
          tabPanel(
            "FAQ",
            h4("Why does my country, state, province, or county not show up at all?"),
            p("By default only locations that have at least one death per million people are compared. Setting initial deaths (per million) to zero, for example, will show all locations."),
            h4("How frequently does this update?"),
            p("Every 24 hours. Last updated date is shown below the title."),
            h4("Where does the data come from?"),
            p("Google's excellent ", a(href = "https://github.com/GoogleCloudPlatform/covid-19-open-data",
                "covid19-open-data"), "."),
            h4("How do I add or remove more locations from the plot?"),
            p("Search as you type from the Location box on the left to add. Delete to remove."),
            h4("How do I hide certain location in my plot?"),
            p("Unclick them from the legend on the right hand side."),
            h4("How do I show just one location in my plot?"),
            p("Double click that location from the legend on the right hand side."),
            h4("When I unclick a location, the plot moves. Why does that happen?"),
            p("The y-axis scales to the minimum and maximum values displayed on the plot."),
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
  
  output$epiPlot <- renderPlotly({
    filter_locs <- all_locs %>%
      filter(location %in% !!input$location)
    # if (nrow(filter_locs) == 0) {
    #   return(empty_plot("no location data"))
    # }
    filter_locs %>%
      genPlotComps(min_thresh = input$min_thresh,
                   per_million = input$per_million,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   show_legend = input$show_legend,
                   plot_type = "epi")
    } %>% plotly::partial_bundle())
  
  output$hospPlot <- renderPlotly({
    filter_locs <- all_locs %>%
      filter(location %in% !!input$location)
    # if (nrow(filter_locs) == 0) {
    #   return(empty_plot("no location data"))
    # }
    filter_locs %>%
      genPlotComps(min_thresh = input$min_thresh,
                   per_million = input$per_million,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   show_legend = input$show_legend,
                   plot_type = "hosp")
    } %>% plotly::partial_bundle())
  
  output$severityTable <- DT::renderDataTable({
    loc_list %>% arrange(-severity_popM)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("covid-comp-data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(all_locs %>% filter(location %in% !!input$location) %>%
                         as_tibble(), file)
    }
  )
  
  # server side location selectize
  updateSelectizeInput(session, "location", choices = loc_list$location,
                       server = TRUE)
  
  # ensure we don't overwrite bookmark locations with default
  session$onRestore(function(state) {
    session$userData$restored <- TRUE
    updateSelectizeInput(session, "location", choices = loc_list$location,
                         selected = state$input$location, server = TRUE)
  })
  session$onFlushed(function() {
    if (is.null(session$userData$restored)) {
      updateSelectizeInput(session, "location", choices = loc_list$location,
                           selected = sample(loc_list$location, size = n_locs,
                                             prob = loc_list$severity_total),
                             # sample(loc_list$location, size = n_locs,
                             #        prob = log(loc_list$severity_popM + 1))),
                           server = TRUE)
    }
  }, once = TRUE)
  
  # shorten bookmark url
  session$onBookmarked(function(url) {
    short_url <- urlshorteneR::isgd_LinksShorten(longUrl = url)
    showBookmarkUrlModal(short_url)
  })
  
  # update min_stat metric
  observeEvent(input$min_stat, {
    stat_label <- names(compare_metrics)[compare_metrics == input$min_stat]
    updateSelectInput(session, "min_thresh",
                      label = paste0("initial number of ", stat_label, ":"))
    updateNumericInput(session, "max_days_since",
                       label = paste("days since initial number of",
                                     stat_label, ":"))
  })  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")