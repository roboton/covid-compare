library(tidyverse)
library(lubridate)
library(shiny)
library(shinythemes)
library(plotly) 
library(DT)
library(shinyjs)
 
# control over mouse over values in plotly plot
options(scipen = 999, digits = 1)

# read data
jhu_csse_uri <- paste0(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
  "csse_covid_19_data/csse_covid_19_time_series/")

confirmed_ts <- read_csv(
  url(paste0(jhu_csse_uri, "time_series_19-covid-Confirmed.csv"))) %>%
  gather(date, confirmed, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = mdy(date))
deaths_ts <- read_csv(
  url(paste0(jhu_csse_uri, "time_series_19-covid-Deaths.csv"))) %>%
  gather(date, deaths, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = mdy(date))
recovered_ts <- read_csv(
  url(paste0(jhu_csse_uri, "time_series_19-covid-Recovered.csv"))) %>%
  gather(date, recovered, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = mdy(date))

# data prep
joined <- confirmed_ts %>%
  left_join(deaths_ts) %>%
  left_join(recovered_ts)
  
# plot comps function
genCompData <- function(df, geo_level = "Country/Region", min_stat = "deaths",
                    min_total = 10) {
    df %>%
    # sum by location
    mutate(location = !!sym(geo_level)) %>%
    group_by(location, date) %>%
    summarise_at(vars(confirmed, deaths, recovered), sum, na.rm = T) %>%
    # add cfr, consider other metrics
    mutate(cfr = deaths / confirmed, crr = recovered / confirmed) %>%
    gather(stat, total, confirmed, deaths, recovered, cfr, crr) %>%
    # get max_total and first_date per location/stat
    group_by(location, stat) %>%
    mutate(max_total = max(total),
           first_date = suppressWarnings(min(date[total >= min_total]))) %>%
    group_by(location) %>%
    # drop earlier dates
    filter(date >= first_date[stat == min_stat]) %>%
    # recenter dates
    mutate(days_since = date - first_date[stat == min_stat]) %>%
    ungroup() %>%
    # calculate double_days
    group_by(location, stat) %>%
    mutate(double_days = date - date[
      sapply(1:length(total), FUN = function(i) {
        suppressWarnings(max(which(total[1:i] <= total[i]/2))) })]) %>%
    ungroup() %>%
    gather(value_type, value, total, double_days)
}

plotComps <- function(df, min_stat = "deaths", min_total = 10,
                      max_days_since = 20, min_days_since = 5,
                      smooth_plots = FALSE) {
  df %>%
    # lazy filter for erroneous data
    filter(value >= 0) %>%
    # truncate days_since
    filter(days_since <= max_days_since) %>%
    # filter not enough points
    group_by(location, stat, value_type) %>%
    filter(n() > min_days_since) %>%
    ungroup() %>%
    # filter plots (double_days for cfr and crr)
    filter(!(stat %in% c("cfr", "crr") & value_type == "double_days")) %>%
    # order plots and readable labels
    mutate(
      value_type = factor(value_type, levels = c("total", "double_days"),
                          labels = c("Total count",
                                     "Days to double total count")),
      stat = factor(stat, levels = c("deaths", "confirmed", "recovered", "cfr",
                                     "crr"),
                    labels = c("Deaths", "Confirmed cases", "Recovered cases",
                               "Case fatality rate", "Case recovery rate"))) %>%
    # plot begins
    ggplot(aes(days_since, value, color = location)) +
    # no smoothing
    {if (!smooth_plots) geom_line(alpha = 0.8)} +
    # smoothing
    {if (smooth_plots) geom_line(stat = "smooth", method = "auto",
                                 alpha = 0.8)} +
    {if (smooth_plots) geom_point(alpha = 0.2)} +
    # .multi_line false doesn't work with ggplotly
    facet_wrap(vars(stat, value_type), scales = "free", ncol = 2,
               labeller = labeller(.multi_line = TRUE)) +
    # thematic things
    theme_minimal() +
    xlab(paste("Days since", min_stat, ">=", min_total)) +
    theme(legend.title = element_blank(), axis.title.y = element_blank())
}

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("lumen"),
  titlePanel("Covid-19 comparisons"),
  tags$a(
    href = "https://github.com/CSSEGISandData/COVID-19",
    target = "_blank", "[data]"),
  tags$a(
    href = "https://ond3.com/exploratory.nb.html",
    target = "_blank", "[analysis]"),
  tags$a(
    href = "https://github.com/roboton/covid-19_meta/tree/master/covidcomp",
    target = "_blank", "[git]"),
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
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot", plotlyOutput(
            "compPlot", width = "100%", height = "1600px")),
        tabPanel(
          "Data", dataTableOutput("compData"),
          downloadButton("downloadCompData", "Download")))
    )
  )
)
 
server <- function(input, output) {
  shinyjs::runjs("toggleCodePosition();")
  output$compPlot <- renderPlotly({
    joined %>% genCompData(min_total = input$min_total) %>%
      plotComps(min_total = input$min_total,
                max_days_since = input$max_days_since,
                smooth_plots = input$smooth_plots)
  })
  output$compData <- renderDataTable({
    joined %>% genCompData(min_total = input$min_total)
  })
  output$downloadCompData <- downloadHandler(
    filename = function() {
      paste0("covid-comp-data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(joined %>% genCompData(min_total = input$min_total), file)
    }
  )
}

shinyApp(ui = ui, server = server)