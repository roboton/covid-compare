library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

options(scipen=999)

# read data
jhu_csse_uri <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

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
plotComps <- function(df, geo_level = "Country/Region", min_stat = "deaths",
                      min_total = 10, max_days_since = 20,
                      smooth_plots = FALSE) {
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
      sapply(total, FUN = function(x) {
        suppressWarnings(max(which(total <= x/2))) })]) %>%
    ungroup() %>%
    gather(value_type, value, total, double_days) %>%
    ## plot things
    # lazy filter for erroneous data
    filter(value >= 0) %>%
    # truncate days_since
    filter(days_since <= max_days_since) %>%
    
    # filter not enough points
    group_by(location, stat, value_type) %>%
    filter(n() > 3) %>%
    ungroup() %>%
    
    # # dropping small locations
    # group_by(location) %>%
    # filter(max_total[stat == min_stat][1] >= min_total) %>%
    # ungroup() %>%
    
    # filter plots
    filter(!(stat %in% c("cfr", "crr") & value_type == "double_days")) %>%
    # order plots
    mutate(
      value_type = factor(value_type, levels = c("total", "double_days"),
                          labels = c("Total count", "Days to double count")),
      stat = factor(stat, levels = c("deaths", "confirmed", "recovered", "cfr",
                                     "crr"),
                    labels = c("Deaths", "Confirmed cases", "Recovered cases",
                               "Case fatality rate", "Case recovery rate"))) %>%
    ggplot(aes(days_since, value, color = location)) +
    {if (!smooth_plots) geom_line()} +
    {if (smooth_plots) geom_line(stat = "smooth", method = "auto")} +
    {if (smooth_plots) geom_point(alpha = 0.2)} +
    facet_wrap(vars(stat, value_type), scales = "free", ncol = 2,
               labeller = labeller(.multi_line = FALSE)) +
    theme_minimal() +
    theme(legend.title = element_blank(), axis.title.y = element_blank()) +
    xlab(paste("Days since", min_stat, ">=", min_total))
}

ui <- fluidPage(
  titlePanel("Covid-19 comparisons"),
  tags$a(
    href = "https://github.com/CSSEGISandData/COVID-19",
    target = "_blank", "[data]"),
  tags$a(
    href = "https://ond3.com/exploratory.nb.html",
    target = "_blank", "[analysis]"),
  tags$a(
    href = "https://github.com/roboton/covid-19_meta/tree/master/deathcomp",
    target = "_blank", "[git]"),
  # Sidebar with a slider input for number of bins 
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
                  min = 1,
                  max = 90,
                  value = 30),
      # plot options
      checkboxInput("smooth_plots",
                    "Smooth plot values", value = FALSE), 
      width = 3
    ),
    mainPanel(
      plotlyOutput("compPlot", height = "1600px")
    )
  )
)
 
server <- function(input, output) {
  output$compPlot <- renderPlotly({
    joined %>%
      plotComps(min_total = input$min_total,
                max_days_since = input$max_days_since,
                smooth_plots = input$smooth_plots) %>%
      ggplotly() %>%
      layout(yaxis = list(hoverformat = 'd'))
  })
}

shinyApp(ui = ui, server = server)