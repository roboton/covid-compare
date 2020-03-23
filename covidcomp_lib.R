library(tidyverse)
library(lubridate)
library(covid19us)

fetchPrepJhuData <- function() {
  # read data
  jhu_csse_uri <- paste0(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
    "csse_covid_19_data/csse_covid_19_time_series/")
  
  read_csv(url(paste0(jhu_csse_uri, "time_series_19-covid-Confirmed.csv"))) %>%
    mutate(stat = "confirmed")  %>%
    bind_rows(read_csv(
      url(paste0(jhu_csse_uri, "time_series_19-covid-Deaths.csv"))) %>%
        mutate(stat = "deaths")) %>%
    bind_rows(read_csv(
      url(paste0(jhu_csse_uri, "time_series_19-covid-Recovered.csv"))) %>%
        mutate(stat = "recovered")) %>%
    select(-Lat, -Long, -`Province/State`) %>%
    gather(date, value, -`Country/Region`, -stat) %>%
    group_by(`Country/Region`, date, stat) %>%
    summarise_all(sum, na.rm = TRUE) %>% ungroup() %>%
    mutate(date = mdy(date)) %>% spread(stat, value) %>%
    mutate(cfr = deaths / confirmed, crr = recovered / confirmed) %>%
    gather(stat, total, -`Country/Region`, -date)
}
 
fetchPrepCovTrackData <- function() {
  covid19us::get_states_daily() %>%
    select(-date_checked, -request_datetime) %>%
    mutate(cfr = death / positive , ptr = positive / (positive + negative),
           hr = hospitalized / positive, dhr = death / hospitalized) %>%
    gather(stat, total, -date, -state)
}

# plot comps function
genCompData <- function(df, geo_level = "Country/Region", min_stat = "deaths",
                        min_total = 10) {
  df %>%
    rename(location = all_of(geo_level)) %>%
    # get max_total and first_date per location/stat
    group_by(location, stat) %>%
    mutate(max_total = max(total, na.rm = TRUE),
           first_date = suppressWarnings(min(date[total >= min_total],
                                             na.rm = TRUE))) %>%
    group_by(location) %>%
    # drop earlier dates
    filter(date >= first_date[stat == min_stat]) %>%
    # recenter dates
    mutate(days_since = date - first_date[stat == min_stat]) %>%
    ungroup() %>%
    # calculate double_days
    group_by(location, stat) %>% arrange(date) %>%
    mutate(
      half_date = date[sapply(1:length(total), FUN = function(i) {
        suppressWarnings(max(which(total[1:i] <= total[i]/2), na.rm = TRUE)) })],
      double_days = date - half_date) %>%
    ungroup() %>%
    gather(value_type, value, total, double_days)
}

plotComps <- function(df, min_stat = "deaths", min_total = 10,
                      max_days_since = 20, min_days_since = 3,
                      smooth_plots = FALSE, scale_to_fit = TRUE) {
  df %>%
    # lazy filter for erroneous data
    filter(value >= 0) %>%
    # truncate days_since
    filter(days_since <= max_days_since) %>%
    # filter not enough points
    group_by(location, stat, value_type) %>%
    filter(n() >= min_days_since) %>%
    ungroup() %>%
    # order plots and readable labels
    mutate(
      value_type = factor(value_type, levels = c("total", "double_days"),
                          labels = c("Total count",
                                     "Days to double total count")),
      stat = factor(stat, levels = c("deaths", "confirmed", "recovered",
                                     "death", "positive", "total",
                                     "negative", "pending", "hospitalized",
                                     "cfr", "crr", "ptr", "hr", "dhr"),
                    labels = c("Deaths", "Confirmed cases", "Recovered cases",
                               "Deaths", "Positive tests", "Total tests",
                               "Negative tests", "Pending tests",
                               "Hospitalized", 
                               "Case fatality rate", "Case recovery rate",
                               "Positive test rate", "Hospitalization rate",
                               "Death/Hosp. rate"))) %>%
    # plot begins
    ggplot(aes(days_since, value, color = location, label = date)) +
    # no smoothing
    {if (!smooth_plots) geom_line(alpha = 0.8)} +
    # smoothing
    {if (smooth_plots) suppressWarnings(
      geom_line(stat = "smooth", method = "auto", alpha = 0.8))} +
    {if (smooth_plots) geom_point(alpha = 0.2)} +
    # .multi_line false doesn't work with ggplotly
    facet_wrap(vars(stat, value_type), ncol = 2,
               scales = {if (scale_to_fit) "free_y" else "fixed"},
               labeller = labeller(.multi_line = TRUE)) +
    # labelling
    ggtitle(paste("Metrics since", min_stat, ">=", min_total)) +
    xlab(paste("Days since", min_stat, ">=", min_total)) +
    # thematic things
    theme_minimal() +
    theme(legend.title = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) + ylim(0, NA)
}
