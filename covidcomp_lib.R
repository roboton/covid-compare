library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(purrr)
library(stringr)
library(vroom)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)

# Generate source data
fetchPrepGoogData <- function(min_deaths = 1, min_cases = 10) {
  vroom(
    "https://storage.googleapis.com/covid19-open-data/v2/main.csv",
    col_select = c(date, key, confirmed = total_confirmed,
                   hospitalized = total_hospitalized,
                   icu = total_intensive_care,
                   deaths = total_deceased, total_tests = total_tested,
                   population, locality_name, subregion2_name,
                   subregion1_name, country_name),
    col_types = c(key = "c", total_confirmed = "n", total_deceased = "n",
                  total_tested = "n", total_hospitalized = "n",
                  population = "n", total_intensive_care = "n", date = "D",
                  locality_name = "c", subregion2_name = "c",
                  subregion1_name = "c", country_name = "c"),
    num_threads = 16
    ) %>%
    unite(name, ends_with("_name"), sep = ", ", na.rm = TRUE) %>%
    mutate(key = str_replace(key, "^.*_", "")) %>%
    unite(location, name, key, sep = " ") %>%
    mutate(#negative_tests = total_tests - confirmed,
           case_fatality_rate = deaths / confirmed,
           positive_test_rate = confirmed / total_tests) %>%
    #filter(case_fatality_rate < 1 & positive_test_rate < 1) %>%
    pivot_longer(c(-date, -location, -population),
                 names_to = "stat", values_to = "total") %>%
    mutate(popM = if_else(
      !str_ends(stat, "_rate"), total / population * 1e6, total)) %>%
    filter(is.finite(total) & is.finite(popM) & !is.na(location)) %>%
    select(-population) %>%
    group_by(location) %>%
    filter(any(stat == "deaths" & (!is.nan(popM) & popM >= min_deaths)) &
             any(stat == "confirmed" & (!is.nan(popM) & popM >= min_cases))) %>%
    ungroup()
}

# plot comps function
genCompData <- function(df, geo_level = NA, min_stat = "deaths",
                        min_thresh = NA, per_million = TRUE) {
  
  stat_col <- {if (per_million) "popM" else "total"}
  if (is.na(min_thresh)) {
    min_thresh <- {if (per_million) 1 else 10}
  } 
 
  df %>%
    rename(location = all_of(geo_level)) %>%
    # get max_total and first_date per location/stat
    group_by(location, stat) %>%
    mutate(max_total = max(!!sym(stat_col), na.rm = TRUE),
           first_date = ifelse(
             stat == min_stat & any(!!sym(stat_col) >= min_thresh),
             min(date[!!sym(stat_col) >= min_thresh]),
             NA_Date_)) %>%
    group_by(location) %>%
    mutate(first_date = first(na.omit(first_date))) %>%
    # drop earlier dates
    filter(any(!is.na(first_date)) & min_stat %in% stat &
             date >= first(first_date)) %>%
    # recenter dates
    mutate(days_since = as.numeric(date - first_date)) %>%
    ungroup() %>%
    # calculate double_days
    group_by(location, stat) %>% arrange(date) %>%
    mutate(
      half_date = date[sapply(1:length(!!sym(stat_col)), FUN = function(i) {
        if(any((!!sym(stat_col))[1:i] <= (!!sym(stat_col))[i]/2)) {
          max(which((!!sym(stat_col))[1:i] <=
                      (!!sym(stat_col))[i]/2), na.rm = TRUE)
        } else {
          NA_Date_
        }})],
      double_days = as.numeric(date - half_date)) %>%
    ungroup() %>%
    gather(value_type, value, !!sym(stat_col), double_days)
}

compLabeller <- function(labels) {
  if (nrow(labels) > 0 & ncol(labels) > 0) {
    labels <- labels %>% mutate_if(is.factor, as.character) %>%
      mutate(value_type = if_else(
        str_ends(stat, " rate"), "", value_type))
  }
  return(labels)
}

plotComps <- function(df, min_stat = "deaths", min_thresh = 10,
                      max_days_since = 20, min_days_since = 3,
                      smooth_plots = TRUE, scale_to_fit = TRUE,
                      per_million = TRUE, span = 0.4, double_days = FALSE,
                      show_daily = FALSE) {
  total_or_daily = if (show_daily) "daily" else "total"
  df %>%
    { if (!double_days) filter(., value_type != "double_days") else . } %>%
    # lazy filter for erroneous data
    filter(value >= 0) %>%
    # truncate days_since
    filter(days_since <= max_days_since) %>%
    # filter not enough points
    group_by(location, stat, value_type) %>%
    filter( n() >= min_days_since) %>%
    { if (show_daily) mutate(., value = if_else(
      value_type != "double_days" & !str_ends(value_type, "_rate"),
      value - lag(value), value)) else . } %>%
    ungroup() %>%
    # order plots and readable labels
    mutate(
      value_type = factor(value_type,
                          levels = c("total", "popM", "double_days"),
                          labels = c(paste(total_or_daily, "count"),
                                     paste(total_or_daily,
                                           "count per million people"),
                                     "Days to double total count")),
      stat = factor(stat, levels = c("deaths", "confirmed",
                                     "active", "recovered",
                                     "total_tests",
                                     "negative_tests", "pending_tests",
                                     "hospitalized", "icu",
                                     "case_fatality_rate",
                                     "case_recovery_rate",
                                     "positive_test_rate",
                                     "hospitalization_rate",
                                     "hospitalization_death_rate"),
                    labels = c("Deaths", "Confirmed cases",
                               "Active cases", "Recovered cases",
                               "Tests",
                               "Negative tests", "Pending tests",
                               "Hospitalized", "Intensive Care",
                               "Case fatality rate", "Case recovery rate",
                               "Positive test rate", "Hospitalization rate",
                               "Death/Hosp. rate"))) %>%
    # plot begins
    ggplot(aes(days_since, value, color = location, label = date)) +
    scale_x_continuous() + 
    geom_point(alpha = 0.4, size = 0.8) + 
    # no smoothing
    {if (!smooth_plots) geom_line(alpha = 0.8)} +
    # smoothing
    {if (smooth_plots) geom_line(stat = "smooth", method = "loess", span = span,
                                 alpha = 0.8, formula = y ~ x)} +
    # .multi_line false doesn't work with ggplotly
    facet_wrap(vars(stat, value_type), ncol = { if (double_days) 2 else 1 },
               scales = {if (scale_to_fit) "free_y" else "fixed"},
               #labeller = labeller(.multi_line = TRUE)) +
               labeller = compLabeller) +
    # labelling
    xlab(paste0("Days since ", min_stat,
                {if (per_million) " per million people " else ""}, " >= ",
                min_thresh)) +
    # thematic things
    theme_minimal() +
    theme(legend.title = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    ylim(0, NA)
}

cleanPlotly <- function(p, smooth_plots = TRUE) {
  gp <- ggplotly(p)
  # compare on mouse over
  gp$x$layout$hovermode <- "x unified"
  # auto scale y-axes (modify in-place)
  sapply(names(gp$x$layout), FUN = function(x) {
    if (startsWith(x, "yaxis")) { gp$x$layout[[x]]$autorange <<- TRUE }
  }) 
  # edit data properties (edit and copy)
  gp$x$data <- lapply(gp$x$data, FUN = function(x) {
    # only remove line hover over text if we're smoothing plots
    if (x$mode == "lines" && smooth_plots) {
      x$hoverinfo <- "none"
      x$text <- NA
    }
    
    return(x)
  })
  return(gp)
}

genPlotComps <- function(
  df, min_stat = "deaths", geo_level = "location", min_thresh = 1,
  max_days_since = 45, min_days_since = 3, smooth_plots = TRUE,
  scale_to_fit = TRUE, per_million = TRUE, refresh_interval = hours(6),
  double_days = TRUE, show_daily = FALSE) {
  
  # refresh data after refresh_interval 
  data_age <- as.period(now() - last_update)
  if (data_age > refresh_interval) {
    warning(paste("Refreshing data after", data_age))
    refreshData()
  }
  
  df %>% genCompData(geo_level = geo_level, min_thresh = min_thresh,
                     per_million = per_million, min_stat = min_stat) %>%
    # filter plots
    filter(!stat %in% c("negative", "pending")) %>%
    filter(!(str_ends(stat, "_rate") & value_type == "double_days")) %>%
    # plot!
    plotComps(
      min_thresh = min_thresh, max_days_since = max_days_since,
      smooth_plots = smooth_plots, min_stat = min_stat,
      scale_to_fit = scale_to_fit, per_million = per_million,
      min_days_since = min_days_since, double_days = double_days,
      show_daily = show_daily) %>%
    cleanPlotly(smooth_plots = smooth_plots)
}

refreshData <- function() {
  # global scope assignment
  goog <<- fetchPrepGoogData()
  last_update <<- now()
}