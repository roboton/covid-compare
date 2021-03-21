library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)

# Helpers
nday_rolling_mean <- function(value, n = 7) {
  #assertthat::are_equal(length(date), length(value))
  map_dbl(1:length(value), function(i) {
    mean(value[max(1, i - (n - 1)):i], na.rm = TRUE)
  }) 
}

empty_plot <- function(title = NULL){
  p <- plotly_empty(type = "scatter", mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = title, yref = "paper", y = 0.5))
  return(p)
}

is_mobile <- function(user_agent, mobile_os = c("Android", "iOS")) {
  ua_tbl <- uaparserjs::ua_parse(user_agent)
  if ("os.family" %in% names(ua_tbl)) {
    #warning(ua_tbl$os.family)
    return(ua_tbl$os.family %in% mobile_os)
  }
  return(FALSE)
}

# Generate source data
fetchPrepGoogData <- function(period = "week", per_million = TRUE,
                              readable_loc = TRUE) {
  vroom::vroom(
    "https://storage.googleapis.com/covid19-open-data/v2/main.csv",
    col_select = c(date, key,
                   total_tested,
                   total_confirmed,
                   total_hospitalized,
                   total_intensive_care,
                   total_deceased,
                   new_tested,
                   new_confirmed,
                   new_hospitalized,
                   new_intensive_care,
                   new_deceased,
                   population,
                   locality_name,
                   subregion2_name,
                   subregion1_name,
                   country_name),

    col_types = c(date = "D", key = "c",
                  total_tested = "n",
                  total_confirmed = "n",
                  total_hospitalized = "n",
                  total_intensive_care = "n",
                  total_deceased = "n",
                  new_tested = "n",
                  new_confirmed = "n",
                  new_hospitalized = "n",
                  new_intensive_care = "n",
                  new_deceased = "n",
                  population = "n",
                  locality_name = "c",
                  subregion2_name = "c",
                  subregion1_name = "c",
                  country_name = "c"),
    num_threads = 16) %>%
    # create name
    { if (readable_loc) {
      unite(., name, ends_with("_name"), sep = ", ", na.rm = TRUE) %>%
        # append most granular identifier 
        mutate(key = str_remove(key, "^.*_")) %>%
        unite(location, name, key, sep = " ")
      } else {
        rename(., location = key) %>%
          select(-ends_with("_name"))
      } } %>%
    mutate(location = as.factor(location)) %>%
    # compute period stats
    mutate(date = floor_date(date, period)) %>%
    lazy_dt(key_by = c(location, date)) %>%
    group_by(location, date) %>%
    arrange(location, date) %>%
    summarise(
      across(starts_with("total_"), ~ last(na.omit(.x))),
      across(starts_with("new_"),
             ~ ifelse(any(!is.na(.x)), sum(.x, na.rm = TRUE), NA_real_)),
      across(population,
             ~ ifelse(any(!is.na(.x)), mean(.x, na.rm = TRUE), NA_real_)),
      num_dates = n()) %>%
    ungroup() %>%
    filter(is.finite(population)) %>%
    # ensure no partial periods
    { if (period == "week") filter(., num_dates == 7) else . } %>%
    { if (period == "day") filter(., num_dates == 1) else . } %>%
    { if (period == "month") filter(
      ., num_dates == days_in_month(month(date))) else . } %>%
    select(-num_dates) %>%
    # counts per million
    { if (per_million) mutate(., across(c(starts_with("total_"),
                                          starts_with("new_")),
                                        ~ .x / population * 1e6)) else . } %>%
    collect() %>%
    pivot_longer(c(-date, -location, -population),
                 names_to = "stat", values_to = "value") %>%
    mutate(stat = as.factor(stat)) %>%
    filter(is.finite(value) & !is.na(location)) %>%
    ungroup()
}

# plot comps function
genCompData <- function(df, min_stat = "total_deceased",  min_thresh = 1,
                        per_million = TRUE, keep_pop = FALSE) {
  stats_available <- df %>% pull(stat) %>% unique() %>%
    str_split(pattern = "_", simplify = TRUE, n = 2) %>% .[,2] %>% unique()
  df %>%
    group_by(location) %>%
    mutate(first_date =
             first(na.omit(date[stat == min_stat & value > min_thresh]))) %>%
    ungroup() %>%
    filter(date >= first_date) %>%
    mutate(days_since = as.numeric(date - first_date)) %>%
    { if(!per_million) mutate(., value = value * population / 1e6) else . } %>%
    { if(!keep_pop) select(., -population, -first_date) else 
      select(., -first_date) } %>%
   collect() %>%
    separate(stat, c("value_type", "stat"), sep = "_", extra = "merge") %>%
    # rate calcs
    pivot_wider(names_from = stat, values_from = value) %>%
    { if("deceased" %in% stats_available && "confirmed" %in% stats_available)
      mutate(., case_fatality_rate = deceased / confirmed) else . } %>%
    { if("confirmed" %in% stats_available && "tested" %in% stats_available)
      mutate(., positive_test_rate = confirmed / tested) else . } %>%
    { if("hospitalized" %in% stats_available && "confirmed" %in% stats_available)
      mutate(., case_hosp_rate = hospitalized / confirmed) else . } %>%
    { if("intensive_care" %in% stats_available && "hospitalized" %in% stats_available)
      mutate(., hosp_icu_rate = intensive_care / hospitalized) else . } %>%
    { if("deceased" %in% stats_available && "hospitalized" %in% stats_available)
      mutate(., hosp_fatality_rate = deceased / hospitalized) else . } %>%
    { if("deceased" %in% stats_available && "intensive_care" %in% stats_available)
      mutate(., icu_fatality_rate = deceased / intensive_care) else . } %>%
    { if(!keep_pop) {
      pivot_longer(., c(-location, -date, -value_type, -days_since),
                   names_to = "stat", values_to = "value")
    } else {
      pivot_longer(., c(-location, -population, -date, -value_type, -days_since),
                   names_to = "stat", values_to = "value")
    } }
}

plotComps <- function(df, min_stat = "total_deceased", min_thresh = 1,
                      max_days_since = 365,
                      smooth_plots = FALSE,
                      span = 0.5,
                      per_million = TRUE,
                      show_legend = TRUE, ncol = 2) {
  if (nrow(df) == 0) {
    return(empty_plot("no data"))
  }
  per_million_label <- { if (per_million) "per million people" else "" }
  df %>%
    # truncate days_since
    filter(days_since <= max_days_since) %>%
    # order plots and readable labels
    mutate(
      value_type = factor(value_type,
                          levels = c("total", "new"),
                          labels = c(paste("total count", per_million_label),
                                     paste("new count", per_million_label))),
      stat = factor(stat,
                    levels = c("deceased", "confirmed", "tested",
                               "case_fatality_rate", "positive_test_rate",
                               "hospitalized", "intensive_care",
                               "case_hosp_rate", "hosp_icu_rate",
                               "hosp_fatality_rate", "icu_fatality_rate"),
                    labels = c("Deaths", "Confirmed cases", "Tests",
                               "Case fatality rate", "Positive test rate",
                               "Hospitalized", "Intensive Care",
                               "Hospitalization rate", "ICU rate",
                               "Hosp. fatality rate", "ICU fatality rate"))) %>%
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
    facet_wrap(vars(stat, value_type), ncol = ncol, scales = "free",
               labeller = labeller(.multi_line = TRUE)) +
    # labelling
    xlab(paste0("Days since ", min_stat,
                {if (per_million) " per million people " else ""}, " >= ",
                min_thresh)) +
    # thematic things
    theme_minimal() +
    theme(legend.title = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    {if (!show_legend) theme(legend.position = "none") }
}

cleanPlotly <- function(p) {
  gp <- ggplotly(p) %>%
    layout(hovermode = "x unified", font = list(size = 12))
  # # compare on mouse over
  # gp$x$layout$hovermode <- "x unified"
  # gp$x$layout$font <- list(size = 12)
  # auto scale y-axes (modify in-place)
  # sapply(names(gp$x$layout), FUN = function(x) {
  #   if (startsWith(x, "yaxis")) { gp$x$layout[[x]]$autorange <<- TRUE }
  # })
  # # edit data properties (edit and copy)
  # gp$x$data <- lapply(gp$x$data, FUN = function(x) {
  #   # only remove line hover over text if we're smoothing plots
  #   if (x$mode == "lines" && smooth_plots) {
  #     x$hoverinfo <- "none"
  #     x$text <- NA
  #   }
  # 
  #   return(x)
  # })
  return(gp)
}

genPlotComps <- function(
  df, min_stat = "total_deceased", min_thresh = 1,
  max_days_since = 365, smooth_plots = FALSE,
  per_million = TRUE,
  show_legend = TRUE,
  plot_type = "epi",
  ncol = 2) {
  
  epi_stats <- c("deceased", "confirmed", "tested", "case_fatality_rate",
                 "positive_test_rate")
  hosp_stats <- c("hospitalized", "intensive_care", "case_hosp_rate",
                  "hosp_icu_rate", "hosp_fatality_rate", "icu_fatality_rate")
  
  comp_data <- df %>% genCompData(
    min_thresh = min_thresh, per_million = per_million,
                     min_stat = min_stat) %>%
    { if (plot_type == "epi") filter(., stat %in% epi_stats) else . } %>%
    { if (plot_type == "hosp") filter(., stat %in% hosp_stats) else . }
  if (nrow(comp_data) == 0) {
    return(empty_plot(paste("no", plot_type, "data")))
  }
  comp_data %>% plotComps(
    min_thresh = min_thresh,
    max_days_since = max_days_since,
    smooth_plots = smooth_plots,
    min_stat = min_stat,
    per_million = per_million,
    show_legend = show_legend,
    ncol = ncol) %>%
    cleanPlotly()
}
