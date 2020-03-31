library(tidyverse)
library(lubridate)
library(covid19us)
library(wbstats)
library(tidycensus)
library(jsonlite)

census_api_key("8900c6e43b36c7974e390b41e93fc60a974afd8f")
exclude_countries <- c("San Marino", "Guyana", "China", "Andorra", "Cabo Verde")
default_locations <- c("US", "Spain", "Korea, South", "Italy", "China", "Iran",
                       "CA", "LA", "NY", "NJ",
                       "King County, Washington",
                       "Los Angeles, California",
                       "Orleans, Louisiana", 
                       "Santa Clara, California",
                       "Wayne, Michigan",
                       "New York City, New York")

valueOrNA <- function(x) {
  ifelse(!is.null(x), x, NA)
}

getTsMax <- function(cds_loc, metric) {
  suppressWarnings(max(as.numeric(sapply(
    Filter(function(y) {!is.null(y[[metric]])}, cds_loc$dates), "[[",
    metric))))
}

join_wb_country <- function(df, join_data,
                            by=c("Country/Region" = "country")) {
  df %>% left_join(
    join_data %>%
      mutate(country = case_when(
        grepl("Syria", country) ~ "Syria",
        grepl("Bahamas", country) ~ "Bahamas, The",
        grepl("Cape Verde", country) ~ "Cabo Verde",
        grepl("Gambia", country) ~ "Gambia, The",
        grepl("Czech", country) ~ "Czechia",
        grepl("Iran", country) ~ "Iran",
        grepl("Ivoire", country) ~ "Cote d'Ivoire",
        grepl("Brunei", country) ~ "Brunei",
        grepl("Republic of Korea", country) ~ "Korea, South",
        grepl("Korea, Rep.", country) ~ "Korea, South",
        grepl("United Kingdom of", country) ~ "United Kingdom",
        grepl("United States", country) ~ "US",
        grepl("Viet", country) ~ "Vietnam",
        grepl("Russia", country) ~ "Russia",
        grepl("Bolivia", country) ~ "Bolivia",
        grepl("Venezuela", country) ~ "Venezuela",
        grepl("Tanzania", country) ~ "Tanzania",
        grepl("Macedonia", country) ~ "North Macedonia",
        grepl("Moldova", country) ~ "Moldova",
        grepl("Egypt", country) ~ "Egypt",
        grepl("Kyrgyz", country) ~ "Kyrgyzstan",
        grepl("Slovak", country) ~ "Slovakia",
        grepl("Vincent", country) ~ "Saint Vincent and the Grenadines",
        grepl("Lucia", country) ~ "Saint Lucia",
        grepl("Martin", country) ~ "Martinique",
        # careful with Congo
        country == "Democratic Republic of the Congo" |
          country == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
        country == "Congo" | country == "Congo, Rep." ~ "Congo (Brazzaville)",
        TRUE ~ country)),
    by = by)
}

add_country_pop <- function(df, min_popM = 1) {
  df %>% join_wb_country(
    wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) %>%
      rename(popM = value) %>%
      mutate(popM = case_when(
        `country` == "Taiwan*" ~ 23780452,
        `country` == "Cruise Ship" ~ 3711,
        TRUE ~ popM)) %>%
      select(country, popM) %>%
      mutate(popM = popM / 1e6)) %>%
    filter(popM >= min_popM) %>%
    mutate(popM = value / popM)
}

# 1. fetch
# 2. normalize
# 3. compute metrics

fetchPrepJhuData <- function() {
  # read data
  jhu_csse_uri <- paste0(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
    "csse_covid_19_data/csse_covid_19_time_series/")
  
  # fetch
  read_csv(url(paste0(jhu_csse_uri,
                      "time_series_covid19_confirmed_global.csv"))) %>%
    mutate(stat = "confirmed")  %>%
    bind_rows(read_csv(
      url(paste0(jhu_csse_uri, "time_series_covid19_deaths_global.csv"))) %>%
        mutate(stat = "deaths")) %>%
    # bind_rows(read_csv(
    #   url(paste0(jhu_csse_uri, "time_series_19-covid-Recovered.csv"))) %>%
    #     mutate(stat = "recovered")) %>%
    # drop unused vars
    select(-Lat, -Long, -`Province/State`) %>%
    # aggregate by country
    group_by(`Country/Region`, stat) %>%
    summarise_all(sum, na.rm = TRUE) %>% ungroup() %>%
    # gather date values into date/value columns by country/stat
    gather(date, value, -`Country/Region`, -stat) %>%
    # convert date
    mutate(date = mdy(date)) %>%
    add_country_pop() %>%
    pivot_wider(names_from = stat, values_from = c(value, popM)) %>%
    mutate(value_cfr = value_deaths / value_confirmed,
           popM_cfr = popM_deaths / popM_confirmed) %>%
    pivot_longer(c(-`Country/Region`, -date), names_to = c(".value", "stat"),
                 names_sep = "_") %>%
    rename(total = value,
           country = `Country/Region`)
}
 
fetchPrepCovTrackData <- function() {
  # fetch
  covid19us::get_states_daily() %>%
    # drop unused vars
    select(-date_checked, -request_datetime) %>%
    # remove new _increase vars
    select_at(
      vars(-ends_with("_increase"), -total_test_results, -fips, -hash)) %>%
    gather(stat, total, -date, -state) %>%
    mutate(total = as.numeric(total)) %>%
    left_join(
      get_estimates("state", variables = "POP") %>%
        left_join(tibble(state = state.abb, NAME = state.name),
                  by = "NAME"), by = "state") %>%
    rename(popM = value) %>%
    mutate(popM = (1e6 * total) / popM) %>%
    select(-GEOID, -variable, -NAME) %>%
    pivot_wider(names_from = stat, values_from = c(total, popM)) %>%
    # compute metrics
    mutate(
      total_cfr = total_death / total_positive,
      total_ptr = total_positive / (total_positive + total_negative),
      # total_hr = total_hospitalized / total_positive,
      # total_dhr = total_death / total_hospitalized,
      popM_cfr = popM_death / popM_positive,
      popM_ptr = popM_positive / (popM_positive + popM_negative) #,
      # popM_hr = popM_hospitalized / popM_positive,
      # popM_dhr = popM_death / popM_hospitalized
      ) %>%
    pivot_longer(c(-state, -date), names_to = c(".value", "stat"),
                 names_sep = "_") %>%
    mutate(stat = case_when(
      stat == "positive" ~ "confirmed",
      stat == "death" ~ "deaths",
      TRUE ~ stat
    ))
}

fetchPrepCovDataScrape <- function() {
  cds_data <- jsonlite::fromJSON(
    "https://coronadatascraper.com/timeseries-byLocation.json")
 
  names(cds_data) %>%
    lapply(FUN = function(x) {
      list(
        location = x,
        country = valueOrNA(cds_data[[x]]$country),
        aggregate = valueOrNA(cds_data[[x]]$aggregate),
        state = valueOrNA(cds_data[[x]]$state),
        population = valueOrNA(cds_data[[x]]$population),
        county = valueOrNA(cds_data[[x]]$county),
        city = valueOrNA(cds_data[[x]]$city),
        max_deaths = getTsMax(cds_data[[x]], "deaths"),
        max_cases = getTsMax(cds_data[[x]], "cases"),
        max_tested = getTsMax(cds_data[[x]], "tested"),
        max_active = getTsMax(cds_data[[x]], "active"),
        max_recovered = getTsMax(cds_data[[x]], "recovered"),
        # deaths_ts = lapply(names(cds_data[[x]]$dates), FUN = function(x) {
        #   c(x = valueOrNA(cds_data[[x]]$dates[[x]]$deaths))
        # }),
        date = list(names(cds_data[[x]]$dates)),
        ts_values = list(cds_data[[x]]$dates))
      }) %>%
    bind_rows() %>% 
    mutate(
      max_deaths = na_if(max_deaths, -Inf),
      max_cases = na_if(max_cases, -Inf),
      max_tested = na_if(max_tested, -Inf),
      max_active = na_if(max_active, -Inf),
      max_recovered = na_if(max_recovered, -Inf)) %>%
    mutate(
      max_deaths_per_capita = max_deaths / population * 1e6,
      max_tested_per_capita = max_tested / population * 1e6,
      max_recovered_per_capita = max_recovered / population * 1e6,
      max_positive_test_rate = max_cases / max_tested) %>%
    unnest(c(date, ts_values)) %>%
    mutate(
      date = ymd(date),
      deaths = unlist(sapply(ts_values, FUN = function(x) {
        return(valueOrNA(x$deaths)) })),
      # active = unlist(sapply(ts_values, FUN = function(x) {
      #   return(valueOrNA(x$active)) })),
      confirmed = unlist(sapply(ts_values, FUN = function(x) {
        return(valueOrNA(x$cases)) })),
      # recovered = unlist(sapply(ts_values, FUN = function(x) {
      #   return(valueOrNA(x$recovered)) })),
      # total = unlist(sapply(ts_values, FUN = function(x) {
      #   return(valueOrNA(x$tested)) })),
      # growthFactor = unlist(sapply(ts_values, FUN = function(x) {
      #   return(valueOrNA(x$growthFactor)) })),
      cfr = deaths / confirmed #,
      #ptr = confirmed / total
      #crr = recovered / confirmed
      ) %>%
    select(-ts_values) %>%
    gather(stat, value, deaths, confirmed, cfr) %>%
    mutate(popM = value / population * 1e6)
}

fetchPrepNyt <- function(min_deaths = 8) {
  read_csv(url(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")) %>%
    rename(confirmed = cases) %>%
    mutate(cfr = deaths / confirmed) %>%
    # mutate(state = unlist(sapply(state, FUN = function(x) {
    #   unlist(valueOrNA(state.abb[which(state.name == x)])) }))) %>%
    gather(stat, total, confirmed, deaths, cfr) %>%
    unite(county, county, state, sep = ", ") %>%
    #mutate(county = paste0(county, ", USA")) %>%
    left_join(
      get_estimates("county", variables = "POP") %>%
        rename(population = value) %>%
        mutate(fips = str_sub(GEOID, 1, 5)) %>%
        select(fips, population),
              by = "fips") %>%
    mutate(population = case_when(
      county == "New York City, New York" ~ 8623000,
      county == "Kansas City, Missouri" ~ 488943,
      TRUE ~ population)) %>%  
    mutate(popM = total / population * 1e6) %>%
    group_by(county) %>%
    mutate(max_deaths = max(total[stat == "deaths"], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(max_deaths >= min_deaths)
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
           first_date = suppressWarnings(
             min(date[!!sym(stat_col) >= min_thresh], na.rm = TRUE))) %>%
    group_by(location) %>%
    # drop earlier dates
    filter(!is.na(first_date) & (date >= first_date[stat == min_stat])) %>%
    # recenter dates
    mutate(days_since = date - first_date[stat == min_stat]) %>%
    ungroup() %>%
    # calculate double_days
    group_by(location, stat) %>% arrange(date) %>%
    mutate(
      half_date = date[sapply(1:length(!!sym(stat_col)), FUN = function(i) {
        suppressWarnings(
          max(which((!!sym(stat_col))[1:i] <=
                      (!!sym(stat_col))[i]/2), na.rm = TRUE))})],
      double_days = date - half_date) %>%
    ungroup() %>%
    gather(value_type, value, !!sym(stat_col), double_days)
}

plotComps <- function(df, min_stat = "deaths", min_thresh = 10,
                      max_days_since = 20, min_days_since = 3,
                      smooth_plots = TRUE, scale_to_fit = TRUE,
                      per_million = TRUE) {
  df %>%
    # lazy filter for erroneous data
    filter(value >= 0) %>%
    # truncate days_since
    filter(days_since <= max_days_since) %>%
    # filter not enough points
    group_by(location, stat, value_type) %>%
    filter( n() >= min_days_since) %>%
    ungroup() %>%
    # order plots and readable labels
    mutate(
      value_type = factor(value_type,
                          levels = c("total", "popM", "double_days"),
                          labels = c("Total count",
                                     "Total count per million people",
                                     "Days to double total count")),
      stat = factor(stat, levels = c("deaths", "confirmed",
                                     "active", "recovered",
                                     "positive", "total",
                                     "negative", "pending",
                                     "hospitalized",
                                     "cfr", "crr",
                                     "ptr", "hr",
                                     "dhr"),
                    labels = c("Deaths", "Confirmed cases",
                               "Active cases", "Recovered cases",
                               "Positive tests", "Tests",
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
    {if (smooth_plots) geom_line(stat = "smooth", method = "loess", span = 1,
                                 alpha = 0.8)} +
    {if (smooth_plots) geom_point(alpha = 0.2)} +
    # .multi_line false doesn't work with ggplotly
    facet_wrap(vars(stat, value_type), ncol = 2,
               scales = {if (scale_to_fit) "free_y" else "fixed"},
               labeller = labeller(.multi_line = TRUE)) +
    # labelling
    ggtitle(paste0("Metrics since ", min_stat,
                   {if (per_million) " per million people " else ""}, " >= ",
                   min_thresh)) +
    xlab(paste0("Days since ", min_stat,
                {if (per_million) " per million people " else ""}, " >= ",
                min_thresh)) +
    # thematic things
    theme_minimal() +
    theme(legend.title = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) + ylim(0, NA)
}

cleanPlotly <- function(p, smooth_plots = TRUE) {
  gp <- suppressWarnings(ggplotly(p))
  # compare on mouse over
  gp$x$layout$hovermode <- "compare"
  # auto scale y-axes (modify in-place)
  sapply(names(gp$x$layout), FUN = function(x) {
    if (startsWith(x, "yaxis")) { gp$x$layout[[x]]$autorange <<- TRUE }
  }) 
  # edit data properties (edit and copy)
  gp$x$data <- lapply(gp$x$data, FUN = function(x) {
    # show default countries by default
    x$visible <- ifelse(x$name %in% default_locations, TRUE, "legendonly")
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
  df, min_stat = "deaths", geo_level = "country", min_thresh = 1,
  max_days_since = 30, min_days_since = 3, smooth_plots = TRUE,
  scale_to_fit = TRUE, per_million = TRUE) {
  df %>% genCompData(geo_level = geo_level, min_thresh = min_thresh,
                     per_million = per_million, min_stat = min_stat) %>%
    # filter plots
    filter(!stat %in% c("negative", "pending")) %>%
    filter(!(endsWith(stat, "r") & value_type == "double_days")) %>%
    # plot!
    plotComps(
      min_thresh = min_thresh, max_days_since = max_days_since,
      smooth_plots = smooth_plots, min_stat = min_stat,
      scale_to_fit = scale_to_fit, per_million = per_million,
                     min_days_since = min_days_since) %>%
    cleanPlotly(smooth_plots = smooth_plots)
}