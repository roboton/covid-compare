library(tidyverse)
library(vroom)
library(lubridate)
library(dtplyr)
library(plotly)
library(jsonlite)
# data sources
library(covid19us)
library(wbstats)
library(tidycensus)
library(cdcfluview)

valueOrNA <- function(x) {
  ifelse(!is.null(x), x, NA)
}

getTsMax <- function(cds_loc, metric) {
  suppressWarnings(max(as.numeric(sapply(
    Filter(function(y) {!is.null(y[[metric]])}, cds_loc$dates), "[[",
    metric))))
}

joinWbCountry <- function(df, join_data,
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

addCountryPop <- function(df) {
  df %>% joinWbCountry(
    wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) %>%
      rename(popM = value) %>%
      mutate(popM = case_when(
        `country` == "Taiwan*" ~ 23780452,
        `country` == "Cruise Ship" ~ 3711,
        TRUE ~ popM)) %>%
      select(country, popM) %>%
      mutate(popM = popM / 1e6)) %>%
    mutate(popM = value / popM)
}

# location list
addForecast <- function(df, ext_days = 7, span = 1, min_days = 10) {
  has_dates <- df$date[!is.na(df$total) & !is.na(df$popM)]
  if (sum(has_dates >= today() - days(min_days)) < min_days) { 
    pred_dat <- df %>% cbind(total_pred = NA, popM_pred = NA)
    return(pred_dat)
  }
  new_dates <- df$date %>% c(seq(max(., na.rm = T) + 1,
                                 max(., na.rm = T) + 1 + days(ext_days),
                                 by = "day"))
  
  ts_dat <- df %>%
    right_join(tibble(date = new_dates), by = "date")
  
  pred_dat <- ts_dat %>%
    mutate(
      total_pred = loess(total ~ as.numeric(date), data = ts_dat, span = span,
                         control = loess.control(surface = "direct")) %>%
        predict(as.numeric(new_dates)),
      popM_pred = loess(popM ~ as.numeric(date), data = ts_dat, span = span,
                         control = loess.control(surface = "direct")) %>%
        predict(as.numeric(new_dates))) %>%
    mutate_at(vars(ends_with("_pred")), function(x) {
      x[x < 0] <- 0
      for (i in 2:length(x)) {
        x[i] <- { if (x[i] < x[i - 1]) x[i - 1] else x[i] }
      }
      return(x)
    })
  return(pred_dat)
}

cleanSd <- function(x) {
  sd(Filter(function(y) !is.infinite(y), x), na.rm = TRUE)
}


getForecastSeverity <- function(all_locs) {
  all_locs %>% filter(stat == "deaths") %>%
    group_by(location) %>% nest() %>%
    rowwise() %>%
    mutate(data = list(addForecast(data))) %>% unnest(cols = c(data)) %>%
    group_by(location) %>%
    summarise(severity_total = suppressWarnings(
      max(total_pred, na.rm = TRUE) - max(total, na.rm = TRUE)),
              severity_popM = suppressWarnings(
                max(popM_pred, na.rm = TRUE) - max(popM, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(severity = severity_total / cleanSd(severity_total) +
           severity_popM / cleanSd(severity_popM)) %>%
    mutate(location = fct_reorder(location, desc(severity)))
}

getSimpleSeverity <- function(all_locs) {
  all_locs %>% filter(stat == "deaths") %>%
    group_by(location) %>%
    arrange(desc(popM), desc(date)) %>%
    top_n(2, wt = popM) %>%
    arrange(desc(date)) %>%
    mutate(day_before = date - days(1) == lead(date),
           total_diff = total - lead(total),
           popM_diff = popM - lead(popM)) %>%
    ungroup() %>%
    filter(day_before | is.na(day_before)) %>%
    mutate(total_diff = pmax(as.vector(scale(total_diff, center = FALSE)), 0),
           popM_diff = pmax(as.vector(scale(popM_diff, center = FALSE)), 0)) %>%
    mutate(severity = replace_na(total_diff + popM_diff, 0)) %>%
    arrange(desc(unlist(severity))) %>%
    select(location, total_diff, popM_diff, severity)
}

# severity can be one of "none", "simple" or "forecast"
getLocationList <- function(all_locs, severity = "none")  {
  if (severity == "simple") {
    return(getSimpleSeverity(all_locs))
  } else if (severity == "forecast") {
    return(getForecastSeverity(all_locs))
  }
  return(all_locs %>% select(location) %>% unique() %>% mutate(severity = 1))
}

fetchPrepGoogData <- function() {
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
    # col_types = c(locality_code = "c", locality_name = "c",
    #               new_intensive_care = "n", total_intensive_care = "n",
    #               current_intensive_care = "n", new_ventilator = "n",
    #               total_ventilator = "n", current_ventilator = "n"),
    num_threads = 16
    ) %>%
    unite(name, ends_with("_name"), sep = ",", na.rm = TRUE) %>%
    mutate(key = str_replace(key, "^.*_", "")) %>%
    unite(location, name, key, sep = " - ") %>%
    mutate(#negative_tests = total_tests - confirmed,
           cfr = deaths / confirmed,
           ptr = confirmed / total_tests) %>%
    pivot_longer(c(-date, -location, -population),
                 names_to = "stat", values_to = "total") %>%
    mutate(popM = if_else(
        !stat %in% c("cfr", "ptr"), total / population * 1e6, total)) %>%
    filter(is.finite(total) & is.finite(popM) & !is.na(location)) %>%
    select(-population) %>%
    group_by(location) %>%
    filter("deaths" %in% stat & "confirmed" %in% stat) %>% ungroup()
}

# countries
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
    addCountryPop() %>%
    pivot_wider(names_from = stat, values_from = c(value, popM)) %>%
    mutate(value_cfr = value_deaths / value_confirmed,
           # don't pop normalize rates (cfr)
           popM_cfr = value_deaths / value_confirmed) %>%
    pivot_longer(c(-`Country/Region`, -date), names_to = c(".value", "stat"),
                 names_sep = "_") %>%
    rename(total = value,
           country = `Country/Region`)
}

# states
fetchPrepCovTrackData <- function(add_flu = FALSE) {
  # fetch
  read_csv("https://covidtracking.com/api/v1/states/daily.csv") %>%
    select(date, state, confirmed = positive, negative_tests = negative,
           pending_tests = pending, hospitalized, deaths = death,
           total_tests = total) %>%
    gather(stat, total, -date, -state) %>%
    mutate(total = as.numeric(total)) %>%
    rename(state_abb = state) %>%
    # convert state abbrevations to state names
    left_join(tibble(state_abb = c(state.abb, "DC", "PR", "AS"),
                     state = c(state.name, "District of Columbia",
                               "Puerto Rico Commonwealth",
                               "American Samoa")),
                  by = "state_abb") %>%
    select(-state_abb) %>%
    # filter out American Samoa, which is missing from census state popluations.
    filter(!is.na(state)) %>%
    left_join(
     read_csv("data/SCPRC-EST2019-18+POP-RES.csv") %>%
        select(popM = POPESTIMATE2019, state = NAME),
      by = "state") %>%
    mutate(popM = (1e6 * total) / popM) %>%
    pivot_wider(names_from = stat, values_from = c(total, popM)) %>%
    # compute metrics
    mutate(
      total_cfr = total_deaths / total_confirmed,
      total_ptr = total_confirmed / (total_confirmed + total_negative_tests),
      # don't pop normalize rates (cfr)
      popM_cfr = total_deaths / total_confirmed,
      popM_ptr = total_confirmed / (total_confirmed +
                                     total_negative_tests) #,
      ) %>%
    pivot_longer(c(-state, -date), names_to = c(".value", "stat"),
                 names_pattern = "^(total|popM)_(.*)$") %>%
    mutate(state = paste0(state, ", USA"),
           date = ymd(date)) %>%
    { if (add_flu) bind_rows(., fetchPrepCdcFlu()) else identity(.) }
}

# flu states
fetchPrepCdcFlu <- function(seasons = 2017:2019) {
  # region: one of "national", "hhs", "census", or "state"
  flu_cases_data <- cdcfluview::who_nrevss(region = "state", years = seasons) %>%
    `$`("clinical_labs")
  # coverage_area: coverage area for data (national, state or region)
  flu_deaths_data <- cdcfluview::pi_mortality(coverage_area = "state",
                                              years = seasons)

  flu_deaths_data %>%
    select(region_name, seasonid, wk_start, number_influenza,
           number_pneumonia) %>%
    rename(state = region_name, date = wk_start) %>%
    group_by(seasonid) %>% 
    mutate(year = year(min(date))) %>%
    ungroup() %>% select(-seasonid) %>%
    mutate(deaths = number_influenza) %>%
    #mutate(deaths = number_influenza + number_pneumonia) %>%
    select(-starts_with("number_")) %>%
    left_join(
      flu_cases_data %>%
        mutate_at(vars(total_specimens:percent_b), as.numeric) %>%
        mutate(confirmed = total_a + total_b) %>%
        select(state = region, date = wk_date, confirmed, total = total_specimens),
      by = c("state", "date")) %>%
    mutate_at(vars(deaths, confirmed, total), coalesce, 0) %>%
    group_by(state, year) %>% arrange(date) %>%
    mutate(deaths = cumsum(deaths), confirmed = cumsum(confirmed),
           total_tests = cumsum(total)) %>%
    ungroup() %>% 
    #mutate(cfr = deaths/confirmed, ptr = confirmed / total) %>%
    gather(stat, total, -state, -date, -year) %>%
    left_join(
      read_csv("data/SCPRC-EST2019-18+POP-RES.csv") %>%
        select(popM = POPESTIMATE2019, state = NAME), by = "state") %>%
    mutate(popM = if_else(state == "New York City", 8623000, popM),
           popM = (total * 1e6) / popM) %>%
    mutate(state = paste0(state, ", USA ", year, " flu")) %>%
    select(-year)
}

fetchPrepCorDataScrape <- function() {
  cds_data <- jsonlite::fromJSON(
    "https://coronadatascraper.com/timeseries-byLocation.json")
 
  names(cds_data) %>%
    lapply(FUN = function(x) {
      list(
        location = x,
        # geo hierarchy
        aggregate = valueOrNA(cds_data[[x]]$aggregate),
        level = valueOrNA(cds_data[[x]]$level),
        country = valueOrNA(cds_data[[x]]$country),
        state = valueOrNA(cds_data[[x]]$state),
        county = valueOrNA(cds_data[[x]]$county),
        city = valueOrNA(cds_data[[x]]$city),
        # values
        population = valueOrNA(cds_data[[x]]$population),
        population_density = valueOrNA(cds_data[[x]]$populationDensity),
        # max vals
        max_deaths = getTsMax(cds_data[[x]], "deaths"),
        max_cases = getTsMax(cds_data[[x]], "cases"),
        max_tested = getTsMax(cds_data[[x]], "tested"),
        max_active = getTsMax(cds_data[[x]], "active"),
        max_recovered = getTsMax(cds_data[[x]], "recovered"),
        # time series data
        date = list(names(cds_data[[x]]$dates)),
        ts_values = list(cds_data[[x]]$dates))
      }) %>%
    bind_rows() %>% 
    # fill in missing populations
    mutate(
      population = if_else(str_starts(location, "New York City"),
                           8623000, as.numeric(population))) %>%
    filter(!is.na(population)) %>%
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
      confirmed = unlist(sapply(ts_values, FUN = function(x) {
        return(valueOrNA(x$cases)) })),
      total_tests = unlist(sapply(ts_values, FUN = function(x) {
        return(valueOrNA(x$tested)) })),
      hospitalized = unlist(sapply(ts_values, FUN = function(x) {
        return(valueOrNA(x$hospitalized_current)) })),
      icu = unlist(sapply(ts_values, FUN = function(x) {
        return(valueOrNA(x$icu_current)) })),
      cfr = deaths / confirmed,
      ptr = confirmed / total_tests,
      ) %>%
    select(-ts_values) %>%
    gather(stat, value, deaths, confirmed, total_tests, hospitalized, icu,
           cfr, ptr) %>%
    # don't pop normalize rates (cfr)
    mutate(popM = if_else(
      !stat %in% c("cfr", "ptr"), value / population * 1e6, value)) %>%
    group_by(location) %>%
    filter(any(!is.na(max_deaths))) %>% ungroup() %>%
    mutate(
      location = if_else(
        str_ends(location, ", USA"),
        str_replace(location, ", USA", ", United States"), location),
      location = if_else(
        level == "county",
        paste(county, state, sep = ", "),
        location)) %>%
    select(location, date, stat, total = value, popM)
}

# counties
fetchPrepNyt <- function(min_deaths = 5) {
  read_csv(url(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")) %>%
    rename(confirmed = cases) %>%
    mutate(cfr = deaths / confirmed) %>%
    gather(stat, total, confirmed, deaths, cfr) %>%
    # add population
    left_join(
      read_csv("data/co-est2019-alldata.csv") %>%
        unite(fips, STATE, COUNTY, sep = "") %>%
        select(fips, population = POPESTIMATE2019),
      by = "fips") %>%
    mutate(population = case_when(
      county == "New York City" ~ 8623000,
      county == "Kansas City" ~ 488943,
      TRUE ~ population)) %>%  
    mutate(popM = total / population * 1e6) %>%
    # fix county names
    mutate(county = case_when(
      county %in% c("New York City", "Kansas City",
                    "District of Columbia") ~ county,
      county == "James City" ~ "James City County",
      str_ends(county, " [Cc]ity") ~ str_remove(county, " [Cc]ity"),
      state == "Louisiana" ~ paste(county, "Parish"),
      TRUE ~ paste(county, "County"))) %>%
    unite(county, county, state, sep = ", ") %>%
    #mutate(county = paste0(county, ", USA")) %>%
    group_by(county) %>%
    mutate(max_deaths = max(total[stat == "deaths"], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(max_deaths >= min_deaths) %>%
    select(-fips, -population, -max_deaths)
}

# add mobility data
fetchJoinMobility <- function(.data) {
  read_csv("data/google_mob_cpt.csv", col_types="cccccDnnDnnnnnn") %>%
    filter(value_type == "mobility_index_change_from_baseline") %>%
    mutate(sub_region_2 = if_else(
      sub_region_1 == "District of Columbia", sub_region_1, sub_region_2)) %>%
    mutate(location = case_when(
      # US Counties (nyt, cds)
      country_region_code == "US" & !is.na(sub_region_2) ~
        paste(sub_region_2, sub_region_1, sep = ", "),
      # US States(ctp, cds)
      !is.na(sub_region_1) & is.na(sub_region_2) ~
        paste(sub_region_1, country_region, sep = ", "),
      # Countries (jhu, cds)
      TRUE ~ country_region)) %>%
    select(location, change_diff, change_start_date, change_end_date) %>%
    right_join(.data, by = "location") %>%
    mutate(change_period = if_else(
      date >= change_end_date, # & date <= change_end_date,
      "[SIPO]", "[Pre-SIPO]")) %>%
    return()
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
    mutate(days_since = date - first_date) %>%
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
                      per_million = TRUE, span = 0.75, double_days = FALSE,
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
    { if (show_daily) mutate(., value = if_else(value_type != "double_days" &
                                                  !str_ends(value_type, "r"),
                                                value - lag(value),
                                                value)) else . } %>%
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
                                     "cfr", "crr",
                                     "ptr", "hr",
                                     "dhr"),
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
    geom_point(alpha = 0.2) + 
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
          plot.title = element_text(hjust = 0.5)) +
    ylim(0, NA)
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
    # x$visible <- ifelse(x$name %in% default_locations, TRUE, "legendonly")
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
    filter(!(endsWith(stat, "r") & value_type == "double_days")) %>%
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
  jhu <<- fetchPrepJhuData()
  covtrack <<- fetchPrepCovTrackData()
  nyt <<- fetchPrepNyt()
  last_update <<- now()
}
