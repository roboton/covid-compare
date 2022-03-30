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
                              readable_loc = TRUE, add_labels = FALSE,
                              use_cache = FALSE, add_vax = FALSE) {
  main_url <- "https://storage.googleapis.com/covid19-open-data/v2/main.csv"
  vax_url <- "https://storage.googleapis.com/covid19-open-data/v3/vaccinations.csv"
  main_file <- "data/main.csv"
  vax_file <- "data/vaccinations.csv"
  
  if (!use_cache) {
    curl::curl_download(main_url, main_file)
    curl::curl_download(vax_url, vax_file)
  }
  
  vroom::vroom(
    main_file,
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
    { if (add_labels) {
        unite(., label, ends_with("_name"), sep = ", ", na.rm = TRUE) %>%
        mutate(label = str_c(label, str_remove(key, "^.*_"), sep = " "))
      } else { . }
    } %>%
    { if (readable_loc) {
        unite(., name, ends_with("_name"), sep = ", ", na.rm = TRUE) %>%
        # append most granular identifier 
        mutate(key = str_remove(key, "^.*_")) %>%
        unite(location, name, key, sep = " ")
      } else {
        rename(., location = key) %>%
        select(-ends_with("_name"))
      }
    } %>%
    { if (add_vax) {
      left_join(., vroom::vroom(
        vax_file,
        col_select = c(date, location_key, ends_with("administered")),
        col_types = readr::cols(.default = "d", date = "D",
                                location_key = "c")) %>%
          rename(location = location_key,
                 total_vaccine_doses_administered =
                   cumulative_vaccine_doses_administered),
        by = c("location", "date"))
      } else { . }
    } %>%
    mutate(location = as.factor(location)) %>%
    # compute period stats
    mutate(date = floor_date(date, period)) %>%
    lazy_dt(key_by = c(location, date)) %>%
    group_by(across(any_of(c("location", "date", "label")))) %>%
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
    pivot_longer(-any_of(c("date", "location", "label", "population")),
                 names_to = "stat", values_to = "value") %>%
    mutate(stat = as.factor(stat)) %>%
    filter(is.finite(value) & !is.na(location)) %>%
    ungroup()
}

fetchPrepVaxData <- function(use_cache = FALSE) {
  fetchPrepGoogData(use_cache = use_cache, add_vax = TRUE,
                               readable_loc = FALSE, add_labels = TRUE) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    select(location, date, label, population,
           cases = new_confirmed, deaths = new_deceased,
           doses = total_vaccine_doses_administered) %>%
    # only rows with all stats available
    filter(complete.cases(.)) %>%
    # outcomes
    pivot_longer(c(cases, deaths)) %>%
    mutate(geo_level = str_count(location, "_"),
           geo = if_else(geo_level == 0, "GLOBAL",
                         str_sub(location, 1, 2))) %>%
    unite(geo, geo, geo_level) %>%
    group_by(geo) %>%
    mutate(geo_count = n_distinct(location)) %>%
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
      pivot_longer(.,
                   -any_of(c("location", "label", "date", "value_type",
                             "days_since")),
                   names_to = "stat", values_to = "value")
    } else {
      pivot_longer(.,
                   -any_of(c("location", "population", "label", "date",
                             "value_type", "days_since")),
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

# getSubGeoNames <- function(set_name) {
#   set_names <- c("GLOBAL_0", fs::dir_ls(regexp = "^[A-Z]{2}_"))
#   # set choice
#   set_name <- sample(set_names, 1)
#   mdl_data <- read_rds(fs::path(set_name, "group_timeseries_model.rds"))
#   
#   geo_names <- tibble(
#     name = names(mdl_data),
#     cumrel_effect = map_dbl(mdl_data, ~ .x$summary["Cumulative", "RelEffect"])) %>%
#     arrange(desc(abs(cumrel_effect)))
#   
#   # geo choice
#   geo_name <- str_remove(
#     sample(geo_names$name, 1, prob = abs(geo_names$cumrel_effect)),
#     "ts_")
# }

countryCodeToName <- function(geo_name) {
  return(countrycode::countrycode(geo_name, "iso2c", "cldr.name.en",
                                  custom_match = c("XK" = "Kosovo")))
}

genTsCompPlot <- function(set_name, geo_name, cur_mdl,
                          show_peers = FALSE, num_peers = 5, scale_peers = TRUE,
                          output_plotly = TRUE, geo_name_full = NULL) {
  
  # TODO(robon): clean this up (fixed model parameters)
  num_match_months <- 6
  num_eval_months <- 6
  # time "zero" without actual reference to a specific date (b/c of re-alignment)
  base_date <- ymd("2020-01-01")
  eval_date <- base_date + months(num_match_months)

  if(is.null(geo_name_full)) {
    geo_name_full <- geo_name
  }
  
  orig_counts <- as.vector(cur_mdl$series$response)
  orig_dates <- zoo::index(cur_mdl$series$response)
  main_plot <- cur_mdl$series %>% broom::tidy() %>%
    mutate(series_type = case_when(str_ends(series, ".lower") ~ "lower",
                                   str_ends(series, ".upper") ~ "upper",
                                   TRUE ~ "value"),
           series = str_remove(series, "(.lower)|(.upper)"),
           series = if_else(series == "response", "point.response", series)) %>%
    pivot_wider(names_from = series_type, values_from = value) %>%
    separate(series, c("cumulative", "type")) %>%
    mutate(cumulative = if_else(cumulative == "cum", "cumulative", "point"),
           plot_group = if_else(type == "effect", "effect", "response"),
           predicted = if_else(is.na(lower), "actual", "predicted"),
           across(c(lower, upper), ~ if_else(is.na(.x), value, .x)),
           # index = index + diff_days,
           label = str_c(cumulative, plot_group, sep = " ")) %>%
    filter(label == "point response") %>%
    mutate(index = as.numeric(index - min(index), unit = "days")) %>%
    ggplot(aes(index, value)) +
    geom_vline(xintercept = as.numeric(eval_date - base_date), linetype = 2, alpha = 0.5) +
    geom_ribbon(aes(ymin = lower, ymax = upper, linetype = predicted), alpha = 0.2) +
    geom_line((aes(linetype = predicted))) +
    ggtitle(geo_name_full) +
    ylab("deaths count") +
    xlab("days since first outbreak") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  if (show_peers) {
    bsts.object <- cur_mdl$model$bsts.model
    if (is.null(bsts.object)) {
      warning(paste("BSTS failed to compute for", geo_name))
    } else {
      burn <- bsts::SuggestBurn(0.1, bsts.object)
      beta <- as_tibble(bsts.object$coefficients)
      predictors <- as_tibble(bsts.object$predictors)
    
      peer_plot <- beta %>%
        as_tibble() %>%
        slice(-1:-burn) %>%
        pivot_longer(everything()) %>%
        group_by(name) %>%
        summarise(
          inclusion = mean(value != 0),
          pos_prob = if_else(all(value == 0), 0, mean(value[value != 0] > 0)),
          sign = if_else(pos_prob > 0.5, 1, -1)) %>%
        slice_max(inclusion, n = num_peers) %>%
        left_join(predictors %>% mutate(index = orig_dates) %>%
                    pivot_longer(-index), by = "name") %>%
        mutate(name = str_remove(name, "ts_")) %>%
        group_by(name) %>%
        mutate(value = value * sign) %>% ungroup() %>%
        bind_rows(tibble(name = geo_name, inclusion = 1, pos_prob = 1, sign = 1,
                         index = orig_dates, value = as.vector(orig_counts))) %>%
        { if (scale_peers) group_by(., name) %>%
            mutate(., value = as.vector(scale(value))) %>%
            ungroup() else . } %>%
        mutate(index = as.numeric(index - min(index))) %>%
        ggplot(aes(index, value, color = name, alpha = inclusion)) +
        geom_vline(xintercept = as.numeric(eval_date - base_date), linetype = 2,
                   alpha = 0.5) +
        geom_line() +
        theme_minimal() +
        ylab("scaled deaths count") +
        xlab("days since first outbreak") +
        theme(legend.title = element_blank())
        
      main_plot <- subplot(main_plot, peer_plot, shareX = TRUE, nrows = 2)
    }
  }

  if (output_plotly) {
    return(ggplotly(main_plot))
  } else {
    return(main_plot)
  }
}