source("covidcomp_lib.R", local = TRUE)

## analysis parameters

refresh_tscomp <- TRUE
refresh_data <- TRUE
# how many months to match on before evaluating
num_match_months <- 6
num_eval_months <- 8
# significance for CausalImpact
sig_p <- 0.05
# aggregate to this level of data
period <- "week"
# minimum population size to be considered
pop_thresh <- 1e5
# align timeseries starting from this many deaths per million
death_thresh <- 1
# how many locations needed to run CausalImpact
min_locs <- 10
eval_stat <- "deceased"

## code below

# time "zero" without actual reference to a specific date (b/c of re-alignment)
base_date <- ymd("2020-01-01")
eval_date <- base_date + months(num_match_months)
end_date <- eval_date + months(num_eval_months)

## load covid data

fst_file <- paste0("data/comp_data_", death_thresh, floor_date(today(), "week"),
                   ".fst")
if (file.exists(fst_file) && !refresh_data) {
  comp_data <- fst::read_fst(fst_file) 
} else {
  comp_data <- fetchPrepGoogData(period = period, readable_loc = FALSE,
                                 add_labels = TRUE, use_cache = FALSE) %>%
    genCompData(min_thresh = death_thresh, per_million = TRUE,
                min_stat = "total_deceased", keep_pop = TRUE) %>%
    fst::write_fst(fst_file)
}

## data prep for analysis

comp_sets <- comp_data %>%
  mutate(orig_date = date,
         date = base_date + days(days_since)) %>%
  # pop filter
  filter(population >= {{pop_thresh}} & stat == {{eval_stat}} &
           value_type == "new") %>%
  group_by(location) %>%
  filter(max(date) >= {{end_date}}) %>%
  # min_loc filter
  group_by(country = str_sub(location, 1, 2),
           level = str_count(location, "_")) %>%
  filter(n_distinct(location) >= min_locs | level == 0) %>%
  ungroup() %>%
  mutate(country = if_else(level == 0, "GLOBAL", country)) %>%
  unite(set, country, level) %>%
  select(set, ts_id = location, date, orig_date, count = value) %>%
  filter(date <= end_date)

## run tscompare

if (refresh_tscomp) {
  devtools::install_github("roboton/tscompare")
  library(furrr)
  plan(multisession, workers = round(availableCores() - 2))
  
  out_dirs <- future_map_chr(
    group_split(comp_sets, set),
    ~ tscompare::ts_analysis(.x, group_id = first(.x$set),
                             start_date = {{eval_date}},
                             period = period, min_pre_periods = 0,
                             min_post_periods = 0,
                             min_timeseries = {{min_locs}},
                             sig_p = {{sig_p}},
                             gen_output = FALSE),
    .options = furrr_options(seed = TRUE))
}

## create plots

set_names <- unique(comp_sets$set)
tscomp_summary <- map_dfr(set_names, function(set_name) {
  
  print(set_name)
  tsc_dir <- fs::path(set_name, "tscomp")
  fs::dir_create(tsc_dir, recurse = TRUE)
  
  mdl_data <- readr::read_rds(fs::path(set_name, "group_timeseries_model.rds"))
  geo_names <- names(mdl_data)

  map_dfr(geo_names, function(geo_name) {
    print(geo_name)
    cur_mdl <- mdl_data[[geo_name]]
    geo_name_short <- str_remove(geo_name, "ts_")
    tsc_plot <- genTsCompPlot(set_name, geo_name_short, cur_mdl,
                              output_plotly = FALSE, show_peers = TRUE)
    tsc_file <- fs::path(tsc_dir, geo_name_short, ext = "rds")
    readr::write_rds(tsc_plot, tsc_file)
    # return(tsc_file)
    geo_mdl <- mdl_data[[geo_name]]
    if (is.null(geo_mdl$summary)) {
      return(NULL)
    }
    geo_mdl$summary %>%
      mutate(set_name = set_name, geo_name = geo_name_short) %>%
      tibble::rownames_to_column("stat_type")
  })
}) %>% pivot_wider(names_from = stat_type,
                   values_from = -c(set_name, geo_name, stat_type)) %>%
  left_join(comp_data %>%
              select(label, geo_name = location) %>% distinct(),
            by = geo_name) %>%
  select(set_name, geo_name, label, everything()) %>%
  mutate(label = str_replace_all(label, "United States of America", "USA"))

readr::write_rds(tscomp_summary, "tscomp_summary.rds")