library(shiny)

source("covidcomp_lib.R", local = TRUE)

# set up global params
min_global <- 5
refresh_interval <- hours(24)
data_file <- "data/goog_weekly.fst"
last_update <- file.info(data_file)$mtime
n_locs <- 3
exceptions <- str_c(c("Taiwan", "Singapore"), collapse = "|")

# pull in data
all_locs <- lazy_dt(fst::read_fst(data_file), key_by = "location")

loc_list <- all_locs %>% filter(stat == "new_deceased") %>%
  filter(!is.na(value) | str_detect(location, exceptions)) %>%
  group_by(location) %>%
  arrange(location, date) %>%
  summarise(
    severity_popM = last(value),
    severity_total = last(value) * last(population) / 1e6) %>%
  ungroup() %>%
  filter(is.finite(severity_total) & severity_total > 0 |
           str_detect(location, exceptions)) %>%
  mutate(level = str_count(location, ",")) %>%
  arrange(level) %>% select(-level) %>%
  collect()

init_locs <- sample(loc_list$location, 3)
cur_locs <- init_locs

compare_metrics <- c("deaths" = "total_deceased", "cases" = "total_confirmed",
                     "tests" = "total_tested")

# for tscomp
tscomp_data_file <- "data/sets/tscomp_summary.rds"
tscomp_summary <- readr::read_rds(tscomp_data_file)
tscomp_last_update <- file.info(tscomp_data_file)$mtime
set_names <- unique(tscomp_summary$set_name) 
set_labels <- tibble(set_names) %>%
  separate(set_names, c("name", "level")) %>%
  mutate(name = if_else(name != "GLOBAL", countryCodeToName(name), name),
         label = str_glue("{name} (level {level})")) %>%
  pull(label)
set_names <- setNames(set_names, set_labels)

default_set_name <- "GLOBAL_0"

# for vax data
vax_geo_limit <- 250
vax_data_file <- "data/comp_data_vax.fst"
vax_dat <- fst::read_fst(vax_data_file)
vax_last_update <- file.info(vax_data_file)$mtime
vax_set_names <- vax_dat %>% filter(geo_count <= vax_geo_limit) %>%
  pull(geo) %>% unique()
vax_set_labels <- tibble(vax_set_names) %>%
  separate(vax_set_names, c("name", "level")) %>%
  mutate(name = if_else(name != "GLOBAL", countryCodeToName(name), name),
         label = str_glue("{name} (level {level})")) %>%
  pull(label)
vax_set_names <- setNames(vax_set_names, vax_set_labels)
vax_default_set_name <- "US_1" #sample(vax_set_names, 1)

# control over mouse over values in plotly plot
options(scipen = 999, digits = 1)

ui <- function(request) {
  mobile_req <- is_mobile(request$HTTP_USER_AGENT)
 
  dashboard_panel <- fluidPage(
    #shinyjs::useShinyjs(), # for moving showcase code to the bottom
    shinybusy::add_busy_bar(color = "CornflowerBlue", timeout = 800),
    titlePanel("COVID-19 dashboard"),
    tags$a(
      href = "https://github.com/roboton/covid-compare",
      target = "_blank", "[git]"),
    tags$a(
      href = "mailto:roberton@gmail.com",
      target = "_blank", "[contact]"),
    tags$div(paste(" Last updated",
                    round(as.numeric(now() - last_update, units = "days"), 2),
                    "days ago")),
    sidebarLayout(
      sidebarPanel(
        # data options
        selectizeInput(
          "location", "Location", choices = NULL, multiple = TRUE,
          options = list(placeholder = 'type to select a location')),
          # "location", "Location", choices = loc_list$location, multiple = TRUE,
          # options = list(placeholder = 'type to select a location')),
        bookmarkButton(),
        selectInput("min_stat", "metric to compare by:", compare_metrics),
        numericInput("min_thresh",
                     "initial number of deaths:", min = 0, value = min_global),
        checkboxInput("per_million",
                      "Counts per million people", value = TRUE),
        numericInput("max_days_since",
                     "days since initial number of deaths:", min = 0,
                     value = as.numeric(today() - ymd("2020-01-01"),
                                        unit = "days")),
        # plot options
        numericInput("num_cols",
                     "Number of columns", value = if_else(mobile_req, 1, 2)),
        checkboxInput("show_legend",
                      "Show legend", value = !mobile_req),
        checkboxInput("smooth_plots",
                      "Smooth plot values", value = FALSE), 
        width = 2),
      mainPanel(
        tabsetPanel(
          id = "plotTabs", type = "tabs",
          tabPanel(
            "Epidemiology", value = "epi_plots",
            plotlyOutput(
              "epiPlot",
              width = if_else(mobile_req, "auto", "auto"),
              height = if_else(mobile_req, "2800px", "1400px")),
            downloadButton("downloadData", "download data (csv)")
          ),
          tabPanel(
            "Hospitalization", value = "hosp_plots",
            plotlyOutput(
              "hospPlot",
              width = if_else(mobile_req, "auto", "auto"),
              height = if_else(mobile_req, "2800px", "1400px"))
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

  tscomp_panel <- fluidPage(
    shinybusy::add_busy_bar(color = "CornflowerBlue", timeout = 800),
    titlePanel("COVID-19 comparables"),
    tags$a(
      href = "https://github.com/roboton/covid-compare",
      target = "_blank", "[git]"),
    tags$a(
      href = "mailto:roberton@gmail.com",
      target = "_blank", "[contact]"),
    tags$a(
      href = "https://docs.google.com/presentation/d/1GNaVzcFbK_bo5R4avZmq0gpYDTRFFiiiowBR7Ovbr_I/edit#slide=id.p",
      target = "_blank", "[tutorial]"),
    tags$div(paste(" Last updated",
                   round(as.numeric(now() - tscomp_last_update, units = "days"), 2),
                   "days ago")),
    sidebarLayout(
      sidebarPanel(
        # data options
        selectizeInput(
          "set_name", "Country/Global",
          choices = set_names,
          multiple = FALSE,
          selected = default_set_name,
          options = list(placeholder = 'type to select a country')),
        selectizeInput(
          "geo_name", "Division", choices = NULL, multiple = FALSE,
          options = list(placeholder = 'type to select a division')),
        tags$div("Plots of a single jurisdictions COVID-19 deaths per million compared against a set of other jurisdictions that had a comparable first six months of the pandemic with respect to weekly deaths per million. See tutorial link above for more."),
        width = 2
      ),
      mainPanel(
        plotlyOutput(
          "tsCompPlot",
          width = if_else(mobile_req, "auto", "auto"),
          # height = if_else(mobile_req, "auto", "auto")
          height = "800px"
        )
      )
    )
  )
  
  vax_panel <- fluidPage(
    shinybusy::add_busy_bar(color = "CornflowerBlue", timeout = 800),
    titlePanel("COVID-19 vaccinations"),
    tags$a(
      href = "https://github.com/roboton/covid-compare",
      target = "_blank", "[git]"),
    tags$a(
      href = "mailto:roberton@gmail.com",
      target = "_blank", "[contact]"),
    tags$div(paste(" Last updated",
                    round(as.numeric(now() - vax_last_update, units = "days"), 2),
                    "days ago")),
    
    sidebarLayout(
      sidebarPanel(
        # data options
        selectizeInput(
          "vax_set_name", "Country/Global",
          choices = vax_set_names,
          multiple = FALSE,
          selected = vax_default_set_name,
          options = list(placeholder = 'type to select a country')),
        tags$div("Plots of vaccination doses per million vs COVID-19 deaths/cases per million for each jurisdiction animated over time."),
        width = 2
      ),
      mainPanel(
        plotlyOutput(
          "vaxPlot",
          width = if_else(mobile_req, "auto", "auto"),
          height = "800px"
        )
      )
    )
  )
 
  navbarPage("covidcompare.org", position = "fixed-bottom",
             tabPanel("Comparables", tscomp_panel),
             tabPanel("Vaccinations", vax_panel),
             tabPanel("Dashboard", dashboard_panel))
} 
   
server <- function(input, output, session) {
  #mobile_req <- is_mobile(session$request$HTTP_USER_AGENT)
  output$epiPlot <- renderPlotly({
    filter_locs <- all_locs %>%
      filter(location %in% !!input$location)
    filter_locs %>%
      genPlotComps(min_thresh = input$min_thresh,
                   per_million = input$per_million,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   show_legend = input$show_legend,
                   plot_type = "epi",
                   ncol = input$num_cols)
    })
    #} %>% plotly::partial_bundle())
  
  output$hospPlot <- renderPlotly({
    filter_locs <- all_locs %>%
      filter(location %in% !!input$location)
    filter_locs %>%
      genPlotComps(min_thresh = input$min_thresh,
                   per_million = input$per_million,
                   min_stat = input$min_stat,
                   max_days_since = input$max_days_since,
                   smooth_plots = input$smooth_plots,
                   show_legend = input$show_legend,
                   plot_type = "hosp")
    })
    #} %>% plotly::partial_bundle())
  
  output$severityTable <- DT::renderDataTable({
    loc_list %>% arrange(-severity_total)
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
  
  observeEvent(input$set_name, {
    geo_names <- tscomp_summary %>%
      filter(set_name == input$set_name) %>%
      arrange(p_Cumulative) %>%
      select(name = geo_name, label, effect = AbsEffect_Cumulative,
             pvalue = p_Cumulative) %>%
      mutate(label = str_glue("{label} ({round(effect)})"))

    geo_name <- sample(geo_names$name, 1, prob = 1 - geo_names$pvalue)
    updateSelectizeInput(session, "geo_name",
                         choices = setNames(geo_names$name, geo_names$label),
                         selected = geo_name, server = TRUE)
    
  })
  
  output$tsCompPlot <- renderPlotly({
    if (input$geo_name == "" | input$set_name == "") {
      return(empty_plot("select division"))
    }
    readr::read_rds(fs::path("data", "sets", input$set_name, "tscomp",
                             input$geo_name, ext = "rds"))
  })
  
  output$vaxPlot <- renderPlotly({
    p <- vax_dat  %>%
      # geo filter
      filter(geo == input$vax_set_name &
               !location %in% c("US_DC")) %>%
      # outlier value filtering
      # filter(value >= 0) %>%
      # # scale outcomes between 0 and 1
      # group_by(name, date) %>%
      # mutate(value = value / sum(value)) %>%
      # ungroup() %>%
      # fix date for frame animation
      mutate(date = as.character(date)) %>%
      # # wait until at least x locations %>%
      # mutate(total_locs = n_distinct(location)) %>%
      # group_by(date) %>%
      # filter(n_distinct(location) > total_locs / 2 &
      #          sum(doses > 0) >= total_locs / 2) %>%
      # select(-total_locs) %>%
      # plot
      mutate(plot_label = str_remove(location, ".*_")) %>%
      ggplot(aes(doses, value, frame = date, text = label)) +
      # geom_point(aes(label = label, ids = location)) +
      geom_text(aes(label = plot_label, ids = plot_label)) +
      ggtitle(names(vax_set_names)[vax_set_names == input$vax_set_name]) + 
      ylab("weekly cases/deaths per million") +
      xlab("cumulative doses per million") +
      facet_wrap(~ name, scales = "free_y", nrow = 2)
    
    ggplotly(p) %>%
      animation_opts(redraw = FALSE)
  })
  
  # server side location selectize
  updateSelectizeInput(session, "location", choices = loc_list$location,
                       selected = sample(loc_list$location, 3,
                                         prob = loc_list$severity_total),
                       server = TRUE)
  
  # ensure we don't overwrite bookmark locations with default
  session$onRestore(function(state) {
    session$userData$restored <- TRUE
    updateSelectizeInput(session, "location", choices = loc_list$location,
                         selected = state$input$location, server = TRUE)
  })
  # session$onFlushed(function() {
  #   if (is.null(session$userData$restored)) {
  #     updateSelectizeInput(session, "location", choices = loc_list$location,
  #                          selected = sample(loc_list$location, size = n_locs,
  #                                            prob = loc_list$severity_total),
  #                            # sample(loc_list$location, size = n_locs,
  #                            #        prob = log(loc_list$severity_popM + 1))),
  #                          server = TRUE)
  #   }
  # }, once = TRUE)
  
  # # shorten bookmark url
  # session$onBookmarked(function(url) {
  #   short_url <- urlshorteneR::isgd_LinksShorten(longUrl = url)
  #   showBookmarkUrlModal(short_url)
  # })
  
  # update min_stat metric
  observeEvent({ list(input$min_stat, input$per_million, input$min_thresh) }, {
    per_million_label <- if_else(input$per_million, " per million", "")
    stat_label <- names(compare_metrics)[compare_metrics == input$min_stat]
    updateSelectInput(session, "min_thresh",
                      label = paste0("initial number of ", stat_label,
                                     per_million_label, ":"))
    updateNumericInput(session, "max_days_since",
                       label = paste0("days since ", input$min_thresh, " ",
                                      stat_label, per_million_label, ":"))
  })  
}

shinyApp(ui = ui, server = server, enableBookmarking = "server")
