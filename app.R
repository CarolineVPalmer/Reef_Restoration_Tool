# =============================================================================
# app.R -- Reef Restoration Prioritisation Tool (interface)
#
# HOW TO RUN
#   Open the project folder in RStudio, open app.R and click "Run App"
#   (top right of the script pane), or run shiny::runApp() from the project
#   folder. Upload a transect CSV, choose the data period and snapshot, and
#   click "Run analysis". All calculations are in the R/ folder; this file
#   holds only the interface.
#
# Packages: shiny and ggplot2 (required); ggrepel (optional, tidier labels).
# =============================================================================

# Use the folder this script is saved in as the working directory (RStudio "Source")
try({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p) && dir.exists(file.path(dirname(p), "R"))) setwd(dirname(p))
  }
}, silent = TRUE)

suppressPackageStartupMessages({ library(shiny); library(ggplot2) })

# Load the calculation functions. Shiny (>= 1.5) also sources R/ automatically
# when the app is started with runApp(); sourcing them here as well means the
# app also works when app.R is run line by line. The files only define
# functions and constants, so loading them twice is harmless.
for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f, local = TRUE)
EXAMPLE_FILE <- file.path("data", "example_transects.csv")
options(shiny.maxRequestSize = 100 * 1024^2)   # uploads up to 100 MB

# ---- interface helpers --------------------------------------------------------
weight_input <- function(r) {
  sliderInput(paste0("w_", r$id), label = sprintf("%s (%s)", r$label, ifelse(r$sign > 0, "+", "\u2212")),
              min = r$min, max = r$max, value = r$weight, step = 0.25)
}
weight_group <- function(g) {
  rows <- DEFAULT_WEIGHTS[DEFAULT_WEIGHTS$group == g, ]
  lapply(seq_len(nrow(rows)), function(i) weight_input(rows[i, ]))
}
fmt <- function(d, digits = 2) {
  d <- as.data.frame(d)
  num <- vapply(d, is.numeric, logical(1))
  d[num] <- lapply(d[num], function(x) if (all(is.na(x) | x == round(x))) as.integer(x) else round(x, digits))
  d
}
pct <- function(x) sprintf("%.0f%%", 100 * x)

ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".note{color:#555;font-size:0.9em} .warn{color:#9A6B1F} h4{margin-top:18px}
     .alert-box{background:#FFF4E5;border-left:4px solid #C0392B;padding:8px 12px;margin:10px 0}
     .adv{border-top:1px solid #ccc;margin-top:12px;padding-top:6px}
     .shiny-notification{position:fixed;top:10px;right:10px}
     #running-box{display:none;position:fixed;top:12px;right:12px;z-index:2000;max-width:340px;background:#fff;
       border-left:5px solid #0072B2;box-shadow:0 2px 8px rgba(0,0,0,.25);padding:12px 14px;font-size:0.95em}
     #running-box .spin{display:inline-block;width:14px;height:14px;margin-right:8px;vertical-align:-2px;
       border:3px solid #cfd8e3;border-top-color:#0072B2;border-radius:50%;animation:rrspin 1s linear infinite}
     @keyframes rrspin{to{transform:rotate(360deg)}}"))),
  # "Running" message shown by the browser itself as soon as Run analysis or
  # Compare is clicked, and hidden when R reports that the results are ready.
  # It is drawn in the browser rather than by R because, in the in-browser
  # version, R cannot update the page until a calculation has finished.
  tags$div(id = "running-box", tags$span(class = "spin"), tags$strong("Running\u2026"), tags$br(),
           "This usually takes under a minute. In the browser version, large files or many bootstrap ",
           "replicates can take several minutes. Results appear when it is finished."),
  tags$script(HTML("
    $(document).on('click', '#analyze, #ev_run', function() { $('#running-box').show(); });
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('running_done', function(x) { $('#running-box').hide(); });
    });")),
  titlePanel("Reef Restoration Prioritisation Tool"),
  sidebarLayout(
    sidebarPanel(width = 4,
      h4("1. Data"),
      fileInput("datafile", "Upload transect CSV", accept = c(".csv", ".txt")),
      downloadLink("dl_example", "Download an example file (synthetic data)"),
      checkboxInput("day_first", "Dates written day first (e.g. 25/01/2025)", TRUE),
      fileInput("load_settings", "Load saved settings (optional)", accept = ".csv"),

      h4("2. Which surveys"),
      dateRangeInput("period", "Use surveys from / to"),
      radioButtons("snapshot_mode", "Compare sites using",
                   c("The most recent survey period (recommended)" = "period",
                     "A survey window you choose" = "window",
                     "The most recent survey at each site (dates may differ)" = "latest",
                     "All data in the period (pooled)" = "all"), selected = "period"),
      conditionalPanel("input.snapshot_mode == 'window'",
        dateRangeInput("window", "Survey window")),

      h4("3. Reef context"),
      selectInput("preset", NULL, names(PRESETS)),
      p(class = "note", textOutput("preset_why", inline = TRUE)),

      h4("4. Run"),
      actionButton("analyze", "Run analysis", class = "btn-primary"),
      br(), br(),
      downloadButton("dl_results", "Results"),
      downloadButton("dl_plot", "Decision plane (PNG)"),
      downloadButton("dl_settings", "Settings"),

      div(class = "adv", checkboxInput("advanced", strong("Show advanced settings"), FALSE)),
      conditionalPanel("input.advanced",
        h4("Options"),
        numericInput("min_tr", "Minimum transects per site (flag below)", 3, min = 2, step = 1),
        helpText("Three transects per site is enough to run the tool; five or six give more confident classes."),
        numericInput("topN", "Donor shortlist length", 5, min = 1, step = 1),
        numericInput("B", "Bootstrap replicates", 200, min = 50, max = 2000, step = 50),
        numericInput("seed", "Random seed", 123, min = 1, step = 1),
        radioButtons("pca_set", "Algal dominance (PCA) uses",
                     c("Benthic composition only (recommended)" = "composition",
                       "Composition plus bleaching, disease and predation (as version 1)" = "extended")),
        h4("Survey periods and trends"),
        selectInput("period_months", "Survey period length", c("1 month" = 1, "2 months" = 2, "3 months" = 3, "6 months" = 6), selected = 3),
        selectInput("period_start", "Periods start in", setNames(1:12, month.name), selected = 3),
        checkboxInput("trends", "Add each site's trend (rate of change) to Urgency", TRUE),
        conditionalPanel("input.trends",
          numericInput("trend_min_periods", "Trend used only for sites with at least this many periods", 3, min = 3, step = 1),
          numericInput("trend_min_days", "... spanning at least this many days", 365, min = 180, step = 30)),
        h4("Weights"),
        actionButton("reset_w", "Reset to the selected reef context"),
        br(), br(),
        tabsetPanel(
          tabPanel("Feasibility", weight_group("Feasibility")),
          tabPanel("Urgency",     weight_group("Urgency")),
          tabPanel("Donor",       weight_group("Donor")),
          tabPanel("Trend",       weight_group("Trend"))
        ),
        p(class = "note", "Signs (+/−) are fixed so each indicator always acts in its ecological direction; ",
          "you choose how much it counts (0 switches it off). Record why you change any weight."),
        h4("More downloads"),
        downloadButton("dl_donors", "Donor shortlist"),
        downloadButton("dl_sens", "Sensitivity"),
        downloadButton("dl_trends", "Trends"),
        downloadButton("dl_traj", "Trajectories")
      ),
      uiOutput("weight_warning")
    ),
    mainPanel(width = 8,
      tabsetPanel(id = "tabs",
        tabPanel("Start here", htmlOutput("start")),
        tabPanel("Overview", htmlOutput("overview"), tableOutput("counts")),
        tabPanel("Decision plane", plotOutput("scorePlot", height = "600px"), htmlOutput("plotExplanation")),
        tabPanel("Recommendations", htmlOutput("recNote"), tableOutput("recommendationTable")),
        tabPanel("Donor sites", p(class = "note", textOutput("donorNote", inline = TRUE)), tableOutput("donorSiteTable")),
        tabPanel("Trends", htmlOutput("trendNote"), plotOutput("trendScorePlot", height = "480px"),
                 plotOutput("trendHeatmap", height = "560px"), h5("Change across the reef (mean of eligible sites)"),
                 tableOutput("reefMean"), h5("Site trends"), tableOutput("trendTable")),
        # Optional bleaching-event mode (v2.5.0). Independent of the standard
        # analysis, which works the same whether or not this tab is used.
        tabPanel("Bleaching event (optional)",
          htmlOutput("evIntro"),
          fluidRow(
            column(4, checkboxInput("ev_use_before", "I have surveys before the event", TRUE),
                   conditionalPanel("input.ev_use_before", dateRangeInput("ev_before", "Surveys before the event")),
                   helpText("Untick this if monitoring began only once bleaching was under way. Change is then measured from the surveys during the event to those after it, so any loss before the first survey is missed.")),
            column(4, checkboxInput("ev_use_during", "I have surveys during the event", FALSE),
                   conditionalPanel("input.ev_use_during", dateRangeInput("ev_during", "Surveys during the event"))),
            column(4, checkboxInput("ev_use_after", "I have surveys after the event", FALSE),
                   conditionalPanel("input.ev_use_after", dateRangeInput("ev_after", "Surveys after the event")))),
          actionButton("ev_run", "Compare before / during / after", class = "btn-primary"),
          downloadButton("dl_event", "Event results"),
          htmlOutput("evSummary"),
          plotOutput("evPlot", height = "480px"),
          tableOutput("evTable")),
        navbarMenu("More",
          tabPanel("Site scores", p(class = "note", "Scores are relative to the sites analysed (0 = their mean). Class support = % of bootstrap replicates giving the same class."),
                   tableOutput("scoreTable")),
          tabPanel("Weight sensitivity", htmlOutput("sensText"), tableOutput("sensSite"),
                   h5("Scenarios that changed at least one class"), tableOutput("sensScen"),
                   h5("Classes under each reef context (and Minimal overlap)"), tableOutput("sensPresets")),
          tabPanel("Trajectories", plotOutput("trajPlot", height = "700px"), htmlOutput("trajNote"), tableOutput("trajTable")),
          tabPanel("Community context (PCA)", plotOutput("pcaPlot", height = "520px"), textOutput("pcaNote"), tableOutput("pcaLoadings")),
          tabPanel("Diagnostics", tableOutput("diagnostics")),
          tabPanel("Methods and weights", htmlOutput("methodsText"), tableOutput("weightsTable")))
      )
    )
  )
)

server <- function(input, output, session) {

  # ---- weights ----------------------------------------------------------------
  set_weights <- function(w) for (i in seq_len(nrow(w))) updateSliderInput(session, paste0("w_", w$id[i]), value = w$weight[i])
  current_weights <- reactive({
    w <- DEFAULT_WEIGHTS
    for (i in seq_len(nrow(w))) { v <- input[[paste0("w_", w$id[i])]]; if (!is.null(v)) w$weight[i] <- v }
    w
  })
  observeEvent(input$preset, set_weights(apply_preset(input$preset)), ignoreInit = TRUE)
  observeEvent(input$reset_w, set_weights(apply_preset(input$preset)))
  output$preset_why <- renderText(PRESETS[[input$preset]]$why)
  output$weight_warning <- renderUI({
    w <- current_weights(); d <- DEFAULT_WEIGHTS$weight
    far <- (d > 0 & (w$weight > 2 * d | w$weight < 0.5 * d)) | (d == 0 & w$weight > 0)
    if (!any(far)) return(NULL)
    div(class = "warn", strong("Note: "), sprintf("%d weight(s) differ from the defaults by more than a factor of two, or switch on an optional indicator. Check the Weight sensitivity tab and record your reasons.", sum(far)))
  })

  # ---- data ---------------------------------------------------------------------
  raw <- reactive({
    req(input$datafile)
    tryCatch(read_transect_file(input$datafile$datapath),
             error = function(e) { showModal(modalDialog(title = "Could not read the file", conditionMessage(e), easyClose = TRUE)); NULL })
  })
  prep <- reactive({
    r <- raw(); req(r)
    tryCatch(prepare_data(r, day_first = input$day_first),
             error = function(e) { showModal(modalDialog(title = "Problem with the input file", conditionMessage(e), easyClose = TRUE)); NULL })
  })
  loaded <- reactiveVal(NULL)
  # when a new file is read, set the date controls to its range
  observeEvent(prep(), {
    p <- prep(); req(p)
    if (p$has_dates) {
      rng <- range(p$df$date, na.rm = TRUE)
      ls <- loaded()
      st <- if (!is.null(ls) && !is.na(as_date_or_na(ls$data_from))) as_date_or_na(ls$data_from) else rng[1]
      en <- if (!is.null(ls) && !is.na(as_date_or_na(ls$data_to))) as_date_or_na(ls$data_to) else rng[2]
      updateDateRangeInput(session, "period", start = st, end = en, min = rng[1], max = rng[2])
      wf <- if (!is.null(ls) && !is.na(as_date_or_na(ls$window_from))) as_date_or_na(ls$window_from) else max(rng[1], rng[2] - 90)
      wt <- if (!is.null(ls) && !is.na(as_date_or_na(ls$window_to))) as_date_or_na(ls$window_to) else rng[2]
      updateDateRangeInput(session, "window", start = wf, end = wt, min = rng[1], max = rng[2])
      # event phases: loaded settings if any, otherwise first and last 90 days of the data
      pick <- function(key, default) if (!is.null(ls) && !is.na(as_date_or_na(ls[[key]]))) as_date_or_na(ls[[key]]) else default
      updateDateRangeInput(session, "ev_before", start = pick("event_before_from", rng[1]),
                           end = pick("event_before_to", min(rng[2], rng[1] + 90)), min = rng[1], max = rng[2])
      updateDateRangeInput(session, "ev_during", start = pick("event_during_from", max(rng[1], rng[2] - 90)),
                           end = pick("event_during_to", rng[2]), min = rng[1], max = rng[2])
      updateDateRangeInput(session, "ev_after", start = pick("event_after_from", max(rng[1], rng[2] - 90)),
                           end = pick("event_after_to", rng[2]), min = rng[1], max = rng[2])
    }
  })

  # ---- settings ------------------------------------------------------------------
  current_settings <- reactive({
    list(day_first = input$day_first,
         data_from = input$period[1], data_to = input$period[2],
         snapshot_mode = input$snapshot_mode, window_from = input$window[1], window_to = input$window[2],
         min_transects = input$min_tr, top_n = input$topN, B = input$B, seed = input$seed,
         pca_set = input$pca_set, include_trends = input$trends,
         period_months = as.numeric(input$period_months), period_start_month = as.numeric(input$period_start),
         trend_min_periods = input$trend_min_periods, trend_min_days = input$trend_min_days,
         event_use_before = input$ev_use_before, event_before_from = input$ev_before[1], event_before_to = input$ev_before[2],
         event_use_during = input$ev_use_during, event_during_from = input$ev_during[1], event_during_to = input$ev_during[2],
         event_use_after = input$ev_use_after, event_after_from = input$ev_after[1], event_after_to = input$ev_after[2])
  })
  observeEvent(input$load_settings, {
    st <- tryCatch(read_settings(input$load_settings$datapath), error = function(e) NULL)
    if (is.null(st)) { showNotification("Not a valid settings file.", type = "error"); return() }
    s <- st$settings
    loaded(s)
    updateCheckboxInput(session, "day_first", value = isTRUE(s$day_first))
    if (!is.na(as_date_or_na(s$data_from)) || !is.na(as_date_or_na(s$data_to)))
      updateDateRangeInput(session, "period", start = as_date_or_na(s$data_from), end = as_date_or_na(s$data_to))
    updateRadioButtons(session, "snapshot_mode", selected = s$snapshot_mode)
    if (!is.na(as_date_or_na(s$window_from))) updateDateRangeInput(session, "window", start = as_date_or_na(s$window_from), end = as_date_or_na(s$window_to))
    updateNumericInput(session, "min_tr", value = s$min_transects); updateNumericInput(session, "topN", value = s$top_n)
    updateNumericInput(session, "B", value = s$B); updateNumericInput(session, "seed", value = s$seed)
    updateRadioButtons(session, "pca_set", selected = s$pca_set)
    updateCheckboxInput(session, "trends", value = isTRUE(s$include_trends))
    updateSelectInput(session, "period_months", selected = s$period_months)
    updateSelectInput(session, "period_start", selected = s$period_start_month)
    updateNumericInput(session, "trend_min_periods", value = s$trend_min_periods)
    updateNumericInput(session, "trend_min_days", value = s$trend_min_days)
    updateCheckboxInput(session, "ev_use_before", value = !isFALSE(s$event_use_before))
    updateCheckboxInput(session, "ev_use_during", value = isTRUE(s$event_use_during))
    updateCheckboxInput(session, "ev_use_after", value = isTRUE(s$event_use_after))
    for (ph in c("before", "during", "after")) {
      f <- as_date_or_na(s[[paste0("event_", ph, "_from")]]); t <- as_date_or_na(s[[paste0("event_", ph, "_to")]])
      if (!is.na(f) && !is.na(t)) updateDateRangeInput(session, paste0("ev_", ph), start = f, end = t)
    }
    set_weights(st$weights)
    showNotification("Settings loaded. Check them, then click Run analysis.", type = "message")
  })

  # ---- analysis ------------------------------------------------------------------
  # Switch to the Overview when Run analysis is clicked: Shiny computes the
  # analysis only when an output that needs it is visible (before v2.5.1,
  # clicking Run analysis on the Start page did nothing until another tab was
  # opened).
  observeEvent(input$analyze, updateTabsetPanel(session, "tabs", selected = "Overview"))
  running_done <- function() session$sendCustomMessage("running_done", list())
  res <- eventReactive(input$analyze, {
    on.exit(running_done(), add = TRUE)
    p <- prep()
    if (is.null(p)) { showNotification("Upload a transect CSV first.", type = "error"); req(FALSE) }
    withProgress(message = "Running analysis\u2026", value = 0, {
      out <- tryCatch(run_analysis(p, current_settings(), current_weights(),
                                   progress = function(x, msg) setProgress(x, detail = msg)),
                      error = function(e) { showModal(modalDialog(title = "The analysis could not run", conditionMessage(e), easyClose = TRUE)); NULL })
    })
    req(out)
    out
  })

  # ---- outputs -------------------------------------------------------------------
  output$start <- renderUI(HTML(paste0(
    "<h4>What this tool does</h4>",
    "<p>It turns routine reef transect data into <strong>relative</strong> restoration priorities for the sites you monitor: ",
    "a Feasibility score (reef condition), an Urgency score (pressure), a management class with a broad intervention pathway, ",
    "uncertainty from bootstrap resampling, a check of how much the classes depend on the weights, and a donor-site shortlist. ",
    "It is a first-pass screening step before detailed site-suitability assessment, not a measure of restoration success.</p>",
    "<h4>Input file</h4><p>One row per transect. Required: <code>site</code>, <code>coral</code> (live coral cover). ",
    "Recommended: <code>date</code> (or <code>year</code> + <code>month</code>), <code>turf</code>, <code>cca</code>, <code>cyanobacteria</code>, ",
    "<code>macroalgae</code>, <code>bleached</code>, <code>disease</code>, <code>predation</code> (proportion of colonies affected) ",
    "and coral colony counts per taxon in columns starting <code>Tot_</code> (for diversity). Optional: <code>depth</code> (shown beside the results, not scored), <code>rubble</code>, ",
    "<code>rugosity</code>, <code>herbivores</code>, <code>sand</code> (shown, not scored), <code>method</code> (e.g. transect or photogrammetry; used only to warn when methods are mixed). One row per transect or photogrammetry plot. Cover can be 0\u20131 or 0\u2013100. Comma- or semicolon-separated files are accepted.</p>",
    "<h4>Steps</h4><ol><li>Upload the file (or try the example).</li><li>Choose which surveys to use. By default sites are compared on the most ",
    "recent survey period, and each site's rate of change over the whole record is added to Urgency.</li>",
    "<li>Choose the reef context that fits your situation (or keep General).</li><li>Click <strong>Run analysis</strong>, then read the Overview ",
    "(including any alerts), Decision plane and Recommendations. Download the results and the settings file so the run can be repeated.</li></ol>",
    "<h4>Two kinds of output</h4><p><strong>Classes</strong> are relative: they rank your sites against each other. ",
    "<strong>Alerts</strong> are absolute: coral cover below 10%, bleaching above 50% of colonies (severe), or rubble above 30% cover. ",
    "During a reef-wide event such as a marine heatwave, every site can be in poor condition while the classes still spread sites across all four pathways; ",
    "the alerts show when that is happening.</p>",
    "<h4>Bleaching events (optional)</h4><p>The <em>Bleaching event</em> tab compares surveys before, during and after an event, ",
    "site by site. It is separate from the standard analysis and is needed only during or after an event.</p>",
    "<p class='note'>Weights, trend settings and extra outputs are under <em>Show advanced settings</em> and the <em>More</em> tab.</p>")))

  output$overview <- renderUI({
    r <- res(); s <- r$sites
    al <- alert_summary(r)
    HTML(paste0(
      if (length(al)) paste0("<div class='alert-box'><strong>Alerts (absolute thresholds)</strong><ul>",
                             paste0("<li>", htmltools::htmlEscape(al), "</li>", collapse = ""), "</ul>",
                             if (any(grepl("bleaching", al))) "<p class='note'>Where most sites are bleaching, treat the classes as interim and re-screen after the event.</p>" else "",
                             "</div>") else "",
      "<p><strong>", nrow(s), "</strong> sites scored from <strong>", sum(s$n_transects), "</strong> transects. Snapshot: ", r$snapshot$description, ".</p>",
      "<p>Scores are <strong>relative to the sites analysed</strong>: zero is their average, not an absolute threshold.</p>",
      sprintf("<p>Feasibility\u2013Urgency correlation (Spearman \u03c1): <strong>%s</strong>. Some negative correlation is expected because both indices share core indicators with opposite signs.</p>",
              ifelse(is.na(r$rho), "n/a", sprintf("%.2f", r$rho))),
      sprintf("<p>Sites below %d transects: <strong>%d</strong>. Borderline sites (95%% interval crosses zero): <strong>%d</strong>. Median class support: <strong>%s</strong>.</p>",
              r$settings$min_transects, sum(s$below_min), sum(s$borderline != "No"), pct(stats::median(s$class_support))),
      if (length(r$notes)) paste0("<h5>Data and method notes</h5><ul>", paste0("<li>", htmltools::htmlEscape(r$notes), "</li>", collapse = ""), "</ul>") else ""))
  })
  output$counts <- renderTable({
    q <- res()$quadrant_counts
    data.frame(Pathway = CLASSES$pathway, Class = names(q), Sites = as.integer(q))
  })
  output$scorePlot <- renderPlot(plot_decision_plane(res(), base_size = 14))
  output$plotExplanation <- renderUI(HTML(paste0(
    "<p><strong>Reading the plot.</strong> Zero lines are the average of the sites analysed. Bars are 95% bootstrap intervals: ",
    "transects were resampled within all sites and the whole calculation repeated, so the bars include uncertainty in the zero lines. ",
    "Where a bar crosses zero, the class is borderline: collect more data or stage actions. Open circles: fewer transects than the minimum.</p>",
    paste0("<p><strong>", CLASSES$pathway, ". ", CLASSES$class, ":</strong> ", CLASSES$recommendation, "</p>", collapse = ""))))

  output$scoreTable <- renderTable({
    t <- fmt(site_table(res())); t$class_support <- pct(t$class_support); t
  }, digits = 2)
  output$recNote <- renderUI(HTML(paste0("<p class='note'>Pathways follow from each site's class (relative to the other sites). ",
    "Alerts are absolute: ", paste(sprintf("<strong>%s</strong>: %s", FLAG_RULES$short, htmltools::htmlEscape(FLAG_RULES$meaning)), collapse = " "), "</p>")))
  output$recommendationTable <- renderTable({
    s <- res()$sites
    data.frame(site = s$site, depth_m = s$depth_m, pathway = s$pathway, class = s$class, class_support = pct(s$class_support),
               alerts = s$alerts, borderline = s$borderline, recommendation = s$recommendation)
  })
  output$donorNote <- renderText(sprintf("Ranked by donor score in the snapshot. Rank interval = 95%% bootstrap range; top-%d support = %% of replicates in which the site is in the top %d. Check colony health and permits before collecting.",
                                         min(input$topN, nrow(res()$donors)), min(input$topN, nrow(res()$donors))))
  output$donorSiteTable <- renderTable({
    d <- res()$donors
    d$donor_topK_support <- pct(d$donor_topK_support)
    names(d)[names(d) == "donor_topK_support"] <- "top_K_support"
    fmt(head(d, input$topN))
  })

  output$sensText <- renderUI({
    r <- res()$sensitivity
    HTML(sprintf(paste0("<p><strong>One at a time:</strong> each Feasibility and Urgency weight multiplied by %.1f and %.1f in turn, plus the Minimal overlap preset (%d scenarios). ",
                        "<strong>Global:</strong> all these weights varied together, each multiplied by a random factor between %.1f and %.1f (%d draws). ",
                        "<strong>Rank-preserving:</strong> random weights that keep the order of the default weights (%d draws), which tests whether classes depend on the exact numbers or only on which indicators matter most. ",
                        "Stability = share of scenarios in which a site keeps its class. Indicator values are held fixed. ",
                        "The equal-weights benchmark (every indicator weighted 1) is shown with the reef contexts below.</p>"),
                 1 - r$factor, 1 + r$factor, r$n_scenarios, 1 - r$factor, 1 + r$factor, r$n_global, r$n_global))
  })
  output$sensSite <- renderTable({
    s <- res()$sensitivity$by_site
    s$stability_one_at_a_time <- pct(s$stability_one_at_a_time); s$stability_global <- pct(s$stability_global)
    s$stability_rank_preserving <- pct(s$stability_rank_preserving); s
  })
  output$sensScen <- renderTable({
    s <- res()$sensitivity$by_scenario; s <- s[s$sites_changed > 0, ]; s[order(-s$sites_changed), ]
  })
  output$sensPresets <- renderTable(res()$sensitivity$presets)

  output$trajPlot <- renderPlot({
    p <- plot_trajectories(res(), base_size = 13)
    if (is.null(p)) { plot.new(); text(0.5, 0.5, "Trajectories need dated surveys in at least two periods.", cex = 1.1) } else p
  })
  output$trajNote <- renderUI({
    t <- res()$trajectories
    if (is.null(t)) return(NULL)
    dr <- attr(t, "dropped")
    HTML(paste0("<p class='note'>Each site is scored in each survey period; all site-periods are standardised together, so movement reflects change at the site ",
                "relative to the whole record. Trajectories are descriptive and do not change the classes.",
                if (length(dr)) sprintf(" Left out because missing in more than 20%% of site-periods: %s.", paste(dr, collapse = ", ")) else "", "</p>"))
  })
  output$trajTable <- renderTable({ t <- res()$trajectories; req(t); t$period_start <- NULL; fmt(t) })
  output$trendNote <- renderUI({
    r <- res(); s <- r$settings
    if (!isTRUE(s$include_trends)) return(HTML("<p>Trends not requested. Tick the option under advanced settings to add them.</p>"))
    HTML(paste0(
      "<p><strong>What the trend shows.</strong> For each site, every indicator is averaged per survey period and a straight line is fitted ",
      "through those period means over the whole data period; the slope is the change per year. Only sites surveyed in at least ",
      s$trend_min_periods, " periods spanning at least ", s$trend_min_days, " days are used. ",
      "The slopes are compared across sites and combined with the Trend weights into a <strong>trend score</strong> (positive = deteriorating ",
      "faster than the other sites), which is added to Urgency. Intervals come from the same bootstrap as the other scores.</p>",
      "<p>The trend score is <strong>relative</strong>: a change that happens everywhere (e.g. reef-wide bleaching) does not raise one site ",
      "above another, so it is shown separately in the table of change across the reef.</p>",
      if (!s$trend_used) "<p class='warn'>Trends were not used in this run: too few sites had enough survey periods.</p>" else ""))
  })
  output$trendScorePlot <- renderPlot({ p <- plot_trend_scores(res(), base_size = 14); req(p); p })
  output$trendHeatmap <- renderPlot({ p <- plot_trend_heatmap(res(), base_size = 13); req(p); p })
  output$reefMean <- renderTable({ x <- res()$trends$reef_mean; req(x); fmt(x) })
  output$trendTable <- renderTable({
    tr <- res()$trends; req(tr$table)
    x <- tr$table; x$span_days <- round(x$span_days)
    fmt(x, 2)
  })

  output$pcaPlot <- renderPlot({
    p <- plot_pca(res())
    if (is.null(p)) { plot.new(); text(0.5, 0.5, res()$pca$note, cex = 1.1) } else p
  })
  output$pcaNote <- renderText(res()$pca$note)
  output$pcaLoadings <- renderTable({ l <- res()$pca$loadings; req(l); fmt(l) })
  output$diagnostics <- renderTable(res()$diagnostics)

  output$methodsText <- renderUI({
    r <- res()
    HTML(paste0("<h4>Formulas used in this run</h4>", paste0("<p>", htmltools::htmlEscape(methods_text(r)), "</p>", collapse = ""),
                "<p>Classes: A Immediate restoration (Feasibility \u2265 0, Urgency \u2265 0); B Triage and foundational support (Feasibility < 0, Urgency \u2265 0); ",
                "C Proactive reinforcement (Feasibility \u2265 0, Urgency < 0); D Monitoring only (both < 0).</p>"))
  })
  output$weightsTable <- renderTable({
    w <- res()$weights_used; wr <- res()$weights_requested
    data.frame(group = w$group, indicator = w$label, direction = ifelse(w$sign > 0, "+", "-"),
               default = DEFAULT_WEIGHTS$weight, requested = wr$weight, used = w$weight, range = paste0(w$min, " to ", w$max))
  })

  # ---- downloads -------------------------------------------------------------------
  output$dl_example <- downloadHandler(
    filename = "example_transects_synthetic.csv",
    content = function(f) {
      if (file.exists(EXAMPLE_FILE)) file.copy(EXAMPLE_FILE, f)
      else writeLines("The example file was not found in the app folder.", f)
    })
  header_lines <- function(r) {
    s <- r$settings
    c(sprintf("# Reef Restoration Prioritisation Tool v%s, run %s (%s)", s$version, s$run_at, s$r_version),
      "# Scores are relative to the sites in this snapshot (0 = their mean).",
      paste0("# Snapshot: ", r$snapshot$description),
      paste0("# Weights used: ", paste(sprintf("%s=%s", r$weights_used$id, r$weights_used$weight), collapse = "; ")),
      sprintf("# Trends used: %s; bootstrap B=%d; seed=%d; PCA set: %s", s$trend_used, s$B, s$seed, s$pca_set))
  }
  write_with_header <- function(r, x, f) {
    writeLines(header_lines(r), f)
    suppressWarnings(utils::write.table(x, f, sep = ",", append = TRUE, row.names = FALSE, qmethod = "double"))
  }
  output$dl_results <- downloadHandler(function() paste0("reef_tool_results_", Sys.Date(), ".csv"),
                                       function(f) write_with_header(res(), site_table(res(), digits = 4), f))
  output$dl_donors <- downloadHandler(function() paste0("reef_tool_donors_", Sys.Date(), ".csv"),
                                      function(f) write_with_header(res(), fmt(res()$donors, 4), f))
  output$dl_sens <- downloadHandler(function() paste0("reef_tool_sensitivity_", Sys.Date(), ".csv"),
                                    function(f) write_with_header(res(), merge(res()$sensitivity$by_site, res()$sensitivity$presets, by = "site"), f))
  output$dl_traj <- downloadHandler(function() paste0("reef_tool_trajectories_", Sys.Date(), ".csv"),
                                    function(f) { t <- res()$trajectories
                                      write_with_header(res(), if (is.null(t)) data.frame(note = "No trajectories: dated surveys in at least two periods are needed.") else fmt(t, 4), f) })
  output$dl_trends <- downloadHandler(function() paste0("reef_tool_trends_", Sys.Date(), ".csv"),
                                      function(f) { t <- res()$trends$table
                                        write_with_header(res(), if (is.null(t)) data.frame(note = "Trends not requested.") else fmt(t, 4), f) })
  # ---- optional bleaching-event mode (v2.5.0) -------------------------------------
  # Uses the uploaded data directly (all dates, whatever the data period) and runs
  # only when asked, so the standard analysis is unaffected.
  output$evIntro <- renderUI(HTML(paste0(
    "<p>Compare surveys made <strong>before</strong>, <strong>during</strong> and <strong>after</strong> a bleaching event. ",
    "Choose the dates of each phase you have. With surveys <em>before</em> the event, the comparison works with <em>during</em>, ",
    "<em>after</em> or both, so it can be used while an event is still under way. Without them (untick <em>I have surveys before the event</em>), surveys <em>during</em> the event are compared with those <em>after</em> it; losses before the first survey are then missed. For each site you get bleaching during the event, ",
    "the change in coral, turf and CCA cover since before the event (with 95% bootstrap intervals) and the change across the reef.</p>",
    "<p class='note'>These are observed changes, not forecasts of coral loss. The standard analysis (Overview, Decision plane, ",
    "Recommendations) does not depend on this tab; run it on the most recent survey period to see current priorities.</p>")))
  ev <- eventReactive(input$ev_run, {
    on.exit(running_done(), add = TRUE)
    p <- prep()
    if (is.null(p)) { showNotification("Upload a transect CSV first.", type = "error"); req(FALSE) }
    if (!p$has_dates) { showNotification("The event comparison needs survey dates.", type = "error"); req(FALSE) }
    out <- withProgress(message = "Comparing survey phases\u2026", value = 0.3, {
      tryCatch(event_comparison(p$df, event_phases_from_settings(current_settings()), B = input$B, seed = input$seed),
               error = function(e) { showModal(modalDialog(title = "The comparison could not run", conditionMessage(e), easyClose = TRUE)); NULL })
    })
    req(out)
    out
  })
  output$evSummary <- renderUI({
    e <- ev(); r <- e$reef
    HTML(paste0(
      sprintf("<p><strong>Across the %d sites compared</strong>, live coral cover changed from %.1f%% (%s) to %.1f%% (%+.1f percentage points; 95%% interval %+.1f to %+.1f). ",
              r$sites_compared, r$coral_reference_pct, r$reference_phase, r$coral_latest_pct, r$coral_change_pp, r$coral_change_low, r$coral_change_high),
      sprintf("%d site(s) show a clear loss.", r$sites_with_clear_loss),
      if (!is.na(r$sites_with_severe_bleaching)) sprintf(" %d site(s) had severe bleaching (more than 50%% of colonies) during the event.", r$sites_with_severe_bleaching) else "",
      "</p><ul>", paste0("<li>", htmltools::htmlEscape(e$notes), "</li>", collapse = ""), "</ul>"))
  })
  output$evPlot <- renderPlot({ p <- plot_event_change(ev(), base_size = 14); req(p); p })
  output$evTable <- renderTable(fmt(ev()$sites, 1))
  output$dl_event <- downloadHandler(function() paste0("reef_tool_event_", Sys.Date(), ".csv"), function(f) {
    e <- ev()
    writeLines(c(sprintf("# Reef Restoration Prioritisation Tool v%s, bleaching-event comparison, run %s", APP_VERSION, format(Sys.time(), "%Y-%m-%d %H:%M")),
                 paste0("# Phases: ", paste(sprintf("%s %s to %s", e$phases$phase, e$phases$from, e$phases$to), collapse = "; ")),
                 sprintf("# Bootstrap B=%d; seed=%d. Observed change, not a forecast.", e$B, e$seed)), f)
    suppressWarnings(utils::write.table(e$sites, f, sep = ",", append = TRUE, row.names = FALSE, qmethod = "double"))
  })

  output$dl_settings <- downloadHandler(function() paste0("reef_tool_settings_", Sys.Date(), ".csv"),
                                        function(f) utils::write.csv(settings_table(current_settings(), current_weights()), f, row.names = FALSE))
  output$dl_plot <- downloadHandler(function() paste0("reef_tool_decision_plane_", Sys.Date(), ".png"),
                                    function(f) ggplot2::ggsave(f, plot_decision_plane(res(), base_size = 12), width = 8, height = 7, dpi = 300, bg = "white"))
}

shinyApp(ui, server)
