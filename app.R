# ============================================================
# app.R : Brent running-forecast dashboard
# ============================================================
suppressPackageStartupMessages({
  library(plotly)
  library(shiny)
  library(tidyverse)
  library(forecast)
  library(rugarch)
})

# Use current working directory relative path for server compatibility
proj <- "."
P <- function(f) file.path(proj, f)

H   <- 8
z80 <- qnorm(0.90)
z95 <- qnorm(0.975)

# ---------- DATA ----------
get_data <- function() {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?",
                "id=DCOILBRENTEU&fq=Weekly%2C%20Ending%20Friday&fam=avg")
  d <- tryCatch(read_csv(url, show_col_types = FALSE), error = function(e) NULL)
  src <- "FRED (live download)"
  if (is.null(d) || nrow(d) < 100) {
    backup_file <- P("DCOILBRENTEU-2.csv")
    if (file.exists(backup_file)) {
      d <- read_csv(backup_file, show_col_types = FALSE)
      src <- "local backup file (download failed)"
    } else {
      stop("Could not fetch data from FRED and backup file missing.")
    }
  }
  d <- d %>%
    rename(date = 1, price = 2) %>%
    mutate(date = as.Date(date), price = suppressWarnings(as.numeric(price))) %>%
    drop_na() %>% 
    arrange(date)
  attr(d, "source") <- src
  d
}

# ---------- MODELS ----------
pack <- function(f) tibble(mean = as.numeric(f$mean),
                           sd   = as.numeric(f$upper[, 1] - f$mean) / z80)
fit_naive <- function(x, h) pack(naive(x, h = h, level = 80))
fit_ets   <- function(x, h) pack(forecast(ets(x), h = h, level = 80))
fit_arima <- function(x, h) pack(forecast(auto.arima(x), h = h, level = 80))
fit_garch <- function(x, h) {
  r    <- as.numeric(diff(x))
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                     distribution.model = "std")
  fit  <- ugarchfit(spec, r, solver = "hybrid")
  f    <- ugarchforecast(fit, n.ahead = h)
  tibble(mean = as.numeric(tail(x, 1)) + cumsum(as.numeric(fitted(f))),
         sd   = sqrt(cumsum(as.numeric(sigma(f))^2)))
}
models <- list(naive = fit_naive, ets = fit_ets, arima = fit_arima, garch = fit_garch)

make_forecasts <- function(d) {
  y <- ts(log(d$price), frequency = 1)
  origin <- max(d$date)
  imap_dfr(models, function(fn, nm) {
    fc <- tryCatch(fn(y, H), error = function(e) NULL)
    if (is.null(fc)) return(NULL)
    tibble(run_time = format(Sys.time(), "%Y-%m-%d %H:%M"),
           origin = origin, model = nm, horizon = 1:H,
           target_date = origin + 7 * (1:H),
           log_mean = fc$mean, log_sd = fc$sd)
  })
}

# ---------- LOG ----------
log_types <- cols(run_time = col_character(), origin = col_date(),
                  model = col_character(), horizon = col_integer(),
                  target_date = col_date(), log_mean = col_double(),
                  log_sd = col_double())
read_log <- function() {
  if (file.exists(P("forecast_log.csv"))) {
    read_csv(P("forecast_log.csv"), col_types = log_types)
  } else {
    NULL
  }
}
append_log <- function(new) {
  old <- read_log()
  if (!is.null(old)) new <- anti_join(new, old, by = c("origin", "model"))
  all <- bind_rows(old, new)
  try(write_csv(all, P("forecast_log.csv")), silent = TRUE)
  all
}

# ---------- EVENTS ----------
events <- tibble(
  date  = as.Date(c("1990-08-02", "2008-07-11", "2014-11-27", "2020-04-20", "2022-03-08")),
  label = c("Gulf War", "2008 peak", "2014-16 glut", "COVID crash", "Ukraine war"))

event_layout <- function(p, ev) {
  if (nrow(ev) == 0) return(p)
  shapes <- lapply(ev$date, function(d) list(
    type = "line", x0 = as.character(d), x1 = as.character(d),
    y0 = 0, y1 = 1, yref = "paper", line = list(color = "grey", dash = "dot")))
  anns <- lapply(seq_len(nrow(ev)), function(i) list(
    x = as.character(ev$date[i]), y = 1, yref = "paper", text = ev$label[i],
    showarrow = FALSE, textangle = -90, xanchor = "left", yanchor = "top",
    font = list(size = 10)))
  layout(p, shapes = shapes, annotations = anns)
}

roll_sd <- function(x, k) {
  out <- rep(NA_real_, length(x))
  if (length(x) >= k) for (i in k:length(x)) out[i] <- sd(x[(i - k + 1):i])
  out
}

prob_fn <- function(thr, m, s, dir) {
  p <- 1 - pnorm((log(thr) - m) / s)
  if (dir == "below") 1 - p else p
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("Brent Crude: Running Weekly Forecast"),
  uiOutput("cards"),
  htmlOutput("status"),
  hr(),
  fluidRow(
    column(3, selectInput("origin", "Forecast made from week ending", choices = NULL)),
    column(3, sliderInput("level", "Range confidence (%)", 50, 99, 80, step = 1)),
    column(3, sliderInput("vmult", "Volatility what-if (x model estimate)", 0.5, 2, 1, step = 0.1)),
    column(3, br(), actionButton("refresh", "Check for new data now"),
           br(), br(), downloadButton("dl_fc", "Download forecast CSV"))),
  tabsetPanel(
    tabPanel("Live forecast", br(),
             fluidRow(
               column(3, selectInput("model", "Model", c("garch", "arima", "ets", "naive"), "garch"),
                      helpText("Drag the sliders above: confidence changes how wide the range is. ",
                               "The volatility slider asks 'what if markets were calmer or stormier ",
                               "than the model thinks?' (it changes the ranges, not the center).")),
               column(9, plotlyOutput("fc_plot", height = 430))),
             h4("Next-week forecast, all models"), tableOutput("next_tbl")),
    tabPanel("Compare models", br(), plotlyOutput("cmp_plot", height = 480),
             helpText("Black = actual prices (extends into the forecast period as new weeks arrive).")),
    tabPanel("Risk calculator", br(),
             fluidRow(
               column(3,
                      numericInput("thr", "Price threshold ($/barrel)", 130, min = 10, max = 400, step = 5),
                      sliderInput("wk", "Weeks ahead", 1, 8, 4, step = 1),
                      radioButtons("dir", "Probability that Brent is...",
                                   c("above threshold" = "above", "below threshold" = "below")),
                      helpText("Uses each model's forecast and uncertainty, assuming a roughly normal ",
                               "spread of log prices. A rough guide, not a guarantee.")),
               column(9, tableOutput("risk_tbl"), plotlyOutput("risk_plot", height = 330)))),
    tabPanel("History & volatility", br(),
             fluidRow(
               column(4, dateRangeInput("rng", "Date range", start = "2000-01-01", end = Sys.Date())),
               column(3, br(), checkboxInput("logy", "Log scale", FALSE)),
               column(3, selectInput("volwin", "Volatility window (weeks)", c(13, 26, 52), 26))),
             plotlyOutput("hist_plot", height = 320),
             plotlyOutput("vol_plot", height = 280),
             helpText("Volatility = how much the price jumps around week to week. ",
                      "Notice that big-move periods bunch together. That is why we use GARCH.")),
    tabPanel("Scoreboard", br(),
             h4("Live results (only real outcomes)"), tableOutput("board"),
             plotOutput("board_plot", height = 330),
             helpText("RMSE_pct = typical error in percent. cov80 should be near 0.80, ",
                      "cov95 near 0.95. vs_naive below 1 means the model beat 'next week = this week'.")),
    tabPanel("Forecast vs actual", br(), tableOutput("detail")),
    tabPanel("Long-run backtest", br(),
             helpText("60 rolling past forecasts per model (from your backtest.rds file)."),
             tableOutput("bt_tbl")),
    tabPanel("How it works", br(), htmlOutput("how"))
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  state <- reactiveValues(df = NULL, log = NULL, note = "", checked = NULL)
  tick  <- reactiveTimer(6 * 60 * 60 * 1000)
  
  refresh <- function() {
    withProgress(message = "Fetching data from FRED...", value = 0.2, {
      d <- get_data()
      state$df <- d
      lg <- read_log()
      if (is.null(lg) || !(max(d$date) %in% lg$origin)) {
        setProgress(0.5, message = "New week found. Fitting 4 models...")
        lg <- append_log(make_forecasts(d))
        state$note <- paste("New forecasts logged for week ending", max(d$date))
      } else {
        state$note <- "No new week yet. Showing existing forecasts."
      }
      state$log     <- lg
      state$checked <- format(Sys.time(), "%b %d, %H:%M")
    })
    ch  <- format(sort(unique(state$log$origin), decreasing = TRUE))
    sel <- isolate(input$origin)
    updateSelectInput(session, "origin", choices = ch,
                      selected = if (!is.null(sel) && sel %in% ch) sel else ch[1])
  }
  observe({ tick(); refresh() })
  observeEvent(input$refresh, refresh())
  
  fc_all <- reactive({
    req(input$origin, state$log)
    zl <- qnorm(0.5 + input$level / 200)
    state$log %>%
      filter(origin == as.Date(input$origin)) %>%
      mutate(point = exp(log_mean),
             lo = exp(log_mean - zl * log_sd * input$vmult),
             hi = exp(log_mean + zl * log_sd * input$vmult))
  })
  
  scored <- reactive({
    req(state$log, state$df)
    state$log %>%
      inner_join(state$df %>% transmute(target_date = date, actual = log(price)),
                 by = "target_date") %>%
      mutate(err = actual - log_mean,
             in80 = abs(err) <= z80 * log_sd,
             in95 = abs(err) <= z95 * log_sd)
  })
  
  output$cards <- renderUI({
    req(state$df, input$model)
    d <- state$df; n <- nrow(d)
    r <- diff(log(d$price)); w52 <- tail(d$price, 52)
    f1 <- fc_all() %>% filter(horizon == 1, model == input$model)
    req(nrow(f1) == 1)
    card <- function(title, value, sub) column(3, div(
      style = "background:#f4f6f8;border-radius:8px;padding:10px;text-align:center;margin-bottom:8px;",
      div(style = "font-size:12px;color:#666;", title),
      div(style = "font-size:26px;font-weight:bold;", value),
      div(style = "font-size:12px;color:#666;", sub)))
    fluidRow(
      card("Latest complete week", paste0("$", round(d$price[n], 1)),
           paste0(sprintf("%+.1f", 100 * (d$price[n] / d$price[n - 1] - 1)), "% vs prior week")),
      card("52-week range", paste0("$", round(min(w52)), " to $", round(max(w52))), "low to high"),
      card("Recent volatility", paste0(round(sd(tail(r, 26)) * 100, 1), "%"),
           paste0("26-wk weekly sd (long-run avg ", round(sd(r) * 100, 1), "%)")),
      card(paste("Forecast for", format(f1$target_date)), paste0("$", round(f1$point, 1)),
           paste0(input$level, "% range: $", round(f1$lo), " to $", round(f1$hi))))
  })
  
  output$status <- renderUI({
    req(state$df, state$log)
    HTML(paste0("<small><b>Source:</b> ", attr(state$df, "source"),
                " &nbsp;|&nbsp; <b>Forecast weeks logged:</b> ", n_distinct(state$log$origin),
                " &nbsp;|&nbsp; <b>Forecasts scored so far:</b> ", nrow(scored()),
                " &nbsp;|&nbsp; <b>Last checked:</b> ", state$checked,
                " &nbsp;|&nbsp; <i>", state$note, "</i></small>"))
  })
  
  output$fc_plot <- renderPlotly({
    f <- fc_all() %>% filter(model == input$model)
    req(nrow(f) > 0)
    o      <- as.Date(input$origin)
    hist   <- state$df %>% filter(date >= o - 7 * 40, date <= o)
    actual <- state$df %>% filter(date > o, date <= max(f$target_date))
    p0     <- tail(hist$price, 1)
    fl     <- bind_rows(tibble(target_date = o, point = p0, lo = p0, hi = p0),
                        f %>% select(target_date, point, lo, hi))
    p <- plot_ly() %>%
      add_lines(data = hist, x = ~date, y = ~price, name = "Actual (history)",
                line = list(color = "steelblue", width = 3),
                hovertemplate = "%{x|%b %d, %Y}<br>$%{y:.2f}<extra></extra>") %>%
      add_ribbons(data = fl, x = ~target_date, ymin = ~lo, ymax = ~hi,
                  name = paste0(input$level, "% range"), line = list(width = 0),
                  fillcolor = "rgba(100,100,100,0.25)", hoverinfo = "skip") %>%
      add_lines(data = fl, x = ~target_date, y = ~point, name = "Forecast",
                line = list(color = "black", width = 3, dash = "dash"),
                hovertemplate = "%{x|%b %d, %Y}<br>forecast $%{y:.2f}<extra></extra>")
    if (nrow(actual) > 0)
      p <- p %>% add_markers(data = actual, x = ~date, y = ~price, name = "What happened",
                             marker = list(color = "red", size = 11),
                             hovertemplate = "%{x|%b %d, %Y}<br>actual $%{y:.2f}<extra></extra>")
    p %>% layout(xaxis = list(title = ""), yaxis = list(title = "$ per barrel"),
                 hovermode = "closest", legend = list(orientation = "h"))
  })
  
  output$next_tbl <- renderTable({
    fc_all() %>% filter(horizon == 1) %>%
      left_join(state$df %>% transmute(target_date = date, actual = price), by = "target_date") %>%
      transmute(Model = model, `Target week` = format(target_date),
                Forecast = round(point, 1),
                !!paste0(input$level, "% range") := paste0(round(lo, 1), " to ", round(hi, 1)),
                Actual = ifelse(is.na(actual), "not yet", as.character(round(actual, 1))))
  })
  
  output$cmp_plot <- renderPlotly({
    f <- fc_all(); req(nrow(f) > 0)
    o <- as.Date(input$origin)
    hist <- state$df %>% filter(date >= o - 7 * 26, date <= max(f$target_date))
    p <- plot_ly() %>%
      add_lines(data = hist, x = ~date, y = ~price, name = "Actual",
                line = list(color = "black", width = 4),
                hovertemplate = "%{x|%b %d, %Y}<br>actual $%{y:.2f}<extra></extra>")
    p0 <- state$df$price[state$df$date == o]
    for (m in unique(f$model)) {
      d <- bind_rows(tibble(target_date = o, point = p0),
                     f %>% filter(model == m) %>% select(target_date, point))
      p <- p %>% add_lines(data = d, x = ~target_date, y = ~point, name = m,
                           line = list(width = 2, dash = "dash"),
                           hovertemplate = paste0(m, " $%{y:.2f}<extra></extra>"))
    }
    p %>% layout(xaxis = list(title = ""), yaxis = list(title = "$ per barrel"),
                 hovermode = "x unified", legend = list(orientation = "h"))
  })
  
  output$risk_tbl <- renderTable({
    fc_all() %>% filter(horizon == input$wk) %>%
      transmute(Model = model, `Target week` = format(target_date),
                Forecast = round(point, 1),
                Probability = paste0(round(100 * prob_fn(input$thr, log_mean,
                                                         log_sd * input$vmult, input$dir), 1), "%"))
  })
  
  output$risk_plot <- renderPlotly({
    f <- fc_all() %>% filter(horizon == input$wk, model == input$model)
    req(nrow(f) == 1)
    s <- f$log_sd * input$vmult
    grid <- tibble(x = seq(40, 220, by = 2)) %>%
      mutate(p = prob_fn(x, f$log_mean, s, input$dir))
    plot_ly() %>%
      add_lines(data = grid, x = ~x, y = ~p, name = "Probability",
                hovertemplate = "$%{x}: %{y:.1%}<extra></extra>") %>%
      add_markers(x = input$thr, y = prob_fn(input$thr, f$log_mean, s, input$dir),
                  name = "Your threshold", marker = list(color = "red", size = 12)) %>%
      layout(title = paste0("Chance Brent is ", input$dir, " each price in ", input$wk,
                            " weeks (", input$model, ")"),
             xaxis = list(title = "Price ($/barrel)"),
             yaxis = list(title = "Probability", tickformat = ".0%", range = c(0, 1)))
  })
  
  output$hist_plot <- renderPlotly({
    req(state$df, input$rng)
    d  <- state$df %>% filter(date >= input$rng[1], date <= input$rng[2])
    ev <- events %>% filter(date >= input$rng[1], date <= input$rng[2])
    plot_ly(d, x = ~date, y = ~price, type = "scatter", mode = "lines",
            line = list(color = "steelblue"), name = "Brent",
            hovertemplate = "%{x|%b %d, %Y}<br>$%{y:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "$ per barrel", type = if (input$logy) "log" else "linear")) %>%
      event_layout(ev)
  })
  
  output$vol_plot <- renderPlotly({
    req(state$df, input$rng, input$volwin)
    r  <- diff(log(state$df$price))
    v  <- tibble(date = state$df$date[-1],
                 vol  = roll_sd(r, as.integer(input$volwin)) * 100) %>%
      filter(date >= input$rng[1], date <= input$rng[2])
    ev <- events %>% filter(date >= input$rng[1], date <= input$rng[2])
    plot_ly(v, x = ~date, y = ~vol, type = "scatter", mode = "lines",
            line = list(color = "firebrick"), name = "Rolling volatility",
            hovertemplate = "%{x|%b %d, %Y}<br>%{y:.1f}% per week<extra></extra>") %>%
      add_lines(y = sd(r) * 100, name = "Long-run average",
                line = list(color = "grey", dash = "dash")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = paste0(input$volwin, "-week volatility (%)"))) %>%
      event_layout(ev)
  })
  
  output$board <- renderTable({
    s <- scored()
    validate(need(nrow(s) > 0, "No outcomes yet. Scores appear after the next FRED release."))
    b <- s %>% group_by(model) %>%
      summarise(n = n(), RMSE_pct = sqrt(mean(err^2)) * 100,
                cov80 = mean(in80), cov95 = mean(in95), .groups = "drop")
    if ("naive" %in% b$model) b$vs_naive <- b$RMSE_pct / b$RMSE_pct[b$model == "naive"]
    b %>% arrange(RMSE_pct)
  })
  
  output$board_plot <- renderPlot({
    s <- scored()
    validate(need(nrow(s) > 0, ""))
    s %>% group_by(model, horizon) %>%
      summarise(RMSE_pct = sqrt(mean(err^2)) * 100, .groups = "drop") %>%
      ggplot(aes(horizon, RMSE_pct, colour = model)) +
      geom_line(linewidth = 1) + geom_point(size = 3) +
      labs(x = "Weeks ahead", y = "RMSE (% approx.)", title = "Live error by horizon") +
      theme_minimal(base_size = 14)
  })
  
  output$detail <- renderTable({
    s <- scored()
    validate(need(nrow(s) > 0, "No outcomes yet."))
    s %>% arrange(desc(target_date), model, horizon) %>%
      transmute(Model = model, `Made from` = format(origin), `Target week` = format(target_date),
                Horizon = horizon, Forecast = round(exp(log_mean), 1),
                Actual = round(exp(actual), 1),
                `Error %` = round(100 * (exp(actual) - exp(log_mean)) / exp(actual), 1),
                `In 80%` = ifelse(in80, "yes", "no"))
  })
  
  output$bt_tbl <- renderTable({
    f <- P("backtest.rds")
    validate(need(file.exists(f), "Save your backtest first: saveRDS(bt, 'backtest.rds') in the project folder."))
    bt <- readRDS(f)
    validate(need(all(c("mean", "sd", "actual", "model") %in% names(bt)),
                  "backtest.rds is from the older version. Rerun Step 6 of the cleaner script."))
    b <- bt %>% mutate(err = actual - mean) %>% group_by(model) %>%
      summarise(n = n(), RMSE_pct = sqrt(mean(err^2)) * 100,
                cov80 = mean(abs(err) <= z80 * sd),
                cov95 = mean(abs(err) <= z95 * sd), .groups = "drop")
    b$vs_naive <- b$RMSE_pct / b$RMSE_pct[b$model == "naive"]
    b %>% arrange(RMSE_pct)
  })
  
  output$dl_fc <- downloadHandler(
    filename = function() paste0("brent_forecast_", input$origin, ".csv"),
    content  = function(file) {
      write_csv(fc_all() %>%
                  transmute(model, origin, target_date, horizon,
                            forecast = round(point, 2), lower = round(lo, 2), upper = round(hi, 2),
                            confidence_pct = input$level, vol_multiplier = input$vmult), file)
    })
  
  output$how <- renderUI(HTML(paste0(
    "<h4>What this system does</h4><ol>",
    "<li><b>Fetches</b> weekly Brent prices (week ending Friday) from FRED.</li>",
    "<li><b>Forecasts</b> 8 weeks ahead with four models: naive, ETS, ARIMA, ARIMA-GARCH, ",
    "all fit on log prices.</li>",
    "<li><b>Logs</b> every forecast with a timestamp in an append-only file, ",
    "so forecasts are never edited after the fact.</li>",
    "<li><b>Scores</b> each forecast against the real price once it arrives.</li></ol>",
    "<h4>Limits</h4><p>The models only use past prices, so they cannot anticipate supply shocks. ",
    "The risk calculator assumes a roughly normal spread in log prices, so treat it as a rough guide. ",
    "Early scoreboards have few data points; the long-run backtest gives the broader comparison.</p>")))
}

shinyApp(ui, server)
