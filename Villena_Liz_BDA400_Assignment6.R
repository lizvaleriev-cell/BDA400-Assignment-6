# ================================================================
# BDA400 - Assignment 6
# Technical Analysis using R - Visualization Phase
# Student: Liz Valerie Villena
# ================================================================

# First-time setup (run once if packages are not installed):
# install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "dplyr"))

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)

ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput(
        "date_range", "Select Date Range:",
        start = Sys.Date() - 365, end = Sys.Date()
      ),
      selectInput(
        "time_frame", "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      selectInput(
        "chart_type", "Chart Type:",
        choices = c("Line", "Candlestick", "Area"),
        selected = "Line"
      ),
      checkboxGroupInput(
        "technical_indicators", "Technical Indicators:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages")
      ),
      sliderInput("short_ma", "Short Moving Average:",
                  min = 5, max = 40, value = 20, step = 1),
      sliderInput("long_ma", "Long Moving Average:",
                  min = 20, max = 100, value = 50, step = 1),
      checkboxInput("show_signals", "Show Buy/Sell Signals", value = TRUE),
      actionButton("refresh", "Refresh Data")
    ),
    mainPanel(
      h3(textOutput("dashboard_title")),
      plotOutput("stock_chart", height = "500px"),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') >= 0",
        plotOutput("rsi_chart", height = "280px")
      ),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('MACD') >= 0",
        plotOutput("macd_chart", height = "300px")
      ),
      h3("Latest Trading Information"),
      tableOutput("latest_table"),
      verbatimTextOutput("summary_text")
    )
  )
)

server <- function(input, output, session) {

  raw_stock <- eventReactive(input$refresh, {
    sym <- toupper(trimws(input$symbol))
    validate(need(nchar(sym) > 0, "Please enter a stock symbol."))

    tryCatch(
      getSymbols(
        sym, src = "yahoo",
        from = input$date_range[1],
        to = input$date_range[2] + 1,
        auto.assign = FALSE,
        warnings = FALSE
      ),
      error = function(e) {
        showNotification(
          paste("Unable to download data for", sym, "-", e$message),
          type = "error", duration = 8
        )
        NULL
      }
    )
  }, ignoreNULL = FALSE)

  stock_data <- reactive({
    x <- raw_stock()
    validate(need(!is.null(x), "No stock data are available."))

    if (input$time_frame == "Weekly") {
      x <- to.weekly(x, indexAt = "lastof", drop.time = TRUE)
    } else if (input$time_frame == "Monthly") {
      x <- to.monthly(x, indexAt = "lastof", drop.time = TRUE)
    }
    x
  })

  analysis_data <- reactive({
    x <- stock_data()
    n <- NROW(x)
    validate(need(n >= 2, "Not enough observations for analysis."))

    close <- as.numeric(Cl(x))
    open  <- as.numeric(Op(x))
    high  <- as.numeric(Hi(x))
    low   <- as.numeric(Lo(x))

    short_n <- min(input$short_ma, max(1, n - 1))
    long_n  <- min(input$long_ma,  max(2, n - 1))
    if (short_n >= long_n) short_n <- max(1, long_n - 1)

    short_ma <- as.numeric(SMA(close, n = short_n))
    long_ma  <- as.numeric(SMA(close, n = long_n))

    # RSI and MACD need enough observations. NA values are expected at the start.
    rsi_n <- min(14, max(2, n - 1))
    rsi_v <- as.numeric(RSI(close, n = rsi_n))

    macd_fast <- min(12, max(2, n - 2))
    macd_slow <- min(26, max(macd_fast + 1, n - 1))
    macd_sig  <- min(9, max(2, n - macd_slow))
    macd_obj <- tryCatch(
      MACD(close, nFast = macd_fast, nSlow = macd_slow,
           nSig = macd_sig, maType = "EMA"),
      error = function(e) matrix(NA_real_, nrow = n, ncol = 2)
    )
    macd_v <- as.numeric(macd_obj[, 1])
    signal_v <- as.numeric(macd_obj[, 2])

    state <- ifelse(short_ma > long_ma, "Buy",
                    ifelse(short_ma < long_ma, "Sell", "Hold"))

    crossover <- rep(NA_character_, n)
    if (n >= 2) {
      crossover[-1] <- ifelse(
        state[-1] == "Buy" & state[-n] != "Buy", "Buy",
        ifelse(state[-1] == "Sell" & state[-n] != "Sell", "Sell", NA)
      )
    }

    data.frame(
      Date = as.Date(index(x)),
      Open = open, High = high, Low = low, Close = close,
      ShortMA = short_ma, LongMA = long_ma,
      RSI = rsi_v, MACD = macd_v, MACDSignal = signal_v,
      Signal = state, Crossover = crossover
    )
  })

  output$dashboard_title <- renderText({
    paste(toupper(trimws(input$symbol)), "-", input$time_frame, "Technical Analysis")
  })

  output$stock_chart <- renderPlot({
    d <- analysis_data()

    p <- ggplot(d, aes(x = Date))

    if (input$chart_type == "Candlestick") {
      candle <- ifelse(d$Close >= d$Open, "Up", "Down")
      d$Candle <- candle
      p <- p +
        geom_segment(aes(y = Low, yend = High, xend = Date)) +
        geom_rect(
          aes(
            xmin = Date - 0.35, xmax = Date + 0.35,
            ymin = pmin(Open, Close), ymax = pmax(Open, Close),
            fill = Candle
          ),
          color = "black", linewidth = 0.2
        ) +
        scale_fill_manual(values = c("Down" = "#D55E5E", "Up" = "#3C8D40"))
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), alpha = 0.35) +
        geom_line(aes(y = Close), linewidth = 0.5)
    } else {
      p <- p + geom_line(aes(y = Close), linewidth = 0.65)
    }

    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = ShortMA, linetype = "Short MA"),
                  linewidth = 0.7, na.rm = TRUE) +
        geom_line(aes(y = LongMA, linetype = "Long MA"),
                  linewidth = 0.7, na.rm = TRUE) +
        scale_linetype_manual(
          name = "Moving Averages",
          values = c("Long MA" = "dashed", "Short MA" = "solid")
        )
    }

    if (isTRUE(input$show_signals)) {
      sig <- d[!is.na(d$Crossover), ]
      if (nrow(sig) > 0) {
        p <- p +
          geom_point(
            data = sig,
            aes(y = Close, shape = Crossover),
            size = 2.5, inherit.aes = FALSE
          ) +
          geom_text(
            data = sig,
            aes(x = Date, y = Close, label = Crossover),
            vjust = -0.7, size = 3, inherit.aes = FALSE
          ) +
          scale_shape_manual(
            name = "Signal",
            values = c("Buy" = 16, "Sell" = 17)
          )
      }
    }

    p +
      labs(
        title = paste(toupper(trimws(input$symbol)), "Stock Price"),
        subtitle = paste(min(d$Date), "to", max(d$Date)),
        x = "Date", y = "Price",
        caption = "Data source: Yahoo Finance via quantmod"
      ) +
      theme_minimal(base_size = 12)
  })

  output$rsi_chart <- renderPlot({
    d <- analysis_data()
    ggplot(d, aes(Date, RSI)) +
      geom_line(na.rm = TRUE) +
      geom_hline(yintercept = 70, linetype = "dashed") +
      geom_hline(yintercept = 30, linetype = "dashed") +
      annotate("text", x = max(d$Date), y = 70,
               label = "Overbought 70", hjust = 1, vjust = -0.4) +
      annotate("text", x = max(d$Date), y = 30,
               label = "Oversold 30", hjust = 1, vjust = 1.4) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = "Relative Strength Index (RSI)", x = "Date", y = "RSI") +
      theme_minimal(base_size = 12)
  })

  output$macd_chart <- renderPlot({
    d <- analysis_data()
    hist <- d$MACD - d$MACDSignal
    md <- data.frame(
      Date = d$Date, MACD = d$MACD,
      Signal = d$MACDSignal, Histogram = hist
    )
    ggplot(md, aes(Date)) +
      geom_col(aes(y = Histogram), alpha = 0.35, na.rm = TRUE) +
      geom_line(aes(y = MACD, linetype = "MACD"), na.rm = TRUE) +
      geom_line(aes(y = Signal, linetype = "Signal"), na.rm = TRUE) +
      geom_hline(yintercept = 0) +
      scale_linetype_manual(
        name = "Lines",
        values = c("MACD" = "solid", "Signal" = "dashed")
      ) +
      labs(
        title = "Moving Average Convergence Divergence (MACD)",
        x = "Date", y = "MACD"
      ) +
      theme_minimal(base_size = 12)
  })

  output$latest_table <- renderTable({
    d <- analysis_data()
    tail(
      transform(
        d[, c("Date", "Close", "ShortMA", "LongMA", "RSI", "MACD", "Signal")],
        Close = round(Close, 2),
        ShortMA = round(ShortMA, 2),
        LongMA = round(LongMA, 2),
        RSI = round(RSI, 2),
        MACD = round(MACD, 2)
      ),
      10
    )
  })

  output$summary_text <- renderText({
    d <- analysis_data()
    cross <- d[!is.na(d$Crossover), c("Date", "Crossover")]
    recent <- if (nrow(cross) == 0) {
      "No crossover in selected period"
    } else {
      paste(cross$Crossover[nrow(cross)], "on", cross$Date[nrow(cross)])
    }

    paste0(
      "Data source: Yahoo Finance\n",
      "Symbol: ", toupper(trimws(input$symbol)), "\n",
      "Observations: ", nrow(d), "\n",
      "Most recent crossover: ", recent
    )
  })
}

shinyApp(ui = ui, server = server)
