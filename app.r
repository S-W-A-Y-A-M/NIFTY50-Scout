library(shiny)
library(quantmod)
library(bslib)
library(TTR)

# --- 1. CONFIGURATION ---
# The "Hunting Ground" - Top liquid stocks
scan_list <- c(
  "^NSEI",
  "ADANIENT.NS", "ADANIPORTS.NS", "APOLLOHOSP.NS", "ASIANPAINT.NS",
  "AXISBANK.NS", "BAJAJ-AUTO.NS", "BAJFINANCE.NS", "BAJAJFINSV.NS",
  "BPCL.NS", "BHARTIARTL.NS", "BRITANNIA.NS", "CIPLA.NS",
  "COALINDIA.NS", "DIVISLAB.NS", "DRREDDY.NS", "EICHERMOT.NS",
  "GRASIM.NS", "HCLTECH.NS", "HDFCBANK.NS", "HDFCLIFE.NS",
  "HEROMOTOCO.NS", "HINDALCO.NS", "HINDUNILVR.NS", "ICICIBANK.NS",
  "ITC.NS", "INDUSINDBK.NS", "INFY.NS", "JSWSTEEL.NS",
  "KOTAKBANK.NS", "LT.NS", "LTIM.NS", "M&M.NS",
  "MARUTI.NS", "NESTLEIND.NS", "NTPC.NS", "ONGC.NS",
  "POWERGRID.NS", "RELIANCE.NS", "SBIN.NS", "SUNPHARMA.NS",
  "TCS.NS", "TATACONSUM.NS", "TATAMOTORS.NS", "TATASTEEL.NS",
  "TECHM.NS", "TITAN.NS", "ULTRACEMCO.NS", "WIPRO.NS"
)

# --- 2. DATA ENGINE ---

get_technical_analysis <- function(symbol) {
  
  # DATA FETCHING
  data <- tryCatch({
    fetch_sym <- symbol
    if(symbol == "^NSEI") fetch_sym <- "NIFTYBEES.NS" # Proxy for speed
    
    # Get sufficient data for EMA 50
    getSymbols(fetch_sym, src = "yahoo", auto.assign = FALSE, from = Sys.Date() - 400)
  }, error = function(e) { return(NULL) })
  
  # Data Cleaning: Remove NAs to prevent "Series contains non-leading NAs" error
  data <- na.omit(data)
  
  # Ensure we have enough data rows
  if (is.null(data) || nrow(data) < 60) return(NULL)
  
  # INDICATORS
  cl <- Cl(data)
  
  # Wrap indicators in tryCatch to prevent crashing on calculation errors
  res <- tryCatch({
    list(
      rsi = tail(RSI(cl, n = 14), 1),
      ema20 = tail(EMA(cl, n = 20), 1),
      ema50 = tail(EMA(cl, n = 50), 1),
      macd = tail(MACD(cl, nFast=12, nSlow=26, nSig=9), 1),
      price = as.numeric(tail(cl, 1)),
      change = as.numeric(tail(ROC(cl), 1)) * 100
    )
  }, error = function(e) return(NULL))
  
  if(is.null(res)) return(NULL)
  
  # EXTRACT VALUES SAFELY (Handle potential NAs)
  rsi_val <- as.numeric(res$rsi)
  ema20_val <- as.numeric(res$ema20)
  ema50_val <- as.numeric(res$ema50)
  macd_line <- as.numeric(res$macd[,"macd"])
  macd_sig <- as.numeric(res$macd[,"signal"])
  
  # SCORING ALGORITHM (Strict)
  score <- 50
  
  # 1. Trend (EMA)
  if (!is.na(ema20_val) && res$price > ema20_val) score <- score + 15 else score <- score - 10
  if (!is.na(ema20_val) && !is.na(ema50_val) && ema20_val > ema50_val) score <- score + 10
  
  # 2. Momentum (RSI)
  if (!is.na(rsi_val)) {
    if (rsi_val > 50 && rsi_val < 70) score <- score + 10 # Sweet spot
    if (rsi_val < 40) score <- score - 20 # Weak
    if (rsi_val > 75) score <- score - 5 # Overbought risk
  }
  
  # 3. MACD
  if (!is.na(macd_line) && !is.na(macd_sig) && macd_line > macd_sig) score <- score + 15 else score <- score - 15
  
  # VERDICT LOGIC
  verdict <- "IGNORE"
  color <- "secondary" 
  
  if (score >= 75) { 
    verdict <- "STRONG BUY"; 
    color <- "success" # Green
  } else if (score >= 60) {
    verdict <- "WATCH"; 
    color <- "warning" # Yellow
  } else if (score <= 40) {
    verdict <- "TRASH (DUMP)"; 
    color <- "danger" # Red
  } else {
    verdict <- "MEDIOCRE";
    color <- "dark"
  }
  
  # Format signals for display
  ema_sig <- "Neutral"
  if(!is.na(ema20_val)) ema_sig <- ifelse(res$price > ema20_val, "Bullish (>EMA20)", "Bearish (<EMA20)")
  
  macd_sig_text <- "Neutral"
  if(!is.na(macd_line)) macd_sig_text <- ifelse(macd_line > macd_sig, "Bullish Cross", "Bearish Cross")

  # Name Fix
  display_name <- symbol
  if (symbol == "NIFTYBEES.NS" || symbol == "^NSEI") display_name <- "MARKET TREND (NIFTY)"
  
  list(
    symbol = display_name,
    real_ticker = symbol,
    price = res$price,
    change = round(res$change, 2),
    rsi = round(rsi_val, 1),
    score = score,
    verdict = verdict,
    color = color,
    ema_signal = ema_sig,
    macd_signal = macd_sig_text,
    history = data
  )
}

# --- 3. UI ---
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "litera"), 
  title = "NIFTY50-Scout",
  
  sidebar = sidebar(
    selectizeInput("stock_input", "Analyze Asset", choices = scan_list, 
                   options = list(placeholder = 'Search...')),
    actionButton("run_analysis", "Judge Stock", class = "btn-dark w-100"),
    hr(),
    h6("Market Scan"),
    actionButton("check_nifty", "SCAN NIFTY50", class = "btn-danger w-100", icon = bsicons::bs_icon("crosshair")),
    br(), br(),
    div(class="text-muted small", "")
  ),
  
  # DASHBOARD
  div(
    class = "container-fluid",
    
    # 1. VERDICT CARD (Removed AI Comments Column)
    card(
      min_height = "180px",
      card_header("Ruthless Verdict", class = "bg-dark text-white"),
      div(
        class = "p-4 text-center",
        h2(textOutput("stock_name"), style = "margin-bottom: 5px; font-weight: 900;"),
        uiOutput("verdict_badge"),
        h3(textOutput("stock_price"), style = "color: #555; margin-top: 10px;")
      )
    ),
    
    br(),
    
    # 2. THE HIT LIST
    uiOutput("hit_list_card"),
    
    br(),
    
    # 3. METRICS & CHART
    layout_columns(
      col_widths = breakpoints(xs = 12, md = 4, lg = 4),
      
      card(
        card_header("Technical DNA"),
        tableOutput("metrics_table")
      ),
      
      card(
        card_header("Scorecard"),
        div(
          class = "text-center p-4",
          uiOutput("score_circle"),
          p("Score < 70 = IGNORE", class = "small text-muted pt-2")
        )
      ),
      
      card(
        card_header("6-Month Trend"),
        plotOutput("trend_chart", height = "250px")
      )
    )
  )
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  
  current_data <- reactiveVal(NULL)
  hit_list_data <- reactiveVal(NULL)
  
  # MAIN ANALYSIS FUNCTION
  process_stock <- function(symbol) {
    withProgress(message = paste("Analyzing", symbol, "..."), {
      res <- get_technical_analysis(symbol)
      if (is.null(res)) {
        showNotification("Data Error. Asset ignored.", type = "warning")
        return(NULL)
      }
      current_data(res)
      return(res)
    })
  }
  
  # AUTO-RUN: Check Market on Startup
  observe({
    isolate({ process_stock("^NSEI") })
  }, priority = 10)
  
  # BUTTON 1: Analyze Single
  observeEvent(input$run_analysis, {
    req(input$stock_input)
    hit_list_data(NULL) 
    process_stock(input$stock_input)
  })
  
  # BUTTON 2: SCAN FOR BUYS (The Hit List)
  observeEvent(input$check_nifty, {
    
    withProgress(message = "HUNTING FOR TARGETS...", value = 0, {
      
      valid_buys <- data.frame(Symbol=character(), Price=numeric(), Score=numeric(), Verdict=character())
      
      count <- 0
      total <- length(scan_list)
      
      for(sym in scan_list) {
        count <- count + 1
        incProgress(1/total, detail = paste("Scanning", sym))
        
        res <- get_technical_analysis(sym)
        
        # FILTER: ONLY SHOW STRONG STOCKS (Score >= 70)
        if(!is.null(res) && res$score >= 70) {
          new_row <- data.frame(
            Symbol = sym, 
            Price = res$price, 
            Score = res$score, 
            Verdict = res$verdict
          )
          valid_buys <- rbind(valid_buys, new_row)
        }
      }
      
      hit_list_data(valid_buys)
    })
  })
  
  # --- RENDERERS ---
  
  output$stock_name <- renderText({ req(current_data()); current_data()$symbol })
  
  output$verdict_badge <- renderUI({
    req(current_data())
    d <- current_data()
    color_code <- if(d$color == "success") "#28a745" else if(d$color == "danger") "#dc3545" else "#ffc107"
    h1(d$verdict, style = paste0("font-weight: 900; color: ", color_code, ";"))
  })
  
  output$stock_price <- renderText({
    req(current_data())
    d <- current_data()
    paste0("₹", format(d$price, big.mark=","))
  })
  
  # THE HIT LIST UI
  output$hit_list_card <- renderUI({
    req(hit_list_data())
    df <- hit_list_data()
    
    if(nrow(df) == 0) {
      div(class="alert alert-danger text-center", h4("MARKET WEAK. NO APPROVED BUYS FOUND."))
    } else {
      card(
        card_header("APPROVED BUY LIST (Score > 70)", class="bg-success text-white"),
        tableOutput("hit_list_table")
      )
    }
  })
  
  output$hit_list_table <- renderTable({
    req(hit_list_data())
    hit_list_data()
  }, width = "100%", striped = TRUE, hover = TRUE)
  
  output$metrics_table <- renderTable({
    req(current_data())
    d <- current_data()
    data.frame(
      Metric = c("RSI (14)", "MACD Signal", "Trend (EMA)"),
      Value = c(d$rsi, d$macd_signal, d$ema_signal)
    )
  }, width = "100%", striped = TRUE)
  
  output$score_circle <- renderUI({
    req(current_data())
    score <- current_data()$score
    color <- if(score >= 70) "#28a745" else if(score <= 40) "#dc3545" else "#ffc107"
    
    div(
      style = paste0(
        "width: 100px; height: 100px; border-radius: 50%; border: 8px solid ", color, ";",
        "margin: 0 auto; display: flex; align-items: center; justify-content: center;",
        "font-size: 2rem; font-weight: bold; color: ", color, ";"
      ),
      score
    )
  })
  
  output$trend_chart <- renderPlot({
    req(current_data())
    chartSeries(current_data()$history, 
                theme = chartTheme("white"), 
                type = "line", 
                TA = "addEMA(n=20, col='blue'); addEMA(n=50, col='red')",
                name = current_data()$symbol)
  })
}

shinyApp(ui, server)