library(shiny)
library(quantmod)
library(bslib)
library(TTR)
library(PerformanceAnalytics)
library(httr)      
library(jsonlite)  
library(dotenv)    
library(future)    
library(promises)

# Setup Async Plan
plan(multisession)

# Load API Key
tryCatch({ load_dot_env() }, error = function(e) { message("No .env file found.") })
OPENROUTER_API_KEY <- Sys.getenv("OPENROUTER_API_KEY")

# --- 1. CONFIGURATION ---

sector_map <- list(
  "BANKING" = c("HDFCBANK.NS", "ICICIBANK.NS", "SBIN.NS", "AXISBANK.NS", "KOTAKBANK.NS", "INDUSINDBK.NS", "BAJFINANCE.NS", "BAJAJFINSV.NS", "HDFCLIFE.NS"),
  "IT" = c("TCS.NS", "INFY.NS", "HCLTECH.NS", "WIPRO.NS", "LTIM.NS", "TECHM.NS"),
  "AUTO" = c("TATAMOTORS.NS", "MARUTI.NS", "M&M.NS", "BAJAJ-AUTO.NS", "EICHERMOT.NS", "HEROMOTOCO.NS"),
  "ENERGY" = c("RELIANCE.NS", "NTPC.NS", "ONGC.NS", "POWERGRID.NS", "BPCL.NS", "COALINDIA.NS"),
  "FMCG" = c("ITC.NS", "HINDUNILVR.NS", "NESTLEIND.NS", "BRITANNIA.NS", "TATACONSUM.NS"),
  "METALS" = c("TATASTEEL.NS", "JSWSTEEL.NS", "HINDALCO.NS"),
  "PHARMA" = c("SUNPHARMA.NS", "DRREDDY.NS", "CIPLA.NS", "DIVISLAB.NS", "APOLLOHOSP.NS"),
  "INFRA" = c("LT.NS", "ULTRACEMCO.NS", "GRASIM.NS", "ADANIPORTS.NS"),
  "TELECOM" = c("BHARTIARTL.NS"),
  "CONSUMER" = c("TITAN.NS"),
  "PAINTS" = c("ASIANPAINT.NS"),
  "MINING" = c("ADANIENT.NS")
)

# CHANGED: Removed NIFTYBEES.NS and sorted alphabetically
scan_list <- sort(unique(unlist(sector_map)))

get_sector_name <- function(sym) {
  for(sec in names(sector_map)) {
    if(sym %in% sector_map[[sec]]) return(sec)
  }
  return("OTHER")
}

# --- 2. HELPER: CRASH GUARD ---
is_valid <- function(x) {
  return(!is.null(x) && length(x) > 0 && !is.na(x))
}

# --- 3. AI ENGINE ---

get_ai_double_check <- function(tech_data) {
  if(OPENROUTER_API_KEY == "") return("⚠️ AI Skipped: No API Key found.")
  
  system_prompt <- "You are a skeptical, ruthless trading risk manager. DOUBLE CHECK the verdict. 
  1. If BUY, look for reasons to fail.
  2. If SELL, check if oversold.
  3. Be brief (max 20 words)."
  
  user_prompt <- paste0(
    "Symbol: ", tech_data$symbol, " | Score: ", tech_data$score, "/100 | Verdict: ", tech_data$verdict, "\n",
    "RSI: ", tech_data$rsi, " | Vol Ratio: ", tech_data$vol_ratio, "\n",
    "Validate this trade."
  )

  tryCatch({
    response <- POST(
      url = "https://openrouter.ai/api/v1/chat/completions",
      add_headers("Authorization" = paste("Bearer", OPENROUTER_API_KEY), "Content-Type" = "application/json"),
      body = toJSON(list(
        model = "meta-llama/llama-3-8b-instruct", 
        messages = list(list(role = "system", content = system_prompt), list(role = "user", content = user_prompt)),
        max_tokens = 60, temperature = 0.3
      ), auto_unbox = TRUE)
    )
    content(response, "parsed")$choices[[1]]$message$content
  }, error = function(e) "⚠️ AI Connection Failed")
}

get_ai_best_pick <- function(symbol, score, price) {
  if(OPENROUTER_API_KEY == "") return("Configure .env for recommendations.")
  
  prompt <- paste0("Stock ", symbol, " is the top pick with Score ", score, " at price ", price, ". Briefly say why it is the winner (15 words).")
  
  tryCatch({
    response <- POST(
      url = "https://openrouter.ai/api/v1/chat/completions",
      add_headers("Authorization" = paste("Bearer", OPENROUTER_API_KEY), "Content-Type" = "application/json"),
      body = toJSON(list(
        model = "meta-llama/llama-3-8b-instruct", 
        messages = list(list(role = "user", content = prompt)),
        max_tokens = 40, temperature = 0.4
      ), auto_unbox = TRUE)
    )
    content(response, "parsed")$choices[[1]]$message$content
  }, error = function(e) "AI unavailable.")
}

# --- 4. DATA ENGINE ---

get_technical_analysis <- function(symbol, timeframe = "daily", market_score = NULL, top_sector = NULL) {
  
  data <- tryCatch({
    fetch_sym <- symbol
    if(symbol == "^NSEI") fetch_sym <- "NIFTYBEES.NS"
    raw <- getSymbols(fetch_sym, src = "yahoo", auto.assign = FALSE, from = Sys.Date() - 700)
    raw <- na.locf(raw, na.rm = TRUE)
    if(timeframe == "weekly") raw <- to.weekly(raw)
    raw
  }, error = function(e) return(NULL))
  
  if (is.null(data) || nrow(data) < 60) return(NULL)
  
  res <- tryCatch({
    cl <- Cl(data)
    vol <- Vo(data)
    ret <- if(timeframe == "weekly") weeklyReturn(cl) else dailyReturn(cl)
    change_val <- tail(ROC(cl, type = "discrete"), 1) * 100
    
    rsi_series <- RSI(cl, n = 14)
    ema20_series <- EMA(cl, n = 20)
    ema50_series <- EMA(cl, n = 50)
    macd_obj <- MACD(cl, nFast=12, nSlow=26, nSig=9)
    
    list(
      rsi = tail(rsi_series, 1),
      rsi_prev = if (length(rsi_series) > 10) tail(lag(rsi_series, 5), 1) else NA,
      ema20 = tail(ema20_series, 1),
      ema50 = tail(ema50_series, 1),
      ema_slope = tail(ema20_series, 1) - tail(lag(ema20_series, 5), 1),
      macd_line = tail(macd_obj[,"macd"], 1),
      macd_sig  = tail(macd_obj[,"signal"], 1),
      vol_curr = tail(vol, 1),
      vol_avg = tail(SMA(vol, n = 20), 1),
      price = tail(cl, 1),
      change = change_val,
      drawdown = maxDrawdown(cumprod(1 + na.omit(ret)))
    )
  }, error = function(e) return(NULL))
  
  if(is.null(res)) return(NULL)
  
  # Unpack
  price     <- as.numeric(res$price)
  rsi_val   <- as.numeric(res$rsi)
  ema20_val <- as.numeric(res$ema20)
  ema50_val <- as.numeric(res$ema50)
  
  score <- 50
  
  # 1. Market Context
  if (!is.null(market_score) && symbol != "^NSEI" && symbol != "NIFTYBEES.NS") {
    if (market_score < 50) score <- score - 15
  }
  
  # 2. Trend & Slope
  if (is_valid(ema20_val) && price > ema20_val) {
    score <- score + 10
    if (is_valid(res$ema_slope) && res$ema_slope > 0) score <- score + 5
  } else {
    score <- score - 10
  }
  if (is_valid(ema20_val) && is_valid(ema50_val) && ema20_val > ema50_val) score <- score + 10
  
  # 3. Momentum
  if (is_valid(rsi_val)) {
    if (rsi_val > 50 && rsi_val < 70) score <- score + 10
    if (rsi_val < 30) score <- score - 25
    if (rsi_val > 80) score <- score - 10
    
    rsi_prev <- as.numeric(res$rsi_prev)
    if(is_valid(rsi_prev)) {
      if(rsi_val - rsi_prev > 5) score <- score + 5
      if(rsi_val > rsi_prev && res$change < 0) score <- score + 5 
    }
  }
  
  # 4. Volume (Skip Weekly)
  vol_ratio <- 1.0 
  if (timeframe == "daily") {
    vol_curr <- as.numeric(res$vol_curr)
    vol_avg  <- as.numeric(res$vol_avg)
    if (is_valid(vol_curr) && is_valid(vol_avg) && vol_avg > 0) {
      vol_ratio <- round(vol_curr / vol_avg, 2)
      if (vol_ratio > 1.3) score <- score + 10
      if (vol_ratio < 0.7) score <- score - 10
    }
  }
  
  # 5. Drawdown
  dd <- as.numeric(res$drawdown)
  if (is_valid(dd) && dd < -0.20) score <- score - 15
  
  # 6. MACD
  m_line <- as.numeric(res$macd_line)
  m_sig  <- as.numeric(res$macd_sig)
  if (is_valid(m_line) && is_valid(m_sig)) {
    if (abs(m_line - m_sig) < 0.02) {
      score <- score + 2 
    } else if (m_line > m_sig) {
      score <- score + 5
    } else {
      score <- score - 5
    }
  }
  
  # Sector Bias
  if (!is.null(top_sector)) {
    current_stock_sector <- get_sector_name(symbol)
    if (current_stock_sector == top_sector) {
      score <- score + 5
    }
  }
  
  score <- max(min(score, 100), 0)
  
  verdict <- "MEDIOCRE"
  risk_label <- "⚠️ SPECULATIVE"
  color <- "secondary"
  
  if (score >= 80) { 
    verdict <- "▲ STRONG BUY"; color <- "success"; risk_label <- "💎 HIGH CONVICTION"
  } else if (score >= 75) {
    verdict <- "BUY"; color <- "primary"; risk_label <- "🚀 MOMENTUM"
  } else if (score >= 60) {
    verdict <- "WATCH"; color <- "warning"; risk_label <- "MEDIUM RISK"
  } else if (score <= 40) {
    verdict <- "▼ TRASH (DUMP)"; color <- "danger"; risk_label <- "HIGH RISK"
  }
  
  if (!is.null(market_score) && market_score < 45 && score < 75) {
    verdict <- "NO TRADE ZONE"; color <- "dark"; risk_label <- "⛔ MARKET WEAK"
  }
  
  display_name <- symbol
  if (symbol == "NIFTYBEES.NS" || symbol == "^NSEI") display_name <- "MARKET TREND (NIFTY)"
  
  list(
    symbol = display_name,
    real_ticker = symbol,
    price = price,
    change = round(as.numeric(res$change), 2),
    rsi = round(rsi_val, 1),
    vol_ratio = vol_ratio,
    score = score,
    verdict = verdict,
    color = color,
    risk = risk_label,
    ema_slope_val = round(as.numeric(res$ema_slope), 2),
    history = data,
    ai_check = "Pending..."
  )
}

# --- 5. UI ---
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "litera"), 
  title = "NIFTY50 Scout",
  
  sidebar = sidebar(
    selectInput("timeframe", "Timeframe Strategy:", 
                choices = c("Swing (Daily)" = "daily", "Positional (Weekly)" = "weekly")),
    
    selectizeInput("stock_input", "Analyze Asset", choices = scan_list, 
                   options = list(placeholder = 'Search...')),
    
    actionButton("run_analysis", "Judge Stock", class = "btn-dark w-100"),
    hr(),
    h6("Market Scan"),
    actionButton("scan_market", "Scan NIFTY50", class = "btn-danger w-100", icon = bsicons::bs_icon("radioactive")),
    br(), br(),
    downloadButton("downloadData", "Export Hit List (CSV)", class = "btn-outline-secondary w-100 btn-sm")
  ),
  
  div(
    class = "container-fluid",
    
    # 1. VERDICT CARD
    card(
      min_height = "180px",
      card_header("Quantitative Verdict", class = "bg-dark text-white"),
      layout_columns(
        col_widths = breakpoints(xs = 12, md = 6),
        div(class = "p-4 text-center",
            h2(textOutput("stock_name"), style = "margin-bottom: 5px; font-weight: 900;"),
            uiOutput("verdict_badge"),
            h3(textOutput("stock_price"), style = "color: #555;")
        ),
        div(class = "p-4",
            h5("Confidence Level:", class="text-secondary text-center"),
            div(class="text-center", uiOutput("risk_badge")),
            hr(),
            div(class="alert alert-info", style="margin-top:10px;",
                h6("🤖 AI Double Check (Llama 3):", style="font-weight:bold;"),
                textOutput("ai_commentary"),
                div(class="text-muted small text-center pt-2", "⚠️ AI commentary is informational only.")
            )
        )
      )
    ),
    br(),
    
    # 2. SECTOR & HIT LIST
    layout_columns(
      col_widths = breakpoints(xs = 12, md = 12),
      uiOutput("sector_card"), 
      uiOutput("hit_list_card")
    ),
    br(),
    
    # 3. METRICS
    layout_columns(
      col_widths = breakpoints(xs = 12, md = 4),
      card(card_header("Technical DNA"), tableOutput("metrics_table")),
      card(card_header("Scorecard"), 
           div(class="text-center p-4", uiOutput("score_circle"), p("Target: >75", class="small text-muted"))),
      card(card_header("Trend Chart"), plotOutput("trend_chart", height = "250px"))
    )
  )
)

# --- 6. SERVER ---
server <- function(input, output, session) {
  
  current_data <- reactiveVal(NULL)
  hit_list_data <- reactiveVal(NULL)
  sector_data <- reactiveVal(NULL)
  market_score <- reactiveVal(50) 
  ai_output_val <- reactiveVal("Pending...") 
  ai_busy <- reactiveVal(FALSE) 
  
  process_stock <- function(symbol, tf) {
    withProgress(message = paste("Analyzing", symbol, "..."), {
      
      top_sec_bias <- NULL
      if (!is.null(sector_data()) && nrow(sector_data()) > 0) {
        top_sec_bias <- sector_data()$Sector[1]
      }
      
      res <- get_technical_analysis(symbol, timeframe = tf, market_score = market_score(), top_sector = top_sec_bias)
      if (is.null(res)) { showNotification("Data Unavailable.", type = "warning"); return(NULL) }
      
      if (symbol == "^NSEI" || symbol == "NIFTYBEES.NS") market_score(res$score)
      
      res$ai_check <- "Calculating..."
      current_data(res)
      
      if (!ai_busy() || symbol == "NIFTYBEES.NS") {
        ai_output_val("Checking...")
        ai_busy(TRUE)
        
        should_run_ai <- (res$verdict %in% c("▲ STRONG BUY", "▼ TRASH (DUMP)")) || (symbol == "NIFTYBEES.NS")
        
        future({
          if (should_run_ai) {
            get_ai_double_check(res)
          } else {
            "AI skipped (low-confidence signal)"
          }
        }) %...>% (function(ai_result) {
          ai_output_val(ai_result)
          ai_busy(FALSE)
        })
      } else {
        ai_output_val("AI Busy (Slow down click)...")
      }
      
      return(res)
    })
  }
  
  observe({ isolate({ process_stock("NIFTYBEES.NS", "daily") }) }, priority = 10)
  
  observeEvent(input$run_analysis, {
    req(input$stock_input)
    hit_list_data(NULL)
    sector_data(NULL)
    process_stock(input$stock_input, input$timeframe)
  })
  
  observeEvent(input$scan_market, {
    
    mkt <- get_technical_analysis("NIFTYBEES.NS", input$timeframe)
    curr_mkt_score <- if(!is.null(mkt)) mkt$score else 50
    market_score(curr_mkt_score) 
    
    withProgress(message = paste("SCANNING (", toupper(input$timeframe), ")..."), value = 0, {
      
      rows_list <- list()
      sector_scores <- list()
      total <- length(scan_list)
      count <- 0
      
      for(sym in scan_list) {
        count <- count + 1
        incProgress(1/total)
        
        if (sym == "NIFTYBEES.NS" || sym == "^NSEI") next 
        
        res <- tryCatch({
           get_technical_analysis(sym, timeframe = input$timeframe, market_score = curr_mkt_score)
        }, error = function(e) return(NULL))
        
        if(!is.null(res)) {
          sec_name <- get_sector_name(sym)
          if(res$score >= 0) { 
            new_row <- data.frame(Symbol = sym, Sector = sec_name, Price = res$price, Score = res$score, 
                                  VolRatio = res$vol_ratio, Verdict = res$verdict, Risk = res$risk)
            rows_list[[length(rows_list) + 1]] <- new_row
          }
          if(sec_name != "OTHER") {
            sector_scores[[sec_name]] <- c(sector_scores[[sec_name]], res$score)
          }
        }
      }
      
      if(length(rows_list) > 0) {
        valid_buys <- do.call(rbind, rows_list)
      } else {
        valid_buys <- data.frame(Symbol=character(), Sector=character(), Price=numeric(), Score=numeric(), 
                                 VolRatio=numeric(), Verdict=character(), Risk=character())
      }
      
      sec_df <- data.frame(Sector=character(), MedianScore=numeric())
      for(sec in names(sector_scores)) {
        avg <- median(sector_scores[[sec]], na.rm=TRUE)
        sec_df <- rbind(sec_df, data.frame(Sector=sec, MedianScore=avg))
      }
      sec_df <- sec_df[order(-sec_df$MedianScore), ]
      sector_data(sec_df)
      
      if(nrow(sec_df) > 0 && nrow(valid_buys) > 0) {
        top_sec <- sec_df$Sector[1]
        is_top_sec <- valid_buys$Sector == top_sec
        valid_buys$Score[is_top_sec] <- pmin(valid_buys$Score[is_top_sec] + 5, 100)
        
        valid_buys$Verdict <- ifelse(valid_buys$Score >= 80, "▲ STRONG BUY",
                              ifelse(valid_buys$Score >= 75, "BUY",
                              ifelse(valid_buys$Score >= 60, "WATCH",
                              ifelse(valid_buys$Score <= 40, "▼ TRASH (DUMP)", "MEDIOCRE"))))
        
        valid_buys$Risk <- ifelse(valid_buys$Score >= 80, "💎 HIGH CONVICTION",
                           ifelse(valid_buys$Score >= 75, "🚀 MOMENTUM",
                           ifelse(valid_buys$Score <= 40, "HIGH RISK", "⚠️ SPECULATIVE")))
      }
      
      if(nrow(valid_buys) > 0) {
        final_list <- valid_buys[valid_buys$Score >= 70, ]
        if(nrow(final_list) > 0) final_list <- final_list[order(-final_list$Score), ]
      } else {
        final_list <- valid_buys
      }
      
      hit_list_data(final_list)
      
      if(nrow(final_list) > 0) {
        top_stock <- final_list[1,]
        ai_output_val("Finding Scan Winner...")
        future({
          get_ai_best_pick(top_stock$Symbol, top_stock$Score, top_stock$Price)
        }) %...>% (function(ai_res) {
          ai_output_val(paste("🏆 TOP PICK:", top_stock$Symbol, "-", ai_res))
        })
      } else {
        ai_output_val("Scan Complete: No strong buys found.")
      }
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("hit_list_", input$timeframe, "_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { req(hit_list_data()); write.csv(hit_list_data(), file, row.names = FALSE) }
  )
  
  output$stock_name <- renderText({ req(current_data()); current_data()$symbol })
  
  output$verdict_badge <- renderUI({
    req(current_data()); d <- current_data()
    color_code <- if(d$color == "success") "#28a745" else if(d$color == "primary") "#007bff" else if(d$color == "danger") "#dc3545" else if(d$color == "dark") "#333" else "#ffc107"
    h1(d$verdict, style = paste0("font-weight: 900; color: ", color_code, ";"))
  })
  
  output$risk_badge <- renderUI({
    req(current_data()); d <- current_data()
    color_code <- if(grepl("HIGH CONVICTION", d$risk)) "success" else if(grepl("MOMENTUM", d$risk)) "primary" else if(grepl("HIGH RISK", d$risk)) "danger" else "warning"
    span(class=paste0("badge bg-", color_code), style="font-size: 1.2rem;", d$risk)
  })
  
  output$ai_commentary <- renderText({ ai_output_val() })
  output$stock_price <- renderText({ req(current_data()); d <- current_data(); paste0("₹", format(d$price, big.mark=",")) })
  
  output$sector_card <- renderUI({
    req(sector_data()); df <- sector_data()
    if(nrow(df) > 0) {
      top_sec <- df[1,1]
      card(card_header(paste("🔥 LEADING SECTOR (Median):", top_sec), class="bg-info text-white"), tableOutput("sector_table"))
    }
  })
  output$sector_table <- renderTable({ req(sector_data()); sector_data() }, width="100%", striped=TRUE)
  
  output$hit_list_card <- renderUI({
    req(hit_list_data()); df <- hit_list_data()
    if(nrow(df) == 0) {
      div(class="alert alert-danger text-center", h4("MARKET WEAK. NO APPROVED BUYS FOUND."))
    } else {
      card(card_header("APPROVED BUY LIST (Score > 70)", class="bg-success text-white"), tableOutput("hit_list_table"))
    }
  })
  output$hit_list_table <- renderTable({ req(hit_list_data()); hit_list_data() }, width = "100%", striped = TRUE)
  
  output$metrics_table <- renderTable({
    req(current_data()); d <- current_data()
    data.frame(Metric = c("RSI (14)", "EMA Slope", "Vol Ratio"), Value = c(d$rsi, d$ema_slope_val, d$vol_ratio))
  }, width = "100%", striped = TRUE)
  
  output$score_circle <- renderUI({
    req(current_data()); score <- current_data()$score
    color <- if(score >= 80) "#28a745" else if(score <= 40) "#dc3545" else "#ffc107"
    div(style = paste0("width: 100px; height: 100px; border-radius: 50%; border: 8px solid ", color, "; margin: 0 auto; display: flex; align-items: center; justify-content: center; font-size: 2rem; font-weight: bold; color: ", color, ";"), score)
  })
  
  output$trend_chart <- renderPlot({
    req(current_data())
    chartSeries(current_data()$history, theme = chartTheme("white"), type = "line", 
                TA = "addEMA(n=20, col='blue'); addEMA(n=50, col='red'); addVo()", 
                name = current_data()$symbol)
  })
}

shinyApp(ui, server)
