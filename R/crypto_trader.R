
# 01 LOAD LIBRARIES, FUNCTIONS & VARS -------------------------------------

# Get working directory
crypto_dir <- Sys.getenv("CRYPTO_TRADING_FOLDER")

# Set timezone
Sys.setenv(TZ = "Australia/Sydney")

# trading_mode can be "TEST" or "LIVE"
trading_mode <- "TEST"

source(file.path(crypto_dir, "R", "crypto_load_libs.R"))
source(file.path(crypto_dir, "R", "crypto_vars.R"))
source(file.path(crypto_dir, "R", "crypto_functions.R"))


# 02 LOGGER & AUTHENTICATION ----------------------------------------------

# Setup Logger
log_dir <- file.path(crypto_dir, "logs", "crypto_trader", trading_mode, Sys.Date())

# Create the directory if it doesn't exist (recursive = TRUE handles parents)
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

# Define the log file directly in the daily folder
log_file <- file.path(log_dir, "crypto_trader.log")

# Set the appender
log_appender(appender_file(log_file, append = TRUE))
log_layout(get_log_layout())

log_info("--- TRADER SESSION START ---")

# Independent Reserve authentication
# For get_bws_secret function to work below, ensure BWS's path is listed in
# Environment Variables > System Variables > Path
tryCatch({
  
  ir_keys <- map(
    .x = seq_along(ir_uuid)
    ,.f = ~get_bws_secret(key = ir_uuid[[.x]])
  ) %>% 
    set_names(nm = c("api_key", "secret_key"))
  
  log_success("BWS Secret Retrieval: SUCCESS")
  
}, error = function(e) {
  
  log_error(paste("CRITICAL: BWS Secret Retrieval FAILED -", e$message))
  
  stop()
  
})

# Azure authentication
is_azure_cloud <- Sys.getenv("AZURE_FUNCTIONS_ENVIRONMENT") != ""

tryCatch({
  
  azure_container <- get_azure_container(is_azure_cloud = is_azure_cloud)
  
}, error = function(e) {
  
  log_error(paste("CRITICAL: Azure Authentication FAILED -", e$message))
  
  stop()
  
})


# 03 DATA INGESTION (SYNC FROM CLOUD) --------------------------------------

log_info("Syncing bot memory from Azure Blob Storage")

tryCatch({
  
  bot_state     <- get_azure_blob("bot_state.rds", azure_container)
  price_history <- get_azure_blob("xbt_aud_price_history.rds", azure_container)
  
}, error = function(e) {
  
  log_error(paste("CRITICAL: Failed to sync bot memory from Azure -", e$message))
  
  stop()
  
})

log_info(paste0(
  "Memory Loaded. Position: ", if_else(bot_state$in_position, "YES", "NO")
  ," | BTC: ", bot_state$current_crypto_holdings
  ," | AUD: ", bot_state$current_cash_holdings
))


# 04 GET ACTUAL ACCOUNT BALANCES ------------------------------------------

log_info("Retrieving balance info from Independent Reserve")

ir_balances <- tryCatch({
  
  get_ir_accounts(
    req_type = "GetAccounts"
    ,key     = ir_keys$api_key
    ,secret  = ir_keys$secret_key
  )
  
}, error = function(e) {
  
  log_error(paste("CRITICAL: Failed to retrieve IR Account Balances -", e$message))
  
  stop()
  
})

# Extract balances outside tryCatch for clean scoping
actual_aud <- ir_balances %>% filter(CurrencyCode == "Aud") %>% pull(AvailableBalance)
actual_btc <- ir_balances %>% filter(CurrencyCode == "Xbt") %>% pull(AvailableBalance)

log_success(paste0("Account Balances Sync: SUCCESS | AUD: ", actual_aud, " | BTC: ", actual_btc))


# Are we holding any crypto in IR?
is_holding_btc <- actual_btc > btc_dust_threshold

log_info(paste("Verified Position State: ", ifelse(is_holding_btc, "Holding BTC", "CASH")))

# Compare bot_state (Cloud Memory) vs is_holding_btc (Exchange Reality)
desync_detected <- bot_state$in_position != is_holding_btc

if (desync_detected) {
  log_warn(paste0(
    "DESYNC: Cloud memory says in_position=", bot_state$in_position, 
    " but Exchange says is_holding_btc=", is_holding_btc
  ))
  
  # Option A: Force the bot to trust the Exchange (Self-fixing)
  # log_info("Self-fixing: Updating bot_state to match Exchange reality.")
  # bot_state$in_position <- is_holding_btc
  
  # Option B: Stop the script if you want to manual intervene
  log_error("Critical State Desync detected. Manual intervention required.")
  
  stop()
  
} else {
  
  log_info("Sync Check: Cloud memory and Exchange reality are aligned.")
  
}

# 05 SIGNAL GENERATION (THE BRAIN) -----------------------------------------

## Downsample hourly data to calendar daily (midnight-to-midnight AEST) ----
# Note: Using floor_date("day") preserves consistency with backtesting which
# used daily OHLC data. We use closing price of each day (last hourly close).
price_history_daily <- price_history %>%
  mutate(date = as.Date(floor_date(sttm_aest, unit = "day"))) %>%
  group_by(date) %>%
  summarise(
    close = dplyr::last(ClosingSecondaryCurrencyPrice)
    ,.groups = "drop"
  ) %>%
  # Exclude today's incomplete day - only use full completed days
  filter(date < Sys.Date())

log_info(paste0(
  "Daily bars available: ", nrow(price_history_daily),
  " | From: ", min(price_history_daily$date),
  " | To: ", max(price_history_daily$date)
))

## Convert to XTS for strategy function ------------------------------------
prices_xts <- xts(
  x = price_history_daily$close
  ,order.by = price_history_daily$date
  ,tzone = "Australia/Sydney"
)

colnames(prices_xts) <- "Close"

## Calculate Signals (Using the 20/50 SMA Cross) ---------------------------
# Note: Use tail(..., 1) to get the most recent signal
current_signals <- strat_sma_cross(prices_xts, short_n = 20, long_n = 50)
latest_signal <- tail(current_signals$trade_signal, 1)
latest_price <- tail(current_signals$price, 1)

log_info(paste0("Current Price: ", latest_price, " | Signal: ", latest_signal))


# 06 RISK VALIDATION (THE SHIELD) ------------------------------------------

stop_loss_triggered <- FALSE

if (is_holding_btc) {
  # Calculate current drop since purchase
  # bot_state$purchase_price was saved when we last bought
  price_drop_pct <- (latest_price - bot_state$purchase_price) / bot_state$purchase_price
  
  log_info(paste0("Current P/L: ", round(price_drop_pct * 100, 2), "%"))
  
  if (price_drop_pct <= -0.10) {
    stop_loss_triggered <- TRUE
    log_warn("SHIELD TRIGGERED: Price dropped 10% below entry. Forcing SELL.")
  }
}


# 07 EXECUTION (THE HANDS) -------------------------------------------------

decision <- "HOLD"

if (stop_loss_triggered) {
  decision <- "SELL"
} else if (latest_signal == 1 && !is_holding_btc) {
  decision <- "BUY"
} else if (latest_signal == -1 && is_holding_btc) {
  decision <- "SELL"
}

log_info(paste0("FINAL DECISION: ", decision))

if (decision == "BUY") {
  
  log_info(paste0(">>> STRATEGY SIGNAL: BUY (Mode: ", trading_mode, ")"))
  
  if (trading_mode == "LIVE") {
    
    tryCatch({
      place_ir_order(
        type    = "MarketBuy"
        ,amount = actual_aud
        ,key    = ir_keys$api_key
        ,secret = ir_keys$secret_key
      )
      
      log_info(paste0("LIVE ORDER EXECUTED: Market Buy | AUD: ", actual_aud))
      
    }, error = function(e) {
      
      log_error(paste("CRITICAL: Order placement FAILED -", e$message))
      
      stop()
      
    })
    
  } else {
    
    log_info(paste0("TEST MODE: Market Buy simulated | AUD: ", actual_aud, " | Price: ", latest_price))
    
  }
  
} else if (decision == "SELL") {
  
  log_info(paste0(">>> STRATEGY SIGNAL: SELL (Mode: ", trading_mode, ")"))
  
  if (trading_mode == "LIVE") {
    
    tryCatch({
      place_ir_order(
        type    = "MarketSell"
        ,amount = actual_btc
        ,key    = ir_keys$api_key
        ,secret = ir_keys$secret_key
      )
      
      log_info(paste0("LIVE ORDER EXECUTED: Market Sell | BTC: ", actual_btc))
      
    }, error = function(e) {
      
      log_error(paste("CRITICAL: Order placement FAILED -", e$message))
      
      stop()
      
    })
    
  } else {
    
    log_info(paste0("TEST MODE: Market Sell simulated | BTC: ", actual_btc, " | Price: ", latest_price))
    
  }
  
} else {
  
  log_info("NO TRADE REQUIRED: Strategy is in HOLD state.")
  
}


# 08 STATE PERSISTENCE (SYNC TO CLOUD) -------------------------------------

if (decision == "BUY") {
  bot_state$in_position             <- TRUE
  bot_state$purchase_price          <- latest_price # purchase_price is price per BTC at time of purchase (not total AUD spent)
  bot_state$last_trade_time         <- Sys.time()
  bot_state$current_cash_holdings   <- 0
  bot_state$current_crypto_holdings <- actual_aud / latest_price  # approx BTC acquired
  log_info(paste0("Memory Updated: Recorded BUY at ", latest_price))
  
} else if (decision == "SELL") {
  bot_state$in_position             <- FALSE
  bot_state$purchase_price          <- 0
  bot_state$last_trade_time         <- Sys.time()
  bot_state$current_cash_holdings   <- actual_btc * latest_price  # approx AUD received
  bot_state$current_crypto_holdings <- 0
  log_info("Memory Updated: Position cleared.")
}

log_info("Uploading updated bot memory to Azure Blob Storage...")

tryCatch({
  
  upload_azure_blob(bot_state, "bot_state.rds", azure_container)
  upload_azure_blob(price_history, "xbt_aud_price_history.rds", azure_container)
  
  log_success("Bot memory successfully synced to Azure.")
  
}, error = function(e) {
  
  log_error(paste("CRITICAL: Failed to upload bot memory to Azure -", e$message))
  
  stop()
  
})

log_info("--- TRADER SESSION END ---")
