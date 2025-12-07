
# 01 HELPER FUNCTIONS -----------------------------------------------------

#' get_crypto_data
#' 
#' @description
#' Fetches trading data from yahoo. Wrapper function for quantmod::getSymbols
#' 
#' @param symbol Valid symbol string to pass onto quantmod::getSymbols
#' @param from Start of trading data to fetch from, yyyy-mm-dd format
#' @param to End of trading data to fetch, yyyy-mm-dd format
#'
#' @returns xts-style data frame

get_crypto_data <- function(
    symbol
    ,from = NULL
    ,to = NULL
) {
  
  getSymbols(
    Symbols = symbol
    ,src = "yahoo"
    ,from = from
    ,to = to
    ,auto.assign = FALSE
  )
  
}


#' backtest_strategy
#' 
#' @description
#' Simulates trading performance by executing buy/sell signals on historical price data.
#' Tracks position entries/exits, calculates transaction fees, applies optional stop-loss
#' risk management, and generates an equity curve showing portfolio value over time.
#' The function enforces long-only positions (no shorting) and includes realistic 
#' constraints like minimum trade values. Stop-loss checks take priority over signal-based
#' exits to ensure risk management is enforced first.
#' 
#' @param signal_tbl tibble containing dates, Close prices and trade signals
#' @param fee_rate fee charged by exchange both on buy and sell transactions,
#' defaults to 0.5%
#' @param initial_equity initial cash outlay for trading
#' @param stop_loss stop loss value to apply, defaults to 0 i.e. no stop loss
#' @param min_trade_value minimum cash required to buy (some exchanges impose
#' a minimum trade value)
#' @param force_close_final close open positions at end of window
#'
#' @returns A tibble with trade metadata, trades, equity curve and final equity
#' amount

backtest_strategy <- function(
    signal_tbl
    ,fee_rate = 0.005
    ,initial_equity = 10000
    ,stop_loss = 0
    ,min_trade_value = 10
    ,force_close_final = TRUE # If a strategy ends with an open position, need to  force-close it at the last price for accurate P&L
) {
  
  # 1. Validation & Setup
  stopifnot(
    "signal_tbl must contain required columns" = 
      all(c("date", "price", "trade_signal") %in% names(signal_tbl))
  )
  
  # Extract vectors for speed
  dates   <- as.Date(signal_tbl$date)
  prices  <- as.numeric(signal_tbl$price)
  signals <- replace_na(as.integer(signal_tbl$trade_signal), 0L)
  n       <- length(prices)
  
  # 2. Pre-allocate Output Vectors
  equity_curve <- numeric(n)
  equity_curve[1] <- initial_equity
  
  trade_list <- vector("list", length = n) 
  trade_count <- 0
  
  # 3. State Variables
  cash   <- initial_equity
  units  <- 0
  pos    <- 0L        # 0 = flat, 1 = long
  entry  <- NA_real_
  
  # 4. The Loop
  for (i in 1:n) {
    
    price_i  <- prices[i]
    date_i   <- dates[i]
    signal_i <- signals[i]
    
    # Mark current equity (Market Value)
    current_equity <- cash + (units * price_i)
    
    # Skip logic if price is missing
    if (is.na(price_i)) {
      equity_curve[i] <- current_equity
      next
    }
    
    action_taken <- NA_character_
    trade_price  <- NA_real_
    trade_fee    <- 0
    exit_reason  <- NA_character_
    
    # --- LOGIC START ---
    
    # A. Check Stop Loss (Priority: Risk checks happen before Signals)
    if (pos == 1L && stop_loss > 0) {
      
      if (price_i <= entry * (1 - stop_loss)) {
        
        # Calculate Exit
        proceeds  <- units * price_i
        trade_fee <- proceeds * fee_rate
        cash      <- cash + proceeds - trade_fee
        units     <- 0
        pos       <- 0L
        entry     <- NA_real_
        
        action_taken <- "SELL"
        exit_reason  <- "STOP_LOSS"
        trade_price  <- price_i
      }
    }
    
    # B. Check Sell Signal (Only if we are still Long)
    if (pos == 1L && signal_i == -1L) {
      
      proceeds  <- units * price_i
      trade_fee <- proceeds * fee_rate
      cash      <- cash + proceeds - trade_fee
      units     <- 0
      pos       <- 0L
      entry     <- NA_real_
      
      action_taken <- "SELL"
      exit_reason  <- "SIGNAL"
      trade_price  <- price_i
    }
    
    # C. Check Buy Signal (Only if we are flat/holding cash position)
    if (pos == 0L && signal_i == 1L) {
      
      # Check minimum trade value requirement
      if (cash >= min_trade_value * (1 + fee_rate)) {
        
        # Position sizing calculation:
        # We want: units * price * (1 + fee_rate) = cash
        # Therefore: units = cash / (price * (1 + fee_rate))
        # Cost = units * price
        # Fee = cost * fee_rate
        # Total deduction = cost + fee = units * price * (1 + fee_rate) = cash ✓
        
        trade_price <- price_i
        units       <- cash / (price_i * (1 + fee_rate))
        cost        <- units * price_i
        trade_fee   <- cost * fee_rate
        
        cash  <- cash - cost - trade_fee 
        pos   <- 1L
        entry <- price_i
        
        action_taken <- "BUY"
        exit_reason  <- NA_character_  # N/A for entries
      }
    }
    
    # --- LOGIC END ---
    
    # Update Equity Curve after potential trades
    current_equity  <- cash + (units * price_i)
    equity_curve[i] <- current_equity
    
    # Record Trade if one happened
    if (!is.na(action_taken)) {
      
      trade_count <- trade_count + 1
      
      trade_list[[trade_count]] <- list(
        date = date_i
        ,action = action_taken
        ,exit_reason = exit_reason
        ,price = trade_price
        ,units = units
        ,fee = trade_fee
        ,equity_after = current_equity
      )
      
    }
  }
  
  # 5. Force Close Final Position (if enabled and position is open)
  if (force_close_final && pos == 1L && !is.na(prices[n])) {
    
    final_proceeds <- units * prices[n]
    final_fee <- final_proceeds * fee_rate
    cash <- cash + final_proceeds - final_fee
    
    trade_count <- trade_count + 1
    
    trade_list[[trade_count]] <- list(
      date = dates[n]
      ,action = "SELL"
      ,exit_reason = "FORCE_CLOSE"
      ,price = prices[n]
      ,units = 0
      ,fee = final_fee
      ,equity_after = cash
    )
    
    # Update final equity
    equity_curve[n] <- cash
    
    # Reset position
    units <- 0
    pos <- 0L
  }
  
  # 6. Final Output Construction
  trades_final <- if (trade_count > 0) {
    bind_rows(trade_list[1:trade_count])
  } else {
    tibble()
  }
  
  list(
    parameters = list(
      stop_loss = stop_loss, 
      fee = fee_rate,
      min_trade_value = min_trade_value,
      force_close_final = force_close_final
    )
    ,trades = trades_final
    ,equity_curve = tibble(date = dates, equity = equity_curve)
    ,final_equity = tail(equity_curve, 1)
  )
}