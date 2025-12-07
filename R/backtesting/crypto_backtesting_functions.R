
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


#' get_performance_metrics
#'
#' @description
#' Calculates comprehensive performance statistics across multiple backtest results.
#' Computes trade-level metrics (number of trades, win rate, profitability) and 
#' portfolio-level metrics (CAGR, max drawdown, Sharpe, Sortino, Calmar ratios).
#' Uses parallel processing via furrr for efficient computation across many strategies.
#' Only counts complete round-trip trades (BUY → SELL pairs) for win rate calculations
#' to avoid biased statistics from incomplete positions.
#'
#' @param results Named list of backtest results from backtest_strategy(), where 
#' each element contains trades, equity_curve, final_equity, and parameters
#' @param initial_equity initial cash outlay for trading
#'
#' @returns Tibble with performance metrics per strategy: returns, trade stats, 
#' CAGR, drawdown, Sharpe/Sortino/Calmar ratios

get_performance_metrics <- function(results, initial_equity = 10000) {
  
  # Using furrr::future_map_dfr for parallel execution across backtest results
  furrr::future_map_dfr(names(results), function(name) {
    
    message("Running: ", name)
    
    res <- results[[name]]
    strategy_type <- res$strategy_nm
    
    # 1. Extract and Calculate Basic Metrics
    # stop_loss is nested under 'parameters' in the optimised output
    stop_loss <- res$parameters$stop_loss
    trades <- res$trades
    equity_curve <- res$equity_curve
    final_equity <- res$final_equity
    total_return <- (final_equity - initial_equity) / initial_equity
    
    # number of trades (rows in trades table)
    num_trades <- nrow(trades)
    
    # 2. Profitable Pairs Calculation (Pair BUY then SELL/STOP_LOSS)
    # The calculation uses units_traded (from the BUY leg) and the prices/fees.
    
    pct_profitable <- NA_real_
    num_complete_trades <- 0L
    num_profitable_trades <- 0L
    
    if (num_trades >= 2) {
      
      # Step 1: Group trades into round-trip pairs
      trade_pairs <- trades %>%
        # Identify the start of a new trade (BUY action)
        dplyr::mutate(pair_id = cumsum(action == "BUY")) %>%
        # Group by the trade ID
        dplyr::group_by(pair_id) %>%
        # Mark if this is a complete pair
        dplyr::mutate(is_complete = dplyr::n() == 2) %>%
        # Filter for completed trades (must have 2 legs: BUY and SELL)
        dplyr::filter(is_complete) %>%
        dplyr::summarise(
          # Critical: get the units from the BUY trade
          units_traded = dplyr::first(units)
          ,buy_price = dplyr::first(price)
          ,sell_price = dplyr::last(price)
          ,buy_fee = dplyr::first(fee)
          ,sell_fee = dplyr::last(fee)
          ,exit_reason = dplyr::last(exit_reason)
          ,.groups = "drop"
        ) %>%
        # Step 2: Calculate Profit/Loss
        dplyr::mutate(
          # P&L = (Sell Price - Buy Price) * Units - Total Fees
          profit_loss = (sell_price - buy_price) * units_traded - (buy_fee + sell_fee)
          ,profitable = profit_loss > 0
        )
      
      if (nrow(trade_pairs) > 0) {
        num_complete_trades <- nrow(trade_pairs)
        num_profitable_trades <- sum(trade_pairs$profitable, na.rm = TRUE)
        pct_profitable <- num_profitable_trades / num_complete_trades
      }
    }
    
    # 3. PerformanceAnalytics Metrics (CAGR, DD, Sharpe, etc.)
    # Build xts equity series for PerformanceAnalytics
    eq_xts <- xts::xts(equity_curve$equity, order.by = as.Date(equity_curve$date))
    # daily returns (arithmetic)
    daily_rets <- na.omit(PerformanceAnalytics::Return.calculate(eq_xts, method = "discrete"))
    
    # CAGR, max drawdown, sharpe, sortino, calmar
    if (NROW(daily_rets) >= 2) {
      years <- as.numeric(difftime(max(zoo::index(eq_xts)), min(zoo::index(eq_xts)), units = "days")) / 365.25
      
      # Ensure CAGR is only calculated if duration > 0
      CAGR <- ifelse(
        years > 0
        ,(as.numeric(final_equity) / initial_equity)^(1/years) - 1
        ,NA_real_
      )
      max_dd <- as.numeric(PerformanceAnalytics::maxDrawdown(daily_rets))
      
      # Use tryCatch for robust statistic calculation
      sharpe <- tryCatch(
        as.numeric(PerformanceAnalytics::SharpeRatio.annualized(daily_rets, Rf = 0, scale = 252))
        ,error = function(e) NA_real_
      )
      sortino <- tryCatch(
        as.numeric(PerformanceAnalytics::SortinoRatio(daily_rets, MAR = 0))
        ,error = function(e) NA_real_
      )
      calmar <- ifelse(
        !is.na(max_dd) && max_dd > 0
        ,CAGR / max_dd
        ,NA_real_
      )
    } else {
      CAGR <- NA_real_
      max_dd <- NA_real_
      sharpe <- NA_real_
      sortino <- NA_real_
      calmar <- NA_real_
    }
    
    # 4. Final Tibble Output
    tibble::tibble(
      strategy = name
      ,strategy_type = strategy_type
      ,stop_loss = stop_loss
      ,final_equity = round(as.numeric(final_equity), 2)
      ,total_return = round(total_return, 4)
      ,num_trades = num_trades
      ,num_complete_trades = num_complete_trades
      ,num_profitable_trades = num_profitable_trades
      ,pct_profitable = ifelse(is.na(pct_profitable), NA_real_, round(pct_profitable, 4))
      ,CAGR = ifelse(is.na(CAGR), NA_real_, round(CAGR, 4))
      ,max_drawdown = ifelse(is.na(max_dd), NA_real_, round(max_dd, 4))
      ,sharpe_ratio = ifelse(is.na(sharpe), NA_real_, round(sharpe, 4))
      ,sortino_ratio = ifelse(is.na(sortino), NA_real_, round(sortino, 4))
      ,calmar_ratio = ifelse(is.na(calmar), NA_real_, round(calmar, 4))
    )
  }
  
  # Arguments for future_map_dfr
  ,.progress = TRUE
  ,.options = furrr_options(globals = c("initial_equity")) # only copy this variable to worker function
  )
}


#' sample_xts_window
#' 
#' @description
#' Randomly samples multiple time windows from an xts object for Monte Carlo
#' simulation. Each window has a randomly chosen length between min_window_length
#' and the full dataset length, starting at a random valid index.
#'
#' @param prices_xts xts dataframe containing price data
#' @param sample_size Number of random windows to generate (default 3)
#' @param min_window_length Minimum number of periods in each window (default 250)
#'
#' @returns List with:
#'   - sampled_windows: list of xts subsets
#'   - window_lengths: integer vector of window sizes
#'   - start_dates: Date vector of window start dates
#'   - end_dates: Date vector of window end dates

sample_xts_window <- function(prices_xts, sample_size = 3, min_window_length = 250) {
  
  n <- NROW(prices_xts)
  
  # Validation
  stopifnot(min_window_length < n)
  
  # Randomly sample multiple window lengths
  win_lengths <- sample(min_window_length:(n - 1), size = sample_size, replace = TRUE)
  
  # For each window length, sample a random valid start index
  start_indices <- map_int(win_lengths, ~ sample(1:(n - .x + 1), 1))
  end_indices   <- start_indices + win_lengths - 1
  
  # Extract each window of data
  sampled_windows <- map2(start_indices, end_indices, ~ prices_xts[.x:.y]) %>% 
    set_names(nm = paste0("window_", seq_along(win_lengths)))
  
  # Extract metadata for tracking
  start_dates <- unname(map_chr(sampled_windows, ~ as.character(as.Date(index(.x)[1]))))
  end_dates   <- unname(map_chr(sampled_windows, ~ as.character(as.Date(index(.x)[nrow(.x)]))))
  
  message("Generated ", sample_size, " time windows")
  message("Window length range: ", min(win_lengths), " to ",
          max(win_lengths), " days")
  
  list(
    sampled_windows = sampled_windows,
    window_lengths = win_lengths,
    start_dates = start_dates,
    end_dates = end_dates
  )
}