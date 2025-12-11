
# Assumptions:
# Only enter and exit long positions. Short positions (leveraged positions) 
# are ignored here.
# Risk exposure: Losses are limited to investment amount, never leveraged.


# 01 LOAD LIBS ------------------------------------------------------------

required_packages <- c(
  "quantmod"
  ,"TTR"
  ,"dplyr"
  ,"tidyr"
  ,"tibble"
  ,"purrr"
  ,"stringr"
  ,"xts"
  ,"PerformanceAnalytics"
  ,"lubridate"
  ,"furrr"
  ,"ggplot2"
)

# Load the packages and suppress startup messages
suppressPackageStartupMessages({
  purrr::walk(required_packages, library, character.only = TRUE)
})

# Load all packages except furrr, lubridate and ggplot2 for parallel worker function
worker_packages <- required_packages[!required_packages %in% c("furrr", "lubridate", "ggplot2")]


# 02 LOAD DEPENDABLES -----------------------------------------------------

crypto_dir <- Sys.getenv("CRYPTO_TRADING_FOLDER")

# furrr options
plan(multisession, workers = availableCores() - 1)
# options(future.globals.maxSize = 7168 * 1024^2)
options(future.globals.maxSize = +Inf)
options(future.packages = worker_packages) # Only load worker packages for workers

init_equity <- 1000
backtesting_crypto_symbol <- "BTC-AUD"
backtesting_crypto_from <- as.Date("2014-09-17") # first trading day of BTC-AUD
backtesting_crypto_to <- Sys.Date() - days(2) # avoid potential NAs on later days

stop_losses <- c(0, 0.02, 0.05, 0.1, 0.15)  # 0 = no SL
stop_losses_pct <- paste0(stop_losses * 100, "%")


# 03 LOAD FUNCTIONS -------------------------------------------------------

source(file.path(crypto_dir, "R", "backtesting", "crypto_backtesting_functions.R"))


# 04 GET TRADING DATA -----------------------------------------------------

crypto_xts <- get_crypto_data(
  symbol = backtesting_crypto_symbol
  ,from = backtesting_crypto_from
  ,to = backtesting_crypto_to
)


# 05 BACKTESTING SINGLE WINDOW --------------------------------------------

strategy_fns <- list(
  buy_hold = strat_buy_hold,
  sma = strat_sma,
  sma_cross = strat_sma_cross,
  rsi = strat_rsi,
  bb = strat_bb,
  macd = strat_macd,
  macd_vol = strat_macdv,
  macd_vol_dynamic_strength = strat_macdv_dynamic_strength
)

# Generate all signals
strategies <- future_map(strategy_fns, ~.x(crypto_xts))

# Create combinations of strategy name + stop-loss options
param_grid <- expand_grid(
  name = names(strategies)
  ,stop_loss = stop_losses
)

# Simulate trades and equity progression
results_list <- future_pmap(param_grid, function(name, stop_loss) {
  message(
    "Running strategy: ", name, 
    " | Stop loss %: ", stop_loss * 100, "%"
  )
  
  res <- backtest_strategy(
    signal_tbl = strategies[[name]]
    ,fee_rate = 0.005
    ,initial_equity = init_equity
    ,stop_loss = stop_loss
    ,min_trade_value = 10
    ,force_close_final = TRUE      
  )
  
  res$strategy_nm = name
  
  return(res)
  
})

# Name each result meaningfully for clarity
names(results_list) <- paste0(
  param_grid$name, 
  "_SL", param_grid$stop_loss * 100
)

# Summarise performance metrics
summary_metrics <- get_performance_metrics(results_list, initial_equity = init_equity)

# Show sorted results
summary_metrics %>% 
  arrange(desc(total_return)) %>% 
  print(n = 40)


# 06 MONTE CARLO SIMULATION -----------------------------------------------

## Monte Carlo parameters -------------------------------------------------
n_samples <- 1000
min_window_length <- 250

strength_threshold_grid <- seq(from = 20, to = 80, by = 10) # 50 is default for signals_macdv
rolling_window_grid <- seq(from = 10, to = 50, by = 10) # 20 is default
strength_quantile_grid <- seq(from = 0.5, to = 0.9, by = 0.1) # 0.8 is default

macd_vol_dynamic_params <- expand_grid(
  rolling_window = rolling_window_grid
  ,strength_quantile = strength_quantile_grid
)

set.seed(42)

## Generate sample windows ------------------------------------------------

sample_windows <- sample_xts_window(
  prices_xts = crypto_xts
  ,sample_size = n_samples
  ,min_window_length = min_window_length
)

# Create window metadata lookup table
window_metadata <- tibble(
  window_id = names(sample_windows$sampled_windows),
  window_length = sample_windows$window_lengths,
  window_start_date = sample_windows$start_dates,
  window_end_date = sample_windows$end_dates
)

## Generate trade signals -------------------------------------------------
# NOTE: The monte_carlo_signals object is still large, but it must be kept in 
# memory to run the backtests in the next section.
monte_carlo_signals <- future_map(
  .x = seq_along(sample_windows$sampled_windows)
  ,function(sample_index) {
    
    win_xts <- sample_windows$sampled_windows[[sample_index]]
    
    message(
      "Window ", sample_index, "/", length(sample_windows$sampled_windows)
      ," | Length: ", sample_windows$window_lengths[sample_index]
      ," | Dates: ", sample_windows$start_dates[sample_index]
      ," to ", sample_windows$end_dates[sample_index]
    )
    
    # Base strategies (default parameters)
    base_strategies <- list(
      buy_hold = strat_buy_hold(win_xts)
      ,sma = strat_sma(win_xts)
      ,sma_cross = strat_sma_cross(win_xts)
      ,rsi = strat_rsi(win_xts)
      ,bb = strat_bb(win_xts)
      ,macd = strat_macd(win_xts)
    )
    
    # MACD-V with varying fixed strength thresholds
    macd_vol_fixed <- map(
      .x = strength_threshold_grid
      ,~ strat_macdv(
        prices_xts = win_xts
        ,strength_threshold = .x
      )
    ) %>% 
      set_names(paste0("macd_vol_fixed_", strength_threshold_grid))
    
    # MACD-V with dynamic strength (parameter grid)
    macd_vol_dynamic <- pmap(
      .l = macd_vol_dynamic_params
      ,function(rolling_window, strength_quantile) {
        strat_macdv_dynamic_strength(
          prices_xts = win_xts
          ,roll_window = rolling_window
          ,strength_quantile = strength_quantile
        )
      }
    ) %>% 
      set_names(
        paste(
          "macd_vol_dynamic"
          ,macd_vol_dynamic_params$rolling_window
          ,macd_vol_dynamic_params$strength_quantile
          ,sep = "_"
        )
      )
    
    # Combine all strategies
    c(base_strategies, macd_vol_fixed, macd_vol_dynamic)
  }
  ,.progress = TRUE
  ,.options = furrr_options(seed = TRUE)  # Ensures reproducibility in parallel
) %>% 
  set_names(names(sample_windows$sampled_windows))

# Get all unique strategy names
all_strategy_names <- names(monte_carlo_signals[[1]])

## Run Monte Carlo backtesting in chunks by Stop Loss ----------------------

### Setup ------------------------------------------------------------------
all_monte_carlo_summaries <- list()

message("Total samples (windows): ", length(sample_windows$sampled_windows))

### Loop over stop losses --------------------------------------------------
for (sl in stop_losses) {
  
  message("Starting Monte Carlo for Stop Loss: ", sl * 100, "%")
  
  #### In loop setup -------------------------------------------------------
  mc_param_grid_sl <- expand_grid(
    window_id = names(sample_windows$sampled_windows)
    ,strategy_name = all_strategy_names
    ,stop_loss = sl
  ) %>% 
    
    # Buy-hold only runs when stop_loss is 0 (or technically any SL>0 when SL=0 is removed)
    filter(!(strategy_name == "buy_hold" & sl > 0)) %>%
    
    # Create unified identifier
    unite(
      col = "backtest_id",
      window_id, strategy_name, stop_loss,
      sep = "_SL",
      remove = FALSE
    )
  
  message("Total backtests to run in this chunk: ", nrow(mc_param_grid_sl))
  
  ### Backtesting ---------------------------------------------------------
  monte_carlo_results_sl <- future_pmap(
    .l = mc_param_grid_sl
    ,function(window_id, strategy_name, stop_loss, backtest_id) {
      
      message("Running: ", backtest_id)
      
      res <- backtest_strategy(
        signal_tbl = monte_carlo_signals[[window_id]][[strategy_name]]
        ,fee_rate = 0.005
        ,initial_equity = init_equity
        ,stop_loss = stop_loss
        ,min_trade_value = 10
        ,force_close_final = TRUE
      )
      
      # Add metadata
      window_index <- as.integer(str_extract(window_id, "\\d+"))
      
      res$strategy_nm <- strategy_name
      res$window_id <- window_id
      res$window_length <- sample_windows$window_lengths[window_index]
      res$window_start_date <- sample_windows$start_dates[window_index]
      res$window_end_date <- sample_windows$end_dates[window_index]
      
      return(res)
    }
    ,.progress = TRUE
    ,.options = furrr_options(seed = TRUE, globals = c("init_equity", "backtest_strategy", "monte_carlo_signals", "sample_windows")) 
  ) %>% 
    set_names(nm = mc_param_grid_sl$backtest_id)
  
  ### Summarise performance metrics ---------------------------------------
  monte_carlo_summary_sl <- get_performance_metrics(
    monte_carlo_results_sl
    ,initial_equity = init_equity
  )
  
  ### Add window metadata -------------------------------------------------
  monte_carlo_summary_sl <- monte_carlo_summary_sl %>%
    left_join(
      mc_param_grid_sl %>% 
        select(backtest_id, window_id)
      ,by = c("strategy" = "backtest_id")
    ) %>% 
    left_join(
      window_metadata
      ,by = "window_id"
    ) 
  
  ### Store results ------------------------------------------------------
  all_monte_carlo_summaries[[paste0("SL", sl * 100)]] <- monte_carlo_summary_sl
  
  ### Memory management --------------------------------------------------
  rm(monte_carlo_results_sl)
  gc(verbose = FALSE) 
  
}


## Clean up --------------------------------------------------------------

plan(sequential) # Explicitly close multisession workers by switching plan


## Combine all results ---------------------------------------------------

monte_carlo_summary <- bind_rows(all_monte_carlo_summaries)

# Save the final combined summary
saveRDS(
  object = monte_carlo_summary
  ,file = file.path(crypto_dir, "outputs", "backtesting", paste0(Sys.Date(), "_monte_carlo_summary.rds"))
)


## Aggregate results -----------------------------------------------------

agg_summary <- monte_carlo_summary %>%
  # Treat 0 trades as 0 return for fair comparison
  mutate(CAGR = ifelse(num_trades == 0, 0, CAGR)) %>%
  group_by(strategy_type, stop_loss) %>%
  summarise(
    # METRIC 1: Geometric Mean of Return / Expected log-returns
    # Formula: exp(mean(ln(1 + R))) - 1
    geo_mean_CAGR = exp(mean(log(1 + pmax(CAGR, -0.99)), na.rm = TRUE)) - 1
    
    # METRIC 2: Stability (Percent of windows with Positive Return)
    ,win_rate_windows = mean(total_return > 0, na.rm = TRUE)
    
    # METRIC 3: Risk-Adjusted Stability (Mean Sharpe is noisy, Median is robust)
    ,median_Sharpe = median(sharpe_ratio, na.rm = TRUE)
    
    # METRIC 4: Tail Risk (The average of the worst 5% of outcomes)
    ,cvar_drawdown = mean(sort(max_drawdown, decreasing = TRUE)[1:ceiling(n() * 0.05)], na.rm = TRUE)
    
    # Context
    ,n_samples = n()
    ,.groups = "drop"
  ) %>%
  arrange(desc(geo_mean_CAGR))

ranked_summary <- agg_summary %>%
  mutate(
    # Robust Calmar: How much return do I get for risking a crash? (Return per 
    # Unit of Extreme Risk)
    robust_calmar = geo_mean_CAGR / cvar_drawdown
    
    # Probability Score: How reliable (quality) and consistent is the performance?
    # - win_rate_windows indicates reliability - percentage of Monte Carlo windows
    # where strategy achieved a positive total return, across various market
    # conditions (bull, bear, choppy, long, short)
    # - median_Sharpe indicates performance quality - quality of risk-adjusted
    # returns. Crypto is highly skewed, so using median Sharpe is more robust
    # than mean Sharpe.
    ,prob_score = win_rate_windows * median_Sharpe
    
    # CAPS (Calmar-adjusted Probability Score) - final composite metric that ranks
    # strategies by balancing return, consistency, and worst-case risk.
    # - penalises the high-risk, high-return strategies (like Buy & Hold)
    # - rewards strategies that effectively manage downside risk and maintain 
    # consistency.
    ,CAPS = robust_calmar * prob_score
  ) %>%
  arrange(desc(CAPS))