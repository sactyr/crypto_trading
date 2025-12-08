
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
)

# Load the packages and suppress startup messages
suppressPackageStartupMessages({
  purrr::walk(required_packages, library, character.only = TRUE)
})

# Load all packages except furrr and lubridate for parallel worker function
worker_packages <- required_packages[!required_packages %in% c("furrr", "lubridate")]


# 02 LOAD DEPENDABLES -----------------------------------------------------

crypto_dir <- Sys.getenv("CRYPTO_TRADING_FOLDER")

# furrr options
plan(multisession, workers = availableCores() - 1)
options(future.globals.maxSize = 2048 * 1024^2)
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