test_that("strat_sma_cross returns correct columns", {
  
  prices <- xts(
    x = c(1:60) * 100
    ,order.by = as.Date("2025-01-01") + 0:59
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  expect_true(all(c("date", "price", "sma_short", "sma_long", "trade_signal") %in% names(result)))
  
})

test_that("strat_sma_cross returns correct number of rows", {
  
  prices <- xts(
    x = c(1:60) * 100
    ,order.by = as.Date("2025-01-01") + 0:59
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  expect_equal(nrow(result), 60)
  
})

test_that("strat_sma_cross trade_signal only contains -1, 0, or 1", {
  
  prices <- xts(
    x = c(1:60) * 100
    ,order.by = as.Date("2025-01-01") + 0:59
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  expect_true(all(result$trade_signal %in% c(-1L, 0L, 1L)))
  
})

test_that("strat_sma_cross generates a BUY signal on upward crossover", {
  
  # First 50 periods declining (short SMA stays below long SMA),
  # then sharply rising to force short SMA to cross above long SMA
  prices <- xts(
    x        = c(seq(200, 100, length.out = 50), seq(110, 300, length.out = 30))
    ,order.by = as.Date("2025-01-01") + 0:79
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  expect_true(any(result$trade_signal == 1L))
  
})

test_that("strat_sma_cross generates a SELL signal on downward crossover", {
  
  # First 50 periods rising (short SMA above long SMA),
  # then sharply declining to force short SMA to cross below long SMA
  prices <- xts(
    x = c(seq(100, 200, length.out = 50), seq(190, 50, length.out = 30))
    ,order.by = as.Date("2025-01-01") + 0:79
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  expect_true(any(result$trade_signal == -1L))
  
})

test_that("strat_sma_cross returns NA for sma_short before short_n periods", {
  
  prices <- xts(
    x = c(1:60) * 100
    ,order.by = as.Date("2025-01-01") + 0:59
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  expect_true(all(is.na(result$sma_short[1:19])))
  
})

test_that("strat_sma_cross returns NA for sma_long before long_n periods", {
  
  prices <- xts(
    x = c(1:60) * 100
    ,order.by = as.Date("2025-01-01") + 0:59
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  expect_true(all(is.na(result$sma_long[1:49])))
  
})

test_that("strat_sma_cross returns all HOLD when prices trend perfectly flat", {
  
  # Flat prices mean SMAs never cross, so no BUY or SELL signals expected
  prices <- xts(
    x = rep(100, 60)
    ,order.by = as.Date("2025-01-01") + 0:59
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  expect_true(all(result$trade_signal == 0L))
  
})

test_that("strat_sma_cross date column matches xts index", {
  
  dates  <- as.Date("2025-01-01") + 0:59
  prices <- xts(
    x        = c(1:60) * 100
    ,order.by = dates
  )
  colnames(prices) <- "Close"
  
  result <- strat_sma_cross(prices, short_n = 20, long_n = 50)
  
  result_dates <- result$date
  attributes(result_dates) <- NULL
  
  expect_equal(result_dates, as.numeric(dates))
  
})