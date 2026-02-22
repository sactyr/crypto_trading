# Helper to build a mock httr2 response
mock_httr2_response <- function(status, body) {
  structure(
    list(
      status_code = as.integer(status)
      ,body       = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
      ,headers    = list("content-type" = "application/json")
      ,url        = "https://api.independentreserve.com/Private/PlaceMarketBuyOrder"
      ,cache      = new.env(parent = emptyenv())
    )
    ,class = "httr2_response"
  )
}

# Mock responses
successful_buy_response <- mock_httr2_response(
  status = 200
  ,body  = list(
    OrderGuid             = "aaa-111-bbb-222"
    ,OrderType            = "MarketBuy"
    ,Status               = "Open"
    ,PrimaryCurrencyCode  = "Xbt"
    ,SecondaryCurrencyCode = "Aud"
  )
)

successful_sell_response <- mock_httr2_response(
  status = 200
  ,body  = list(
    OrderGuid             = "ccc-333-ddd-444"
    ,OrderType            = "MarketSell"
    ,Status               = "Open"
    ,PrimaryCurrencyCode  = "Xbt"
    ,SecondaryCurrencyCode = "Aud"
  )
)

failed_response <- mock_httr2_response(
  status = 400
  ,body  = list(Message = "Unauthorized")
)


# Tests -----------------------------------------------------------------------

test_that("place_ir_order stops on invalid order type", {
  
  expect_error(
    place_ir_order(
      type    = "InvalidType"
      ,amount = 1000
      ,key    = "dummy_key"
      ,secret = "dummy_secret"
    )
    ,regexp = "Invalid order type"
  )
  
})

test_that("place_ir_order returns correct fields on successful MarketBuy", {
  
  resp <- place_ir_order(
    type       = "MarketBuy"
    ,amount    = 1000
    ,key       = "dummy_key"
    ,secret    = "dummy_secret"
    ,perform_fn = function(...) successful_buy_response
  )
  
  expect_equal(resp$OrderType, "MarketBuy")
  expect_equal(resp$OrderGuid, "aaa-111-bbb-222")
  expect_equal(resp$Status,    "Open")
  
})

test_that("place_ir_order returns correct fields on successful MarketSell", {
  
  resp <- place_ir_order(
    type       = "MarketSell"
    ,amount    = 0.01
    ,key       = "dummy_key"
    ,secret    = "dummy_secret"
    ,perform_fn = function(...) successful_sell_response
  )
  
  expect_equal(resp$OrderType, "MarketSell")
  expect_equal(resp$OrderGuid, "ccc-333-ddd-444")
  expect_equal(resp$Status,    "Open")
  
})

test_that("place_ir_order stops and logs on non-200 response", {
  
  expect_error(
    place_ir_order(
      type       = "MarketBuy"
      ,amount    = 1000
      ,key       = "dummy_key"
      ,secret    = "dummy_secret"
      ,perform_fn = function(...) failed_response
    )
    ,regexp = "Order placement failed"
  )
  
})