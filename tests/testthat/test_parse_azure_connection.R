test_that("parse_azure_connection extracts account name correctly", {
  
  conn_str <- "DefaultEndpointsProtocol=https;AccountName=myaccount;AccountKey=mykey123==;EndpointSuffix=core.windows.net"
  
  result <- parse_azure_connection(conn_str)
  
  expect_equal(result$account_name, "myaccount")
  
})

test_that("parse_azure_connection extracts account key correctly", {
  
  conn_str <- "DefaultEndpointsProtocol=https;AccountName=myaccount;AccountKey=mykey123==;EndpointSuffix=core.windows.net"
  
  result <- parse_azure_connection(conn_str)
  
  expect_equal(result$account_key, "mykey123==")
  
})

test_that("parse_azure_connection builds correct endpoint URL", {
  
  conn_str <- "DefaultEndpointsProtocol=https;AccountName=myaccount;AccountKey=mykey123==;EndpointSuffix=core.windows.net"
  
  result <- parse_azure_connection(conn_str)
  
  expect_equal(result$endpoint, "https://myaccount.blob.core.windows.net/")
  
})

test_that("parse_azure_connection returns a list with correct names", {
  
  conn_str <- "DefaultEndpointsProtocol=https;AccountName=myaccount;AccountKey=mykey123==;EndpointSuffix=core.windows.net"
  
  result <- parse_azure_connection(conn_str)
  
  expect_true(all(c("account_name", "account_key", "endpoint") %in% names(result)))
  
})