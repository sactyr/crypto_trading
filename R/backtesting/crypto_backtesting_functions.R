
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
