
# ACTIVE FUNCTIONS - Used in crypto_get_price_history.R ------------------------

#' Convert UTC Datetime to AEST
#' 
#' @description
#' Converts a UTC datetime string to Australian Eastern Standard Time (AEST/AEDT).
#' 
#' @param utc_dttm Character string or POSIXct datetime in UTC timezone
#'
#' @return POSIXct datetime converted to Australia/Sydney timezone
#' 
#' @examples
#' \dontrun{
#' convert_utc_aest("2026-02-07 12:00:00")
#' }
convert_utc_aest <- function(utc_dttm) {
  utc_time <- ymd_hms(utc_dttm, tz = "UTC")
  with_tz(utc_time, "Australia/Sydney")
}


#' Get Hourly Historical Price Summary from Independent Reserve
#' 
#' @description
#' Fetches hourly OHLC (Open, High, Low, Close) price data from Independent 
#' Reserve's public API. Limited to past 240 hours (10 days).
#' 
#' @param pri_curr_code Primary currency code (e.g., "xbt" for Bitcoin)
#' @param sec_curr_code Secondary currency code (e.g., "aud" for Australian Dollar)
#' @param number_of_past_hours Number of hours to retrieve (max 240, default 240)
#' @param ir_pub_api_url Base URL for Independent Reserve's public API
#'
#' @return Tibble with columns:
#' \itemize{
#'   \item sttm_aest - Start timestamp in AEST
#'   \item edtm_aest - End timestamp in AEST
#'   \item PrimaryCurrencyVolume - Volume of primary currency traded
#'   \item SecondaryCurrencyVolume - Volume of secondary currency traded
#'   \item NumberOfTrades - Count of trades in the hour
#'   \item HighestSecondaryCurrencyPrice - Highest price
#'   \item LowestSecondaryCurrencyPrice - Lowest price
#'   \item OpeningSecondaryCurrencyPrice - Opening price
#'   \item ClosingSecondaryCurrencyPrice - Closing price
#'   \item dttm_updated - Timestamp when data was fetched
#'   \item md5_hash - MD5 hash for deduplication
#' }
#' 
#' @examples
#' \dontrun{
#' prices <- get_price_history("xbt", "aud", 24, ir_public_api_url)
#' }
get_price_history <- function(
    pri_curr_code
    ,sec_curr_code
    ,number_of_past_hours = 240
    ,ir_pub_api_url
) {
  
  url <- file.path(ir_pub_api_url, "GetTradeHistorySummary")
  
  # 1. Build the Request
  req <- request(url) %>% 
    req_url_query(
      primaryCurrencyCode = pri_curr_code
      ,secondaryCurrencyCode = sec_curr_code
      ,numberOfHoursInThePastToRetrieve = number_of_past_hours
    ) %>% 
    req_retry(max_tries = 3)
  
  # 2. Perform the Request
  resp <- req_perform(req)
  
  # 3. Handle Response 
  if (resp_status(resp) == 200) {
    
    # Parse JSON directly using httr2 helper
    data_raw <- resp_body_json(resp)
    
    # Process into tibble format
    data <- data_raw$HistorySummaryItems %>%  
      map_dfr(as_tibble) %>% 
      mutate(
        across(
          .cols = c(StartTimestampUtc, EndTimestampUtc)
          ,.fns = ~convert_utc_aest(utc_dttm = .x)
        )
        ,dttm_updated = Sys.time()
      ) %>% 
      rename(
        sttm_aest = StartTimestampUtc
        ,edtm_aest = EndTimestampUtc
      ) %>% 
      select(-AverageSecondaryCurrencyPrice) %>% 
      unite(
        col = "temp_concat"
        ,-dttm_updated
        ,remove = FALSE
      ) %>%  
      mutate(md5_hash = md5(temp_concat)) %>% 
      select(-temp_concat)
    
    return(data)
    
  } else {
    stop(paste("Failed to retrieve price history. Status:", resp_status(resp)))
  }
  
}


#' Merge Price History with New Data
#' 
#' @description
#' Fetches recent price data from Independent Reserve API and merges it with 
#' existing historical data. Uses MD5 hashing to identify new records and 
#' validates completeness of the merged dataset.
#' 
#' @param pri_curr_code Primary currency code (e.g., "xbt")
#' @param sec_curr_code Secondary currency code (e.g., "aud")
#' @param ir_pub_api_url Base URL for Independent Reserve's public API
#' @param base_df Existing price history tibble with md5_hash column
#'
#' @return Updated tibble with new rows merged, or NULL if no new data
#' 
#' @details
#' - Fetches last 240 hours of data from API
#' - Identifies new rows using MD5 hash comparison
#' - Validates no missing hours in merged dataset
#' - Stops execution if gaps are detected
#' 
#' @examples
#' \dontrun{
#' updated <- merge_price_history("xbt", "aud", ir_public_api_url, base_history)
#' }
merge_price_history <- function(
    pri_curr_code
    ,sec_curr_code
    ,ir_pub_api_url
    ,base_df
) {
  
  delta_df <- get_price_history(
    pri_curr_code = pri_curr_code
    ,sec_curr_code = sec_curr_code
    ,ir_pub_api_url = ir_pub_api_url
    ,number_of_past_hours = 240
  )
  
  # Find new rows based on md5 hashes
  diff_rows_md5 <- setdiff(
    delta_df$md5_hash
    ,base_df$md5_hash
  )
  
  if (length(diff_rows_md5) > 0) {
    
    log_info("Found ", length(diff_rows_md5), " additional price history rows")
    
    # Extract new rows
    diff_df <- delta_df %>% 
      filter(md5_hash %in% diff_rows_md5)
    
    # Merge with base data
    merged_df <- base_df %>% 
      bind_rows(diff_df)
    
    # Validate completeness
    missing_row <- tibble(
      sttm_aest = seq(
        min(merged_df$sttm_aest)
        ,floor_date(lubridate::now() - hours(1), unit = "hours")
        ,by = "hour"
      )
    ) %>%
      left_join(
        y = merged_df
        ,by = "sttm_aest"
      ) %>% 
      filter(
        if_any(
          .cols = -sttm_aest
          ,.fns = ~(is.na(.x))
        )
      )
    
    if (nrow(missing_row) > 1) {
      
      log_error("Missing rows found in price history. Requires manual investigation")
      stop()
      
    } else {
      
      log_success("Price history successfully merged in memory.")
      return(merged_df)
      
    }
    
  } else {
    
    log_info("No new rows detected/to be added to price history")
    return(NULL)
    
  }
  
}


#' Log Error and Stop Execution
#' 
#' @description
#' Logs an error message and terminates script execution. Optionally syncs 
#' log file to cloud storage before terminating (for cloud deployments).
#' 
#' @param e Error object from tryCatch
#' @param log_msg Character string with context about what failed
#' @param is_cloud Logical. TRUE if running in cloud (Azure/GCP), FALSE if local
#' @param log_file Path to log file (for cloud sync)
#' @param cloud_container Cloud storage container object (for cloud sync)
#'
#' @return Does not return - stops execution
#' 
#' @examples
#' \dontrun{
#' tryCatch({
#'   risky_operation()
#' }, error = function(e) {
#'   log_and_stop(e, "Operation failed", is_azure, log_file, container)
#' })
#' }
log_and_stop <- function(e, log_msg, is_cloud, log_file, cloud_container) {
  
  # 1. Format the error message
  full_msg <- paste0(log_msg, ": ", e$message)
  
  # 2. Log to file and console
  log_error(full_msg)
  message(full_msg)
  
  # 3. Sync log to cloud storage before terminating
  if (is_cloud) {
    try({
      log_info("Cloud detected: Attempting to sync failure log to storage...")
      
      # Flush the log
      log_appender(appender_console)
      
      # Upload log file
      upload_log_file_azure(
        log_file_path = log_file
        ,blob_name = paste0("logs/crypto_get_price_history/", Sys.Date(), "/get_price_history_log.log")
        ,azure_container = cloud_container
      )
    }, silent = TRUE)
  }
  
  # 4. Terminate
  stop(e)
}


#' Parse Azure Storage Connection String
#' 
#' @description
#' Extracts account name, account key, and blob endpoint URL from an Azure 
#' Storage connection string. Used for local development authentication.
#' 
#' @param conn_str Character string containing the full Azure Storage connection 
#' string (format: "DefaultEndpointsProtocol=https;AccountName=...;AccountKey=...")
#'
#' @return A list containing:
#' \itemize{
#'   \item account_name - Storage account name
#'   \item account_key - Storage account access key
#'   \item endpoint - Full blob endpoint URL
#' }
#' 
#' @examples
#' \dontrun{
#' conn_str <- Sys.getenv("AZURE_STORAGE_CONNECTION_STRING")
#' creds <- parse_azure_connection(conn_str)
#' }
parse_azure_connection <- function(conn_str) {
  parts <- strsplit(conn_str, ";")[[1]]
  account_name <- sub(".*AccountName=", "", parts[grep("AccountName", parts)])
  account_key <- sub(".*AccountKey=", "", parts[grep("AccountKey", parts)])
  
  list(
    account_name = account_name
    ,account_key = account_key
    ,endpoint = paste0("https://", account_name, ".blob.core.windows.net/")
  )
}


#' Get Azure Blob Storage Container Connection
#' 
#' @description
#' Establishes connection to Azure Blob Storage container. Automatically detects 
#' environment and uses appropriate authentication:
#' - LOCAL: Uses connection string from .Renviron
#' - AZURE CLOUD: Uses Managed Identity (no credentials needed)
#' 
#' This is the Azure equivalent of GCP's gcs_auth().
#' 
#' @param is_azure_cloud Logical. TRUE if running in Azure (uses Managed Identity), 
#' FALSE if running locally (uses connection string). Default FALSE.
#'
#' @return An AzureStor container object that can be used for storage operations
#' (upload, download, list files)
#' 
#' @details
#' Required environment variables:
#' - Local: AZURE_STORAGE_CONNECTION_STRING, AZURE_CONTAINER_NAME
#' - Cloud: AZURE_STORAGE_ACCOUNT, AZURE_CONTAINER_NAME
#' 
#' @examples
#' \dontrun{
#' # Local development
#' container <- get_azure_container(is_azure_cloud = FALSE)
#' 
#' # In Azure cloud environment
#' container <- get_azure_container(is_azure_cloud = TRUE)
#' }
get_azure_container <- function(is_azure_cloud = FALSE) {
  
  if (!is_azure_cloud) {
    # LOCAL: Use connection string from .Renviron
    log_info("Using local Azure connection string")
    
    conn_str <- Sys.getenv("AZURE_STORAGE_CONNECTION_STRING")
    creds <- parse_azure_connection(conn_str)
    
    blob_endpoint <- blob_endpoint(
      endpoint = creds$endpoint
      ,key = creds$account_key
    )
    
  } else {
    # AZURE CLOUD: Use Managed Identity
    log_info("Using Azure Managed Identity")
    
    # Get token using Azure Instance Metadata Service (IMDS)
    token_url <- "http://169.254.169.254/metadata/identity/oauth2/token?api-version=2018-02-01&resource=https://storage.azure.com/"
    
    token_response <- httr2::request(token_url) %>%
      httr2::req_headers("Metadata" = "true") %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
    
    access_token <- token_response$access_token
    
    account_name <- Sys.getenv("AZURE_STORAGE_ACCOUNT")
    endpoint_url <- paste0("https://", account_name, ".blob.core.windows.net/")
    
    blob_endpoint <- blob_endpoint(
      endpoint = endpoint_url
      ,token = access_token
    )
  }
  
  # Connect to container
  container_name <- Sys.getenv("AZURE_CONTAINER_NAME")
  container <- storage_container(blob_endpoint, container_name)
  
  log_success("Azure container connection established: ", container_name)
  
  return(container)
}


#' Download and Read RDS Object from Azure Blob Storage
#' 
#' @description
#' Downloads an RDS file from Azure Blob Storage and reads it into R memory.
#' Includes logging and error handling. This is the Azure equivalent of get_gcs_obj().
#' 
#' @param blob_name Character string specifying the blob name in Azure 
#' (e.g., "xbt_aud_price_history.rds")
#' @param azure_container AzureStor container object from get_azure_container()
#'
#' @return The R object stored in the RDS file (typically a data frame/tibble)
#' 
#' @details
#' - Downloads blob to a temporary file
#' - Reads RDS content into memory
#' - Validates the object (logs warnings if NULL)
#' - Logs row count for data frames
#' - Cleans up temporary file
#' - Stops execution on error
#' 
#' @examples
#' \dontrun{
#' container <- get_azure_container()
#' price_data <- get_azure_blob("xbt_aud_price_history.rds", container)
#' }
get_azure_blob <- function(blob_name, azure_container) {
  tryCatch({
    
    # Download blob to temp file
    temp_file <- tempfile(fileext = ".rds")
    
    storage_download(
      container = azure_container
      ,src = blob_name
      ,dest = temp_file
      ,overwrite = TRUE
    )
    
    # Read into memory
    obj <- readRDS(temp_file)
    
    # Validation logging
    if (is.null(obj)) {
      log_warn(paste("Object retrieved but is NULL:", blob_name))
    } else {
      info_msg <- paste0("Retrieved ", blob_name)
      if (is.data.frame(obj)) info_msg <- paste0(info_msg, " [Rows: ", nrow(obj), "]")
      log_info(info_msg)
    }
    
    # Cleanup
    unlink(temp_file)
    
    return(obj)
    
  }, error = function(e) {
    log_error(paste("FAILED to retrieve", blob_name, "-", e$message))
    stop()
  })
}


#' Upload R Object to Azure Blob Storage as RDS
#' 
#' @description
#' Saves an R object as RDS and uploads it to Azure Blob Storage.
#' Includes logging and error handling. This is the Azure equivalent of gcs_upload().
#' 
#' @param obj R object to upload (data frame, list, model object, etc.)
#' @param blob_name Character string specifying the destination blob name 
#' (e.g., "xbt_aud_price_history.rds")
#' @param azure_container AzureStor container object from get_azure_container()
#'
#' @return NULL (called for side effect of uploading to Azure)
#' 
#' @details
#' - Saves object to temporary RDS file
#' - Uploads to Azure Blob Storage
#' - Logs success/failure
#' - Cleans up temporary file
#' - Stops execution on error
#' 
#' @examples
#' \dontrun{
#' container <- get_azure_container()
#' updated_data <- merge_price_history(...)
#' upload_azure_blob(updated_data, "xbt_aud_price_history.rds", container)
#' }
upload_azure_blob <- function(obj, blob_name, azure_container) {
  tryCatch({
    
    # Save to temp file
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(obj, temp_file)
    
    # Upload to Azure
    storage_upload(
      container = azure_container
      ,src = temp_file
      ,dest = blob_name
    )
    
    log_success("Uploaded ", blob_name, " to Azure Blob Storage")
    
    # Cleanup
    unlink(temp_file)
    
  }, error = function(e) {
    log_error(paste("FAILED to upload", blob_name, "-", e$message))
    stop()
  })
}


#' Upload Log File to Azure Blob Storage
#' 
#' @description
#' Uploads a plain text log file to Azure Blob Storage, preserving formatting.
#' Uses direct file upload (not RDS) to maintain readability.
#' 
#' @param log_file_path Character string specifying the local log file path
#' @param blob_name Character string specifying the destination blob name
#' @param azure_container AzureStor container object from get_azure_container()
#'
#' @return NULL (called for side effect of uploading to Azure)
#' 
#' @details
#' Errors are logged but do not stop execution (log upload is non-critical).
#' 
#' @examples
#' \dontrun{
#' upload_log_file_azure(
#'   log_file_path = "/tmp/logs/app.log",
#'   blob_name = "logs/2026-02-08/app.log",
#'   azure_container = container
#' )
#' }
upload_log_file_azure <- function(log_file_path, blob_name, azure_container) {
  tryCatch({
    
    # Upload file directly (not as RDS)
    storage_upload(
      container = azure_container
      ,src = log_file_path
      ,dest = blob_name
    )
    
    log_success("Uploaded log file ", blob_name, " to Azure Blob Storage")
    
  }, error = function(e) {
    log_error(paste("FAILED to upload log file", blob_name, "-", e$message))
    # Don't stop execution for log upload failures
  })
}


# INACTIVE FUNCTIONS - Not used in crypto_get_price_history.R ------------------

#' Sign Independent Reserve Private API Requests
#' 
#' @description
#' Helper function to authenticate requests to Independent Reserve's private API
#' using HMAC-SHA256 signature.
#' 
#' @param req httr2 request object
#' @param api_key Independent Reserve API key
#' @param api_secret Independent Reserve API secret
#'
#' @return Modified httr2 request object with authentication headers
ir_private_auth <- function(req, api_key, api_secret) {
  
  sig_url <- req$url
  nonce <- Sys.time() %>% as.integer() 
  
  sig_param <- paste(
    sig_url
    ,paste0("apiKey=", api_key)
    ,paste0("nonce=", nonce)
    ,sep = ","
  )
  
  sig <- sha256(
    key = api_secret
    ,sig_param
  ) %>%
    toupper() %>% 
    as.character()
  
  req %>%
    req_method("POST") %>%
    req_body_json(
      list(
        apiKey = api_key
        ,nonce = nonce
        ,signature = sig
      )
    )
  
}


#' Get Independent Reserve Account Balances
#' 
#' @description
#' Fetches account balances from Independent Reserve's private API.
#' 
#' @param req_type API endpoint type (default "GetAccounts")
#' @param key Independent Reserve API key
#' @param secret Independent Reserve API secret
#'
#' @return Tibble with columns: AccountGuid, CurrencyCode, TotalBalance, AvailableBalance
get_ir_accounts <- function(req_type = "GetAccounts", key, secret) {
  
  # Build the request
  req <- request(
    base_url = file.path(ir_private_api_url, req_type)
  )
  
  # Authenticate and perform request
  resp <- req %>%
    ir_private_auth(key, secret) %>%
    req_perform()
  
  # Parse the response
  resp %>%
    resp_body_json() %>%
    map_df(as_tibble) %>%
    select(
      AccountGuid
      ,CurrencyCode
      ,TotalBalance
      ,AvailableBalance
    )
}


#' Simple Moving Average Crossover Trading Strategy
#' 
#' @description
#' Trend-following strategy that detects trend direction changes using two SMAs.
#' Works best in trending markets and underperforms in sideways/choppy markets.
#' Buy signals triggered when short SMA crosses above long SMA.
#' Sell signals triggered when short SMA crosses below long SMA.
#' 
#' @param prices_xts xts dataframe containing trading data with Close prices
#' @param short_n Short SMA period (default 20)
#' @param long_n Long SMA period (default 50)
#'
#' @return Tibble with columns: date, price, sma_short, sma_long, trade_signal
#' where trade_signal is 1 (buy), -1 (sell), or 0 (hold)
#' 
#' @examples
#' \dontrun{
#' signals <- strat_sma_cross(price_xts, short_n = 20, long_n = 50)
#' }
strat_sma_cross <- function(prices_xts, short_n = 20, long_n = 50) {
  
  tibble(
    date = index(prices_xts)
  ) %>% 
    mutate(
      price = as.numeric(Cl(prices_xts))
      ,sma_short = as.numeric(SMA(price, n = short_n))
      ,sma_long = as.numeric(SMA(price, n = long_n))
      ,diff_now = sma_short - sma_long
      ,diff_prev = dplyr::lag(diff_now)
      ,trade_signal = case_when(
        !is.na(diff_prev) & (diff_prev <= 0) & (diff_now > 0) ~  1L # buy
        ,!is.na(diff_prev) & (diff_prev >= 0) & (diff_now < 0) ~ -1L # sell 
        ,.default = 0L # hold
      )
    ) %>% 
    select(
      -diff_now
      ,-diff_prev
    )
  
}