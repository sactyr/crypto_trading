
# 01 LOAD LIBRARIES, FUNCTIONS & VARS -------------------------------------

# Determine environment
is_azure <- Sys.getenv("AZURE_CONTAINER_INSTANCE") != "" || 
  Sys.getenv("WEBSITE_INSTANCE_ID") != ""

# Set timezone for Azure (containers default to UTC)
if (is_azure) {
  Sys.setenv(TZ = "Australia/Sydney")
}

trade_env <- if (is_azure) {
  "Azure"
} else {
  "Local machine"
}

# Get working directory from environment variable (set in .Renviron locally or Dockerfile in GCP)
crypto_dir <- Sys.getenv("CRYPTO_TRADING_FOLDER")

# Ensure the working directory is set to the project root
setwd(crypto_dir)

# Source files
source(file.path(crypto_dir, "R", "crypto_load_libs.R"))
source(file.path(crypto_dir, "R", "crypto_vars.R"))
source(file.path(crypto_dir, "R", "crypto_functions.R"))

# Define data paths
local_data_dir <- file.path(crypto_dir, "outputs", "historical trade data")

historical_file_name <- paste(
  primary_currency_code
  ,secondary_currency_code
  ,"price_history.rds"
  ,sep = "_"
)


# 02 LOGGER & AUTHENTICATION ----------------------------------------------

# Setup Logger Path
if (is_azure) {
  # In Azure, we use /tmp to ensure write permissions
  log_dir <- file.path("/tmp", "logs", Sys.Date())
} else {
  # Locally, use your preferred nested structure
  log_dir <- file.path(crypto_dir, "logs", "crypto_get_price_history", Sys.Date())
}

# Create the directory if it doesn't exist
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

# Define the log file
log_file <- file.path(log_dir, "get_price_history_log.log")

# Set the appender (append if file exists, create if not)
log_appender(appender_file(log_file, append = file.exists(log_file)))

log_info("--- GETTING HISTORICAL PRICES START ---")

log_info("Running on ", trade_env)

# Azure authentication
tryCatch({
  
  azure_container <- get_azure_container(is_azure_cloud = is_azure)
  
  log_success("Azure Authentication: SUCCESS")
  
}, error = function(e) {
  
  azure_auth_error_msg <- paste("CRITICAL: Azure Authentication FAILED -", e$message)
  
  # Log this error to file
  log_error(azure_auth_error_msg)
  
  # Also print to Console so it's guaranteed to be in Azure Logs
  message(azure_auth_error_msg)
  
  stop()
  
})


# 03 GET PRICE HISTORY ----------------------------------------------------

log_info("Attempting to pull historical data from Azure: ", historical_file_name)

# Pull from Azure Blob Storage
tryCatch({
  
  base_history <- get_azure_blob(
    blob_name = historical_file_name
    ,azure_container = azure_container
  )
  
  log_success("Historical data loaded successfully from Azure. Rows: ", nrow(base_history))
  
}, error = function(e) {
  
  log_and_stop(
    e = e
    ,log_msg = "Failed to pull history from Azure"
    ,is_cloud = is_azure
    ,log_file = log_file
    ,cloud_container = azure_container
  )
  
})


# 04 MERGE, UPDATE & SAVE -------------------------------------------------

# The merge_price_history function is already wrapped in internal logging
tryCatch({
  
  updated_history <- merge_price_history(
    pri_curr_code = primary_currency_code
    ,sec_curr_code = secondary_currency_code
    ,ir_pub_api_url = ir_public_api_url
    ,base_df = base_history
  )
  
  if (!is.null(updated_history)) {
    
    if (!is_azure) {
      
      # LOCAL SAVE
      log_info("Saving updated RDS locally")
      
      saveRDS(
        object = updated_history
        ,file = file.path(local_data_dir, historical_file_name)
      )
      
    } else {
      
      # AZURE SAVE
      log_info("Uploading updated RDS to Azure Blob Storage")
      
      upload_azure_blob(
        obj = updated_history
        ,blob_name = historical_file_name
        ,azure_container = azure_container
      )
      
    }
    
    log_success("Price history update cycle complete.")
    
  } else {
    
    log_info("No update required for Azure/Local storage.")
    
  }
  
}, error = function(e) {
  
  log_and_stop(
    e = e
    ,log_msg = "Error during merge or save process"
    ,is_cloud = is_azure
    ,log_file = log_file
    ,cloud_container = azure_container
  )
  
})


# 05 FINAL LOG SYNC & CLEANUP ---------------------------------------------

log_info("--- GETTING HISTORICAL PRICES END ---")

# The following ensures logs in Azure are written to Blob Storage
if (is_azure) {
  # Flush and sync the successful log to Azure
  try({
    
    upload_log_file_azure(
      log_file_path = log_file,
      blob_name = paste0("logs/crypto_get_price_history/", Sys.Date(), "/get_price_history_log.log"),
      azure_container = azure_container
    )
  }, silent = TRUE)
  
}