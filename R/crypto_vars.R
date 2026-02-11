# Get Independent Reserve's API URLs
ir_base_api_url <- "https://api.independentreserve.com"
ir_private_api_url <- file.path(ir_base_api_url, "Private")
ir_public_api_url <- file.path(ir_base_api_url, "Public")

# Get Independent Reserve's API key identifiers
ir_uuid <- Sys.getenv(x = c("IR_API_UUID", "IR_SECRET_KEY_UUID"))

# Crypto currency variables
primary_currency_code <- "xbt" # bitcoin's code
secondary_currency_code <- "aud"

# In crypto trading, when you sell all your Bitcoin, the exchange often leaves 
# behind a tiny, microscopic fraction (e.g., $0.00000012$ BTC) due to rounding 
# or fee calculations. This is called "Dust."
btc_dust_threshold <- 0.0001

# Azure Storage
azure_storage_account <- Sys.getenv("AZURE_STORAGE_ACCOUNT")
azure_container_name <- Sys.getenv("AZURE_CONTAINER_NAME")
azure_connection_string <- Sys.getenv("AZURE_STORAGE_CONNECTION_STRING")
azure_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"