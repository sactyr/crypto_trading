# Test runner - source this file from RStudio to execute all tests locally.
# Never include in Docker/Azure - tests are development-only.

crypto_dir <- Sys.getenv("CRYPTO_TRADING_FOLDER")

source(file.path(crypto_dir, "R", "crypto_vars.R"))
source(file.path(crypto_dir, "R", "crypto_load_libs_test.R"))
source(file.path(crypto_dir, "R", "crypto_functions.R"))

# Run all tests
test_dir(
  path = file.path(crypto_dir, "tests", "testthat")
  ,reporter = "progress"
)
