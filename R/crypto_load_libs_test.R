# Test-only library loader
# Use this file locally for running tests. Never include in Docker/Azure.
# Sources production libs first, then adds test-specific dependencies.

# Source production libs
source(file.path(crypto_dir, "R", "crypto_load_libs.R"))

# Test-only dependencies
suppressPackageStartupMessages({
  library(testthat)
})