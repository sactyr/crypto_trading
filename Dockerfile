# Use Rocker R base image with R 4.5.2
FROM rocker/r-ver:4.5.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy R scripts folder
COPY R/ /app/R/

# Install R packages
RUN R -e "install.packages(c('vvbitwarden', 'purrr', 'httr2', 'jsonlite', 'dplyr', 'tidyr', 'readr', 'stringr', 'lubridate', 'logger', 'openssl', 'AzureStor', 'AzureAuth', 'xts', 'quantmod'), repos='https://cran.rstudio.com/')"

# Set environment variables for Azure (will be overridden at runtime)
ENV CRYPTO_TRADING_FOLDER=/app
ENV AZURE_CONTAINER_INSTANCE=true

# Run the script
CMD ["Rscript", "/app/R/crypto_get_price_history.R"]