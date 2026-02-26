# Automated Crypto Trading System

A production-grade infrastructure for automated cryptocurrency trading, demonstrating end-to-end deployment of algorithmic trading systems on Azure. Built for BTC-AUD trading on Independent Reserve with comprehensive backtesting, cloud deployment, and monitoring.

> **Project Status:** Infrastructure complete and operational. Trading execution paused pending market stability assessment. Lessons learned are being applied to a pivot toward traditional markets (stocks/ETFs).

## 🎯 Project Overview

### What Was Built

A fully automated cryptocurrency data pipeline running on Azure serverless infrastructure, supporting algorithmic trading strategy development and deployment. The system demonstrates:

- **Quantitative Strategy Development:** Monte Carlo backtesting framework with 8 algorithms, 20+ parameter combinations, and 5 stop-loss levels
- **Production Cloud Infrastructure:** Azure serverless deployment with Managed Identity, automated scheduling, and monitoring
- **Data Engineering:** Automated hourly price collection with MD5 deduplication, gap detection, and validation
- **DevOps Excellence:** Docker optimization, CI/CD workflows, structured logging, and error alerting
- **Security:** Credential-less authentication, secrets management via Bitwarden, zero secrets in Git

### Strategy Performance

The winning strategy (SMA Crossover 20/50 with 10% stop-loss) was validated through 1000 Monte Carlo time window samples:

| Metric | Value | Description |
|--------|-------|-------------|
| **CAPS Score** | 0.918 | Highest among all 8 strategies tested |
| **CAGR** | 62.2% | Geometric mean across all windows |
| **Win Rate** | 97.1% | Percentage of profitable time windows |
| **Sharpe Ratio** | 0.999 | Risk-adjusted return (median) |
| **CVaR Drawdown** | 65.7% | Worst-case tail risk |

**Deployment Decision:** Despite strong backtested performance, live deployment was postponed due to 2025-2026 crypto market volatility exceeding acceptable risk parameters. The infrastructure and methodology are being applied to traditional markets (stocks/ETFs) instead.

## 🛠️ Tech Stack

### Core Technologies
- **Language:** R 4.5.2
- **Cloud Platform:** Microsoft Azure
- **Containerization:** Docker
- **Version Control:** Git/GitHub

### Azure Services
- **Azure Container Instances** - Serverless compute for R scripts
- **Azure Container Registry** - Docker image storage
- **Azure Blob Storage** - Historical price data and logs
- **Azure Logic Apps** - Hourly scheduling (cron-like triggers)
- **Azure Managed Identity** - Secure authentication (no hardcoded credentials)

### R Packages
- **Data Manipulation:** `dplyr`, `tidyr`, `purrr`, `lubridate`
- **API Interaction:** `httr2`, `jsonlite`
- **Cloud Storage:** `AzureStor`, `AzureAuth`
- **Logging:** `logger` (with custom formatting)
- **Security:** `vvbitwarden` (secrets management)
- **Trading Strategy:** `xts`, `TTR` (technical indicators)

### External APIs
- **Independent Reserve** - Public API (price data) & Private API (trading)
- **Bitwarden CLI** - API key retrieval

## 🏗️ System Architecture
```
┌─────────────────┐
│  Azure Logic    │──── Triggers every hour at :05 AEDT/AEST
│     Apps        │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   Container     │──── Runs R script (crypto_get_price_history.R)
│   Instance      │
└────────┬────────┘
         │
         ├──────► Independent Reserve API (fetch hourly BTC-AUD prices)
         │
         └──────► Azure Blob Storage (store price history + logs)
```

### Key Features

**Data Pipeline:**

- Hourly price fetching with MD5-based deduplication
- Automatic gap detection and validation
- Timezone handling (UTC → AEST/AEDT conversion)
- Daily log files with cross-run appending

**Infrastructure:**

- Dockerized R environment (~2 second rebuilds via package caching)
- Azure Managed Identity (credential-less authentication)
- Structured logging with aligned timestamps
- Email alerts on failures (Logic Apps + Outlook)

**Operations:**

- $6.30/month total cost (720 hourly executions)
- 80% cheaper than always-on VM
- Pay-per-execution serverless model

**Security:**

- API keys in Bitwarden, retrieved dynamically
- Zero credentials committed to Git
- Private container registry
- Least-privilege access controls

## 📁 Repository Structure
```
crypto_trading/
├── R/
│   ├── backtesting/                # Monte Carlo simulation & strategy testing
│   ├── crypto_functions.R          # Core functions (Azure, IR API, logging)
│   ├── crypto_get_price_history.R  # Hourly price fetcher (deployed)
│   ├── crypto_load_libs.R          # Package dependencies
│   ├── crypto_vars.R               # Configuration variables
│   └── crypto_trader.R             # Trading execution (framework only)
├── tests/                          # Unit tests for core functions
├── Dockerfile                      # Optimized container definition
├── .gitignore                      # Excludes secrets and data files
└── README.md
```

## 💰 Operational Costs

| Service | Monthly Cost | Notes |
|---------|--------------|-------|
| Container Instances | ~$0.50 | 720 executions × ~2 seconds |
| Blob Storage | ~$0.20 | Price history + logs |
| Logic App | ~$0.60 | 720 hourly triggers |
| Container Registry | ~$5.00 | Docker image storage (required) |
| **Total** | **~$6.30** | 80% cheaper than always-on VM |

## 📝 Blog Posts

Technical deep-dives documenting this project:

1. [**Part 1: Survival of the Fittest (Backtesting)**](https://sactyr.github.io/posts/2025-12-19-building-an-automated-crypto-trader-part-1-survival-of-the-fittest-backtesting/)  
   Monte Carlo simulation, CAPS scoring methodology, efficient frontier analysis

2. [**Part 2: Automating Crypto Price Collection with R and Azure**](https://sactyr.github.io/posts/2026-02-11-automating-crypto-price-collection-azure/)  
   Azure deployment, Managed Identity setup, Docker optimization, logging architecture

3. **Part 3: Trading Execution & Lessons Learned** *(planned)*  
   Risk management, order execution, why market conditions matter

## 🎓 Key Learnings

### Technical Insights

1. **Azure Managed Identity** is significantly simpler than GCP service accounts
2. **Logger buffering** requires explicit flushing before file upload in ephemeral containers
3. **Docker layer optimization** (install packages before copying code) = 30x faster rebuilds
4. **Timezone management** must be explicit in cloud containers (`Sys.setenv(TZ)`)
5. **Log file appending** in serverless requires downloading existing logs first

### Strategic Insights

1. **Backtesting ≠ Deployment Readiness** - Market conditions matter
2. **Crypto volatility** (2026 market instability) can invalidate otherwise solid strategies
3. **Risk management** must account for real-world execution challenges beyond backtests
4. **Infrastructure before trading** - Build robust pipelines first, trade second
5. **Cost optimization** matters - Serverless saves 80-90% vs always-on compute

## 🔄 Future Direction

The infrastructure and methodologies developed here are being applied to:

- **Traditional markets** (stocks, ETFs via Interactive Brokers)
- **Lower volatility instruments** (S&P 500 index)
- **Same strategy** (SMA 20/50 with 10% stop-loss) re-backtested on stock data

This repository remains as a complete reference implementation of quantitative strategy development, production cloud deployment, and automated trading infrastructure.

## 📚 Technical References

- [Independent Reserve API Documentation](https://www.independentreserve.com/products/api)
- [Azure Container Instances Documentation](https://docs.microsoft.com/en-us/azure/container-instances/)
- [Rocker Project (R + Docker)](https://rocker-project.org/)
- [Azure Managed Identity Overview](https://docs.microsoft.com/en-us/azure/active-directory/managed-identities-azure-resources/)

## 📬 Contact

Questions about the implementation? Connect on [LinkedIn](https://linkedin.com/in/nagakaruppiah).

---

**Disclaimer:** This is an educational project demonstrating algorithmic trading infrastructure. Cryptocurrency and automated trading involve substantial risk. Code provided as-is with no guarantees of profitability. Past backtested performance does not guarantee future results.