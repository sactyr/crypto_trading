Work in progress (WIP): A fully automated end-to-end crypto trading system, 
primarily trading BTC-AUD.

Trading decisions/signals are 100% determined by algorithms, with no human input.
These algorithms have been tested via a Monte Carlo simulation, in which 1000 
time window samples are backtested using combination of trading algorithms,
stop-losses and various other parameters.

Hourly prices will be automatically downloaded and merged to ~~a local dataset~~ Azure Blob Storage.
