
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(rfinance)
library(GGally)


# Support functions -------------------------------------------------------
source("finance_analysis_functions.R")


# Stock Data --------------------------------------------------------------
msf <- rfinance::get_prices(symbol = 'MSFT')
apl <- rfinance::get_prices(symbol = 'AAPL')
idxx <- rfinance::get_prices(symbol = 'IDXX')
bbuy <- rfinance::get_prices(symbol = 'BBY')
intc <- rfinance::get_prices(symbol = 'INTC')
work <- rfinance::get_prices(symbol = 'WORK')

rtyinc <- rfinance::get_prices(symbol = 'O')

# APPLE data has numbers coded as characters
apl[, 2:7] <- sapply(apl[, 2:7], as.numeric)
idxx[, 2:7] <- sapply(idxx[, 2:7], as.numeric)
bbuy[, 2:7] <- sapply(bbuy[, 2:7], as.numeric)
stock_data <- list(msf = msf,
                   apl = apl, 
                   idxx = idxx, 
                   bbuy = bbuy,
                   intc = intc,
                   work = work,
                   rtyinc = rtyinc)

# Are starting and ending dates the same?
min(msf$Date) == min(apl$Date) # FALSE  
max(msf$Date) == max(apl$Date) # TRUE

# Filter the stock data by the starting date
starting_date <- select_starting_date(stock_data)
stock_data_filtered <- map(stock_data, ~.x %>% filter(Date >= starting_date))

# Now check that all the dates match
logic_check <- stock_data_filtered$bbuy$Date == stock_data_filtered$apl$Date
n_rows <- nrow(stock_data_filtered$apl)

# This will be true if every date coincides
sum(logic_check)/n_rows == 1

# Get log-returns



logr_df <- map_df(stock_data_filtered, ~log(.x$Close / .x$Open))
saveRDS(logr_df, file = "logr_data.RDS")



