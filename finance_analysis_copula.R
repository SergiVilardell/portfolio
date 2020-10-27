
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(rfinance)


# Support functions -------------------------------------------------------
source("finance_analysis_functions.R")


# Stock Data --------------------------------------------------------------
msf <- rfinance::get_prices(symbol = 'MSFT')
apl <- rfinance::get_prices(symbol = 'AAPL')
stock_data <- list(msf = msf, apl = apl)

# Are starting and ending dates the same?
min(msf$Date) == min(apl$Date) # FALSE  
max(msf$Date) == max(apl$Date) # TRUE

# Filter the stock data by the starting date
starting_date <- select_starting_date(stock_data)
stock_data_filtered <- map(stock_data, ~.x %>% filter(Date >= starting_date))

# Now check that all the dates match
logic_check <- stock_data_filtered$msf$Date == stock_data_filtered$apl$Date
n_rows <- nrow(stock_data_filtered$apl)

# This will be true if every date coincides
sum(logic_check)/n_rows == 1