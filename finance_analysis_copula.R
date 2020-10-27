
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(rfinance)


# Support functions -------------------------------------------------------
source("finance_analysis_functions.R")


# Stock Data --------------------------------------------------------------
msf <- rfinance::get_prices(symbol = 'MSFT')
apl <- rfinance::get_prices(symbol = 'AAPL')

# APPLE data has numbers coded as characters
apl[, 2:7] <- sapply(apl[, 2:7], as.numeric)
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

# Get log-returns
msf_logr <- stock_data_filtered$msf %>% 
  mutate(log_returns = log(Close / Open)) %>% 
  pull(log_returns)

apl_logr <- stock_data_filtered$apl %>% 
  mutate(log_returns = log(Close / Open)) %>% 
  pull(log_returns)


all_logr <- data.frame(MSF = msf_logr,
                       APPLE = apl_logr) %>% 
  melt(measure.vars = c("MSF", "APPLE"),
       variable.name = "Company",
       value.name = "LogReturn")

all_logr %>% 
  ggplot(aes(x = Company, y = LogReturn))+
  #geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 1, alpha = 0.1, width = 0.4)+
  coord_flip()+
  theme_bw()



