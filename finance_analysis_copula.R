
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

all_logr <- logr_df %>% 
  melt(measure.vars = c("msf", "apl", "idxx", "bbuy"),
       variable.name = "Company",
       value.name = "LogReturn")

all_logr %>% 
  ggplot(aes(x = Company, y = LogReturn))+
  #geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 1, alpha = 0.1, width = 0.4)+
  coord_flip()+
  theme_bw()


GGally::ggpairs(logr_df, 
                diag = list(continuous = wrap("densityDiag")),
                lower = list(continuous = wrap("smooth", alpha = 0.1, size = 1)))+
  theme_bw()


compute_tail_dep <- function(samp1, samp2, probs){
  taildep <- c()
  for(i in 1:length(probs)){
    taildep[i] <- extRemes::taildep(
      samp1,
      samp2,
      u = probs[i]
    )["chi"]
  }
  
  return(taildep)
}

n <- 100
p <- seq(0.5, 0.99, length.out = n)
right_tail_dep_msf_apl <- compute_tail_dep(logr_df$msf, logr_df$apl, probs = p)
left_tail_dep_msf_apl <- compute_tail_dep(-logr_df$msf, -logr_df$apl, probs = p) %>% rev()

whole_tail_dep_msf_apl <- c(left_tail_dep_msf_apl[-n], right_tail_dep_msf_apl)
plot(whole_tail_dep_msf_apl)

right_tail_dep_msf_bbuy <- compute_tail_dep(logr_df$msf, logr_df$bbuy, probs = p)
left_tail_dep_msf_bbuy <- compute_tail_dep(-logr_df$msf, -logr_df$bbuy, probs = p) %>% rev()

whole_tail_dep_msf_bbuy <- c(left_tail_dep_msf_bbuy[-n], right_tail_dep_msf_bbuy)
plot(whole_tail_dep_msf_bbuy)

right_tail_dep_idxx_bbuy <- compute_tail_dep(logr_df$idxx, logr_df$bbuy, probs = p)
left_tail_dep_idxx_bbuy <- compute_tail_dep(-logr_df$idxx, -logr_df$bbuy, probs = p) %>% rev()

whole_tail_dep_idxx_bbuy <- c(left_tail_dep_idxx_bbuy[-n], right_tail_dep_idxx_bbuy)
plot(whole_tail_dep_idxx_bbuy)


right_taildep_df <- data.frame(msf_vs_apl = right_tail_dep_msf_apl, 
                               msf_vs_bbuy = right_tail_dep_msf_bbuy, 
                               idxx_vs_bbuy = right_tail_dep_idxx_bbuy,
                               probs = seq(0.5, 0.99, length.out = n))

left_taildep_df <- data.frame(msf_vs_apl = left_tail_dep_msf_apl, 
                              msf_vs_bbuy = left_tail_dep_msf_bbuy, 
                              idxx_vs_bbuy = left_tail_dep_idxx_bbuy,
                              probs = seq(0.01, 0.99, length.out = n))
right_taildep_df %>% 
  melt(id.vars = "probs") %>%
  ggplot()+
  geom_point(aes(x = probs, y = value, color = variable))


left_taildep_df %>% 
  melt(id.vars = "probs") %>%
  ggplot()+
  geom_point(aes(x = probs, y = value, color = variable))



# PCA ---------------------------------------------------------------------

library(factoextra)


res.pca <- prcomp(logr_df, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "cos2", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



