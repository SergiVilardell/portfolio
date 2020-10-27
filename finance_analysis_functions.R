
# Select starting date ----------------------------------------------------

# Get the minimum starting date among all stocks so that we get a common starting point using most of the data
select_starting_date <- function(data){
  mins_date <- reduce(map(data, ~min(.x$Date)), c) 
  starting_date <- max(mins_date)
  return(starting_date)
}

