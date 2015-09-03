cumgrowth(x, growth_path, end=6)
cumgrowth <- function(x, growth_path=vector(), start=(x[21]+1), end=18){
  # Function to use with apply for calcualting the forecasted LTR
  # If no need to forecast, return actual 18 month LTR
  if(x[21]>=end){
    return(x[(end+1)])
  }
  
  # If less than three months of data, return NA
  if(x[21]<3){
    return(NA)
  }
  
  i_last_ltr <- start
  i_growth_start <- start
  
  cum_growth <- growth_path[i_growth_start:end]
  cum_growth <- prod(exp(cum_growth))

  growth_f <- x[i_last_ltr] * cum_growth
  return(growth_f)
}