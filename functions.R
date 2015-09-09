library(data.table)

cumgrowth <- function(x, growth_path=vector(), start=(x[21]+1), end=18, adjustment=0){
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
  cum_growth <- prod(exp(cum_growth/(1+adjustment))) 

  growth_f <- x[i_last_ltr] * cum_growth
  return(growth_f)
}

model_performance <- function(s=NULL, db, g=growth_path$d_value){
  pb <- txtProgressBar(max = 14*14)
  count <- 0
  table <- as.data.frame(matrix(NA, 14, 14))
  rownames(table) <- 4:17
  colnames(table) <- 1:14

  for(j in colnames(table)){
    for(i in rownames(table)){
      count <- count + 1
      setTxtProgressBar(pb, count)
      if(!is.na(table[i,j])){
        next()
      }
      last_available <- as.numeric(i)
      forecast_month <- last_available + as.numeric(j)
      if((forecast_month) > 18){
        next()
      }
      
      if(length(s)==0){
        db_test <- db
      } else {
        db_test <- db[s,]
      }
      
      db_test <- db_test[which(!is.na(db_test[forecast_month+1])),]
      db_test$actual <- db_test[,(forecast_month+1)]
      db_test[(last_available+2):19] <- NA 
      
      
      db_test$missing <- apply(db_test, 1, function(x) length(which(!is.finite(x)))) #  Count number of periods to corecast
      db_test$last_value <- 18 - db_test$missing #  Row containing last actual ltr value
      
      db_test$ltr_18_month_forecast <- apply(db_test, 1, function(x) cumgrowth(x, g, end=forecast_month)) #  Forecast LTR
      
      res <- mean(db_test$actual) - mean(db_test$ltr_18_month_forecast)
      table[i,j] <- res
    }
  }
  return(table)
}

# Updated version of function from the BTYD package for increased speed
dc.SplitUpElogForRepeatTrans <- function(elog) {
  elog <- elog[order(elog$date),]
  elog <- elog[order(elog$cust),]
  
  dc.WriteLine("Started Creating Repeat Purchases")
  unique.custs <- unique(elog$cust)
  
  x <- data.table(elog)
  x$i <- 1:nrow(elog)
  keycols <- c('cust', 'date')
  setkeyv(x, keycols)
  first <- x[J(unique(cust)), mult='first']
  first <- as.data.frame(first)
  last <- x[J(unique(cust)), mult='last']
  last <- as.data.frame(last)
  
  repeat.trans.elog <- elog[-first$i, ]
  first.trans.data <- as.data.frame(first)
  last.trans.data <- as.data.frame(last)
  
  
  # [-1] is because we don't want to change the column name for custs
  names(first.trans.data)[-1] <- paste("first.", names(first.trans.data)[-1], sep = "")
  names(first.trans.data)[which(names(first.trans.data) == "first.date")] <- "birth.per"
  names(last.trans.data) <- paste("last.", names(last.trans.data), sep = "")
  
  # [-1] is because we don't want to include two custs columns
  cust.data <- data.frame(first.trans.data, last.trans.data[, -1])
  names(cust.data) <- c(names(first.trans.data), names(last.trans.data)[-1])
  
  dc.WriteLine("Finished Creating Repeat Purchases")
  return(list(repeat.trans.elog = repeat.trans.elog, cust.data = cust.data))
}
