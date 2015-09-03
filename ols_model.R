setwd('/Users/transferwise/LTR-forecast')

library(data.table)
library(tseries)
library(sqldf)
library(ggplot2)
library(plyr)
library(quantmod)
library(reshape2)
library(Kmisc)
source('functions.R')

db <- db_d <- fread('Data/ready_data.csv', data.table=F, stringsAsFactors=T)
db[,'value'] <- as.numeric(db[,'value'])

head(db)
for(i in c('attr_category_level_1', 'cntry_first_address')){
  db[,i]  <- as.factor(db[,i])
}


# Table with month on month expected growth
growth_path <- sqldf('select variable, avg(value) as value from db group by 1')
growth_path$d_value <- Delt(growth_path$value, type='log')

fit <- loess(growth_path$d_value~c(1:24))
growth_path$loess <- c(NA, fit$fitted)

n <- nrow(growth_path)


db_f <- db
db_f <- db_f[c('id_user', 'variable', 'value')]
db_f <- dcast(db_f, id_user~variable, fun.aggregate=mean)
db_f <- db_f[1:19] #  Remove LTR beyond 18 months

db_f$missing <- apply(db_f, 1, function(x) length(which(!is.finite(x)))) #  Count number of periods to corecast
db_f$last_value <- 18 - db_f$missing #  Row containing last actual ltr value

db_f$ltr_18_month_forecast <- apply(db_f, 1, function(x) cumgrowth(x, growth_path$d_value)) #  Forecast LTR

write.csv(db_f, 'Data/forecast.csv')

steps=100000
counter=1
check <- 0
for(i in seq(1, nrow(db_f), by=steps)){
  for_upload <- (db_f[i:min(nrow(db_f),(i+steps-1)), c('id_user', 'ltr_18_month_forecast')])
  print(paste(counter, nrow(for_upload), sep=', '))
  write.table(for_upload, paste('Data/for_upload', counter, '.csv', sep=""), sep=',', na='NULL', row.names=FALSE)
  counter <- counter+1
  check <- check + nrow(for_upload)
}

