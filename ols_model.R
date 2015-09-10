# Not an actual ols model, legacy name

setwd('/Users/transferwise/LTR-forecast')

library(data.table)
library(tseries)
library(sqldf)
library(ggplot2)
library(plyr)
library(quantmod)
library(reshape2)
library(Kmisc)
library(foreach)
source('functions.R')

db <- fread('Data/ready_data.csv', data.table=F, stringsAsFactors=T)
db[,'value'] <- as.numeric(db[,'value'])

# The measures variable should always be the same as in the data_munging script
#measures <- c('id_user','first_successful_payment', 'attr_category_level_1','cntry_first_address','first_ccy_target')

for(i in measures){
  db[,i]  <- as.factor(db[,i])
}

db$id_user <- as.numeric(as.character(db$id_user))
growth_path <- sqldf('select variable, avg(value) as value from db group by 1')
growth_path$d_value <- Delt(growth_path$value, type='log')

db_f <- db
db_f <- db_f[c('id_user', 'variable', 'value')]
db_f <- dcast(db_f, id_user~variable, fun.aggregate=mean)
db_f <- db_f[1:19] #  Remove LTR beyond 18 months

db_f$missing <- apply(db_f, 1, function(x) length(which(!is.finite(x)))) #  Count number of periods to forecast
db_f$last_value <- 18 - db_f$missing #  Row containing last actual ltr value

db_f$ltr_18_month_forecast <- apply(db_f, 1, function(x) cumgrowth(x, growth_path$pred)) #  Forecast LTR

for(i in 1:18){
  db_f[ncol(db_f)+1] <- apply(db_f, 1, function(x) cumgrowth(x, growth_path$d_value, end=i)) #  Forecast LTR
  names(db_f)[ncol(db_f)] <- paste('ltr_', i, '_month_forecast', sep='')
  print(i)
}

write.csv(db_f, 'Data/forecast.csv')


# Break the file into parts for easier upload to MySQL
steps=100000
counter=1
check <- 0
for(i in seq(1, nrow(db_f), by=steps)){
  for_upload <- (db_f[i:min(nrow(db_f),(i+steps-1)), c(1, 20, 22:39)])
  print(paste(counter, nrow(for_upload), sep=', '))
  write.table(for_upload, paste('Data/for_upload', counter, '.csv', sep=""), sep=',', na='NULL', row.names=FALSE, col.names = FALSE)
  counter <- counter+1
  check <- check + nrow(for_upload)
}
names(growth_path

db_f[which(db_f$missing=2)[100:200],]
?write.table
