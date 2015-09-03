setwd('/Users/transferwise/LTR-forecast')

library(data.table)
library(tseries)
library(sqldf)
library(ggplot2)
library(plyr)
library(quantmod)
library(reshape2)
source('functions.R')

db <- db_d <- fread('Data/ready_data.csv', data.table=F, stringsAsFactors=T)
#load('Data/ready_data.RData') #  Data frame is named ltr
#db <- ltr

#db[c('variable', 'value', 'diff')] <- apply(db[c('variable', 'value', 'diff')], 2, as.numeric)
#db[c('variable', 'value')] <- apply(db[c('variable', 'value')], 2, as.numeric)
db[,'value'] <- as.numeric(db[,'value'])

head(db)
for(i in c('attr_category_level_1', 'cntry_first_address')){
  db[,i]  <- as.factor(db[,i])
}

#db <- db[-which(db$variable==1),]
#db <- db[-which(db$variable==2),]
#db$variable <- db$variable -1
#db$diff[which(!is.finite(db$diff))] <- 0
#db$value_p1 <- db$value + 1

# Aggregate data by attribution
#head(db)
#x <- sqldf('select variable, attr_category_level_1, avg(value) as value from db group by 1,2')

#x$d_value <- Delt(x$value, type='log')[,1]
#x <- ddply(x, "attr_category_level_1", transform,  DeltaCol = Delt(value, type='arithmetic'), .progress='text')
#names(x)[ncol(x)] <- 'd_value'
#ggplot(x, aes(variable, d_value, color=attr_category_level_1))+geom_line()

# Table with month on month expected growth
growth_path <- sqldf('select variable, avg(value) as value from db group by 1')
growth_path$d_value <- Delt(growth_path$value, type='log')
growth_path$cum_d_value <- NA
n <- nrow(growth_path)
for(i in 1:n){
  growth_path$cum_d_value[i] <- prod(exp(growth_path$d_value[i:n]))
}


# Old code for creating samples
#users <- sample(unique(db$id_user), 10000)
#r <- which(db$id_user %in% users)
#db_f <- db[r,]

db_f <- db
db_f <- db_f[c('id_user', 'variable', 'value')]
db_f <- dcast(db_f, id_user~variable, fun.aggregate=mean)
db_f <- db_f[1:19] #  Remove LTR beyond 18 months

db_f$missing <- apply(db_f, 1, function(x) length(which(!is.finite(x)))) #  Count number of periods to corecast
db_f$last_value <- 18 - db_f$missing #  Row containing last actual ltr value

db_f$ltr_18_month_forecast <- apply(db_f, 1, function(x) cumgrowth(x, growth_path)) #  Forecast LTR
#db_f$ltr_17_month_forecast <- apply(db_f, 1, function(x) cumgrowth(x, growth_path, end=17)) #  Forecast LTR
#db_f$ltr_6_month_forecast <- apply(db_f, 1, function(x) cumgrowth(x, growth_path, end=6)) #  Forecast LTR

write.csv(db_f, 'Data/forecast.csv')


for_upload1 <- for_upload[1:100000,]
for_upload2 <- for_upload[nrow(),]
for_upload3
for_upload4

db_upload <- db_f[c('id_user', 'ltr_18_month_forecast')]

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

nrow(db_f) - check


