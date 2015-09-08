# Model testing
library(data.table)
library(reshape2)
library(plyr)
library(quantmod)
library(lubridate)
library(ggplot2)
library(sqldf)
library(Kmisc)
source('multiplot.R')

db <- fread('Data/forecast.csv', data.table=F)
db <- db[-1]
mean(db$ltr_18_month_forecast, na.rm=T)
mean(db$'18', na.rm=T)
i=4


#s <- sample(nrow(db), 50000)
db[which(db$id_users %in% users),]
table <- as.data.frame(matrix(NA, 14, 14))
rownames(table) <- 4:17
colnames(table) <- 1:14
table

j=14
i=4

for(j in colnames(table)){
  for(i in rownames(table)){
    
    print(paste(as.numeric(i),as.numeric(j),sep=', '))
    if(!is.na(table[i,j])){
      #print(table[i,j])
      print('skip')
      next()
    }
    last_available <- as.numeric(i)
    forecast_month <- last_available + as.numeric(j)
    if((forecast_month) > 18){
      print(last_available)
      print(forecast_month)
      print(last_available+forecast_month)
      next()
    }
    
    db_test <- db
    db_test <- db_test[which(!is.na(db_test[forecast_month+1])),]
    db_test$actual <- db_test[,(forecast_month+1)]
    db_test[(last_available+2):19] <- NA 
    
    
    db_test$missing <- apply(db_test, 1, function(x) length(which(!is.finite(x)))) #  Count number of periods to corecast
    db_test$last_value <- 18 - db_test$missing #  Row containing last actual ltr value
    
    db_test$ltr_18_month_forecast <- apply(db_test, 1, function(x) cumgrowth(x, growth_path$d_value, end=forecast_month)) #  Forecast LTR
    
    res <- mean(db_test$actual) - mean(db_test$ltr_18_month_forecast)
    table[i,j] <- res
    print(res)
  }
}
mean(db_test$ltr_18_month_forecast)
mean(db_test$actual)

write.cb(table)
head(d)
d <- fread('Data/ready_data.csv', data.table=T, stringsAsFactors=T)
d_s <- d[,.N, by=id_user]
d_s <- d[attr_category_level_1=='Online Performance']
users <- as.data.frame(d_s[,id_user])

