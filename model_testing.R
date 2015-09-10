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

d <- fread('Data/ready_data.csv')
d[,.N, by=attr_category_level_1]
d_s <- d[,.N, by=.(id_user, attr_category_level_1)]
d_s1 <- d[attr_category_level_1=='Online Performance']
d_s2 <- d[attr_category_level_1=='Brand']
d_s3 <- d[attr_category_level_1=='Virality']
d_s4 <- d[attr_category_level_1=='Unknown']
users1 <- as.numeric(d_s1[,id_user])
users2 <- as.numeric(d_s2[,id_user])
users3 <- as.numeric(d_s3[,id_user])
users4 <- as.numeric(d_s4[,id_user])
s <- which(db$id_user %in% users4)

table <- model_performance(s=502000, db=db)
write.cb(table)
  
d_s <- d[,.N, by=.(id_user, first_ccy_target)]
d[,.N, by=first_ccy_target]
d_s1 <- d[first_ccy_target=='GBP']
d_s2 <- d[first_ccy_target=='EUR']
d_s3 <- d[first_ccy_target=='USD']
d_s4 <- d[first_ccy_target=='INR']
users1 <- as.numeric(d_s1[,id_user])
users2 <- as.numeric(d_s2[,id_user])
users3 <- as.numeric(d_s3[,id_user])
users4 <- as.numeric(d_s4[,id_user])
s <- which(db$id_user %in% users4)
table <- model_performance(db=db)
write.cb(table)


head(d)
d_s <- d[,.N, by=.(id_user, first_ccy_target, attr_category_level_1)]
d[,.N, by=.(first_ccy_target, attr_category_level_1)]
d_s1 <- d[attr_category_level_1=='Virality']
d_s1 <- d_s1[first_ccy_target=='INR']

users1 <- as.numeric(d_s1[,id_user])

s <- which(db$id_user %in% users1)
table <- model_performance(s=s, db=db)
write.cb(table)
