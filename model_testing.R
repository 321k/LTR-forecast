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
mean(db_test$ltr_18_month_forecast, na.rm=T)
mean(db$'18', na.rm=T)
i=4

table <- as.data.frame(matrix(NA, 14, 14))
rownames(table) <- 4:17
colnames(table) <- 1:14
table

for(j in colnames(table)){
  for(i in rownames(table)){
    
    print(as.numeric(i),as.numeric(j),sep=', ')
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
    db_test <- db_test[which(!is.na(db_test[1+forecast_month])),]
    db_test$actual <- db_test[,(last_available+2)]
    db_test[(last_available+2):19] <- NA
    
    db_test$missing <- apply(db_test, 1, function(x) length(which(!is.finite(x)))) #  Count number of periods to corecast
    db_test$last_value <- 18 - db_test$missing #  Row containing last actual ltr value
    
    db_test$ltr_18_month_forecast <- apply(db_test, 1, function(x) cumgrowth(x, growth_path$d_value, end=forecast_month)) #  Forecast LTR
    
    res <- mean(db_test$actual, na.rm=T) - mean(db_test$ltr_18_month_forecast, na.rm=T)
    table[i,j] <- res
    print(res)
  }
}

write.cb(table)

table1 <- table
table_bu

last_available <- i
forecast_month <- last_available+2

print(i)
db_test <- db
db_test <- db_test[which(!is.na(db_test[1+forecast_month])),]
db_test$actual <- db_test[,(last_available+2)]
db_test[(last_available+2):19] <- NA

db_test$missing <- apply(db_test, 1, function(x) length(which(!is.finite(x)))) #  Count number of periods to corecast
db_test$last_value <- 18 - db_test$missing #  Row containing last actual ltr value

db_test$ltr_18_month_forecast <- apply(db_test, 1, function(x) cumgrowth(x, growth_path$d_value, end=forecast_month)) #  Forecast LTR

res <- mean(db_test$actual, na.rm=T) - mean(db_test$ltr_18_month_forecast, na.rm=T)
print(res)
write.cb(res, col.names=F)
i=i+1

# Growth path comparisons
db_g <- fread('Data/ready_data.csv', data.table=F, stringsAsFactors=T)
db_g[,'value'] <- as.numeric(db_g[,'value'])




for(i in c('attr_category_level_1', 'cntry_first_address')){
  db_g[,i]  <- as.factor(db_g[,i])
}

############################################
# Table with month on month expected growth
############################################

db <- fread('Data/ltr_data.csv', sep=',', na.strings='NULL', data.table=F)
db$attr_category_level_1[which(is.na(db$attr_category_level_1))]='Unknown'
db$cntry_first_address[which(is.na(db$cntry_first_address))]='Unknown'
fees_incr <- 32:55
db[fees_incr] <- apply(db[fees_incr], 2, as.numeric)

include <- names(db)[c(24, 18,1,8, fees_incr)]
ltr <- db[include]
ltr <- melt(ltr, na.rm=T, id.vars=include[1:4])
sum(ltr$value) - sum(colSums(db[fees_incr], na.rm=T)) #  The total sum stays intact
ltr$variable <- as.character(ltr$variable)
ltr$variable <- as.numeric(substr(ltr$variable, 12, 13))
ltr <- ltr[order(ltr$variable),]
ltr <- ltr[order(ltr$id_user),]
ltr$first_successful_payment <- as.Date(ltr$first_successful_payment, '%Y-%m-%d')
#ltr$yearmon <- yearmon(ltr$first_successful_payment)
ltr$year <- year(ltr$first_successful_payment)
gp1 <- sqldf('select variable, year, avg(value) as value from ltr group by 1,2')
gp1$year <- as.character(gp1$year)
gp1 <- ddply(gp1, "year", transform,  DeltaCol = Delt(value, type='log'))
names(gp1)[ncol(gp1)] <- 'd_value'

p1 <- ggplot(gp1, aes(variable, d_value, color=year))+geom_line()+xlab('Months since first transfer')+ylab('Change in actual LTR')+ggtitle('Change in actual LTR by year of first transfer')

gp2 <- sqldf('select variable, avg(value) as value from ltr group by 1')
gp2$d_value <- Delt(gp2$value, type='log')[,1]

p2 <- ggplot(gp2, aes(variable, d_value))+geom_line()+xlab('Months since first transfer')+ylab('Change in actual LTR')+ggtitle('Change in actual LTR on average')
multiplot(p1,p2)

gp3 <- sqldf('select variable, attr_category_level_1, avg(value) as value from ltr group by 1,2')
gp3 <- ddply(gp3, "attr_category_level_1", transform,  DeltaCol = Delt(value, type='log'))
names(gp3)[ncol(gp3)] <- 'd_value'
p3 <- ggplot(gp3, aes(variable, d_value, color=attr_category_level_1))+geom_line()+xlab('Months since first transfer')+ylab('Change in actual LTR')+ggtitle('Change in actual LTR oby attribution')
p3
head(gp3)
