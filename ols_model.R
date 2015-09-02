library(data.table)
library(tseries)
library(sqldf)
library(ggplot2)
library(plyr)
library(quantmod)
library(reshape2)
setwd('/Users/transferwise/LTR-forecast')
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
head(db)
x <- sqldf('select variable, attr_category_level_1, avg(value) as value from db group by 1,2')
head(x)
#x$d_value <- Delt(x$value, type='log')[,1]
x <- ddply(x, "attr_category_level_1", transform,  DeltaCol = Delt(value, type='arithmetic'), .progress='text')
names(x)[ncol(x)] <- 'd_value'
ggplot(x, aes(variable, d_value, color=attr_category_level_1))+geom_line()

# Aggregate data, no grouping
growth <- sqldf('select variable, avg(value) as value from db group by 1')
growth$d_value <- Delt(x$value, type='log')[,1]
ggplot(growth, aes(variable, d_value))+geom_line()

users <- sample(unique(db$id_user), 1000)
r <- which(db$id_user %in% users)
db_f <- db[r,]
#tmp <- db
db_f <- db_f[c('id_user', 'variable', 'value')]
db_f_act <- db_f <- dcast(db_f, id_user~variable, fun.aggregate=mean)

start=2
end=19
db_f <- db_f[1:end]

db_f$missing <- apply(db_f, 1, function(x) length(which(is.na(x))))
db_f$last_value <- 18 - forecast$missing
db_f[100:200,]
db_f$g18 <- apply(db_f, 1, function(x) cumgrowth(x, growth))
tail(db_f)
db_f$f18 <- apply(db_f, 1, function(x) foo(x, growth))
db_f[100:120,]
x=db_f[300,]
x
cumgrowth <- function(x, growth){
  if(x[20]==18){
    return(x[19])
  }
  growth_base = x[20]
  start = as.numeric(x[20]+1)
  end = 18
  pgrowth <- prod(exp(growth[start:end]))
  return(pgrowth)
}





growth[start:end]
start
end
growth<-growth_x$d_value
growth_x<-growth
head(db_f)
forecast$growth_forecast
x$d_value
names(db_f)
db_f[forecast$last_value:forecast$total_months,]  
x[1]
db_f[10,21]
x[2,2]
head(db_f)
start = forecast$last_value[1]+1



pb <- txtProgressBar(min = 1, max = nrow(tmp), initial = 1, char = "=", width = NA)
for(i in 1:nrow(tmp)){
  
  if(is.na(tmp[i,end])){
    last_value <- min(which(is.na(tmp[i,])))-1
    last_period <- last_value - 1
    remaining_periods <- 18-last_period
    if(remaining_periods > 16){
      next()
    }
    #for(j in 1:remaining_periods){
    #  forecast <- tmp[i,(last_value+j-1)] * exp( x$d_value[last_period+j] )
      forecast <- tmp[i,(last_value+j-1)] * prod(exp( x$d_value[(last_period+1):(end-1)] ))
      tmp[i,end] <- forecast
      #tmp[i,last_value+j] <- forecast
    #}
  }
  setTxtProgressBar(pb, i)
}
close(pb)
??setTxtProgessBar
x
tmp[i,last_value+j+1]
tmp
head(tmp)
head(tmp_act)
x$d_value
sum(exp( x$d_value[(last_period+1):end]))
mean(tmp$`18`)
mean(tmp_act$`18`, na.rm=T)
a <- melt(tmp, id='id_user')
a_act <- melt(tmp_act, id='id_user')
a <- sqldf("select variable, avg(value) as value from a group by 1")
a_act <- sqldf("select variable, avg(value) as value from a_act group by 1")

a<- a[order(a[,1]),]
a_act <- a_act[order(a_act[,1]),]
a[,1] <- as.numeric(a[,1])
a_act[,1] <- as.numeric(a_act[,1])
plot(a, type='l')
lines(a_act, col='red')
a_act

db[r,]
