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
growth_path <- sqldf('select variable, avg(value) as value from db group by 1')
growth_path$d_value <- Delt(growth_path$value, type='log')
growth_path$cum_d_value <- NA
n <- nrow(growth_path)
for(i in 1:n){
  growth_path$cum_d_value[i] <- prod(exp(growth_path$d_value[i:n]))
}

ggplot(growth, aes(variable, d_value))+geom_line()

users <- sample(unique(db$id_user), 10000)
r <- which(db$id_user %in% users)
db_f <- db
#db_f <- db[r,]
#tmp <- db
db_f <- db_f[c('id_user', 'variable', 'value')]
db_f_act <- db_f <- dcast(db_f, id_user~variable, fun.aggregate=mean)

start=2
end=19
db_f <- db_f[1:19]
?cast

db_f$missing <- apply(db_f, 1, function(x) length(which(!is.finite(x))))
db_f$last_value <- 18 - db_f$missing
db_f$g18 <- apply(db_f, 1, function(x) cumgrowth(x, growth_path))
head(db_f)

x <- (db_f[100000:(100000+2),])
x
cumgrowth <- function(x, growth_path){
  if(x[21]==18){
    return(x[19])
  }
  i_last_ltr <- (x[21])+1
  i_growth_start <- (x[21])+1
  growth <- growth_path$cum_d_value[i_growth_start]
  growth_f <- x[i_last_ltr] * growth
  return(growth_f)
}


db_f[280:300,-c(1:6)]
db[200:500,]
growth_path

