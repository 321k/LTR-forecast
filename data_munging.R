setwd('/Users/transferwise/LTR-forecast')

library(data.table)
library(reshape2)
library(plyr)
library(quantmod)

db <- fread('Data/ltr_data.csv', sep=',', na.strings='NULL', data.table=F)
#markets <- fread('Data/Market definition.csv', sep=',', na.strings='#N/A', data.table=F)[c(2,4)]
#names(markets) <- c('cntry_first_address', 'market')
#db <- join(db, markets, type='left')
#db$market[which(is.na(db$market))] <- 'ROW'
db$attr_category_level_1[which(is.na(db$attr_category_level_1))] <- 'other'
db$cntry_first_address[which(is.na(db$cntry_first_address))] <- 'other'

names(db)
fees_incr <- 109:132
fees_incr <- 32:55
db[fees_incr] <- apply(db[fees_incr], 2, as.numeric)
include <- names(db)[c(18,1,8, fees_incr)]

ltr <- db[include]

ltr <- melt(ltr, na.rm=T, id.vars=include[1:3])
sum(ltr$value) - sum(colSums(db[fees_incr], na.rm=T)) #  The total sum stays intact
ltr$variable <- as.character(ltr$variable)
ltr$variable <- as.numeric(substr(ltr$variable, 12, 13))
ltr <- ltr[order(ltr$variable),]
ltr <- ltr[order(ltr$id_user),]

ltr <- data.table(ltr)
setkey(ltr, id_user)
ltr <- ltr[, diff:= Delt(value, type='arithmetic'), by = id_user]
#ltr <- ddply(ltr, "id_user", transform,  DeltaCol = Delt(value, type='arithmetic'), .progress='text')
#ltr2 <- ltr

write.csv(ltr, 'Data/ready_data.csv')
