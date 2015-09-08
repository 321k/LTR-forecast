setwd('/Users/transferwise/LTR-forecast')

library(data.table)
library(reshape2)
library(plyr)
library(quantmod)

db <- db_b <- fread('Data/ltr_data.csv', sep=',', na.strings='NULL', data.table=F)

db$attr_category_level_1[which(is.na(db$attr_category_level_1))]='Unknown'
db$cntry_first_address[which(is.na(db$cntry_first_address))]='Unknown'
fees_incr <- 32:55
db[fees_incr] <- apply(db[fees_incr], 2, as.numeric)
measures <- c('id_user','first_successful_payment', 'attr_category_level_1','cntry_first_address','first_ccy_target')
include <- c(measures, names(db)[fees_incr])

ltr <- db[include]
ltr <- melt(ltr, na.rm=T, id.vars=measures)
sum(ltr$value) - sum(colSums(db[fees_incr], na.rm=T)) #  The total sum stays intact
ltr$variable <- as.character(ltr$variable)
ltr$variable <- as.numeric(substr(ltr$variable, 12, 13))
ltr <- ltr[order(ltr$variable),]
ltr <- ltr[order(ltr$id_user),]
ltr$first_successful_payment <- as.Date(ltr$first_successful_payment, '%Y-%m-%d')
ltr$first_successful_payment_year <- year(ltr$first_successful_payment)

n <- nrow(ltr)
prev_value <- c(NA, ltr$value[-n])
prev_value[which(ltr$variable==1)] = NA
ltr$d_value <- log(ltr$value/prev_value)

write.csv(ltr, 'Data/ready_data.csv')
