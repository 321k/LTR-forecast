# Model testing
library(data.table)

db <- fread('Data/forecast.csv')

mean(db_f$ltr_18_month_forecast, na.rm=T)
mean(db_f$ltr_18_month_forecast_l, na.rm=T)
mean(db$'18', na.rm=T)

test_start = 6
test <-
  head(db[,1])

time <- 1:24

plot(exp(growth_path$d_value)-1, type='l', xlab='Months since first transfer', ylab='MoM revenue growth', main='Average for all users')
?plot
fit <- loess(growth_path$d_value~time)
#fit <- (growth_path$d_value~poly(time,3,raw=T))
lines(time, predict(fit, data.frame(time)), col='red')
length(fit$fitted)

predict(fit, data.frame(c(1:30)))

