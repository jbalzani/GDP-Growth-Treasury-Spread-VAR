rm(list = ls())
setwd("C:/Users/John/Documents/Binghamton University/ECON 504 Economic Forecasting/hw10")

library(tidyverse)
library(fpp2)

library(readr)
vardata <- read_csv("vardata.csv")

vardata <- na.omit(vardata[,2:3])
vardata_ts <- ts(vardata, start = c(1980,3), end = c(2012,4), frequency = 4)
ggtsdisplay(vardata_ts[,1])
ggtsdisplay(vardata_ts[,2])

library(vars)
VARselect(vardata_ts, lag.max = 8, type = "const")

model1 <- VAR(vardata_ts, p=2, type = "const")
summary(model1)

serial.test(model1, type = "PT.asymptotic")

library(lmtest)
grangertest(vardata_ts[,"TSPREAD"], vardata_ts[,"GDPGROWTH"], order = 2)

grangertest(vardata_ts, order = 2)

(fcast <- forecast(model1, h = 10, level = 95) )

autoplot(fcast)

plot(fancy)



