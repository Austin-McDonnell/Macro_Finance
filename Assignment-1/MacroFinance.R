library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(plotly)
library(ts)
setwd("C:\\Users\\austi\\Documents\\Github_Repos\\Macro_Finance\\Assignment-1\\Data")

crsp = read.csv('crsp_monthly.csv')
tbilla = read.csv('TBill_Annual.csv')
tbillq = read.csv('TBill_Quarterly.csv')

#Fix dates and setup data
##################################
colnames(crsp)[1] <- "date"
colnames(tbilla)[1] <- "date"
colnames(tbillq)[1] <- "date"

crsp$date = ymd(crsp$date)
tbilla$date = ymd(tbilla$date)
tbillq$date = ymd(tbillq$date)
#################################



crsp['div'] = crsp$vwretd - crsp$vwretx# Generate Dividend returns

filter(crsp, div < 0)# Check to see if Div returns are all positive

plot_ly(crsp, x = ~date, y = ~div, type = "scatter", mode = 'lines')#plot initial dividends

crsp$quarterYear <- as.Date(as.yearqtr(crsp$date))

crsp$yearly = year(crsp$date)

crspq = crsp %>%
  group_by(quarterYear) %>%
  summarise(ret = sum(vwretx), retd = sum(vwretd), div = sum(div))

crspa = crsp %>%
  group_by(yearly) %>%
  summarise(ret = sum(vwretx), retd = sum(vwretd), div = sum(div))

plot_ly(crspq, x = ~quarterYear, y = ~div, type = "scatter", mode = 'lines')


plot_ly(crspa, x = ~yearly, y = ~div, type = "scatter", mode = 'lines')


