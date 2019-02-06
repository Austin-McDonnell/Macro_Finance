library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(plotly)
library(ts)
library(shiny)
library(PerformanceAnalytics)
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

tbilla$year = year(tbilla$date)
tbillq$quarter = as.Date(as.yearqtr(tbillq$date))

crsp$quarter = as.Date(as.yearqtr(crsp$date))
crsp$year = year(crsp$date)
#################################

# Generate Dividend returns
crsp['div'] = crsp$vwretd - crsp$vwretx

# Check to see if Div returns are all positive
filter(crsp, div < 0)

# Plot initial monthly dividends
#plot_ly(crsp, x = ~date, y = ~div, type = "scatter", mode = 'lines')

# Generate quarterly geometric cumulative sum
crspq = crsp %>%
  group_by(quarter) %>%
  summarise(vwretx = Return.cumulative(vwretx, geometric = TRUE),
            vwretd = Return.cumulative(vwretd, geometric = TRUE),
            div = Return.cumulative(div, geometric = TRUE))
# Generate yearlt geometric cumulative sums
crspa = crsp %>%
  group_by(year) %>%
  summarise(vwretx = Return.cumulative(vwretx, geometric = TRUE),
            vwretd = Return.cumulative(vwretd, geometric = TRUE),
            div = Return.cumulative(div, geometric = TRUE))

# Join matching data based on the quarter and year dates
tbillq = left_join(tbillq, crspq, by = 'quarter')
tbilla = left_join(tbilla, crspa, by = 'year')

# Calculate the excess dividend inclusive index return 
tbillq$excessRetd = tbillq$vwretd - tbillq$t90ret
tbilla$excessRetd = tbilla$vwretd - tbilla$b1ret

# Plot the dividend returns quarterly and yearly
#plot_ly(crspq, x = ~quarter, y = ~div, type = "scatter", mode = 'lines')
#plot_ly(crspa, x = ~yearly, y = ~div, type = "scatter", mode = 'lines')



