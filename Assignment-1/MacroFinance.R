library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(plotly)
library(ts)
library(shiny)
library(PerformanceAnalytics)
library(aTSA)
setwd("C:\\Users\\austi\\Documents\\Github_Repos\\Macro_Finance\\Assignment-1\\Data")

# Helper Functions
############################################################
# Shortcut for making line charts
linePlot = function(x, y){
  return(
    plot_ly(x = x, y = y, type = "scatter", mode = 'lines')
  )
}
###########################################################

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

# Convert Simple returns (calculated by CRSP) to Log Returns: r = log(R + 1)
# Calculate the excess dividend inclusive index log return 
tbillq$logExcessRetd = log(tbillq$vwretd + 1) - log(tbillq$t90ret + 1)
tbilla$logExcessRetd = log(tbilla$vwretd + 1) - log(tbilla$b1ret + 1)

# Calculate the excess dividend exclusive index log return
tbillq$logExcessRet = log(tbillq$vwretx + 1) - log(tbillq$t90ret + 1)
tbilla$logExcessRet = log(tbilla$vwretx + 1) - log(tbilla$b1ret + 1)

#Calculate the log of each dividend return
tbillq$logDiv = log(tbillq$div + 1)
tbilla$logDiv = log(tbilla$div + 1)

# INITIAL PLOTTING
############################################################
# PLotting Q & Y Log Div: Does not look stationary
linePlot(tbillq$quarter, tbillq$logDiv)
linePlot(tbilla$year, tbilla$logDiv)

# Plotting Log Excess Return, Div Inclusive, Q & Y: Both look more stationary
linePlot(tbillq$quarter, tbillq$logExcessRetd)
linePlot(tbilla$year, tbilla$logExcessRetd)

# Plotting Log Excess Return, Div Exclusive, Q & Y: Both look more stationary
linePlot(tbillq$quarter, tbillq$logExcessRet)
linePlot(tbilla$year, tbilla$logExcessRet)
#############################################################


# Runs ADF Tests for stationarity on each variable
vars = list(tbillq$logDiv, tbillq$logExcessRetd, tbillq$logExcessRet,
            tbilla$logDiv, tbilla$logExcessRetd, tbilla$logExcessRet)
ans = 0
for(i in vars){
  print(adf.test(i))
  print("Enter 1 to continue or 0 to exit")
  ans = readline(prompt="Answer: ")
  
  if(ans == 1){
    next
  }
  else(break)
}


# TODO: Build VARS Model Selection for the appropriate regression









