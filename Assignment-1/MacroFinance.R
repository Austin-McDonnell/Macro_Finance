library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(plotly)
library(ts)
library(shiny)
library(PerformanceAnalytics)
#library(aTSA)
library(tseries)
setwd("C:\\Users\\austi\\Documents\\Github_Repos\\Macro_Finance\\Assignment-1\\Data")

# Helper Functions
############################################################
# Shortcut for making line charts
linePlot = function(x, y, xlabel, ylabel, title){
  return(
    plot_ly(x = x, y = y, type = "scatter", mode = 'lines') %>%
      layout(
        title = toString(title),
        xaxis = list(title = xlabel),
        yaxis = list(title = ylabel)
      )
  )
}

# Saves all of the plots names with 'plot(number)' to the WD 
saveAllPlotsHtml = function(numPlots){
  for(i in seq.int(numPlots)){
    htmlwidgets::saveWidget(eval(parse(paste('plot', toString(i), sep =''))),
                            paste('plot', toString(i), '.html', sep = ''))
  }
}


# Returns the results from variously lagged linear regressions
regLaggedDiv = function(maxLag, dependent, independent){
  coeffMatrix = list()
  rSquared = list()
  adjRsquared = list()
  
  for(i in seq.int(maxLag)){
    independent = lag(independent, k=i)
    if(i == 1){
      independent = independent[-i]
      dependent = dependent[-i]
    }
    else{
      independent = independent[-(1:i)]
      dependent = dependent[-(1:i)]
    }
    coeffMatrix[[i]] = summary(lm(dependent ~ independent))$coefficients
    rSquared[[i]] = summary(lm(dependent ~ independent))$r.squared
    adjRsquared[[i]] = summary(lm(dependent ~ independent))$adj.r.squared
  }
  return(list(coeffMatrix, rSquared, adjRsquared))
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
tbillq$logExcessRetx = log(tbillq$vwretx + 1) - log(tbillq$t90ret + 1)
tbilla$logExcessRetx = log(tbilla$vwretx + 1) - log(tbilla$b1ret + 1)

#Calculate the log of each dividend return
tbillq$logDiv = log(tbillq$div + 1)
tbilla$logDiv = log(tbilla$div + 1)

# INITIAL PLOTTING
############################################################
# PLotting Q & Y Log Div: Does not look stationary
plot1 = linePlot(tbillq$quarter, tbillq$logDiv,
                 'Date', 'Log Dividend Return', 'Quarterly Dividend Log Returns')
plot2 = linePlot(tbilla$year, tbilla$logDiv,
                 'Date', 'Log Dividend Return', 'Annual Dividend Log Returns')

densityq = density(tbillq$logDiv)
plot3 = plot_ly(x = ~densityq$x, y = ~densityq$y,type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  layout(
    title = 'Quarterly Dividend Log Return Density',
    xaxis = list(title = 'Log Dividend Returns: Quarterly'),
    yaxis = list(title = 'Density')
  )

# Plotting Log Excess Return, Div Inclusive, Q & Y: Both look more stationary
plot4 = linePlot(tbillq$quarter, tbillq$logExcessRetd,
                 'Date', 'Log Excess Return', 'Quarterly Log Excess Returns: Dividend Inclusive')
plot5 = linePlot(tbilla$year, tbilla$logExcessRetd,
                 'Date', 'Log Excess Return', 'Yearly Log Excess Returns: Dividend Inclusive')

# Plotting Log Excess Return, Div Exclusive, Q & Y: Both look more stationary
plot6 = linePlot(tbillq$quarter, tbillq$logExcessRetx,
                 'Date', 'Log Excess Return', 'Quarterly Log Excess Returns: Dividend Exclusive')
plot7 = linePlot(tbilla$year, tbilla$logExcessRetx,
                 'Date', 'Log Excess Return', 'Yearly Log Excess Returns: Dividend Exclusive')
#############################################################

# Remove any NaN's
tbillq = na.omit(tbillq)
tbilla = na.omit(tbilla)


# Runs ADF Tests for stationarity on each variable
vars = list(tbillq$logDiv, tbillq$logExcessRetd, tbillq$logExcessRetx,
            tbilla$logDiv, tbilla$logExcessRetd, tbilla$logExcessRetx)
# ADF Tests show that Log Div both Q & Y are not stationary
ans = 1
for(i in vars){
  if(ans == 1){
    print(adf.test(i))
    print("Enter 1 to continue or 0 to exit")
    ans = readline(prompt="Answer: ")
    next
  }
  else(break)
}


plot8 = plot_ly(y = diff(tbillq$logDiv, differences = 1, lag = 1), type = "scatter", mode = 'lines') %>%
  layout(
    title = 'Logged Dividend Returns Differenced and Lagged 1',
    xaxis = list(title = 'Quarterly Date'),
    yaxis = list(title = 'Logged Dividend Returns')
  )
plot9 = plot_ly(y = diff(tbilla$logDiv, differences = 1, lag = 1), type = "scatter", mode = 'lines') %>%
  layout(
    title = 'Logged Dividend Returns Differenced and Lagged 1',
    xaxis = list(title = 'Yearly Date'),
    yaxis = list(title = 'Logged Dividend Returns')
  )

# Shows that the differencing of the log return dividends are stationary
adf.test(diff(tbillq$logDiv, differences = 1, lag = 1))
adf.test(diff(tbilla$logDiv, differences = 1, lag = 1))



# Build the LM for across the whole period for each Q & Y across multiple lags for Divs

fullPeriodRegq = regLaggedDiv(6, tbillq$logExcessRetd, tbillq$logDiv)
fullPeriodRega = regLaggedDiv(6, tbilla$logExcessRetd, tbilla$logDiv)

rollingWindow = function(dependent, independent, window){
  adjRsquared = c()
  periodLength = length(dependent)
  
  for(j in seq.int((periodLength - window))){
    
    y = dependent[j:(j + window -1)]
    x = independent[j:(j + window -1)]
    
    adjRsquared[j] = summary(lm(y ~ x))$adj.r.squared
    
  }
  return(as.data.frame(adjRsquared))
}


rollingRsquaredd = rollingWindow(tbilla$logExcessRetd, tbilla$logDiv, window = 15)

plot_ly(y = rollingRsquaredd$adjRsquared, y = row(rollingRsquaredd), type = "scatter", mode = 'lines') %>%
  layout(
    title = 'Adjusted R Squares: No Lagged Independent Variables'
  )

rollingRsquaredx = rollingWindow(tbilla$logExcessRetx, tbilla$logDiv, window = 15)

plot_ly(y = rollingRsquaredx$adjRsquared, x = row(rollingRsquaredx), type = "scatter", mode = 'lines') %>%
  layout(
    title = 'Adjusted R Squares: No Lagged Independent Variables'
  )


rollingRsquareddL1 = rollingWindow(tbilla$logExcessRetd[-1], lag(tbilla$logDiv)[-1], window = 15)

rollingRsquaredxL1 = rollingWindow(tbilla$logExcessRetx[-1], lag(tbilla$logDiv)[-1], window = 15)

dataL = data.frame(rollingRsquareddL1, rollingRsquaredxL1)

plot10 = plot_ly(y = data$adjRsquared, name = 'Dividends Inclusive: Lag 1',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = data$adjRsquared.1, name = 'Dividends Exclusive: Lag 1',
            type = 'scatter', mode = 'lines') %>%
  layout(
    title = 'Rolling Window Regression Adjusted R Squared',
    xaxis = list(title = 'Window Period'),
    yaxis = list(title = 'Adjusted R Squared')
  )


data = data.frame(rollingRsquaredd, rollingRsquaredx)

plot11 = plot_ly(y = data$adjRsquared, name = 'Dividends Inclusive',
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = data$adjRsquared.1, name = 'Dividends Exclusive',
            type = 'scatter', mode = 'lines') %>%
  layout(
    title = 'Rolling Window Regression Adjusted R Squared',
    xaxis = list(title = 'Window Period'),
    yaxis = list(title = 'Adjusted R Squared')
  )





#diff(tbillq$logDiv, differences = 1, lag = 1)

fullPeriodDifferencedDivdq = summary(lm(tbillq$logExcessRetd[-1] ~ diff(tbillq$logDiv, differences = 1, lag = 1)))

fullPeriodDifferencedDivxq = summary(lm(tbillq$logExcessRetx[-1] ~ diff(tbillq$logDiv, differences = 1, lag = 1)))

fullPeriodDifferencedDivda = summary(lm(tbilla$logExcessRetd[-1] ~ diff(tbilla$logDiv, differences = 1, lag = 1)))

fullPeriodDifferencedDivxa = summary(lm(tbilla$logExcessRetx[-1] ~ diff(tbilla$logDiv, differences = 1, lag = 1)))

rollingWindow(tbilla$logExcessRetd[-1], diff(tbilla$logDiv, differences = 1, lag = 1), window = 15)



plot12 = plot_ly(y = data$adjRsquared, name = 'Dividends Inclusive',
                 type = 'scatter', mode = 'lines') %>%
  add_trace(y = data$adjRsquared.1, name = 'Dividends Exclusive',
            type = 'scatter', mode = 'lines') %>%
  layout(
    title = 'Rolling Window Regression Adjusted R Squared',
    xaxis = list(title = 'Window Period'),
    yaxis = list(title = 'Adjusted R Squared')
  )


rollingLag = function(maxLag, dependent, independent){
  
  for(i in seq.int(maxLag)){
    independent = lag(independent, k=i)
    if(i == 1){
      independent = independent[-i]
      dependent = dependent[-i]
    }
    else{
      independent = independent[-(1:i)]
      dependent = dependent[-(1:i)]
    }
  }
}
