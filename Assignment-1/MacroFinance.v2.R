library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(plotly)
library(ts)
library(shiny)
library(PerformanceAnalytics)
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
  setwd("C:\\Users\\austi\\Documents\\Github_Repos\\Macro_Finance\\Assignment-1\\Data")
  for(i in 1:numPlots){
    print(i)
    htmlwidgets::saveWidget(eval(parse(paste('plot', toString(i), sep =''))), paste('plot', toString(i), '.html', sep = ''))
  }
}


# Returns the results from variously lagged linear regressions; from lag 0 to Max Lag
regLaggedDiv = function(maxLag, dependent, independent){
  coeffMatrix = list()
  rSquared = list()
  adjRsquared = list()
  
  coeffMatrix[[1]] = summary(lm(dependent ~ independent))$coefficients
  rSquared[[1]] = summary(lm(dependent ~ independent))$r.squared
  adjRsquared[[1]] = summary(lm(dependent ~ independent))$adj.r.squared
  
  
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
    coeffMatrix[[i+1]] = summary(lm(dependent ~ independent))$coefficients
    rSquared[[i+1]] = summary(lm(dependent ~ independent))$r.squared
    adjRsquared[[i+1]] = summary(lm(dependent ~ independent))$adj.r.squared
  }
  return(list(coeffMatrix, rSquared, adjRsquared))
}

# Rolling Window regression Function
rollingWindow = function(dependent, independent, window){
  adjRsquared = c()
  rSquared = c()
  periodLength = length(dependent)
  
  for(j in seq.int((periodLength - window))){
    
    y = dependent[j:(j + window -1)]
    x = independent[j:(j + window -1)]
    reg = summary(lm(y ~ x))
    
    adjRsquared[j] = reg$adj.r.squared
    rSquared[j] = reg$r.squared
  }
  return(cbind.data.frame(rSquared, adjRsquared))
}

# Rolling Lag Function
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
crsp['div'] = ((crsp$vwretd + 1)/(crsp$vwretx + 1))-1

# Check to see if Div returns are all positive
filter(crsp, div < 0)

################################################
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

# Calculate the excess dividend inclusive index return; adjusted for inflation
tbillq$excessRetd = ((1 + tbillq$vwretd)/(1 + tbillq$cpiret)) - ((1 +tbillq$t90ret)/(1 + tbillq$cpiret))
tbilla$excessRetd = ((1 + tbilla$vwretd)/(1 + tbilla$cpiret)) - ((1 +tbilla$b1ret)/(1 + tbilla$cpiret))

# Calculate the excess dividend exclusive index return; adjusted for inflation
tbillq$excessRetx = ((1 + tbillq$vwretx)/(1 + tbillq$cpiret)) - ((1 +tbillq$t90ret)/(1 + tbillq$cpiret))
tbilla$excessRetx = ((1 + tbilla$vwretx)/(1 + tbilla$cpiret)) - ((1 +tbilla$b1ret)/(1 + tbilla$cpiret))

# Remove any NaN's
tbillq = na.omit(tbillq)
tbilla = na.omit(tbilla)


# Runs ADF Tests for stationarity on each variable
vars = list(tbillq$div, tbillq$excessRetd, tbillq$excessRetx,
            tbilla$div, tbilla$excessRetd, tbilla$excessRetx)
varString = list('Q Div', 'Q Div Inc Excess Returns', 'Q Div X Excess Returns',
                 'Y Div', 'Y Div Inc Excess Returns', 'Y Div X Excess Returns')

# ADF Tests show that Div both Q & Y are not stationary
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


# Shows that the differencing of the return dividends are stationary
adf.test(diff(tbillq$div, differences = 1, lag = 1))
adf.test(diff(tbilla$div, differences = 1, lag = 1))

# Build the LM for across the whole period for each Q & Y across multiple lags for Divs

fullPeriodRegq = regLaggedDiv(7, tbillq$excessRetd, tbillq$div)
fullPeriodRega = regLaggedDiv(7, tbilla$excessRetd, tbilla$div)


#########################################################
#Rolling Regression

# No Lag: Div Inclusive
rollingRsquaredd = rollingWindow(tbilla$excessRetd, tbilla$div, window = 15)

# No Lag: Div Exclusive
rollingRsquaredx = rollingWindow(tbilla$excessRetx, tbilla$div, window = 15)

# Lag 1: Div Inclusive
rollingRsquareddL1 = rollingWindow(tbilla$excessRetd[-1], lag(tbilla$div)[-1], window = 15)

# Lag:1 Div Exlusive
rollingRsquaredxL1 = rollingWindow(tbilla$excessRetx[-1], lag(tbilla$div)[-1], window = 15)

# Lag & Differenced 1: Div Exclusive
rollingRsquareddDiff = rollingWindow(tbilla$excessRetd[-1], diff(tbilla$div, differences = 1, lag = 1), window = 15)


# INITIAL PLOTTING
############################################################
# PLotting Q & Y Div: Does not look stationary
plot1 = linePlot(tbillq$quarter, tbillq$div,
                 'Date', 'Dividend Return', 'Quarterly Dividend Returns')
plot2 = linePlot(tbilla$year, tbilla$div,
                 'Date', 'Dividend Return', 'Annual Dividend Returns')

densityq = density(tbillq$div)
plot3 = plot_ly(x = ~densityq$x, y = ~densityq$y,type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  layout(
    title = 'Quarterly Dividend Return Density',
    xaxis = list(title = 'Dividend Returns: Quarterly'),
    yaxis = list(title = 'Density')
  )

divDensityq = density(tbillq$div)
divDensitya = density(tbilla$div)
excessRetdDensityq = density(tbillq$excessRetd)
excessRetdDensitya = density(tbilla$excessRetd)
plot11 = plot_ly(x = ~divDensityq$x, y = ~divDensityq$y,type = 'scatter', mode = 'lines', fill = 'tozeroy', name = 'Quarterly') %>%
  add_trace(x = ~divDensitya$x, y = ~divDensitya$y,type = 'scatter', mode = 'lines', name = 'Yearly') %>%
  
  layout(
    title = 'Div Return Density',
    xaxis = list(title = 'Density'),
    yaxis = list(title = 'Density'),
    legend = list(x = 0.30, y = 0.8)
  )

plot12 = plot_ly(x = ~excessRetdDensityq$x, y = ~divDensitya$y,type = 'scatter', mode = 'lines', fill = 'tozeroy', name = 'Quarterly') %>%
          add_trace(x = ~excessRetdDensitya$x, y = ~divDensitya$y,type = 'scatter', mode = 'lines', name = 'Yearly') %>%
  layout(
    title = 'Div Inclusive Excess Return Density',
    xaxis = list(title = 'Density'),
    yaxis = list(title = 'Density'),
    legend = list(x = 0.70, y = 0.6)
  )
  

# Plotting Excess Return, Div Inclusive, Q & Y: Both look more stationary
# Plotting Excess Return, Div Exclusive, Q & Y: Both look more stationary

plot4 = plot_ly(x = tbillq$quarter, y = tbillq$excessRetd, name = 'Excess Return: Div Inclusive',
        type = 'scatter', mode = 'lines') %>%
  
  add_trace(y = tbillq$excessRetx, name = 'Excess Return: Div Exclusive',
            type = 'scatter', mode = 'lines', line = list(dash = 'dot'), opacity = .7) %>%
  
  layout(
    title = 'Quarterly Excess Return',
    xaxis = list(title = 'Date', tickangle = -45),
    yaxis = list(title = 'Excess Return'),
    legend = list(x = 0.40, y = 0.6)
  )

plot5 = plot_ly(x = tbilla$year, y = tbilla$excessRetd, name = 'Excess Return: Div Inclusive',
        type = 'scatter', mode = 'lines') %>%
  
  add_trace(y = tbilla$excessRetx, name = 'Excess Return: Div Exclusive',
            type = 'scatter', mode = 'lines', line = list(dash = 'dot'), opacity = .7) %>%
  
  layout(
    title = 'Yearly Excess Return',
    xaxis = list(title = 'Date', tickangle = -45),
    yaxis = list(title = 'Excess Return'),
    legend = list(x = 0.40, y = 0.98)
  )

# Plotting the differenced Dividend

plot6 = plot_ly(x = tbillq$quarter[-1], y = diff(tbillq$div, differences = 1, lag = 1), type = "scatter", mode = 'lines') %>%
  layout(
    title = 'Dividend Returns Differenced and Lagged 1',
    xaxis = list(title = 'Quarterly Date'),
    yaxis = list(title = 'Dividend Returns')
  )
plot7 = plot_ly(x = tbilla$year[-1], y = diff(tbilla$div, differences = 1, lag = 1), type = "scatter", mode = 'lines') %>%
  layout(
    title = 'Dividend Returns Differenced and Lagged 1',
    xaxis = list(title = 'Yearly Date'),
    yaxis = list(title = 'Dividend Returns')
  )

plot8 = plot_ly(x = tbilla$year[1:60], y = rollingRsquaredx$adjRsquared, name = 'Div Exclusive: Adjusted R<sup>2</sup>',
                type = 'scatter', mode = 'lines') %>%
  
  add_trace(y = rollingRsquaredx$rSquared, name = 'Div Exclusive: R<sup>2</sup>',
            type = 'scatter', mode = 'lines', line = list(dash = 'dot')) %>%
  
  add_trace(y = rollingRsquaredd$adjRsquared, name = 'Div Inclusive: Adjusted R<sup>2</sup>',
            type = 'scatter', mode = 'lines') %>%
  
  add_trace(y = rollingRsquaredd$rSquared, name = 'Div Inclusive: R<sup>2</sup>',
            type = 'scatter', mode = 'lines', line = list(dash = 'dot')) %>%
  
  layout(
    title = 'Rolling Window Regression, Lag: 0',
    xaxis = list(title = 'Window Period', tickangle = -45),
    yaxis = list(title = 'R<sup>2</sup> Values'),
    legend = list(x = 0.40, y = 0.93)
  )
plot9 = plot_ly(x = tbilla$year[1:59], y = rollingRsquaredxL1$adjRsquared, name = 'Div Exclusive: Adjusted R<sup>2</sup>',
                type = 'scatter', mode = 'lines') %>%
  
  add_trace(y = rollingRsquaredxL1$rSquared, name = 'Div Exclusive: R<sup>2</sup>',
            type = 'scatter', mode = 'lines', line = list(dash = 'dot')) %>%
  
  add_trace(y = rollingRsquareddL1$adjRsquared, name = 'Div Inclusive: Adjusted R<sup>2</sup>',
            type = 'scatter', mode = 'lines') %>%
  
  add_trace(y = rollingRsquareddL1$rSquared, name = 'Div Inclusive: R<sup>2</sup>',
            type = 'scatter', mode = 'lines', line = list(dash = 'dot')) %>%
  
  layout(
    title = 'Rolling Window Regression, Lag: 1',
    xaxis = list(title = 'Window Period', tickangle = -45),
    yaxis = list(title = 'R<sup>2</sup> Values'),
    legend = list(x = 0.40, y = 0.98)
  )

plot10 = plot_ly(x = tbilla$year[1:59], y = rollingRsquareddDiff$rSquared, name = 'Div Inclusive: R<sup>2</sup>',
                 type = 'scatter', mode = 'lines') %>%
  add_trace(y = rollingRsquareddDiff$adjRsquared, name = 'Div Inclusive: Adj R<sup>2</sup>',
            type = 'scatter', mode = 'lines', line = list(dash = 'dot')) %>%
  layout(
    title = 'Rolling Window Regression, Lag: 1, Differenced: 1',
    xaxis = list(title = 'Window Period', tickangle = -45),
    yaxis = list(title = 'R<sup>2</sup> Values'),
    legend = list(x = 0.6, y = 0.85)
  )



#############################################################
