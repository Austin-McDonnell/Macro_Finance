# HEADER CODE: DATA SETUP
##################################################
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
crsp['div'] = crsp$vwretd - crsp$vwretx
#################################


build = function(data, period){
  if(period == 'Quarterly'){
    data = generate_data(data, quarter)
    
  }
  
  if(period == 'Yearly'){
    data = generate_data(data, year)
    
  }
  
  
}


generate_data = function(data, period){
  
  return(
    data %>%
      group_by(period) %>%
      summarise(vwretx = Return.cumulative(vwretx, geometric = TRUE),
                vwretd = Return.cumulative(vwretd, geometric = TRUE),
                div = Return.cumulative(div, geometric = TRUE))
  )
  
}




#Functions Code
timePeriod = function(data, period){
  
  if(period == 'Annually'){
    data %>%
      group_by(year) %>%
      summarise(vwretx = Return.cumulative(vwretx, geometric = TRUE),
                vwretd = Return.cumulative(vwretd, geometric = TRUE),
                div = Return.cumulative(div, geometric = TRUE))
    colnames(data)[1] = "date"
  }
  
  if(period == 'Quarterly'){
    data %>%
      group_by(quarter) %>%
      summarise(vwretx = Return.cumulative(vwretx, geometric = TRUE),
                vwretd = Return.cumulative(vwretd, geometric = TRUE),
                div = Return.cumulative(div, geometric = TRUE))
    colnames(data)[1] = "date"
  }
  
  else{
    return(data)
  }
  
}

generate_plot = function(data, time){
  lineChart = plotly(data, x = ~time, y = ~div, type = 'scatter', mode = 'lines')
  
  
}