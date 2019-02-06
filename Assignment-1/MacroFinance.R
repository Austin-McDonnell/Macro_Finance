library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(plotly)
library(ts)
library(shiny)
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
#################################



crsp['div'] = crsp$vwretd - crsp$vwretx# Generate Dividend returns

filter(crsp, div < 0)# Check to see if Div returns are all positive

#plot_ly(crsp, x = ~date, y = ~div, type = "scatter", mode = 'lines')#plot initial dividends

crsp$quarter = as.Date(as.yearqtr(crsp$date))

crsp$year = year(crsp$date)

timePeriod = function(data, period){
  
  if(period == 'Annually'){
    return(
      data %>%
        group_by(year) %>%
        summarise(vwretx = sum(vwretx), vwretd = sum(vwretd), div = sum(div))
    )
  }
  
  if(period == 'Quarterly'){
    return(
      data %>%
        group_by(quarter) %>%
        summarise(vwretx = sum(vwretx), vwretd = sum(vwretd), div = sum(div))
    )
  }
  
  else{
    return(data)
  }
  
}

generate_plot = function(data, time){
  lineChart = plotly(data, x = ~time, y = ~div, type = 'scatter', mode = 'lines')
  
  
}


crspq = crsp %>%
  group_by(quarterYear) %>%
  summarise(vwretx = sum(vwretx), vwretd = sum(vwretd), div = sum(div))

crspa = crsp %>%
  group_by(yearly) %>%
  summarise(vwretx = sum(vwretx), vwretd = sum(vwretd), div = sum(div))

colnames(crspq)[1] <- "date"
colnames(crspa)[1] <- "date"

#plot_ly(crspq, x = ~quarterYear, y = ~div, type = "scatter", mode = 'lines')


#plot_ly(crspa, x = ~yearly, y = ~div, type = "scatter", mode = 'lines')

ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Macro Finance Dividend Analysis"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(),
  
  # Main panel for displaying outputs ----
  mainPanel()
)


