#####Reading in data
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(RCurl)
library(XML)
library(xts)
library(plyr)
library(reshape2)
library(ReporteRs)
library(lubridate)
library(scales)
library(TTR)
library(shiny)

# Use a fluid Bootstrap layout
fluidPage(    
  verticalLayout(
  # Give the page a title
  titlePanel("Utility Debt Issuance Timeline"),
    # Create a spot for the barplot
    mainPanel(
      plotOutput("alldebtPlot"))
    )
  )



