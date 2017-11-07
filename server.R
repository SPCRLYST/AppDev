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

#NA removal function
completeFunc <- function(data, desiredCols) 
{
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
} 

# Define a server for the Shiny app
function(input, output) {
  #coupon data
  CoupURL <- getURL("https://raw.githubusercontent.com/SPCRLYST/CouponProj/master/FinalFIUtilities.csv")
  coupdata <- read.csv(text = CoupURL, stringsAsFactors = FALSE, fileEncoding = "latin1")
  #US treasury data
  USTreasURL <- getURL("https://raw.githubusercontent.com/SPCRLYST/CouponProj/master/USTreasuries.csv")
  USTreasdata <- read.csv(text = USTreasURL)
  
  #renaming treasury columns and fixing dates
  USTreas <- USTreasdata
  USTreas$Date <- as.Date(as.character(USTreas$Date), format="%b-%d-%Y")
  USTreas <- rename(USTreas, c("UST...20.Year"="UST20","UST...10.Year"="UST10","UST...5.Year"="UST5",
                               "UST...1.Year"="UST1"))
  
  #formating the date to plot time series
  coupdata$MaturityDate <- as.Date(as.character(coupdata$MaturityDate), format="%m/%d/%Y")
  coupdata$OfferingDate <- as.Date(as.character(coupdata$OfferingDate), format="%m/%d/%Y")
  #formating offering amount as number
  coupdata$OfferingAmount <- as.numeric(as.character(coupdata$OfferingAmount))
  
  #creating date variables variable
  coupdata$odyear <- strftime(coupdata$OfferingDate, "%Y")
  coupdata$odmyear <- as.Date(cut(coupdata$OfferingDate, breaks = "month"))
  coupdata$mdyear <- strftime(coupdata$MaturityDate, "%Y")
  coupdata$mdmyear <- as.Date(cut(coupdata$MaturityDate, breaks = "month"))
  #finding the term of an issuance
  coupdata$term <- as.numeric(coupdata$mdyear) - as.numeric(coupdata$odyear)
  
  #higher level rating
  coupdata$oRating <- coupdata$Rating
  coupdata$oRating[coupdata$oRating == "AAA-"] <- "AAA"
  coupdata$oRating[coupdata$oRating == "AA+"] <- "AA"
  coupdata$oRating[coupdata$oRating == "AA-"] <- "AA"
  coupdata$oRating[coupdata$oRating == "A+"] <- "A"
  coupdata$oRating[coupdata$oRating == "A-"] <- "A"
  coupdata$oRating[coupdata$oRating == "BBB+"] <- "BBB"
  coupdata$oRating[coupdata$oRating == "BBB+/A-2"] <- "BBB"
  coupdata$oRating[coupdata$oRating == "BBB-"] <- "BBB"
  coupdata$oRating[coupdata$oRating == "BB+"] <- "BB"
  coupdata$oRating[coupdata$oRating == "BB-"] <- "BB"
  coupdata$oRating[coupdata$oRating == "B+"] <- "B"
  coupdata$oRating[coupdata$oRating == "B-"] <- "B"
  
  #keeping only issuances greater than 1 million
  coupdata <- subset(coupdata, OfferingAmount >= 1)
  
  #remove NA's
  coupdata <- completeFunc(coupdata,"OfferingAmount")
  
  #structuring the amount data
  coupdata$fAmount <- ""
  coupdata$fAmount[coupdata$OfferingAmount > 500] <- c("Very Large")
  coupdata$fAmount[coupdata$OfferingAmount <= 500 & coupdata$OfferingAmount > 250] <- c("Large")
  coupdata$fAmount[coupdata$OfferingAmount <= 250 & coupdata$OfferingAmount > 100] <- c("Medium")
  coupdata$fAmount[coupdata$OfferingAmount <= 100 &  coupdata$OfferingAmount > 10] <- c("Small")
  coupdata$fAmount[coupdata$OfferingAmount <= 10 &  coupdata$OfferingAmount > 0] <- c("Very Small")
  coupdata$fAmount <- as.factor(coupdata$fAmount)
  
  #structuring the term
  coupdata$fterm <- ""
  coupdata$fterm[coupdata$term > 19] <- c("Long-term")
  coupdata$fterm[coupdata$term <= 19 & coupdata$term > 4] <- c("Medium-term")
  coupdata$fterm[coupdata$term <= 4] <- c("Short-term")
  coupdata$fterm <- as.factor(coupdata$fterm)
  
  #structuring the subordination data
  coupdata$level <- coupdata$SeniorityLevel
  coupdata$level <- as.character(coupdata$level)
  coupdata$level[coupdata$level == "Junior Subordinate"] <- "Other"
  coupdata$level[coupdata$level == "Not Ranked"] <- "Other"
  coupdata$level[coupdata$level == "Senior Subordinate"] <- "Other"
  coupdata$level[coupdata$level == "Subordinate"] <- "Other"
  coupdata$level <- as.factor(coupdata$level)
  regutil <- coupdata
  
  #creating offering amount per term
  totoffamnt <- aggregate(OfferingAmount ~ odyear, regutil, FUN = sum)
  aveterm <- aggregate(term ~ odyear, regutil, FUN = mean)
  avecoup <- aggregate(CouponatOffer ~ odyear, regutil, FUN = mean)
  
  #breaking out year to build the term variable
  regutil$State <- regutil$State.RegionFromPrimaryAddress
  regutil$State.RegionFromPrimaryAddress <- NULL
  
  #adding yearly debt totals to regutil
  regutil <- (merge(totoffamnt, regutil, by = 'odyear'))
  regutil <- rename(regutil, c("OfferingAmount.x"="YearOffAmnt"))
  regutil <- rename(regutil, c("OfferingAmount.y"="OfferingAmount"))
  regutil$woamnt <- regutil$OfferingAmount/regutil$YearOffAmnt
  
  #weighted term
  regutil$woaterm <- regutil$woamnt*regutil$term
  woterm <- aggregate(woaterm ~ odyear, regutil, FUN = sum)
  woterm$woaterm <- round(woterm$woaterm, digits = 3)
  woterm <- merge(aveterm, woterm, by = 'odyear')
  woterm <- merge(totoffamnt, woterm, by = 'odyear')
  
  #weighted coupon
  regutil$wocoup <- regutil$woamnt*regutil$CouponatOffer
  wocoupn <- aggregate(wocoup ~ odyear, regutil, FUN = sum)
  wocoupn$wocoup <- round(wocoupn$wocoup, digits = 2)
  wocoupn <- merge(avecoup, wocoupn, by = 'odyear')
  wocoupn <- merge(totoffamnt, wocoupn, by ='odyear')
  
  #adding weighted average to regutil data frame
  regutil <- merge(regutil, wocoupn, by = 'odyear')
  colnames(regutil)[colnames(regutil) == "OfferingAmount.y"] <- "TotalYDebt"
  colnames(regutil)[colnames(regutil) == "OfferingAmount.x"] <- "OfferingAmount"
  colnames(regutil)[colnames(regutil) == "wocoup.x"] <- "wocoup"
  colnames(regutil)[colnames(regutil) == "wocoup.y"] <- "YearWCoup"
  colnames(regutil)[colnames(regutil) == "CouponatOffer.x"] <- "CouponatOffer"
  regutil$CouponatOffer.y <- NULL
  regutil$YearOffAmnt <- NULL
  
  #coloring for amount rating level
  rate_color <- c("AAA"="#9E0142",
                  "AA"="#D53E4F",
                  "A"="#F46D43",
                  "BBB"="#FDAE61",
                  "BB"="#FEE08B",
                  "B"="#FFFFBF",
                  "CCC"="#E6F598",
                  "CC"="#ABDDA4",
                  "C"="#66C2A5",
                  "D"="#3288BD",
                  "NR"="#5E4FA2",
                  "Unknown"="gray49")
  rating_list <- c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","NR","Unknown")
  # Fill in the spot we created for a plot
  output$alldebtPlot <- renderPlot({
    # Render a barplot
    #annual version
    regutil <- with(regutil, regutil[order(odyear, factor(oRating, levels = c("AAA","AA","A","BBB","BB","B",
                                                                              "CCC","CC","C","D","NR","Unknown"))),])
    amaxval <- data.frame(aggregate((OfferingAmount/1000) ~ odyear, regutil, FUN = sum))
    amaxval <- max(amaxval$X.OfferingAmount.1000.)
    anndebt <- ggplot(regutil, aes(x = odyear, 
                                   y = OfferingAmount/1000, 
                                   fill = oRating))+
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal()+
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45),
            text = element_text(size = 11),
            legend.box = "horizontal")+
      labs(title = "Annual Debt Issuance by Rating", 
           x = "", 
           y = "Debt Issuances (USD billions)\n")+
      scale_y_continuous(limits = c(0,(1+round(amaxval,digits = 0))))+
      scale_fill_manual(values = rate_color,
                        name = "Rating of Issuances",
                        breaks = c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","NR","Unknown"),
                        labels = c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","NR","Unknown"))+
      guides(fill = guide_legend(nrow = 1))
    print(anndebt)
  })
}

#dynamic date axis
#start_date <- max(regutil$odmyear)
#end_date <- as.Date(start_date) %m-% months(38)

#monthly version
#month_regutil <- subset(regutil, odmyear>=end_date & odmyear<=start_date)
#month_regutil <- with(month_regutil, month_regutil[order(odmyear, factor(oRating, levels = c("AAA","AA","A","BBB",
#                                                                                             "BB","B","CCC","CC","C",
#                                                                                             "D","NR","Unknown"))),])
#mmaxval <- data.frame(aggregate((OfferingAmount/1000) ~ odmyear, month_regutil, FUN = sum))
#mmaxval <- max(mmaxval$X.OfferingAmount.1000.)
#mondebt <- ggplot(month_regutil, aes(x = odmyear, 
#                                     y = OfferingAmount/1000, 
#                                     fill = oRating))+
#  geom_bar(stat = "identity", position = "stack") +
#  theme_minimal()+
#  theme(legend.position = "bottom",
#        axis.text.x = element_text(angle = 90),
#        text = element_text(size = 11),
#        legend.box = "horizontal")+
#  labs(title = "Monthly Debt Issuance by Rating", 
#       x = "", 
#       y = "Debt Issuances (USD billions)\n")+
#  scale_y_continuous(limits = c(0,(1+round(mmaxval,digits = 0))))+
#  scale_x_date(labels = date_format("%m-%Y"),date_breaks = "1 month")+
#  scale_fill_manual(values = rate_color,
#                    name = "Rating of Issuances",
#                    breaks = c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","NR","Unknown"),
#                    labels = c("AAA","AA","A","BBB","BB","B","CCC","CC","C","D","NR","Unknown"))+
#  guides(fill = guide_legend(nrow = 1))
#mon_adj <- mondebt + coord_cartesian(xlim=c(as.Date(end_date) %m+% months(2),as.Date(start_date) %m-% months(1))) 
#mon_adj