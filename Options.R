#option chain
install.packages("quantmod")
library(quantmod)

install.packages("dplyr")
library(dplyr)

install.packages("lubridate")
library(lubridate)

install.packages("ggplot2")
library(ggplot2)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("jsonlite")
library(jsonlite)
install.packages("tidyjson")
library(tidyjson)

install.packages("rlist")
library(rlist)

Ticker <- getOptionChain("QQQ", NULL)
numofexpiries<-length(Ticker)

Ticker_current<-getQuote("QQQ")
Ticker_current<-Ticker_current$Last

#collect calls          
allCalls<-as.data.frame(do.call(cbind,Ticker[[1]][1]$calls[1:15]))
allCalls$Type<-"c"
for (x in 2:numofexpiries) 
  {
  dnm<-as.data.frame(do.call(cbind,Ticker[[x]][1]$calls[1:15]))
  dnm$Type<-"c"
  allCalls<-rbind(allCalls,dnm)
 }

#collect puts  
allPuts<-as.data.frame(do.call(cbind,Ticker[[1]][2]$puts[1:15]))
allPuts$Type<-"c"
for (x in 2:numofexpiries) 
{
  dnm<-as.data.frame(do.call(cbind,Ticker[[x]][2]$puts[1:15]))
  dnm$Type<-"p"
  allPuts<-rbind(allPuts,dnm)
}


df_puts <- data.frame(Expiration = allPuts$Expiration, Strike = allPuts$Strike, PutOI = allPuts$OI,
                      PutVolume=allPuts$Vol,PutBid=allPuts$Bid, PutAsk=allPuts$Ask, PutIV=allPuts$IV)
df_calls <- data.frame(Expiration = allCalls$Expiration, Strike = allCalls$Strike, CallOI = allCalls$OI,
                       CallVolume=allCalls$Vol,CallBid=allCalls$Bid, CallAsk=allCalls$Ask, CallIV=allCalls$IV)

allOptions<-full_join(df_calls,df_puts,by=c("Strike","Expiration"))
allOptions[is.na(allOptions)] <- 0

allOptions$Expiration<-as.numeric(allOptions$Expiration)
allOptions$Expiration<-as.Date(as.POSIXct(allOptions$Expiration,origin = "1970-01-01"))
allOptions$Strike<-as.numeric(allOptions$Strike)

allOptions$PutBid<-as.numeric(allOptions$PutBid)
allOptions$PutAsk<-as.numeric(allOptions$PutAsk)
allOptions$PutVolume<-as.numeric(allOptions$PutVolume)
allOptions$PutIV<-as.numeric(allOptions$PutIV)
allOptions$PutOI<-as.numeric(allOptions$PutOI)

allOptions$CallBid<-as.numeric(allOptions$CallBid)
allOptions$CallAsk<-as.numeric(allOptions$CallAsk)
allOptions$CallVolume<-as.numeric(allOptions$CallVolume)
allOptions$CallIV<-as.numeric(allOptions$CallIV)
allOptions$CallOI<-as.numeric(allOptions$CallOI)

#calculated fields
allOptions$PutValue<-allOptions$PutOI*allOptions$PutBid
allOptions$CallValue<-allOptions$CallOI*allOptions$CallBid
allOptions$NetOI<-allOptions$CallOI-allOptions$PutOI
allOptions$NetValue<-allOptions$CallValue-allOptions$PutValue

current_date <- Sys.Date()
allOptions$DTE<-as.numeric(allOptions$Expiration - current_date)


#filter the term
termOptions <- subset(allOptions, DTE < 7)


UL<-max(termOptions$Strike)
LL<-min(termOptions$Strike)

ngamma <- data.frame("Price"=numeric(),
                     "total_ITM_NetPuts" = numeric(),
                     "total_ITM_NetCalls"= numeric(),
                     "dealer_pays"= numeric(),
                     "total_OTM_NetPuts"= numeric(),
                     "total_OTM_NetCalls"= numeric(),
                     "dealer_collects"= numeric(),
                     "dealer_net"= numeric())
x<-1
for (i in LL:UL) {
  ngamma[nrow(ngamma)+1,] <- NA
  ngamma$Price[x]<-i
  ngamma$total_ITM_NetPuts[x]<-sum(termOptions[which(termOptions$Strike > i), 13])
  ngamma$total_ITM_NetCalls[x]<-sum(termOptions[which(termOptions$Strike < i), 14])
  ngamma$dealer_pays[x]<-total_ITM_NetPuts+total_ITM_NetCalls
  
  ngamma$total_OTM_NetPuts[x]<-sum(termOptions[which(termOptions$Strike < i), 13])
  ngamma$total_OTM_NetCalls[x]<-sum(termOptions[which(termOptions$Strike > i), 14])
  ngamma$dealer_collects[x]<-total_OTM_NetPuts+total_OTM_NetCalls
  ngamma$dealer_net[x]<-dealer_collects-dealer_pays
  x<-x+1
}

min(ngamma$dealer_pays)
max(ngamma$dealer_pays)

p<-plot(ngamma$Price,ngamma$dealer_pays)


total_ITM_NetPuts<-sum(termOptions[which(termOptions$Strike > Ticker_current), 13])
total_ITM_NetCalls<-sum(termOptions[which(termOptions$Strike < Ticker_current), 14])
dealer_pays<-total_ITM_NetPuts+total_ITM_NetCalls

total_OTM_NetPuts<-sum(termOptions[which(termOptions$Strike < Ticker_current), 13])
total_OTM_NetCalls<-sum(termOptions[which(termOptions$Strike > Ticker_current), 14])
dealer_collects<-total_OTM_NetPuts+total_OTM_NetCalls
dealer_net <-dealer_collects-dealer_pays

#NetValue<-termOptions[,16]

p<-ggplot(termOptions, aes(x=Strike, y=NetValue)) + 
  geom_bar(stat = "identity") +
  geom_vline(xintercept=Ticker_current, linetype="dashed", color = "red") +
  coord_flip()






