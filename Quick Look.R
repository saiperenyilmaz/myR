#option chain
install.packages("quantmod")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("hrbrthemes")
install.packages("jsonlite")
install.packages("tidyjson")
install.packages("viridis")
install.packages("viridisLite")
install.packages("rlist")
install.packages("plotly")
install.packages("ggridges")
install.packages("rgl")

library(rgl)
library(data.table)
library(ggridges)
library(quantmod)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(jsonlite)
library(tidyjson)
library(rlist)
library(viridis)
library(viridislite)
library(plotly)


setwd("/Users/saiperenyilmaz/Desktop/MyCode/R/Eren Gamma")

search_set <- c("SPY", "TLT", "GLD", "IWM", "QQQ")
main_list <- data.frame()

for (search_me in search_set) {
  
  Ticker <- getOptionChain(search_me, NULL)
  
  numofexpiries<-length(Ticker)
  
  #collect calls          
  allCalls<-as.data.frame(do.call(cbind,Ticker[[1]][1]$calls[1:15]))
  allCalls$Type<-"c"
  for (x in 2:numofexpiries) 
  {
    if (is.null(Ticker[[x]][1]$calls[1:15])) {
      next  # Skip this iteration
    }
    dnm<-as.data.frame(do.call(cbind,Ticker[[x]][1]$calls[1:15]))
    dnm$Type<-"c"
    allCalls<-rbind(allCalls,dnm)
  }
  
  #collect puts  
  allPuts<-as.data.frame(do.call(cbind,Ticker[[1]][2]$puts[1:15]))
  allPuts$Type<-"p"
  for (x in 2:numofexpiries) 
  {
    if (is.null(Ticker[[x]][2]$puts[1:15])) {
      next  # Skip this iteration
    }
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
  allOptions$Ticker <- search_me
  allOptions$Spot_Price <- Ticker$Last
  tmpp<- getQuote(search_me)
  allOptions$Price <- tmpp$Last
  allOptions$Saved <- Sys.time()
  
  main_list <- rbind(main_list,allOptions)
  
  rm(allOptions, allCalls, allPuts,df_calls,df_puts,dnm,Ticker, numofexpiries, x,tmpp)
  
}



#ETL

main_list$Expiration<-as.numeric(main_list$Expiration)
main_list$Expiration<-as.Date(as.POSIXct(main_list$Expiration,origin = "1970-01-01"))
main_list$Strike<-as.numeric(main_list$Strike)

main_list$PutBid<-as.numeric(main_list$PutBid)
main_list$PutAsk<-as.numeric(main_list$PutAsk)
main_list$PutVolume<-as.numeric(main_list$PutVolume)
main_list$PutIV<-as.numeric(main_list$PutIV)
main_list$PutOI<-as.numeric(main_list$PutOI)

main_list$CallBid<-as.numeric(main_list$CallBid)
main_list$CallAsk<-as.numeric(main_list$CallAsk)
main_list$CallVolume<-as.numeric(main_list$CallVolume)
main_list$CallIV<-as.numeric(main_list$CallIV)
main_list$CallOI<-as.numeric(main_list$CallOI)

#calculated fields
main_list$PutValue<-main_list$PutOI*main_list$PutAsk
main_list$CallValue<-main_list$CallOI*main_list$CallAsk
main_list$NetOI<-main_list$CallOI-main_list$PutOI
main_list$NetValue<-main_list$CallValue-main_list$PutValue
main_list$Call_OIIV<-main_list$CallOI * main_list$CallIV
main_list$Put_OIIV<-main_list$PutOI * main_list$PutIV
main_list$Net_OIIV<-main_list$Call_OIIV - main_list$Put_OIIV

current_date <- Sys.Date()
main_list$DTE<-as.numeric(main_list$Expiration - current_date)


# OI IV Graph

for (Current_Search in search_set) 
{
  
  my_barcharts_data <-filter(main_list, Ticker == Current_Search )
  my_barcharts_data <-filter(my_barcharts_data, DTE <=10)
  
  max_call_OI <- my_barcharts_data[which.max(my_barcharts_data$OI),1]
  min_call_OI <- my_barcharts_data[which.min(my_barcharts_data$OI),1]
  
  price <-mean(my_barcharts_data$Price)
  UL<-round(price*1.10)
  LL<-round(price*0.90)
  my_barcharts_data <-filter(my_barcharts_data, Strike %in% c(LL:UL))
  
  my_barcharts_data<-select(my_barcharts_data,c(2,20,21,23))
  my_barcharts_data$Put_OIIV<-my_barcharts_data$Put_OIIV * -1
  my_barcharts_data<-  pivot_longer(data=my_barcharts_data, cols=c('Call_OIIV', 'Put_OIIV'),
                                    names_to='Type',
                                    values_to='OI')
  my_barcharts_data<-as.data.frame(my_barcharts_data)
  my_barcharts_data$Type<-as.factor(my_barcharts_data$Type)
  

  
  p_oiiv<- ggplot(my_barcharts_data) + 
    geom_bar(aes(x=Strike, y=OI,fill=Type), stat = "identity")+
    scale_x_continuous(breaks=seq(min(my_barcharts_data$Strike),max(my_barcharts_data$Strike),50)) +
    geom_vline(xintercept=price, linetype="dashed", color = "blue") +
    geom_vline(xintercept=max_call_OI,linetype="dashed", color = "green") +
    geom_vline(xintercept=min_call_OI,linetype="dashed", color = "red") +
    scale_x_continuous(breaks=seq(LL,UL,5)) +
    coord_flip() 
  p_oiiv+ facet_grid(. ~ DTE)
  ggsave(paste0(Current_Search,".png"))
  
}




