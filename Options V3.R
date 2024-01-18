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


my_barcharts_data <-filter(main_list, DTE == 0)
my_barcharts_data <-filter(main_list, Ticker == "SPY")
price <-mean(my_barcharts_data$Price)
my_barcharts_data<-select(my_barcharts_data,c(2,20,21))
my_barcharts_data$Put_OIIV<-my_barcharts_data$Put_OIIV * -1
my_barcharts_data<-  pivot_longer(data=my_barcharts_data, cols=c('Call_OIIV', 'Put_OIIV'),
                                  names_to='Type',
                                  values_to='OI')
my_barcharts_data<-as.data.frame(my_barcharts_data)
my_barcharts_data$Type<-as.factor(my_barcharts_data$Type)

p_oiiv<- ggplot(my_barcharts_data) + 
  geom_bar(aes(x=Strike, y=OI,fill=Type), stat = "identity")+
  scale_x_continuous(breaks=seq(min(my_barcharts_data$Strike),max(my_barcharts_data$Strike),50)) +
  geom_vline(xintercept =price, linetype="dashed", color = "red") +
  coord_flip() 












#term_CALL

call_term_data <-allOptions
call_term_data<-select(call_term_data,c(2,3,7,19))
call_term_data <-filter(call_term_data, DTE < 91)

z<-call_term_data$Strike
x<-call_term_data$DTE
y<-call_term_data$CallIV

rgl.open()
rgl.bg(color="white")
rgl.points(x,y,z, color="blue", size="5")
plot3d(call_term_data[,c(1,3,4)])

#term_PUT

put_term_data <-allOptions
put_term_data<-select(put_term_data,c(2,8,12,19))
put_term_data <-filter(put_term_data, DTE < 151)

z<-put_term_data$Strike
x<-put_term_data$DTE
y<-put_term_data$CallIV

rgl.open()
rgl.bg(color="white")
rgl.points(x,y,z, color="red", size="5")


plot3d(put_term_data[,c(1,3,4)])



#graphics

DTEarray<-unique(allOptions$Expiration)
length(DTEarray)
max_strike<-max(allOptions$Strike)
min_strike<-min(allOptions$Strike)

Flip_strike<-0
i<-1
for (x in DTEarray) 
{
  
  dte_select <-filter(allOptions, DTE == x)
  dte_select<-arrange(dte_select, NetValue)
  dte_select$bullish <- abs(cumsum(dte_select$NetValue))
  
  dte_select<-arrange(dte_select, desc(NetValue))
  dte_select$bearish <- abs(cumsum(dte_select$NetValue))
  dte_select$netcumOI<-abs(dte_select$bullish-dte_select$bearish)
  
  Flip_strike[i]<-dte_select[which.min(dte_select$netcumOI),2]
  
  
  allOptions$Flip_Strike[allOptions$Expiration == x] <- Flip_strike[i]
  
  
  i<-i+1

}


final<-as.data.frame(cbind(DTEarray,Flip_strike))
final$DTEarray<-as.Date(final$DTEarray)
final$SaveDate<-as.Date(now())

final %>%
  ggplot( aes(x=DTEarray, y=Flip_strike)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
  theme_ipsum() +
  ggtitle(paste(search_me," NetOI Flip"))


#day by day analysis############################################################

DTEarray<-unique(allOptions$DTE)
length(DTEarray)
max_strike<-max(allOptions$Strike)
min_strike<-min(allOptions$Strike)

for (x in DTEarray) 
{
  ZeroDayDTE <-filter(allOptions, DTE == x)
  
  ZeroDayDTE<-arrange(ZeroDayDTE, NetValue)
  ZeroDayDTE$bullish <- abs(cumsum(ZeroDayDTE$NetValue))
  
  ZeroDayDTE<-arrange(ZeroDayDTE, desc(NetValue))
  ZeroDayDTE$bearish <- abs(cumsum(ZeroDayDTE$NetValue))
  ZeroDayDTE$netcumOI<-abs(ZeroDayDTE$bullish-ZeroDayDTE$bearish)
  
  Flip_strike<-ZeroDayDTE[which.min(ZeroDayDTE$netcumOI),2]
  
  p0 <- ggplot(ZeroDayDTE, aes(x=Strike, y=NetOI)) + 
    geom_bar(stat = "identity") +
    geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red") +
    geom_vline(xintercept = Flip_strike, linetype="dashed", color = "blue") +
    xlim(min_strike,max_strike) +
    ggtitle(paste(search_me,"Flip_Strike: ", Flip_strike))
    
  
  ggsave(paste0(search_me," ",x,".png"))
  
}


#barcharts OIIV############################################################

my_barcharts_data <-filter(allOptions, DTE == 2)
my_barcharts_data<-select(my_barcharts_data,c(2,17,18))
my_barcharts_data$Put_OIIV<-my_barcharts_data$Put_OIIV * -1
my_barcharts_data<-  pivot_longer(data=my_barcharts_data, cols=c('Call_OIIV', 'Put_OIIV'),
               names_to='Type',
               values_to='OI')
my_barcharts_data<-as.data.frame(my_barcharts_data)
my_barcharts_data$Type<-as.factor(my_barcharts_data$Type)
  
p_oiiv<- ggplot(my_barcharts_data, aes(x=Strike, y=OI,fill=Type)) + 
    geom_bar(stat = "identity")+
    geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red") +
   coord_flip()

#barcharts Value and Flip############################################################

my_barcharts_data <-filter(allOptions, DTE == 1)
my_barcharts_data<-select(my_barcharts_data,c(2,13,14))
my_barcharts_data$PutValue<-my_barcharts_data$PutValue * -1
my_barcharts_data<-  pivot_longer(data=my_barcharts_data, cols=c('CallValue', 'PutValue'),
                                  names_to='Type',
                                  values_to='OI')
my_barcharts_data<-as.data.frame(my_barcharts_data)
my_barcharts_data$Type<-as.factor(my_barcharts_data$Type)

p_val<- ggplot(my_barcharts_data, aes(x=Strike, y=OI,fill=Type)) + 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red")


#barcharts Value and Flip V2 ############################################################

my_barcharts_data <-filter(allOptions, DTE == 4)
my_barcharts_data<-allOptions
search_range<-c(round(Ticker_current*0.50):round(Ticker_current*1.50))
flip_data = array() 

i<-1
for (hyp_strike in search_range) 
{

  my_barcharts_data$flip<-my_barcharts_data$NetOI*(my_barcharts_data$Strike-hyp_strike)
  flip_data[i]<-sum(my_barcharts_data$flip)
  i<-i+1
  
}

flip_data<-as.data.frame(flip_data)
flip_data$hype_strike <-c(round(Ticker_current*0.50):round(Ticker_current*1.50))

p<-ggplot(flip_data, aes(x=hype_strike, y=flip_data)) +
  geom_line()

f<-function(x) abs(sum(my_barcharts_data$NetOI*(my_barcharts_data$Strike - x)))
optimise(f, lower = 0, upper=750 ,maximum = FALSE)




#barcharts Value and Flip V3 ############################################################

rm(flip_point)

my_barcharts_data<-allOptions
DTEarray<-unique(allOptions$DTE)
flip_data = array() 
flip_point = array() 

search_range<-c(round(Ticker_current*0.50):round(Ticker_current*1.50))
f<-function(x) abs(sum(my_barcharts_data$NetOI*(my_barcharts_data$Strike - x)))

i<-1
for (search_Day in DTEarray) 
{
  
  my_barcharts_data <-filter(allOptions, DTE == search_Day)

  my_solution<-optimise(f, lower = 0, upper=750 ,maximum = FALSE)
  flip_point[i]<-round(my_solution$minimum, digits = 2)
  i<-i+1
}


flip_point<-as.data.frame(flip_point)
flip_point$DTE <-DTEarray
flip_point$Expiration <-unique(allOptions$Expiration)

p<-ggplot(flip_point, aes(x=DTE, y=flip_point)) +
  geom_line()



p<-ggplot(flip_data, aes(x=hype_strike, y=flip_data)) +
  geom_line()




#OI only############################################################

my_barcharts_data <-filter(main_list, DTE == 0)
my_barcharts_data <-filter(main_list, Ticker == "SPY")

max_call_OI <- round(my_barcharts_data[which.max(my_barcharts_data$CallOI),2])
min_put_OI <- round(my_barcharts_data[which.max(my_barcharts_data$PutOI),2])

next_10DTE <-filter(main_list, Strike %in% min_call_OI:max_call_OI)

my_barcharts_data<-select(my_barcharts_data,c(2,3,8))
my_barcharts_data$PutOI<-my_barcharts_data$PutOI * -1
my_barcharts_data<-  pivot_longer(data=my_barcharts_data, cols=c('CallOI', 'PutOI'),
                                  names_to='Type',
                                  values_to='OI')
my_barcharts_data<-as.data.frame(my_barcharts_data)
my_barcharts_data$Type<-as.factor(my_barcharts_data$Type)

p_OI<- ggplot(my_barcharts_data, aes(x=Strike, y=OI,fill=Type)) + 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = price, linetype="dashed", color = "red") +
  scale_x_continuous(breaks=seq(160,550,5)) +
  coord_flip()
  


#syn val ############################################################
my_barcharts_data <-filter(allOptions, DTE == 0)
my_barcharts_data<-as.data.frame(my_barcharts_data)
my_barcharts_data <-filter(my_barcharts_data, my_barcharts_data$Strike > Ticker_current * 0.95)
my_barcharts_data <-filter(my_barcharts_data, my_barcharts_data$Strike < Ticker_current * 1.05)

my_barcharts_data<-select(my_barcharts_data,c(2,3,8))

p_marginalCallOI<- ggplot(my_barcharts_data, aes(x=Strike, y=CallOI)) + 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red")

p_marginalPutOI<- ggplot(my_barcharts_data, aes(x=Strike, y=PutOI),xlim=c(450,490)) + 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red")

#OI 5% margin############################################################

my_barcharts_data <-filter(allOptions, DTE == 0)
my_barcharts_data<-select(my_barcharts_data,c(2,3,8))


my_barcharts_data$PutOI<-my_barcharts_data$PutOI * -1
my_barcharts_data<-  pivot_longer(data=my_barcharts_data, cols=c('CallOI', 'PutOI'),
                                  names_to='Type',
                                  values_to='OI')
my_barcharts_data<-as.data.frame(my_barcharts_data)
my_barcharts_data$Type<-as.factor(my_barcharts_data$Type)

p_OI<- ggplot(my_barcharts_data, aes(x=Strike, y=OI,fill=Type)) + 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red")


#NetOI 5% margin############################################################

my_barcharts_data <-filter(allOptions, DTE == 8)
my_barcharts_data<-select(my_barcharts_data,c(2,3,8))
my_barcharts_data$NetOI <-my_barcharts_data$CallOI - my_barcharts_data$PutOI
my_barcharts_data<-as.data.frame(my_barcharts_data)


p_NetOI<- ggplot(my_barcharts_data, aes(x=Strike, y=NetOI)) + 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red") +
  scale_x_continuous(breaks=seq(160,320,5))


#ridgeline CallOI############################################################
data_for_ridge<-select(allOptions,c(2,3,17))
data_for_ridge <-filter(data_for_ridge, DTE < 365)
data_for_ridge$DTE<-as.factor(data_for_ridge$DTE)
ridge_graph <-ggplot(data_for_ridge, aes(x = Strike, y = DTE, group=DTE)) +
  geom_density_ridges() +
  xlab("Strike") +
  ylab("DTE")

#ridgeline PutOI############################################################
data_for_ridge<-select(allOptions,c(2,8,17))
data_for_ridge <-filter(data_for_ridge, DTE < 365)
data_for_ridge$DTE<-as.factor(data_for_ridge$DTE)
ridge_graph <-ggplot(data_for_ridge, aes(x = Strike, y = DTE, group=DTE)) +
  geom_density_ridges() +
  xlab("Strike") +
  ylab("DTE")


