#yahoo finance quick look to main tickers

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

search_me <- "SPY"
Ticker <- getOptionChain(search_me, NULL)
numofexpiries<-length(Ticker)

Ticker_current<-getQuote(search_me)
Ticker_current<-Ticker_current$Last

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
allOptions$PutValue<-allOptions$PutOI*allOptions$PutAsk
allOptions$CallValue<-allOptions$CallOI*allOptions$CallAsk
allOptions$NetOI<-allOptions$CallOI-allOptions$PutOI
allOptions$NetValue<-allOptions$CallValue-allOptions$PutValue
allOptions$Call_OIIV<-allOptions$CallOI * allOptions$CallIV
allOptions$Put_OIIV<-allOptions$PutOI * allOptions$PutIV

current_date <- Sys.Date()
allOptions$DTE<-as.numeric(allOptions$Expiration - current_date)


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


#barcharts OIIV############################################################

my_barcharts_data <-filter(allOptions, DTE == 3)
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

my_barcharts_data <-filter(allOptions, DTE == 4)
my_barcharts_data<-select(my_barcharts_data,c(2,3,8))
my_barcharts_data$PutOI<-my_barcharts_data$PutOI * -1
my_barcharts_data<-  pivot_longer(data=my_barcharts_data, cols=c('CallOI', 'PutOI'),
                                  names_to='Type',
                                  values_to='OI')
my_barcharts_data<-as.data.frame(my_barcharts_data)
my_barcharts_data$Type<-as.factor(my_barcharts_data$Type)

p_OI<- ggplot(my_barcharts_data, aes(x=Strike, y=OI,fill=Type)) + 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red") +
  scale_x_continuous(breaks=seq(160,550,5)) +
  coord_flip()



#syn val ############################################################
my_barcharts_data <-filter(allOptions, DTE == 4)
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

my_barcharts_data <-filter(allOptions, DTE == 0)
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


