#GAMMA from CBOE

#https://www.cboe.com/delayed_quotes/spy/quote_table
#https://perfiliev.co.uk/market-commentary/how-to-calculate-gamma-exposure-and-zero-gamma-level/
#https://doc.tradingflow.com/product-docs/concepts/delta-exposure-dex


install.packages("quantmod")
install.packages("ggplot2")
install.packages("stringr")       
install.packages("readxl")  
library(stringr)    
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

#yahoofinance
search_me <- "SPY"
ticker_length<-nchar(search_me)
Ticker_current<-getQuote(search_me)
Last_spot_price <-Ticker_current$Last
current_date <- Sys.Date()

#Collect CBOE EoD data

SPY_data<- read.csv(file.choose(),skip = 3)
SPY_data$Ticker <- "SPY"
SPY_data$Timestamp <- timestamp()

TLT_data<- read.csv(file.choose(),skip = 3)
TLT_data$Ticker <- "TLT"
TLT_data$Timestam <- timestamp()

QQQ_data<- read.csv(file.choose(),skip = 3)
QQQ_data$Ticker <- "QQQ"
QQQ_data$Timestam <- timestamp()

IWM_data<- read.csv(file.choose(),skip = 3)
IWM_data$Ticker <- "IWM"
IWM_data$Timestam <- timestamp()

GLD_data<- read.csv(file.choose(),skip = 3)
GLD_data$Ticker <- "GLD"
GLD_data$Timestam <- timestamp()

VIX_data<- read.csv(file.choose(),skip = 3)
VIX_data$Ticker <- "VIX"
VIX_data$Timestam <- timestamp()

#Append CBOE EoD data

all_data<-rbind(SPY_data,TLT_data,QQQ_data,IWM_data,GLD_data,VIX_data)





#Revise col names
data<- read.csv(file.choose(),skip = 3)
colnames(data) <- c('Expiration',
                    'Calls',
                    'Calls_Last_Sale',
                    'Calls_Net',
                    'Calls_Bid',
                    'Calls_Ask',
                    'Calls_Volume',
                    'Calls_IV',
                    'Calls_Delta',
                    'Calls_Gamma',
                    'Calls_OI',
                    'Strike',
                    'Puts',
                    'Puts_Last_Sales',
                    'Puts_Net',
                    'Puts_Bid',
                    'Puts_Ask',
                    'Puts_Volume',
                    'Puts_IV',
                    'Puts_Delta',
                    'Puts_Gamma',
                    'Puts_OI')

#Date ETL

data$Ind<-unlist(gregexpr("C", data$Calls))-6
year_2d<-paste0("20",substr(data$Calls,data$Ind,data$Ind+1))
month_2d<-substr(data$Calls,data$Ind+2,data$Ind+3)
day_2d<-substr(data$Calls,data$Ind+4,data$Ind+5)
data$Syn_Exp<-paste(year_2d,month_2d,day_2d, sep = "-")
data$Syn_Exp<-as.Date(data$Syn_Exp)
data$DTE<-as.numeric(data$Syn_Exp - current_date)

#moneyness

data <- data %>%
  mutate(Call_Moneyness = case_when(Strike == round(Last_spot_price) ~ "ATM", 
                                    Strike > Last_spot_price ~ "OTM", 
                                    Strike < Last_spot_price  ~ "ITM", 
                                    TRUE ~ "not_extreme"))
data <- data %>%
  mutate(Put_Moneyness = case_when(Strike == round(Last_spot_price) ~ "ATM", 
                                   Strike > Last_spot_price ~ "ITM", 
                                   Strike < Last_spot_price  ~ "OTM", 
                                   TRUE ~ "not_extreme"))

ATM_LL <- Last_spot_price*0.99
ATM_UL <- Last_spot_price*1.01
data$IsATM[data$Strike > ATM_LL & data$Strike < ATM_UL] <- "TRUE"

#Calcs 
data$NetOI <- (data$Calls_OI - data$Puts_OI)
data$NetOIIV <- (data$Calls_OI * data$Calls_IV - data$Puts_OI * data$Puts_IV)/1000
spot_square <-Last_spot_price ^2
data$Calls_DX <- data$Calls_Delta * data$Calls_OI * 10 * Last_spot_price
data$Puts_DX <- data$Puts_Delta * data$Puts_OI * 10 * Last_spot_price 
data$Net_DX <- (data$Calls_DX-data$Puts_DX)
data$Call_GX <- data$Calls_Gamma * data$Calls_OI * 100 * spot_square * 0.01
data$Put_GX <- data$Puts_Gamma * data$Puts_OI * 100 * spot_square * 0.01 
data$Net_GX <- (data$Call_GX- data$Put_GX) / 10000
data$DTE <- as.factor(data$DTE)
data$Calls_Spread <-data$Calls_Bid - data$Calls_Ask 
data$Puts_Spread <-data$Puts_Bid - data$Puts_Ask 



unique(data$DTE)
#OI
next_10DTE <-filter(data, DTE ==0)
max_call_OI <- next_10DTE[which.max(next_10DTE$NetOI),12]+50
min_call_OI <- next_10DTE[which.min(next_10DTE$NetOI),12]-50
next_10DTE <-filter(next_10DTE, Strike %in% min_call_OI:max_call_OI)

p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=NetOI),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(min_call_OI,max_call_OI,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")
p_OI_All+ facet_grid(. ~ DTE)

#box

boxplot(Strike ~ DTE, data = data, xlab = "DTE",
        ylab = "Strike", main = "All OI")

#OI IV

next_10DTE <-filter(data, DTE ==3)
max_call_OI <- next_10DTE[which.max(next_10DTE$NetOIIV),12]+50
min_call_OI <- next_10DTE[which.min(next_10DTE$NetOIIV),12]-50
next_10DTE <-filter(next_10DTE, Strike %in% min_call_OI:max_call_OI)

p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=NetOIIV),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(min_call_OI,max_call_OI,10)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")
p_OI_All+ facet_grid(. ~ DTE)


next_10DTE <-filter(data, DTE %in% 0:3)
max_call_OI <- next_10DTE[which.max(next_10DTE$NetOIIV),12]+50
min_call_OI <- next_10DTE[which.min(next_10DTE$NetOIIV),12]-50
next_10DTE <-filter(next_10DTE, Strike %in% min_call_OI:max_call_OI)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=NetOIIV),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(min_call_OI,max_call_OI,10)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")
p_OI_All+ facet_grid(. ~ DTE)


#0DTE

#max range
next_10DTE <-filter(data, DTE==0)
max_call_OI <- next_10DTE[which.max(next_10DTE$NetOIIV),12]+10
min_call_OI <- next_10DTE[which.min(next_10DTE$NetOIIV),12]-10
next_10DTE <-filter(next_10DTE, Strike %in% min_call_OI:max_call_OI)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=NetOIIV),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(min_call_OI,max_call_OI,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")


next_10DTE <-filter(data, DTE %in% 0:3)
max_call_OI <- next_10DTE[which.max(next_10DTE$NetOIIV),12]+10
min_call_OI <- next_10DTE[which.min(next_10DTE$NetOIIV),12]-10
next_10DTE <-filter(next_10DTE, Strike %in% min_call_OI:max_call_OI)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=NetOIIV),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(min_call_OI,max_call_OI,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")
p_OI_All+ facet_grid(. ~ DTE)

#close look OI
next_10DTE <-filter(data, DTE==0)
UL <- round(Last_spot_price * 1.05)
LL <- round(Last_spot_price * 0.95)
next_10DTE <-filter(next_10DTE, Strike %in% LL:UL)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=Calls_OI),stat="identity", fill="green",colour="green")  +
  geom_bar(aes(x=Strike, y=-Puts_OI),stat="identity", fill="red",colour="red")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(LL,UL,1)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")


next_10DTE <-filter(data, DTE==1)
UL <- round(Last_spot_price * 1.10)
LL <- round(Last_spot_price * 0.90)
next_10DTE <-filter(next_10DTE, Strike %in% LL:UL)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=Calls_OI),stat="identity", fill="green",colour="green")  +
  geom_bar(aes(x=Strike, y=-Puts_OI),stat="identity", fill="red",colour="red")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(LL,UL,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")

next_10DTE <-filter(data, DTE %in% 0:5)
UL <- round(Last_spot_price * 1.05)
LL <- round(Last_spot_price * 0.95)
next_10DTE <-filter(next_10DTE, Strike %in% LL:UL)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=NetOI),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(LL,UL,1)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")
p_OI_All+ facet_grid(. ~ DTE)


#close look
next_10DTE <-filter(data, DTE==1)
UL <- round(Last_spot_price * 1.05)
LL <- round(Last_spot_price * 0.95)
next_10DTE <-filter(next_10DTE, Strike %in% LL:UL)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=NetOI),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(LL,UL,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")

syn_cost = array() 
syn_strike_range <- c(LL:UL)
DTEarray<-unique(my_barcharts_data$DTE)

i<-1
for (syn_strike in 427:569) 
{
  
  syn_cost[i]<-sum(next_10DTE$NetOI * (477 - next_10DTE$Strike))
  i<-i+1

}

syn_cost<-as.data.frame(syn_cost)


#close look Gamma
next_10DTE <-filter(data, DTE==0)
UL <- round(Last_spot_price * 1.05)
LL <- round(Last_spot_price * 0.95)
next_10DTE <-filter(next_10DTE, Strike %in% LL:UL)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=Call_GX),stat="identity", fill="green",colour="green")  +
  geom_bar(aes(x=Strike, y=-Put_GX),stat="identity", fill="red",colour="red")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(LL,UL,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")

next_10DTE <-filter(data, DTE==2)
UL <- round(Last_spot_price * 1.05)
LL <- round(Last_spot_price * 0.95)
next_10DTE <-filter(next_10DTE, Strike %in% LL:UL)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=Call_GX),stat="identity", fill="green",colour="green")  +
  geom_bar(aes(x=Strike, y=-Put_GX),stat="identity", fill="red",colour="red")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(LL,UL,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")


next_10DTE <-filter(data, DTE %in% 0:7)
UL <- round(Last_spot_price * 1.05)
LL <- round(Last_spot_price * 0.95)
next_10DTE <-filter(next_10DTE, Strike %in% LL:UL)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=Net_GX),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(LL,UL,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")
p_OI_All+ facet_grid(. ~ DTE)


#value
next_10DTE <-filter(data, DTE %in% 0:7)
next_10DTE$Put_Value <- next_10DTE$Puts_Last_Sales * next_10DTE$Puts_OI * next_10DTE$Puts_Volume
next_10DTE$Call_Value <- next_10DTE$Calls_Last_Sale * next_10DTE$Calls_OI * next_10DTE$Calls_Volume
next_10DTE$Net_Value <- next_10DTE$Call_Value - next_10DTE$Put_Value 

UL <- round(Last_spot_price * 1.05)
LL <- round(Last_spot_price * 0.95)
next_10DTE <-filter(next_10DTE, Strike %in% LL:UL)
p_OI_All <- ggplot(next_10DTE) + 
  geom_bar(aes(x=Strike, y=Call_Value),stat="identity", fill="green",colour="green")  +
  geom_bar(aes(x=Strike, y=-Put_Value),stat="identity", fill="red",colour="red")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(LL,UL,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "blue")
p_OI_All+ facet_grid(. ~ DTE)


#flip

rm(flip_point)

my_barcharts_data<-data
DTEarray<-unique(my_barcharts_data$DTE)
flip_data = array() 
flip_point = array() 

search_range<-c(round(Last_spot_price*0.50):round(Last_spot_price*1.50))
#f<-function(x) abs(sum(my_barcharts_data$NetOI*(my_barcharts_data$Strike - x)))
f<-function(x) abs(sum(my_barcharts_data$Calls_OI * (x-my_barcharts_data$Strike))+sum(my_barcharts_data$Puts_OI * (my_barcharts_data$Strike-x)))

i<-1
for (search_Day in DTEarray) 
{
  
  my_barcharts_data <-filter(data, DTE ==search_Day)
  
  my_solution<-optimise(f, lower = 0, upper=750 ,maximum = FALSE)
  flip_point[i]<-round(my_solution$minimum, digits = 2)
  i<-i+1
}


flip_point<-as.data.frame(flip_point)
flip_point$DTE <-DTEarray
flip_point$Expiration <-unique(data$Expiration)

p<-ggplot(flip_point) +
  geom_line(aes(x=DTE, y=flip_point))

plot(flip_point$DTE, flip_point$flip_point)




