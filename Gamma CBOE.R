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

/Users/saiperenyilmaz/Downloads/vix_quotedata.csv

#yahoofinance
search_me <- "SPY"
ticker_length<-nchar(search_me)
Ticker_current<-getQuote(search_me)
Last_spot_price <-Ticker_current$Last


#CBOE
data <- read.csv(file.choose(),skip = 3)
current_date <- Sys.Date()

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

data$NetOI <- (data$Calls_OI - data$Puts_OI)*1000
spot_square <-Last_spot_price ^2

data$Calls_DX <- data$Calls_Delta * data$Calls_OI * 10 * Last_spot_price
data$Puts_DX <- data$Puts_Delta * data$Puts_OI * 10 * Last_spot_price 
data$Net_DX <- (data$Calls_DX-data$Puts_DX)

data$Call_GX <- data$Calls_Gamma * data$Calls_OI * 100 * spot_square * 0.01
data$Put_GX <- data$Puts_Gamma * data$Puts_OI * 100 * spot_square * 0.01 
data$Net_GX <- (data$Call_GX- data$Put_GX)

data$DTE <- as.factor(data$DTE)

data$Calls_Spread <-data$Calls_Bid - data$Calls_Ask 
data$Puts_Spread <-data$Puts_Bid - data$Puts_Ask 


p_OI_All <- ggplot(data) + 
  geom_bar(aes(x=Strike, y=NetOI),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(3000,5750,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "red")
p_OI_All+ facet_grid(. ~ DTE)

z<-boxplot(Strike~DTE,data=data, main=search_me,
        xlab="Days To Expire", ylab="Strike")



z1<-geom_line(data, aes(x=DTE, z$stats[3,]),stat="identity",colour="blue")

p_OI_10DTE <- ggplot(mids) + 
  geom_line()



dte_select <-filter(data, DTE == 3)
dte_select <- subset(dte_select, Strike %in% 0:250)
p_OI_10DTE <- ggplot(dte_select) + 
  geom_bar(aes(x=Strike, y=NetOI),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(320,500,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "red")



#Call Volume 

p_Call_Volume <- ggplot(data) + 
  geom_bar(aes(x=Strike, y=Calls_Volume),stat="identity", fill="green",colour="green")  +
  coord_flip() +
  scale_x_continuous(breaks=seq(320,500,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "red")

#ATM IV
dte_select <-filter(data, IsATM == TRUE)
dte_select <- subset(dte_select, DTE %in% 0:91)
p_ATM_IV <- ggplot(dte_select) + 
  geom_line(aes(x=DTE, y=Calls_IV),stat="identity",colour="green") +
  geom_line(aes(x=DTE, y=Puts_IV),stat="identity",colour="red") +
+
  coord_flip() +
  scale_x_continuous(breaks=seq(320,500,5)) +
  geom_vline(xintercept = Last_spot_price, linetype="dashed", color = "red")


#barcharts Value and Flip V3 ############################################################

rm(flip_point)
rm(flip_data)
flip_data<-data
DTEarray<-unique(data$DTE)
flip_data = data
flip_point = array() 

search_range<-c(round(Last_spot_price*0.50):round(Last_spot_price*1.50))
f<-function(x) abs(sum(flip_data$NetOI*(flip_data$Strike - x)))

i<-1
for (search_Day in DTEarray) 
{
  
  flip_data <-filter(flip_data, DTE == search_Day)
  
  my_solution<-optimise(f, lower = 0, upper=750 ,maximum = FALSE)
  flip_point[i]<-round(my_solution$minimum, digits = 2)
  i<-i+1
}


flip_point<-as.data.frame(flip_point)
flip_point$DTE <-DTEarray
flip_point$Expiration <-unique(data$Syn_Exp)

p<-ggplot(flip_point) +
  geom_line(aes(x=DTE, y=flip_point))


aggregate(data$Net_GX, by=list(Category=data$DTE), FUN=sum)
aggregate(data$Net_DX, by=list(Category=data$DTE), FUN=sum)


