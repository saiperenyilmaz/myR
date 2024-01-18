
WeekDTE <-filter(allOptions, DTE == 3)
p3 <- ggplot(WeekDTE, aes(x=Strike, y=NetOI)) + 
  geom_bar(stat = "identity") +
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red")

SevenDTE <-filter(allOptions, DTE ==7)
p7 <- ggplot(SevenDTE, aes(x=Strike, y=NetOI)) + 
  geom_bar(stat = "identity") + 
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red")

SeventeenDTE <-filter(allOptions, DTE ==17)
p17 <- ggplot(SeventeenDTE, aes(x=Strike, y=NetOI)) + 
  geom_bar(stat = "identity") + 
  geom_vline(xintercept = Ticker_current, linetype="dashed", color = "red")


data_for_ridge<-select(allOptions,c(2,3,17))
data_for_ridge <-filter(data_for_ridge, DTE < 60)
data_for_ridge$DTE<-as.factor(data_for_ridge$DTE)
ridge_graph <-ggplot(data_for_ridge, aes(x = Strike, y = DTE, group=DTE)) +
  geom_density_ridges() +
  xlab("Strike") +
  ylab("DTE")


ridge_graph_h <-ggplot(data_for_ridge, aes(x=DTE, y=Strike, width = after_stat(density), group=DTE)) +
  geom_vridgeline(stat="ydensity", trim=FALSE, alpha = 0.85, scale = 2)


ridge_graph_h <-ggplot(data_for_ridge, aes(x=DTE, y=Strike, width = after_stat(density), fill=DTE)) +
  geom_vridgeline(stat="ydensity", trim=FALSE, alpha = 0.85, scale = 2)

#brackets

temp_calls<-allOptions[,c(1,2,3,17)]
call_wall<-temp_calls %>%
  group_by(DTE) %>%
  slice(which.max(CallOI))
colnames(call_wall)[2] <-"CallStrike"


temp_puts<-allOptions[,c(1,2,8,17)]
put_wall<-temp_puts %>%
  group_by(DTE) %>%
  slice(which.max(PutOI))

colnames(put_wall)[2] <-"PutStrike"

Walls<-full_join(call_wall,put_wall,by="DTE")




#heatmap ==============================================================

PutOpenInterest<-ggplot(allOptions, aes(DTE, Strike, fill = PutOI)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red")

CallOpenInterest<-ggplot(allOptions, aes(DTE, Strike, fill = CallOI)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red")

NetOpenInterest<-ggplot(chosenDay, aes(DTE, Strike, fill = NetOI)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red")
#calls minus puts at the same strike and expiration

#==============================================================

#single day density ==============================================================

chosenDay<-filter(allOptions, allOptions$DTE==6)
chosenDay$Strike<-as.factor(chosenDay$Strike)
plot(chosenDay$Strike,chosenDay$NetValue)
plot(chosenDay$Strike,chosenDay$CallOI)
plot(chosenDay$Strike,chosenDay$PutOI)
#==============================================================



#naive gamma

for (d in cd) {
  
  UL<-max(allOptions[allOptions$DTE == d, ]$Strike)
  LL<-min(allOptions[allOptions$DTE == d, ]$Strike)
  
  for (i in LL:UL) {
    ngamma[nrow(ngamma)+1,] <- NA
    x<-nrow(ngamma)
    ngamma$Price[x]<-i
    ngamma$total_ITM_NetPuts[x]<-sum(allOptions[which(allOptions$Strike > i & allOptions$DTE==d), 13])
    ngamma$total_ITM_NetCalls[x]<-sum(allOptions[which(allOptions$Strike < i & allOptions$DTE==d), 14])
    ngamma$dealer_pays[x]<-ngamma$total_ITM_NetPuts[x]+ngamma$total_ITM_NetCalls[x]
    
    ngamma$total_OTM_NetPuts[x]<-sum(allOptions[which(allOptions$Strike < i & allOptions$DTE==d), 13])
    ngamma$total_OTM_NetCalls[x]<-sum(allOptions[which(allOptions$Strike > i & allOptions$DTE==d), 14])
    ngamma$dealer_collects[x]<- ngamma$total_OTM_NetPuts[x]+ngamma$total_OTM_NetCalls[x]
    ngamma$dealer_net[x]<-ngamma$dealer_collects[x]- ngamma$dealer_pays[x]
    ngamma$DTE<-d
  }
  
}

print(d)


for (d in cd) {
  
  print (d * 3)
  
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

#create naive gamma dataframe
ngamma <- data.frame("Price"=numeric(),
                     "total_ITM_NetPuts" = numeric(),
                     "total_ITM_NetCalls"= numeric(),
                     "dealer_pays"= numeric(),
                     "total_OTM_NetPuts"= numeric(),
                     "total_OTM_NetCalls"= numeric(),
                     "dealer_collects"= numeric(),
                     "dealer_net"= numeric(),
                     "DTE"= numeric())