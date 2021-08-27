library(stringr)
library(lubridate)
library(tidyverse)
library(modelr)
library(sp)
library(leaflet)
library(geosphere)
library(knitr)
library(rpart)


summary(ANZ_Dataset)

# change the format of date column
ANZ_Dataset$date<- as.Date(ANZ_Dataset$date,format = "%d/%m/%Y")

# the dateset only contain records for 91 days, one day is missing
DateRange <- seq(min(ANZ_Dataset$date), max(ANZ_Dataset$date), by = 1) 

DateRange[!DateRange %in% ANZ_Dataset$date] # 2018-08-16 transactions are missing


# derive weekday and hour data of each transaction 
ANZ_Dataset$extraction = as.character(ANZ_Dataset$extraction)
ANZ_Dataset$hour = hour(as.POSIXct(substr(ANZ_Dataset$extraction,12,19),format="%H:%M:%S"))
ANZ_Dataset$weekday = weekdays(ANZ_Dataset$date)


# confirm the one -to -one link of account_id and customer_id
ANZ_Dataset %>% select(account,customer_id) %>% 
  unique() %>% 
  nrow()


# split customer & merchant lat_long into individual columns for analysis 
dfloc = ANZ_Dataset[,c("long_lat","merchant_long_lat")]
dfloc<- dfloc %>% separate("long_lat", c("c_long", "c_lat"),sep=' ')
dfloc<- dfloc %>% separate("merchant_long_lat", c("m_long", "m_lat"),sep=' ')
dfloc<- data.frame(sapply(dfloc, as.numeric))
ANZ_Dataset <- cbind(ANZ_Dataset,dfloc)


# check the range of customer location
df_temp <- ANZ_Dataset %>% 
  filter (!(c_long >113 & c_long <154 & c_lat > (-44) & c_lat < (-10))) 
length(unique(df_temp$customer_id))


#Gathering some insights about the data

# filtering out purchase transactions only 
# assuming purchase transactions must be associated with a merchant (have a merchant Id)
df_temp <- ANZ_Dataset %>% filter(merchant_id != '' )
# it turned out that is equivilent to excluding following categories of transactions
df_csmp <- ANZ_Dataset %>%filter(!(txn_description %in% c('PAY/SALARY',"INTER BANK", "PHONE BANK","PAYMEN
T")))
summary(df_csmp)

# visualise the distribution of transaction amount 
hist(df_csmp$amount[!df_csmp$amount %in% boxplot.stats(df_csmp$amount)$out], #exclude outliers
     xlab= 'Transaction Amount', main = 'Histogram of purchase transaction amount')

hist(ANZ_Dataset$amount[!ANZ_Dataset$amount %in% boxplot.stats(ANZ_Dataset$amount)$out], #exclude outliers
     xlab= 'Transaction Amount',main = 'Histogram of overall transaction amount')

#Visualise customers’average monthly transaction volume.
df2 <- ANZ_Dataset %>% 
  group_by(customer_id) %>% 
  summarise(mon_avg_vol = round(n()/3,0))
hist(df2$mon_avg_vol, 
     xlab= 'Monthly transaction volume', ylab='No. of customers', main = "Histogram of customer
s' monthly transaction volume")

# Visualise transaction volume over an average week.
df3 <- ANZ_Dataset %>% 
  select(date,weekday) %>% 
  group_by(date,weekday) %>% 
  summarise(daily_avg_vol = n()) %>% 
  group_by(weekday) %>% 
  summarise(avg_vol=mean(daily_avg_vol,na.rm=TRUE ))
df3$weekday <- factor(df3$weekday, levels=c( "Monday","Tuesday","Wednesday",
                                             "Thursday","Friday","Saturday","Sunday"))
ggplot(df3,aes(x=weekday, y=avg_vol)) +geom_point()+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by weekday') +
  labs(x='Weekday',y='Transaction volume')

# visualize transaction volume over an average week.
df4 <- ANZ_Dataset %>% 
  select(date,hour) %>%
  group_by(date,hour) %>% 
  summarize(trans_vol=n()) %>% 
  group_by(hour) %>% 
  summarize(trans_vol_per_hr = mean(trans_vol,na.rm=TRUE))
ggplot(df4,aes(x=hour,y=trans_vol_per_hr))+geom_point()+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by hour') +
  labs(x='Hour',y='Transaction volume') + expand_limits( y = 0)


# exclude the single foreign customer whose location information was incorrectly stored (i.e lat
itude 573)
df_temp <- df_csmp %>% 
  filter (c_long >113 & c_long <154 & c_lat > (-44) & c_lat < (-10))
dfloc = df_temp [,c("c_long", "c_lat","m_long", "m_lat")] 
dfloc<- data.frame(sapply(dfloc, as.numeric))
dfloc$dst <- distHaversine(dfloc[, 1:2], dfloc[, 3:4]) / 1000
hist(dfloc$dst[dfloc$dst<100], main = "Distance between customer and merchants",xlab= 'Distance
 (km)' )
