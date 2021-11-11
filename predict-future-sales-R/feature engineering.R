#import data

library(xlsx)
#trainset
sales <- read.csv("sales_train.csv",header=TRUE)
head(sales)
 
#testset
test<- read.csv("test.csv",header=TRUE)

#other dataset
items <- read.csv("items.csv",header=TRUE)
head(items)
item_category <- read.csv("item_categories.csv",header=TRUE)
head(item_category)
shops < -read.csv("shops.csv",header=TRUE)
head(shops)

#some datasets include Russian. 

#dealing with missing value 
summary(sales)
#so there is no missing value 

#dealing with outliers
boxplot(sales$item_price)
boxplot(sales$item_cnt_day)
L=sales$item_price>0 
sales <- sales[L,]
M=sales$item_price<=50000 
sales <- sales[M,]
P=sales$item_cnt_day>0 
sales <- sales[P,]
Q=sales$item_cnt_day<=1000
sales <- sales[Q,]

#according to the submission sample, we need to predict monthly sales
#so we need to calculate the month sales of every shop and every item
sales$month<-substr(sales$date,4,5)
head(sales)
library(dplyr)
month <- group_by(sales,shop_id,item_id,date_block_num,month,item_price)
monthsales <- summarise(month,item_cnt_month=sum(item_cnt_day))
head(monthsales)


#merge item_category_id 
items<-subset(items,select=-item_name)
newsales<-merge(monthsales,items,all.x = TRUE, by="item_id")

#add past 1-6 months sales for each shop and each item 
newsales2<-newsales
newsales2<-newsales2 %>%  
  group_by(shop_id,item_id) %>%  
  mutate(item_cnt_last_month1= lag(item_cnt_month, 1, order_by = date_block_num))
newsales2<-newsales2 %>%  
  group_by(shop_id,item_id) %>%  
  mutate(item_cnt_last_month2= lag(item_cnt_month, 2, order_by = date_block_num))
newsales2<-newsales2 %>%    
  group_by(shop_id,item_id) %>%  
  mutate(item_cnt_last_month3= lag(item_cnt_month, 3, order_by = date_block_num))
newsales2<-newsales2 %>%    
  group_by(shop_id,item_id) %>% 
  mutate(item_cnt_last_month4= lag(item_cnt_month, 4, order_by = date_block_num))
newsales2<-newsales2 %>%    
  group_by(shop_id,item_id) %>% 
  mutate(item_cnt_last_month5= lag(item_cnt_month, 5, order_by = date_block_num))
newsales2<-newsales2 %>%   
  group_by(shop_id,item_id) %>%  
  mutate(item_cnt_last_month6= lag(item_cnt_month, 6, order_by = date_block_num))


#add shop sales for each shop and each item 
newsales3<-newsales2
newsales3 <- newsales3 %>%
  group_by(shop_id,date_block_num) %>%
  mutate(month_shop_cnt=sum(item_cnt_month))

#add item category sales for each category
newsales3 <- newsales3 %>%
  group_by(item_category_id,date_block_num) %>%
  mutate(month_category_cnt=sum(item_cnt_month))

#add only item sales for same item
newsales3<-newsales3 %>%
  group_by(item_id,date_block_num) %>%
  mutate(item_cnt=sum(item_cnt_month))

#replace all NA with O
newsales3[is.na(newsales3)]=0

#add all sales of past 2 months and avg sales of past 2 months
newsales3$cum_sales_2_month<-newsales3$item_cnt_last_month1 + newsales3$item_cnt_last_month2
newsales3$avg_sales_2_month<-(newsales3$item_cnt_last_month1 + newsales3$item_cnt_last_month2)/2

#add all sales of past 3 months and avg sales of past 3 months
newsales3$cum_sales_3_month<- newsales3$item_cnt_last_month1 + newsales3$item_cnt_last_month2+newsales3$item_cnt_last_month3
newsales3$avg_sales_3_month<-newsales3$cum_sales_3_month/3

#add all sales of past 3 months and avg sales of past 3 months
newsales3$cum_sales_6_month<- newsales3$item_cnt_last_month1 + newsales3$item_cnt_last_month2+newsales3$item_cnt_last_month3+newsales3$item_cnt_last_month4 + newsales3$item_cnt_last_month5+newsales3$item_cnt_last_month6
newsales3$avg_sales_6_month<-newsales3$cum_sales_6_month/6

#write csv
write.table(newsales3,"newsale_data.csv")
write.csv(newsales3,"newsale_data.csv")
