library(dplyr)
sales<-read.csv("sales_train.csv",stringsAsFactors = FALSE)
summary(sales)
head(sales)
is.na(sales)
na.omit(sales)
Vdate <- as.Date(sales$date,"%d.%m.%Y")
Vdate.bymonth<-cut(Vdate,breaks = "month")
Vdate.bymonth#data format transformation
item<- split(sales,sales$item_id)
item#item classification
summary(item)
shop_item<-split(sales$shop_id,sales$item_id)
#result1<-lapply(shop_item,FUN=function(x) sum(x$AMOUNT))
#result2<-lapply(shop_item,FUN=function(x) max(x$AMOUNT))
#result<-cbind(result1,result2)
sapply(shop_item,mean)
install.packages("xgboost")

library(lightgbm)
data(agaricus.train, package='lightgbm')
train <- agaricus.train
dtrain <- lgb.Dataset(train$data, label=train$label)
params <- list(objective= "regression",
               metric= "l2",
               device= "gpu") #如仅安装cpu版本则改成device = "cpu"
model <- lgb.cv(params, 
                dtrain, 
                nrounds = 10, 
                nfold = 5, 
                min_data = 1, 
                learning_rate = 1, 
                early_stopping_rounds = 10)
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
sales <- sales[sales$item_price>0 && sales$item_price<=50000 && sales$item_cnt_day>0 && sales$item_cnt_day<=1000]

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




# 利用xgboost包的xgb.create.features构造新特征变量
library(xgboost)

traindata1 <- data.matrix(//,[,c(1:8)]) # 将自变量转化为矩阵
library(Matrix)
traindata2 <- Matrix(traindata1,sparse=T) # 利用Matrix函数，将sparse参数设置为TRUE，转化为稀疏矩阵
traindata3 <- as.numeric(as.character(traindata[,9])) # 将因变量转化为numeric
traindata4 <- list(data=traindata2,label=traindata3) # 将自变量和因变量拼接为list
dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label) # 构造模型需要的xgb.DMatrix对象，处理对象为稀疏矩阵

testset1 <- data.matrix(testset[,c(1:8)]) # 将自变量转化为矩阵
testset2 <- Matrix(testset1,sparse=T) # 利用Matrix函数，将sparse参数设置为TRUE，转化为稀疏矩阵
testset3 <- as.numeric(as.character(testset[,9])) # 将因变量转化为numeric
testset4 <- list(data=testset2,label=testset3) # 将自变量和因变量拼接为list
dtest <- xgb.DMatrix(data = testset4$data, label = testset4$label) # 构造模型需要的xgb.DMatrix对象，处理对象为稀疏矩阵

param <- list(max_depth=2, eta=1, silent=1, objective='binary:logistic') # 定义模型参数
nround = 4

bst = xgb.train(params = param, data = dtrain, nrounds = nround, nthread = 2) # 构造xgboost模型

new.features.train <- xgb.create.features(model = bst, traindata4$data) # 生成xgboost构造的新特征组合，训练集
new.features.test <- xgb.create.features(model = bst, testset4$data) # 生成xgboost构造的新特征组合，测试集

newdtrain <- as.data.frame(as.matrix(new.features.train)) # 将训练集的特征组合转化为dataframe格式
newdtest <- as.data.frame(as.matrix(new.features.test)) # 将测试集的特征组合转化为dataframe格式

newtraindata <- cbind(newdtrain,backflag1=traindata$backflag) # 将训练集的自变量和因变量合并
newtestdata <- cbind(newdtest,backflag1=testset$backflag) # 将测试集的自变量和因变量合并

model <- xgb.dump(bst,with_stats = T) # 显示计算过程，查看树结构
model 
names <- dimnames(data.matrix(traindata[,c(1:8)]))[[2]] # 获取特征的真实名称
importance_matrix <- xgb.importance(names,model=bst) # 计算变量重要性

install.packages("lightgbm", repos = "https://cran.r-project.org")
library(lightgbm)
library(lightgbm)
data(agaricus.train, package='lightgbm')
train <- agaricus.train
dtrain <- lgb.Dataset(train$data, label=train$label)
params <- list(objective= "regression",
               metric= "l2",
               device= "cpu") #如仅安装cpu版本则改成device = "cpu"
model <- lgb.cv(params, 
                dtrain, 
                nrounds = 10, 
                nfold = 5, 
                min_data = 1, 
                learning_rate = 1, 
                early_stopping_rounds = 10)