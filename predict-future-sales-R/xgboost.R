setwd(dir = "E:/R learn/project")

#训练集
mydata <- read.csv("train_data_new.csv",header=TRUE)
#mydata = subset(mydata,select = -c(item_single_sale,shop_sale,category_sale,shop_category_sale))
library(dplyr)
library(xgboost)
library(Matrix)
library(ggplot2)
library(Ckmeans.1d.dp)
library(pROC)
# divide train and test set
traindata =mydata[mydata$date_block_num >=10 & mydata$date_block_num <=31 ,]
table(traindata$date_block_num)
testdata = mydata[mydata$date_block_num >31,]
table(testdata$date_block_num)

# A function to change the set to xg Matrix
changetoM = function(traindata){
  traindata1 =  data.matrix(subset(traindata,select = -item_cnt_month))
  traindata2 <- Matrix(traindata1,sparse=T)
  traindata3 <- traindata$item_cnt_month
  traindata4 <- list(data=traindata2,label = traindata3)
  ddata=xgb.DMatrix(data = traindata4$data, label = traindata4$label)
  return(ddata)
}

train = changetoM(traindata)

# set the factor of model
nround = 15

# parameter selection
para = seq(11,19,1)
rmse_save = 100
para_save = 0
for(i in para)
{
  xgb = xgboost( data = train, 
                 nrounds = i,
                 nthread = -1,
                 eta =0.87,
                 max_depth=8,
                 subsample=0.59,
                 colsample_bytree=0.61,
                 alpha = 0.54,
                 lambda=0.5,
                 set.seed(123),
                 eval_metric = 'rmse') 
  pre_xgb = predict(xgb,newdata = test)
  rmse = sqrt(sum((testdata$item_cnt_month-pre_xgb)^2))/nrow(testdata)
  if(rmse<rmse_save){
    rmse_save = rmse
    para_save = i
  }
}
rmse_save
para_save
# construct xgboost model
xgb = xgboost( data = train, 
               nrounds = 16,
               nthread = -1,
               eta =0.11,
               max_depth=8,
               subsample=0.59,
               colsample_bytree=0.61,
               alpha = 0.54,
               lambda=0.5,
               set.seed(123),
               eval_metric = 'rmse') 


# Importance of features
names_d <- dimnames(data.matrix(subset(traindata,select = -item_cnt_month)))[[2]] 
importance <- xgb.importance(names_d, model = xgb)  
head(importance)
xgb.ggplot.importance(importance)

# test data predict
test = changetoM(testdata)

pre_xgb = predict(xgb,newdata = test)
cor(pre_xgb,testdata$item_cnt_month)
rmse = sqrt(sum((testdata$item_cnt_month-pre_xgb)^2))/nrow(testdata)
rmse*100
summary(pre_xgb)
summary(testdata$item_cnt_month)

### fina answer data
fina_answerdata = read.csv("test_data_new.csv")
#fina_answerdata = subset(fina_answerdata,select = -c(item_single_sale,shop_sale,category_sale,shop_category_sale))
fina = changetoM(fina_answerdata)
pre_xgb = predict(xgb,newdata = fina)
answer = cbind( shop_id=fina_answerdata$shop_id,item_id=fina_answerdata$item_id,item_cnt_month=pre_xgb)
answer = as.data.frame(answer)
id_get = read.csv('test.csv')
library(tidyr)
id_new=tidyr::unite(id_get, "shop_item", shop_id, item_id)
answer_new=tidyr::unite(answer, "shop_item", shop_id, item_id)
fina_answer=merge(id_new,answer_new,by ="shop_item" )
fina_answer$shop_item =NULL
fina_answer=fina_answer[order(fina_answer$ID),]
write.csv(fina_answer,"answer.csv",row.names = FALSE)
