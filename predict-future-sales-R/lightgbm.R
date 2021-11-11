library(lightgbm)
library(methods)
library(dplyr)
library(Matrix)
library(ggplot2)
library(xgboost)
# We load in the newsale_data dataset
setwd(dir = "D:/R/Rstudio/project")
newdata<- read.csv("train_data_final.csv",header=TRUE)

train <- newdata[newdata$date_block_num>=10 & newdata$date_block_num<=31 ,]
table(train$date_block_num)
test  <- newdata[newdata$date_block_num >31 ,]
table(test$date_block_num)
# The loaded data is stored in sparseMatrix, and label is a numeric vector in {0,1}

  traindata1 =  data.matrix(subset(train,select = -item_cnt_month))
  traindata2 <- Matrix(traindata1,sparse=T)
  traindata3 <- train$item_cnt_month
  traindata4 <- list(data=traindata2,label = traindata3)
 #traindata4$data, label = traindata4$label)
  testdata1 =  data.matrix(subset(test,select = -item_cnt_month))
  testdata2 <- Matrix(testdata1,sparse=T)
  testdata3 <- test$item_cnt_month
  testdata4 <- list(data=testdata2,label = testdata3)

 #Set parameters for model training
train_params <- list(
  max_depth = 200,
  num_leaves = 100, num_iterations = 200,
  learning_rate = 0.4  , nthread = -1
)
#--------------------Basic Training using lightgbm----------------
# This is the basic usage of lightgbm you can put matrix in data field
print("Training lightgbm with sparseMatrix")
bst <- lightgbm(
  data = traindata2,
  label = traindata3
  , params = train_params,
  obj = "regression"
  , nrounds = 10L,
  verbose = 2L,
  boosting = 'gbdt',
  metric = 'rmse'
)
?lightgbm


# Verbose = 0,1,2
#print("Train lightgbm with verbose 0, no message")
#bst <- lightgbm(
#  data = traindata2
#  , params = train_params
#  , nrounds = 2L
#  , verbose = 0L
#)

#print("Train lightgbm with verbose 1, print evaluation metric")
#bst <- lightgbm(
#  data = traindata2
#  , params = train_params
#  , nrounds = 2L
#  , verbose = 1L
#)

#print("Train lightgbm with verbose 2, also print information about tree")
#bst <- lightgbm(
#  data = traindata2
#  , params = train_params
#, nrounds = 2L
#  , verbose = 2L
#)

# You can also specify data as file path to a LibSVM/TCV/CSV format input
# Since we do not have this file with us, the following line is just for illustration
# bst <- lightgbm(
#     data = "agaricus.train.svm"
#     , num_leaves = 4L
#     , learning_rate = 1.0
#     , nrounds = 2L
#
# )

#--------------------Basic prediction using lightgbm--------------
# You can do prediction using the following line
# You can put in Matrix, sparseMatrix, or lgb.Dataset

#importance of features
importance<- lgb.importance(bst)
importance
lgb.plot.importance(importance)
#test predict
pred <- predict(bst, testdata2)
summary(pred)
rmse = sqrt(sum((test$item_cnt_month-pred)^2))/nrow(test)
rmse*100
answer = cbind( item_id=test$item_id,shop_id=test$shop_id,item_cnt_month_pre=pred,item_cnt_month_actual=test$item_cnt_month)
cor(pred,test$item_cnt_month)

#--------------------Save and load models-------------------------
# Save model to binary local file
#lgb.save(bst, "lightgbm.model")


# pred2 should be identical to pred
#print(paste("sum(abs(pred2-pred))=", sum(abs(pred2 - pred))))
### fina answer data
fina_answerdata = read.csv("test_data_final.csv")
fina1 =  data.matrix(subset(fina_answerdata,select = -item_cnt_month))
fina2 <- Matrix(fina1,sparse=T)
fina3 <- test$item_cnt_month
fina4 <- list(data=fina2,label = fina3)

pre_bst = predict(bst,fina2)
answer = cbind( shop_id=fina_answerdata$shop_id,item_id=fina_answerdata$item_id,item_cnt_month=pre_bst)
answer = as.data.frame(answer)
id_get = read.csv('test.csv')
library(tidyr)
id_new=tidyr::unite(id_get, "shop_item", shop_id, item_id)
answer_new=tidyr::unite(answer, "shop_item", shop_id, item_id)
fina_answer=merge(id_new,answer_new,by ="shop_item" )
fina_answer$shop_item =NULL
fina_answer=fina_answer[order(fina_answer$ID),]
write.csv(fina_answer,"answer.csv",row.names = FALSE)
