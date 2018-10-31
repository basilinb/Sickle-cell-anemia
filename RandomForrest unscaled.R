#Random Forest for Scaled data
#setwd("C:/Users/basil/Desktop/R Project")
library(randomForest)
library(caret)
hemo = read.csv("hemo.csv",header = TRUE)
hemopredictors = hemo[,1:23]
nor_hemopredictors = hemopredictors
nor_hemopredictors <- as.data.frame(nor_hemopredictors)
fhbfclass <- hemo$FHbF
fhbfclass[fhbfclass<15.0] <- 0
fhbfclass[fhbfclass>=15.0] <- 1
fhbfclass <- as.factor(fhbfclass)
nor_hemopredictors$class <- fhbfclass

#1st iteration
set.seed(123)
ind1 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.83,0.17))
train1 <-nor_hemopredictors[ind1==1,]
test1 <- nor_hemopredictors[ind1==2,]
set.seed(222)
rf1 <- randomForest(class~.,data=train1,ntree= 460,mtry = 15,importance=TRUE,proximity=TRUE)
prediction1 <- predict(rf1,test1)
plot(rf1)
tuning = tuneRF(train1[,-15],train1[,15],stepFactor = 0.8,plot = TRUE,ntreeTry = 460,trace = TRUE,improve = 0.05)
varImpPlot(rf1,sort = T,main = "Feature Importance")
print("No. of Trees 460 and split ration of 0.83:")
print(confusionMatrix(prediction1,test1$class))


#2nd iteration
set.seed(123)
ind2 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.66,0.34))
train2 <-nor_hemopredictors[ind2==1,]
test2 <- nor_hemopredictors[ind2==2,]
set.seed(222)
rf2 <- randomForest(class~.,data=train2,ntree= 460,mtry = 15,importance=TRUE,proximity=TRUE)
prediction2 <- predict(rf2,test2)
print("No. of Trees 460 and split ration of 0.66:")
print(confusionMatrix(prediction2,test2$class))


#3rd iteration
set.seed(123)
ind3 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.70,0.30))
train3 <-nor_hemopredictors[ind3==1,]
test3 <- nor_hemopredictors[ind3==2,]
set.seed(222)
rf3 <- randomForest(class~.,data=train3,ntree= 460,mtry = 15,importance=TRUE,proximity=TRUE)
prediction3 <- predict(rf3,test3)
print("No. of Trees 460 and split ration of 0.70:")
print(confusionMatrix(prediction3,test3$class))


#4th iteration
set.seed(123)
ind4 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.70,0.30))
train4 <-nor_hemopredictors[ind4==1,]
test4 <- nor_hemopredictors[ind4==2,]
set.seed(222)
rf4 <- randomForest(class~.,data=train4,ntree= 500,mtry = 15,importance=TRUE,proximity=TRUE)
prediction4 <- predict(rf4,test4)
#plot(rf4)
#tuning = tuneRF(train4[,-15],train4[,15],stepFactor = 0.8,plot = TRUE,ntreeTry = 560,trace = TRUE,improve = 0.05)
#varImpPlot(rf4,sort = T)
print("No. of Trees 500 and split ration of 0.70:")
print(confusionMatrix(prediction4,test4$class))

#5nd iteration
set.seed(123)
ind5 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.66,0.34))
train5 <-nor_hemopredictors[ind5==1,]
test5 <- nor_hemopredictors[ind5==2,]
set.seed(222)
rf5 <- randomForest(class~.,data=train5,ntree= 500,mtry = 15,importance=TRUE,proximity=TRUE)
prediction5 <- predict(rf5,test5)
print("No. of Trees 500 and split ration of 0.66:")
print(confusionMatrix(prediction5,test5$class))

#6th iteration
set.seed(123)
ind6 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.83,0.17))
train6 <-nor_hemopredictors[ind6==1,]
test6 <- nor_hemopredictors[ind6==2,]
set.seed(222)
rf6 <- randomForest(class~.,data=train6,ntree= 500,mtry = 15,importance=TRUE,proximity=TRUE)
p6 <- predict(rf6,train6)
prediction6 <- predict(rf6,test6)
print("No. of Trees 500 and split ration of 0.83:")
print(confusionMatrix(prediction6,test6$class))

#7th iteration
set.seed(123)
ind7 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.70,0.30))
train7 <-nor_hemopredictors[ind7==1,]
test7 <- nor_hemopredictors[ind7==2,]
set.seed(222)
rf7 <- randomForest(class~.,data=train7,ntree= 560,mtry = 15,importance=TRUE,proximity=TRUE)
prediction7 <- predict(rf7,test7)
print("No. of Trees 560 and split ration of 0.70:")
print(confusionMatrix(prediction7,test7$class))

#8nd iteration
set.seed(123)
ind8 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.66,0.34))
train8 <-nor_hemopredictors[ind8==1,]
test8 <- nor_hemopredictors[ind8==2,]
set.seed(222)
rf8 <- randomForest(class~.,data=train8,ntree= 560,mtry = 15,importance=TRUE,proximity=TRUE)
prediction8 <- predict(rf8,test8)
print("No. of Trees 560 and split ration of 0.66:")
print(confusionMatrix(prediction8,test8$class))

#9th iteration
set.seed(123)
ind9 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.83,0.17))
train9 <-nor_hemopredictors[ind9==1,]
test9 <- nor_hemopredictors[ind9==2,]
set.seed(222)
rf9 <- randomForest(class~.,data=train9,ntree= 560,mtry = 15,importance=TRUE,proximity=TRUE)
p9 <- predict(rf9,train9)
prediction9 <- predict(rf9,test9)
print("No. of Trees 560 and split ration of 0.83:")
print(confusionMatrix(prediction9,test9$class))

#7th iteration
set.seed(123)
ind7 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.73,0.27))
train7 <-nor_hemopredictors[ind7==1,]
test7 <- nor_hemopredictors[ind7==2,]
set.seed(222)
rf7 <- randomForest(class~.,data=train7,ntree= 460,mtry = 15,importance=TRUE,proximity=TRUE)
prediction7 <- predict(rf7,test7)
print("No. of Trees 460 and split ration of 0.73:")
print(confusionMatrix(prediction7,test7$class))

#8nd iteration
set.seed(123)
ind8 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.73,0.27))
train8 <-nor_hemopredictors[ind8==1,]
test8 <- nor_hemopredictors[ind8==2,]
set.seed(222)
rf8 <- randomForest(class~.,data=train8,ntree= 500,mtry = 15,importance=TRUE,proximity=TRUE)
prediction8 <- predict(rf8,test8)
print("No. of Trees 500 and split ration of 0.73:")
print(confusionMatrix(prediction8,test8$class))

#9th iteration
set.seed(123)
ind9 <- sample(2,nrow(nor_hemopredictors),replace = TRUE, prob = c(0.73,0.27))
train9 <-nor_hemopredictors[ind9==1,]
test9 <- nor_hemopredictors[ind9==2,]
set.seed(222)
rf9 <- randomForest(class~.,data=train9,ntree= 560,mtry = 15,importance=TRUE,proximity=TRUE)
prediction9 <- predict(rf9,test9)
print("No. of Trees 560 and split ration of 0.73:")
print(confusionMatrix(prediction9,test9$class))

