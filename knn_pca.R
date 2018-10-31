#setwd("/Users/vera/Desktop/BIOMI600/finalproject")
hemo_data <- read.csv("hemo.csv", header = TRUE)
library(class)

# get true classification according to 15% FHbF criteria and store it in 'tclass'
tclass <- hemo_data$FHbF
for (i in 1:length(tclass)) {
  if (tclass[i] >= 15) {tclass[i] <- 1} #if final Hbf value is greater than or equal to 15, assign 1 as a responder
  else {tclass[i] <- 0}
}

# get 23 parameters (exclude FHbF and Class column)
hemo_data <- hemo_data[,1:23]
# scale data to have mean=0, sd=1 
hemo_data <- scale(hemo_data, center = TRUE, scale = TRUE)

##---------------------Dimensionality reduction---------------------
# perform feature reduction using PCA
pc <- prcomp(hemo_data)
pc.use1 <- 13 # choose 13 PCs, which include 90% of data
trunc1 <- pc$x[,1:pc.use1]
pc.use2 <- 15 # choose 15 PCs, which include 95% of data
trunc2 <- pc$x[,1:pc.use2]

# set a vector of k values
k <- c(1,3,5,7,9,11,13,15,17)

# perform knn and calculate accuracy when using 13 PCs
accu1 <- c()
for (i in k) {
  prediction <- knn.cv(trunc1, tclass, k=i, use.all = TRUE)
  result_matrix <- table(prediction, tclass)
  accuracy <- sum(diag(result_matrix))/sum(result_matrix)
  accu1 <- c(accu1,accuracy)
}

# perform knn and calculate accuracy when using 15 PCs
accu2 <- c()
for (i in k) {
  prediction <- knn.cv(trunc2, tclass, k=i, use.all = TRUE)
  result_matrix <- table(prediction, tclass)
  accuracy <- sum(diag(result_matrix))/sum(result_matrix)
  accu2 <- c(accu2,accuracy)
}

# perform knn and calculate accuracy when using all features
accu3 <- c()
for (i in k) {
  prediction <- knn.cv(pc$x, tclass, k=i, use.all = TRUE)
  result_matrix <- table(prediction, tclass)
  accuracy <- sum(diag(result_matrix))/sum(result_matrix)
  accu3 <- c(accu3,accuracy)
}

##---------------------23 predictor ranking---------------------
# Calculate accuracy for each predictor column by column when k=7
predict_result1 <- matrix(data = NA, nrow = 23, ncol = 1) #make a 23x1 matrix to hold accuracy
rownames(predict_result1) <- c(colnames(hemo_data)[1:23])
colnames(predict_result1) <- 'Accuracy'
for (i in 1:23) { 
  para <- as.matrix(hemo_data[,i]) #since hemo_data has parameters in columns, get each of 23 columns as training data
  prediction <- knn.cv(para, tclass, k=7, use.all = TRUE) #one column input as training data, tclass as true classifications, k=7 stands for 7 nearest neighbour
  crosstable <- table(prediction, tclass)
  Accuracy <- sum(diag(crosstable))/sum(crosstable)
  predict_result1[i,1] <- Accuracy #add values to result matrix
}
sorted_rank1 <- predict_result1[order(predict_result1[,1], decreasing = TRUE),]

# Calculate accuracy for each predictor column by column when k=5
predict_result2 <- matrix(data = NA, nrow = 23, ncol = 1) #make a 23x1 matrix to hold accuracy
rownames(predict_result2) <- c(colnames(hemo_data)[1:23])
colnames(predict_result2) <- 'Accuracy'
for (i in 1:23) { 
  para <- as.matrix(hemo_data[,i]) #since hemo_data has parameters in columns, get each of 23 columns as training data
  prediction <- knn.cv(para, tclass, k=5, use.all = TRUE) #one column input as training data, tclass as true classifications, k=7 stands for 7 nearest neighbour
  crosstable <- table(prediction, tclass)
  Accuracy <- sum(diag(crosstable))/sum(crosstable)
  predict_result2[i,1] <- Accuracy #add values to result matrix
}
sorted_rank2 <- predict_result2[order(predict_result2[,1], decreasing = TRUE),]

##---------------------Only use Age and HbF as predictors---------------------
hemo_small <- cbind(hemo_data[,1], hemo_data[,10])
prediction4 <- knn.cv(hemo_small, tclass, k=9, use.all = TRUE)
result_matrix4 <- table(prediction4, tclass)

