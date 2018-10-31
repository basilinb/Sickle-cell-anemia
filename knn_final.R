# Calling library class for the knn function.
library(class)

# Removing the columns for FHbF and Class from the dataset
csv_dataframe = read.csv("hemo.csv",header = TRUE)
altered_csv_dataframe = subset(csv_dataframe, select = -c(FHbF,Class))

# Normalizing the data
hemo_matrix = as.matrix(altered_csv_dataframe)
norm_hemo_matrix = scale(hemo_matrix, center = TRUE, scale = TRUE)


# Dividing the data in to training and test set
data_size = nrow(norm_hemo_matrix)
train_data_size = round(data_size*(0.7))
test_data_start = train_data_size + 1
training_set = norm_hemo_matrix[1:train_data_size, ]
testing_set = norm_hemo_matrix[test_data_start:data_size, ]

# Making a vector of ground truth that contains the correct classification of the patients
ground_truth = vector()
ground_truth[1:data_size] = 0
ground_truth[csv_dataframe$FHbF >= 15] = 1

# Calling the knn function using inputs of training set, test set and ground truth
pred = knn(training_set,testing_set,ground_truth[1:train_data_size],k=7)

# Making a data frame containg results from knn function and 
# Calculating the true positive, true negative, false positive and false negative rates in the data frame
result = data.frame(gt = ground_truth[test_data_start:data_size],pred,
                    TP=rep(0,length(pred)),
                    TN=rep(0,length(pred)),
                    FP=rep(0,length(pred)),
                    FN=rep(0,length(pred)))
result$TP[result$gt == 1 & result$pred == 1] = 1
result$TN[result$gt == 0 & result$pred == 0] = 1
result$FP[result$gt == 0 & result$pred == 1] = 1
result$FN[result$gt == 1 & result$pred == 0] = 1

# Calculating the total of true positive, true negative, false positive and false negative rates
total = (sum(result$TP) + sum(result$TN) + sum(result$FP) + sum(result$FN))

# Calculating the accuracy
accuracy = (sum(result$TP) + sum(result$TN)) * 100/total

# Calculating the false positive rate
fp_rate = sum(result$FP)*100/total

# Calculating the false negative rate
fn_rate = sum(result$FN)*100/total

GroundTruth = result$gt
cmatrix = table(pred,GroundTruth)
# Printing the results
print(paste("Accuracy = ",accuracy, "%"))
print(paste("False positive rate = ",fp_rate, "%"))
print(paste("False negative rate = ",fn_rate, "%"))

sensitivity = sum(result$TP)/(sum(result$TP) + sum(result$FN))
specificity = 1 - (sum(result$FP) / (sum(result$TN) + sum(result$FP)))

print(sensitivity)
print(specificity)
