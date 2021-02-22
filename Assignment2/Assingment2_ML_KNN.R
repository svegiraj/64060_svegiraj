#installing the required below packages

install.packages("ISLR")
install.packages("caret")
install.packages("FNN")
install.packages("gmodels")
install.packages("dummies")
install.packages("e1071")

#loading all the required libraries
library(e1071)
library(dplyr)
library(caret)
library(ISLR)
library(FNN)
library(gmodels)
library(dummies)

#Reading the Universal bank CSV file to UB

UB<-read.csv("D:/MACHINE LEARNING/UniversalBank.csv")

#remove ID and ZIPCODE attributes
Bank<-UB[,c(-1,-5)]
str(Bank)

#Creating dummies for the Education column by using dummies package

edu_dummy_model <- dummy(Bank$Education)
tmp <- cbind(Bank,edu_dummy_model)
head(tmp)

#Remove the education col

UBank <- tmp[c(-6)]
head(UBank)

#Data Partition: training-60% and validation-40%

set.seed(20)
Index <- createDataPartition(UBank$Income, p= 0.6, list = FALSE)

training_data <- UBank[Index,]
dim(training_data)

validation_data <- UBank [-Index,]
dim(validation_data)

#standardise the data using normalization
norm_model<-preProcess(training_data, method = c("center","scale"))
head(norm_model)
train_nf <- predict(norm_model,training_data)
validation_nf <- predict(norm_model,validation_data)
total_nf <- predict(norm_model, UBank)

summary(total_nf)
summary(train_nf)
summary(validation_nf)

set.seed(10)

train_data <- train_nf[,-7]
head(train_data)
train_outcome <- factor(training_data[,7], levels = c(0,1), labels = c("deny","accept"))
head(train_outcome)
valida_data <- validation_nf[,-7]
head(valida_data)
validation_outcome <- factor(validation_data[,7], levels = c(0,1), labels = c("deny","accept"))
head(validation_outcome)
total_data <- total_nf[,-7]
total_outcome <- factor(UBank[,7], levels = c(0,1), labels = c("deny", "accept"))
head(total_outcome)

#1. Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education_1 = 0, Education_2 =1, Education_3 = 0, Mortgage = 0, Securities Account = 0, CD Account = 0, Online = 1, and Credit Card = 1. Perform a k-NN classification with all predictors except ID and ZIP code using k = 1.Remember to transform categorical predictors with more than two categories into dummy variables first. Specify the success class as 1 (loan acceptance), and use the default cutoff value of 0.5. How would this customer be classified?
#Knn method where k=1
#Predicting the Customer with K=1

TestTraining_Data <- c(40, 10, 84, 2, 2, 0, 0, 0, 1, 1, 0, 1, 0)
knn_test <- knn(train_data,TestTraining_Data,cl=train_outcome,k=1,prob = TRUE)
knn_test

#2. What is a choice of k that balances between overfitting and ignoring the predictor information?

bestk <- data.frame(k = seq(1,55,1), accuracy = rep (0, 55))
head(bestk)
for (i in 1:55){
  knn.pred <- knn(train_data, valida_data, cl=train_outcome, k=i)
  bestk[i,2] <- confusionMatrix(knn.pred, validation_outcome)$overall[1]
}

head(bestk)

bestk_fit <- bestk[which.max(bestk$accuracy),]
bestk_fit 
#Here, our optimal k is 4

#3. Show the confusion matrix for the validation data that results from using the best k.

knn.pred <- knn(train_data, valida_data, cl=train_outcome, k=bestk_fit$k, prob = TRUE)

CrossTable(validation_outcome, knn.pred)

#4. Consider the following customer: Age = 40, Experience = 10, Income = 84,
#Family = 2, CCAvg = 2, Education_1 = 0, Education_2 = 1, Education_3 = 0,
#Mortgage = 0, Securities Account = 0, CD Account = 0, Online = 1 and Credit
#Card = 1. Classify the customer using the best k.

TestTraining_Data <- c(40, 10, 84, 2, 2, 0, 0, 0, 1, 1, 0, 1, 0)
bestfitknn <- knn(train_data, TestTraining_Data, cl=train_outcome, k =bestk_fit$k, prob = TRUE)
(bestfitknn)
#By using the Complete Data Set 
totalKnn <- knn(train_data,total_data, cl=train_outcome, k= bestk_fit$k, prob = TRUE)
CrossTable(total_outcome, totalKnn)

#5. Repartition the data, this time into training, validation, and test sets (50% : 30% : 20%). Apply the
#k-NN method with the k chosen above. Compare the confusion matrix of the test set with that of
#the training and validation sets. Comment on the differences and their reason.

#partition the data into training, validation, and test sets (50% : 30% : 20%)

set.seed(15)
IndexNew1 <- createDataPartition(UBank$Income, p = 0.5, list = FALSE)
training_data2 = UBank[IndexNew1,]

Remdata <- UBank[-IndexNew1,]

IndexNew2 <- createDataPartition(Remdata$Income, p = 0.6, list = FALSE)
validation_data2 = Remdata[IndexNew2,]

test_data2 <- Remdata[-IndexNew2,]
head(test_data2)

#standardize the data using normalization

norm_values2 <- preProcess(training_data2, method = c ("center", "scale"))

train_nf2 <- predict(norm_values2, training_data2)

validation_nf2 <- predict(norm_values2, validation_data2)

test_nf2 <- predict(norm_values2, test_data2)

total_nf2 <- predict(norm_values2, UBank)

train_data2 <- train_nf2[,-7]
training_outcome2 <- factor(training_data2[,7], levels = c(0,1), labels = c("Deny","Accept"))

valida_data2 <- validation_nf2[,-7]
validation_outcome2 <- factor(validation_data2[,7], levels = c(0,1), labels = c("Deny", "Accept"))

TestTraining_Data2 <- test_nf2[,-7]
Testraining_outcome2 <- factor(test_data2[,7], levels = c(0,1), labels = c("Deny", "Accept"))

Total_Data2 <- total_nf2[,-7]
Total_Outcome2 <- factor(UBank[,7], levels = c(0,1), labels = c("Deny", "Accept"))

#Applying KNN with the optimum k value (k=4) to the Training and Validation set
#validation
Knn_validation <- knn(train_data2, valida_data2, cl=training_outcome2, k= bestk_fit$k, prob = TRUE)
CrossTable(validation_outcome2,Knn_validation, prop.chisq = FALSE)
#Validation Accuracy : 96.07

#Applying KNN with the optimum k value (k=4) to the Training and Test set
#test
Knn_testing <- knn(train_data2, TestTraining_Data2, cl=training_outcome2, k =bestk_fit$k, prob = TRUE)
CrossTable(Testraining_outcome2,Knn_testing, prop.chisq = FALSE)
#Test Accuracy : 97.09

#Applying KNN with the optimum k value (k=4) to the entire dataset.
#total
Knn_total <- knn(train_data2, Total_Data2, cl=training_outcome2, k =bestk_fit$k, prob = TRUE)
CrossTable(Total_Outcome2,Knn_total, prop.chisq = FALSE)
#Total Accuracy : 97.14

# Reason: When we feed more data to the model, the model performs better, giving you more precision. In the case above, there is more data in the training set compared to the validation set. Accuracy has, thus, increased.

