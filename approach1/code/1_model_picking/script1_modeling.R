#### setting up working directory
rm(list = ls())
getwd()
setwd('/Users/hawooksong/Desktop/kaggle_show_of_hands/approach1')
dir()



#### replace datasets with missing values with imputed datasets
initial_train_test <- read.csv('data/imputed/initial_train_test_imputed.csv')
final_test <- read.csv('data/imputed/final_test_imputed.csv')

str(initial_train_test)
str(final_test)


#### considering data transformation for continuous variables
head(initial_train_test)
hist(initial_train_test$Age)
hist(initial_train_test$votes) 

library(psych)
describe(initial_train_test$Age)
describe(initial_train_test$votes)

# less than ideal distributions but data transformations not necessary



#### split the 'initial_train_test' dataset to train and test
library(caTools)
set.seed(123)
split <- sample.split(initial_train_test$Happy, SplitRatio=0.75)
initial_train <- initial_train_test[split==TRUE, ]
initial_test <- initial_train_test[split==FALSE, ]
nrow(initial_train)
nrow(initial_test)



#### define a function that can plot the number of prediction errors by various cutoff values
# plotErrorsByCutoff <- function(predPerc, observation) {
#   cutoff <- (1:10) * 0.1;
#   errorsByCutoff <- rep(NA, 10);
#   for (i in 1:length(cutoff)) {
#     predicted <- as.integer(predPerc > cutoff[i])
#     nErrors <- sum(predicted != observation)
#     errorsByCutoff[i] <- nErrors
#   };
#   plot(cutoff, errorsByCutoff, xlab='Cutoff', ylab='Error count',
#        main='Number of prediction errors\n under various cutoff values', pch=20);
# }



#### simple baseline model

# accuracy if all predictions were 1 for train$Happy
table(initial_train$Happy)
1660 / (1286 + 1660)  # 56.35% accuracy on train

# accuracy if all predictions were 1 for test$Happy
table(initial_test$Happy)
553 / (428 + 553)  # 56.37% accuracy on test



#### simple logistic regression model
logModel <- glm(Happy ~ ., data = initial_train, family = binomial)  
summary(logModel)

# calculating the accuracy of logModel on test
predPerc.logModel.initial_test <- predict(logModel, newdata = initial_test, type = 'response')
pred.logModel.initial_test <- ifelse(predPerc.logModel.initial_test > 0.5, 1, 0)
table(pred.logModel.initial_test, initial_test$Happy)
(242 + 410) / (242 + 143 + 186 + 410)  # 66.46% accuracy on test



#### simple Random Forest
library(randomForest)

# changing the dependent variable to factor (as required for RF method)
class(initial_train$Happy)
initial_train$Happy <- as.factor(initial_train$Happy)
initial_test$Happy <- as.factor(initial_test$Happy)

# creating a random forest model
?randomForest
set.seed(123)
RFmodel <- randomForest(Happy ~ ., data = initial_train, ntrees=200)

# calculating the accuracy of RFmodel on test
predPerc.RFmodel.initial_test <- predict(RFmodel, newdata = initial_test, type = 'prob')[ , 2]
pred.RFmodel.initial_test <- ifelse(predPerc.RFmodel.initial_test > 0.5, 1, 0)
table(pred.RFmodel.initial_test, initial_test$Happy)
(209 + 454) / (209 + 99 + 219 + 454)  # 67.58% accuracy on test



#### simple CART model with 10-fold cross validation
library(rpart)  # for rpart()
library(rpart.plot)  # for prp()
library(caret)  # for cross-validation
library(e1071)  # for cross-validation

# setting the training parameters and finding the ideal complex parameter (cp)
trControl <- trainControl(method = 'cv', number = 10)
tuneGrid <- expand.grid(.cp = (1:50) * 0.01)
set.seed(123)
train(Happy ~ . - YOB, 
      data = initial_train, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)

# creating a 10-fold cross-validated CART model
CARTmodel <- rpart(Happy ~ ., data = initial_train, cp = 0.01)
prp(CARTmodel)

# calculating the accuracy of CARTmodel on test
predPerc.CARTmodel.initial_test <- predict(CARTmodel, newdata = initial_test)
pred.CARTmodel.initial_test <- ifelse(predPerc.CARTmodel.initial_test > 0.5, 1, 0)
table(pred.CARTmodel.initial_test, initial_test$Happy)
(149 + 485) / (149 + 68 + 279 + 485)  # 64.62% accuray on test



#### linear discriminant analysis method
library(MASS)  # for lda()

# create LDA model
LDAmodel <- lda(Happy ~ ., data = initial_train)
plot(LDAmodel)

# calculating the accuracy of LDAmodel on test
predPerc.LDAmodel.initial_test <- predict(LDAmodel, newdata = initial_test)$posterior[ , 2]
pred.LDAmodel.initial_test <- ifelse(predPerc.LDAmodel.initial_test > 0.5, 1, 0)
table(pred.LDAmodel.initial_test, initial_test$Happy)
(244 + 412) / (244 + 141 + 184 + 412)  # 66.87% accuracy on test



#### quadratic discriminant analysis method
# create QDA model
QDAmodel <- qda(Happy ~ . - YOB, data = initial_train)

# calculating the accuracy of QDA model on test
predPerc.QDAmodel.initial_test <- predict(QDAmodel, newdata = initial_test)$posterior[ , 2]
pred.QDAmodel.initial_test <- ifelse(predPerc.QDAmodel.initial_test > 0.5, 1, 0)
table(pred.QDAmodel.initial_test, initial_test$Happy)
(224 + 354) / (224 + 199 + 204 + 354)  # 58.92% on test

