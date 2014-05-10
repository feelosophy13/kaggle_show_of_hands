#### 
?kmeans
head(initial_train.dc.norm)



#### create 2 cluster groups through k-means method
install.packages('flexclust')
library(flexclust)  # for as.kcca()

set.seed(123)
km2 <- kmeans(initial_train.dc.indep.norm, center=2)
str(km2)

km2.kcca <- as.kcca(km2, initial_train.dc.indep.norm)
kClust.2Groups.initial_train <- km2$cluster
kClust.2Groups.initial_test <- predict(km2.kcca, newdata = initial_test.dc.indep.norm)



#### briefly examine cluster characteristics
table(kClust.2Groups.initial_train)
table(kClust.2Groups.initial_test)

tapply(initial_train$Age, kClust.2Groups.initial_train, mean)
tapply(initial_test$Age, kClust.2Groups.initial_test, mean)

tapply(initial_train$votes, kClust.2Groups.initial_train, mean)
tapply(initial_test$votes, kClust.2Groups.initial_test, mean)

tapply(initial_train$Happy, kClust.2Groups.initial_train, mean)



#### split initial_training and initial_test sets into clusters
initial_train1 <- subset(initial_train, kClust.2Groups.initial_train==1)
initial_train2 <- subset(initial_train, kClust.2Groups.initial_train==2)

initial_test1 <- subset(initial_test, kClust.2Groups.initial_test==1)
initial_test2 <- subset(initial_test, kClust.2Groups.initial_test==2)



#### logistic regression model on the two initial_train clusters

## building models using the logistic regression method
logModel1 <- glm(Happy ~ ., data = initial_train1, family = binomial)
logModel2 <- glm(Happy ~ ., data = initial_train2, family = binomial)

## on the initial_testing dataset
# prediction probabilities
predPerc.logModel1.initial_test1 <- predict(logModel1, newdata = initial_test1, type='response')
predPerc.logModel2.initial_test2 <- predict(logModel2, newdata = initial_test2, type='response')

# predictions
pred.logModel1.initial_test1 <- as.integer(predPerc.logModel1.initial_test1 > 0.5)
pred.logModel2.initial_test2 <- as.integer(predPerc.logModel2.initial_test2 > 0.5)

# accuracy 
table(pred.logModel1.initial_test1, initial_test1$Happy)
table(pred.logModel2.initial_test2, initial_test2$Happy)
(111 + 276 + 120 + 139) /
  (111 + 68 + 117 + 276 + 120 + 70 + 80 + 139)
# 65.85% on the initial_testing set

# AUC
prediction.logModel.initial_test <- prediction(c(predPerc.logModel1.initial_test1, predPerc.logModel2.initial_test2),
                                       c(initial_test1$Happy, initial_test2$Happy))
performance.logModel.initial_test <- performance(prediction.logModel.initial_test, 'tpr', 'fpr')
plot(performance.logModel.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.logModel.initial_test, 'auc')
AUC.logModel.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.logModel.initial_test  # 68.85%



#### linear discriminant analysis method on the two initial_train clusters

## building models on the initial_training set
library(MASS)
LDAmodel1 <- lda(Happy ~ . - YOB, data = initial_train1)
LDAmodel2 <- lda(Happy ~ . - YOB, data = initial_train2)

## on the initial_testing set
# prediction probabilities
predPerc.LDAmodel1.initial_test1 <- predict(LDAmodel1, newdata = initial_test1)$posterior[ , 2]
predPerc.LDAmodel2.initial_test2 <- predict(LDAmodel2, newdata = initial_test2)$posterior[ , 2]

# predictions
pred.LDAmodel1.initial_test1 <- as.integer(predPerc.LDAmodel1.initial_test1)
pred.LDAmodel2.initial_test2 <- as.integer(predPerc.LDAmodel2.initial_test2)
#pred.LDAmodel1.initial_test1 <- predict(LDAmodel1, newdata = initial_test1)$class
#pred.LDAmodel2.initial_test2 <- predict(LDAmodel2, newdata = initial_test2)$class

# accuracy 
table(pred.LDAmodel1.initial_test1, initial_test1$Happy)
table(pred.LDAmodel2.initial_test2, initial_test2$Happy)
(228 + 200) / (228 + 344 + 200 + 209)
# 43.63% on the initial_testing set

# AUC
prediction.LDAmodel.initial_test <- prediction(c(predPerc.LDAmodel1.initial_test1, predPerc.LDAmodel2.initial_test2),
                                       c(initial_test1$Happy, initial_test2$Happy))
performance.LDAmodel.initial_test <- performance(prediction.LDAmodel.initial_test, 'tpr', 'fpr')
plot(performance.LDAmodel.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.LDAmodel.initial_test, 'auc')
AUC.LDAmodel.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.LDAmodel.initial_test  # 69.01%



#### random forest method on the two initial_train clusters

## random forest only works if the dependent variable is a factor variable
initial_train1$Happy <- as.factor(initial_train1$Happy)
initial_train2$Happy <- as.factor(initial_train2$Happy)

initial_test1$Happy <- as.factor(initial_test1$Happy)
initial_test2$Happy <- as.factor(initial_test2$Happy)

## build models using the initial_training set
library(randomForest)
set.seed(123)
RFmodel1 <- randomForest(Happy ~ ., data = initial_train1, ntree = 200)
RFmodel2 <- randomForest(Happy ~ ., data = initial_train2, ntree = 200)

## on the initial_test set
# prediction probabilities
predPerc.RFmodel1.initial_test1 <- predict(RFmodel1, newdata = initial_test1, type = 'prob')[ , 2]
predPerc.RFmodel2.initial_test2 <- predict(RFmodel2, newdata = initial_test2, type = 'prob')[ , 2]

# predictions
pred.RFmodel1.initial_test1 <- as.integer(predPerc.RFmodel1.initial_test1 > 0.5)
pred.RFmodel2.initial_test2 <- as.integer(predPerc.RFmodel2.initial_test2 > 0.5)
#pred.RFmodel1.initial_test1 <- predict(RFmodel1, newdata = initial_test1)
#pred.RFmodel2.initial_test2 <- predict(RFmodel2, newdata = initial_test2)

# accuracy
table(pred.RFmodel1.initial_test1, initial_test1$Happy)
table(pred.RFmodel2.initial_test2, initial_test2$Happy)
(89 + 306 + 122 + 149) / 
  (89 + 38 + 139 + 306 + 122 + 60 + 78 + 149)
# 67.89% accuracy on the initial_testing set

# AUC
prediction.RFmodel.initial_test <- prediction(c(predPerc.RFmodel1.initial_test1, predPerc.RFmodel2.initial_test2),
                                      c(initial_test1$Happy, initial_test2$Happy))
performance.RFmodel.initial_test <- performance(prediction.RFmodel.initial_test, 'tpr', 'fpr')
plot(performance.RFmodel.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.RFmodel.initial_test, 'auc')
AUC.RFmodel.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.RFmodel.initial_test  # 72.06%



#### CART model with 10-fold cross validation on the two initial_train clusters
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

trControl <- trainControl(method = 'cv', number = 10)
tuneGrid <- expand.grid(.cp = (1:100) * 0.01)

set.seed(123)
train(Happy ~ ., 
      data = initial_train1, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)
# cp of 0.07 for cluster 1

set.seed(123)
train(Happy ~ ., 
      data = initial_train2, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)
# cp of 0.01 for cluster 2

## building models with initial_training set
CARTmodel1 <- rpart(Happy ~ ., data = initial_train1, method = 'class', cp = 0.07)
CARTmodel2 <- rpart(Happy ~ ., data = initial_train2, method = 'class', cp = 0.01)

prp(CARTmodel1)
prp(CARTmodel2)

## on the initial_test set
# prediction probabilities 
predPerc.CARTmodel1.initial_test1 <- predict(CARTmodel1, newdata = initial_test1)[ , 2]
predPerc.CARTmodel2.initial_test2 <- predict(CARTmodel2, newdata = initial_test2)[ , 2]

# predictions
pred.CARTmodel1.initial_test1 <- as.integer(predPerc.CARTmodel1.initial_test1 > 0.5)
pred.CARTmodel2.initial_test2 <- as.integer(predPerc.CARTmodel2.initial_test2 > 0.5)

# accuracy
table(pred.CARTmodel1.initial_test1, initial_test1$Happy)
table(pred.CARTmodel2.initial_test2, initial_test2$Happy)
(75 + 303 + 130 + 139) /
  (75 + 41 + 153 + 303 + 130 + 70 + 70 + 139)
# 65.95% accuracy on the initial_testing set

# AUC
prediction.CARTmodel.initial_test <- prediction(c(predPerc.CARTmodel1.initial_test1, predPerc.CARTmodel2.initial_test2),
                                        c(initial_test1$Happy, initial_test2$Happy))
performance.CARTmodel.initial_test <- performance(prediction.CARTmodel.initial_test, 'tpr', 'fpr')
plot(performance.CARTmodel.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.CARTmodel.initial_test, 'auc')
AUC.CARTmodel.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.CARTmodel.initial_test  #64.79%
