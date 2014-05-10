####
?kmeans
head(initial_train.dc.norm)



#### change the class of the dependent variable from factor to numeric
class(initial_train$Happy)
class(initial_test$Happy)

initial_train$Happy <- as.numeric(as.character(initial_train$Happy))
initial_test$Happy <- as.numeric(as.character(initial_test$Happy))



#### create 7 cluster groups through k-means method
library(flexclust)

set.seed(123)
km7 <- kmeans(initial_train.dc.indep.norm, centers = 7)
str(km7)

km7.kcca <- as.kcca(km7, initial_train.dc.indep.norm, k = 7)
kClust.7Groups.initial_train <- km7$cluster
kClust.7Groups.initial_test <- predict(km7.kcca, newdata = initial_test.dc.indep.norm)



#### briefly examine cluster characteristics
table(initial_train$Happy, kClust.7Groups.initial_train)
table(initial_test$Happy, kClust.7Groups.initial_test)

tapply(initial_train$Age, kClust.7Groups.initial_train, mean)
tapply(initial_test$Age, kClust.7Groups.initial_test, mean)

tapply(initial_train$votes, kClust.7Groups.initial_train, mean)
tapply(initial_test$votes, kClust.7Groups.initial_test, mean)

tapply(initial_train$Happy, kClust.7Groups.initial_train, mean)



#### split initial_train and test datasets into clusters
initial_train1 <- subset(initial_train, kClust.7Groups.initial_train==1)
initial_train2 <- subset(initial_train, kClust.7Groups.initial_train==2)
initial_train3 <- subset(initial_train, kClust.7Groups.initial_train==3)
initial_train4 <- subset(initial_train, kClust.7Groups.initial_train==4)
initial_train5 <- subset(initial_train, kClust.7Groups.initial_train==5)
initial_train6 <- subset(initial_train, kClust.7Groups.initial_train==6)
initial_train7 <- subset(initial_train, kClust.7Groups.initial_train==7)

initial_test1 <- subset(initial_test, kClust.7Groups.initial_test==1)
initial_test2 <- subset(initial_test, kClust.7Groups.initial_test==2)
initial_test3 <- subset(initial_test, kClust.7Groups.initial_test==3)
initial_test4 <- subset(initial_test, kClust.7Groups.initial_test==4)
initial_test5 <- subset(initial_test, kClust.7Groups.initial_test==5)
initial_test6 <- subset(initial_test, kClust.7Groups.initial_test==6)
initial_test7 <- subset(initial_test, kClust.7Groups.initial_test==7)



# #### remove columns with values that are constant within groups
# #### also remove non-existent factors by re-factoring 
# summary(initial_train1)
# summary(initial_train2)
# 
# summary(initial_train3)
# table(initial_train3$EducationLevel)
# table(initial_test3$EducationLevel)
# 
# summary(initial_train4)
# table(initial_train4$EducationLevel)
# table(initial_test4$EducationLevel)
# initial_train4$EducationLevel <- factor(initial_train4$EducationLevel)
# initial_test4$EducationLevel <- factor(initial_test4$EducationLevel)
# 
# summary(initial_train5)
# table(initial_train5$HouseholdStatus)
# table(initial_test5$HouseholdStatus)
# initial_train5$HouseholdStatus <- factor(initial_train5$HouseholdStatus)
# initial_test5$HouseholdStatus <- factor(initial_test5$HouseholdStatus)
# initial_train5 <- subset(initial_train5, select = - EducationLevel)
# initial_test5 <- subset(initial_test5, select = - EducationLevel)
# 
# summary(initial_train6)
# table(initial_train6$EducationLevel)
# table(initial_test6$EducationLevel)
# 
# summary(initial_test7)
# table(initial_train7$EducationLevel)
# table(initial_test7$EducationLevel)
# initial_train7$EducationLevel <- factor(initial_train7$EducationLevel)
# initial_test7$EducationLevel <- factor(initial_test7$EducationLevel)



#### logistic regression model
## building models using the logistic regression method
logModel1 <- glm(Happy ~ . - YOB, data = initial_train1, family = binomial)
logModel2 <- glm(Happy ~ . - YOB, data = initial_train2, family = binomial)
logModel3 <- glm(Happy ~ . - YOB, data = initial_train3, family = binomial)
logModel4 <- glm(Happy ~ . - YOB, data = initial_train4, family = binomial)
logModel5 <- glm(Happy ~ . - YOB, data = initial_train5, family = binomial)  # come back later
logModel6 <- glm(Happy ~ . - YOB, data = initial_train6, family = binomial)
logModel7 <- glm(Happy ~ . - YOB, data = initial_train7, family = binomial)

## create logModel5
summary(initial_train5)
logModel5 <- glm(Happy ~ . - YOB - EducationLevel, 
                 data = initial_train5, family = binomial)  # not sure why this doesn't work
logModel5 <- glm(Happy ~ . - YOB, data = initial_train5[-6], family = binomial)

## on test dataset
# prediction probabilities
predPerc.logModel1.initial_test1 <- predict(logModel1, newdata = initial_test1, type='response')
predPerc.logModel2.initial_test2 <- predict(logModel2, newdata = initial_test2, type='response')
predPerc.logModel3.initial_test3 <- predict(logModel3, newdata = initial_test3, type='response')  # come back later
predPerc.logModel4.initial_test4 <- predict(logModel4, newdata = initial_test4, type='response')
predPerc.logModel5.initial_test5 <- predict(logModel5, newdata = initial_test5, type='response')
predPerc.logModel6.initial_test6 <- predict(logModel6, newdata = initial_test6, type='response')
predPerc.logModel7.initial_test7 <- predict(logModel7, newdata = initial_test7, type='response')

# create predPerc.logModel3.initial_test3 with new logModel3
table(initial_train3$EducationLevel)
table(initial_test3$EducationLevel)
logModel3 <- glm(Happy ~ . - YOB - EducationLevel, data = initial_train, family = binomial)
predPerc.logModel3.initial_test3 <- predict(logModel3, newdata = initial_test3, type='response')

# prediction outcomes
pred.logModel1.initial_test1 <- as.integer(predPerc.logModel1.initial_test1 > 0.5)
pred.logModel2.initial_test2 <- as.integer(predPerc.logModel2.initial_test2 > 0.5)
pred.logModel3.initial_test3 <- as.integer(predPerc.logModel3.initial_test3 > 0.5)
pred.logModel4.initial_test4 <- as.integer(predPerc.logModel4.initial_test4 > 0.5)
pred.logModel5.initial_test5 <- as.integer(predPerc.logModel5.initial_test5 > 0.5)
pred.logModel6.initial_test6 <- as.integer(predPerc.logModel6.initial_test6 > 0.5)
pred.logModel7.initial_test7 <- as.integer(predPerc.logModel7.initial_test7 > 0.5)

table(pred.logModel1.initial_test1, initial_test1$Happy)
table(pred.logModel2.initial_test2, initial_test2$Happy)
table(pred.logModel3.initial_test3, initial_test3$Happy)
table(pred.logModel4.initial_test4, initial_test4$Happy)
table(pred.logModel5.initial_test5, initial_test5$Happy)
table(pred.logModel6.initial_test6, initial_test6$Happy)
table(pred.logModel7.initial_test7, initial_test7$Happy)

(28 + 61 + 38 + 63 + 10 + 106 + 58 + 60 + 8 + 14 + 40 + 39 + 37 + 35) /
  nrow(initial_test)
# 60.85% accuracy on the testing set



#### random forest method on the three initial_train clusters

## random forest only works if the dependent variable is a factor variable
initial_train1$Happy <- as.factor(initial_train1$Happy)
initial_train2$Happy <- as.factor(initial_train2$Happy)
initial_train3$Happy <- as.factor(initial_train3$Happy)
initial_train4$Happy <- as.factor(initial_train4$Happy)
initial_train5$Happy <- as.factor(initial_train5$Happy)
initial_train6$Happy <- as.factor(initial_train6$Happy)
initial_train7$Happy <- as.factor(initial_train7$Happy)

initial_test1$Happy <- as.factor(initial_test1$Happy)
initial_test2$Happy <- as.factor(initial_test2$Happy)
initial_test3$Happy <- as.factor(initial_test3$Happy)
initial_test4$Happy <- as.factor(initial_test4$Happy)
initial_test5$Happy <- as.factor(initial_test5$Happy)
initial_test6$Happy <- as.factor(initial_test6$Happy)
initial_test7$Happy <- as.factor(initial_test7$Happy)

## build models using the initial_training set
library(randomForest)
set.seed(123)
RFmodel1 <- randomForest(Happy ~ . - YOB, data = initial_train1, ntree = 200)
RFmodel2 <- randomForest(Happy ~ . - YOB, data = initial_train2, ntree = 200)
RFmodel3 <- randomForest(Happy ~ . - YOB, data = initial_train3, ntree = 200)
RFmodel4 <- randomForest(Happy ~ . - YOB, data = initial_train4, ntree = 200)
RFmodel5 <- randomForest(Happy ~ . - YOB, data = initial_train5, ntree = 200)
RFmodel6 <- randomForest(Happy ~ . - YOB, data = initial_train6, ntree = 200)
RFmodel7 <- randomForest(Happy ~ . - YOB, data = initial_train7, ntree = 200)

## test on the test set
pred.RFmodel1.initial_test1 <- predict(RFmodel1, newdata = initial_test1)
pred.RFmodel2.initial_test2 <- predict(RFmodel2, newdata = initial_test2)
pred.RFmodel3.initial_test3 <- predict(RFmodel3, newdata = initial_test3)
pred.RFmodel4.initial_test4 <- predict(RFmodel4, newdata = initial_test4)
pred.RFmodel5.initial_test5 <- predict(RFmodel5, newdata = initial_test5)
pred.RFmodel6.initial_test6 <- predict(RFmodel6, newdata = initial_test6)
pred.RFmodel7.initial_test7 <- predict(RFmodel7, newdata = initial_test7)

table(pred.RFmodel1.initial_test1, initial_test1$Happy)
table(pred.RFmodel2.initial_test2, initial_test2$Happy)
table(pred.RFmodel3.initial_test3, initial_test3$Happy)
table(pred.RFmodel4.initial_test4, initial_test4$Happy)
table(pred.RFmodel5.initial_test5, initial_test5$Happy)
table(pred.RFmodel6.initial_test6, initial_test6$Happy)
table(pred.RFmodel7.initial_test7, initial_test7$Happy)

(12 + 82 + 39 + 74 + 0 + 112 + 74 + 46 + 2 + 24 + 43 + 40 + 50 + 28) /
  nrow(initial_test)
# 63.81% accuracy on the testing set



#### CART model on the three initial_train clusters
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
# cp of 0.04 for cluster 1

set.seed(123)
train(Happy ~ ., 
      data = initial_train2, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)
# cp of 0.03 for cluster 2

set.seed(123)
train(Happy ~ ., 
      data = initial_train3, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)
# cp of 1 for cluster 3

set.seed(123)
train(Happy ~ ., 
      data = initial_train4, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)
# cp of 0.02 for cluster 4

set.seed(123)
train(Happy ~ ., 
      data = initial_train5, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)
# cp of 0.06 for cluster 5

set.seed(123)
train(Happy ~ ., 
      data = initial_train6, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)
# cp of 0.04 for cluster 6

set.seed(123)
train(Happy ~ ., 
      data = initial_train7, 
      method = 'rpart', 
      trControl = trControl,
      tuneGrid = tuneGrid)
# cp of 0.03 for cluster 7


## building models with initial_training set
CARTmodel1 <- rpart(Happy ~ . -YOB, data = initial_train1, method = 'class', cp = 0.04)
CARTmodel2 <- rpart(Happy ~ . -YOB, data = initial_train2, method = 'class', cp = 0.03)
CARTmodel3 <- rpart(Happy ~ . -YOB, data = initial_train3, method = 'class', cp = 1)
CARTmodel4 <- rpart(Happy ~ . -YOB, data = initial_train4, method = 'class', cp = 0.02)
CARTmodel5 <- rpart(Happy ~ . -YOB, data = initial_train5, method = 'class', cp = 0.06)
CARTmodel6 <- rpart(Happy ~ . -YOB, data = initial_train6, method = 'class', cp = 0.04)
CARTmodel7 <- rpart(Happy ~ . -YOB, data = initial_train7, method = 'class', cp = 0.03)

prp(CARTmodel1)
prp(CARTmodel2)
prp(CARTmodel3)
prp(CARTmodel4)
prp(CARTmodel5)
prp(CARTmodel6)
prp(CARTmodel7)

## testing on the test set
predPerc.CARTmodel1.initial_test1 <- predict(CARTmodel1, newdata = initial_test1)[ , 2]
predPerc.CARTmodel2.initial_test2 <- predict(CARTmodel2, newdata = initial_test2)[ , 2]
predPerc.CARTmodel3.initial_test3 <- predict(CARTmodel3, newdata = initial_test3)[ , 2]
predPerc.CARTmodel4.initial_test4 <- predict(CARTmodel4, newdata = initial_test4)[ , 2]
predPerc.CARTmodel5.initial_test5 <- predict(CARTmodel5, newdata = initial_test5)[ , 2]
predPerc.CARTmodel6.initial_test6 <- predict(CARTmodel6, newdata = initial_test6)[ , 2]
predPerc.CARTmodel7.initial_test7 <- predict(CARTmodel7, newdata = initial_test7)[ , 2]

pred.CARTmodel1.initial_test1 <- as.integer(predPerc.CARTmodel1.initial_test1 > 0.5)
pred.CARTmodel2.initial_test2 <- as.integer(predPerc.CARTmodel2.initial_test2 > 0.5)
pred.CARTmodel3.initial_test3 <- as.integer(predPerc.CARTmodel3.initial_test3 > 0.5)
pred.CARTmodel4.initial_test4 <- as.integer(predPerc.CARTmodel4.initial_test4 > 0.5)
pred.CARTmodel5.initial_test5 <- as.integer(predPerc.CARTmodel5.initial_test5 > 0.5)
pred.CARTmodel6.initial_test6 <- as.integer(predPerc.CARTmodel6.initial_test6 > 0.5)
pred.CARTmodel7.initial_test7 <- as.integer(predPerc.CARTmodel7.initial_test7 > 0.5)

table(pred.CARTmodel1.initial_test1, initial_test1$Happy)
table(pred.CARTmodel2.initial_test2, initial_test2$Happy)
table(pred.CARTmodel3.initial_test3, initial_test3$Happy)
table(pred.CARTmodel4.initial_test4, initial_test4$Happy)
table(pred.CARTmodel5.initial_test5, initial_test5$Happy)
table(pred.CARTmodel6.initial_test6, initial_test6$Happy)
table(pred.CARTmodel7.initial_test7, initial_test7$Happy)

(18 + 74 + 37 + 77 + 112 + 64 + 57 + 4 + 23 + 31 + 41 + 52 + 34) /
  nrow(initial_test)
# 63.61% accuracy on the testing set




#### linear discriminant analysis method on the three initial_train clusters

## building models on the initial_training set
library(MASS)

# LDA model 1 and 2
LDAmodel1 <- lda(Happy ~ . - YOB, data = initial_train1)
LDAmodel2 <- lda(Happy ~ . - YOB, data = initial_train2)

# LDA model 3
LDAmodel3 <- lda(Happy ~ . - YOB, data = initial_train3)
LDAmodel3 <- lda(Happy ~ . - YOB - EducationLevel, data = initial_train3)

# LDA model 4
LDAmodel4 <- lda(Happy ~ . - YOB, data = initial_train4)
LDAmodel4 <- lda(Happy ~ . - YOB - EducationLevel, data = initial_train4)

# LDA model 5 
LDAmodel5 <- lda(Happy ~ . - YOB, data = initial_train5)
summary(initial_train5)

table(initial_train5$HouseholdStatus)
table(initial_test5$HouseholdStatus)

initial_train5$HouseholdStatus <- factor(initial_train5$HouseholdStatus)
initial_test5$HouseholdStatus <- factor(initial_test5$HouseholdStatus)

initial_train5 <- subset(initial_train5, select = - EducationLevel)
initial_test5 <- subset(initial_test5, select = - EducationLevel)

LDAmodel5 <- lda(Happy ~ . - YOB, data = initial_train5)

# LDA model 6
LDAmodel6 <- lda(Happy ~ . - YOB, data = initial_train6)
LDAmodel6 <- lda(Happy ~ . - YOB - EducationLevel, data = initial_train6)

# LDA model 7
LDAmodel7 <- lda(Happy ~ . - YOB, data = initial_train7)
LDAmodel7 <- lda(Happy ~ . - YOB - EducationLevel, data = initial_train7)

## on the testing set
# prediction probabilities
predPerc.LDAmodel1.initial_test1 <- predict(LDAmodel1, newdata = initial_test1)$posterior[ , 2]
predPerc.LDAmodel2.initial_test2 <- predict(LDAmodel2, newdata = initial_test2)$posterior[ , 2]
predPerc.LDAmodel3.initial_test3 <- predict(LDAmodel3, newdata = initial_test3)$posterior[ , 2]
predPerc.LDAmodel4.initial_test4 <- predict(LDAmodel4, newdata = initial_test4)$posterior[ , 2]
predPerc.LDAmodel5.initial_test5 <- predict(LDAmodel5, newdata = initial_test5)$posterior[ , 2]
predPerc.LDAmodel6.initial_test6 <- predict(LDAmodel6, newdata = initial_test6)$posterior[ , 2]
predPerc.LDAmodel7.initial_test7 <- predict(LDAmodel7, newdata = initial_test7)$posterior[ , 2]

# predictions
pred.LDAmodel1.initial_test1 <- as.integer(predPerc.LDAmodel1.initial_test1)
pred.LDAmodel2.initial_test2 <- as.integer(predPerc.LDAmodel2.initial_test2)
pred.LDAmodel3.initial_test3 <- as.integer(predPerc.LDAmodel3.initial_test3)
pred.LDAmodel4.initial_test4 <- as.integer(predPerc.LDAmodel4.initial_test4)
pred.LDAmodel5.initial_test5 <- as.integer(predPerc.LDAmodel5.initial_test5)
pred.LDAmodel6.initial_test6 <- as.integer(predPerc.LDAmodel6.initial_test6)
pred.LDAmodel7.initial_test7 <- as.integer(predPerc.LDAmodel7.initial_test7)

# accuracy 
table(pred.LDAmodel1.initial_test1, initial_test1$Happy)
table(pred.LDAmodel2.initial_test2, initial_test2$Happy)
table(pred.LDAmodel3.initial_test3, initial_test3$Happy)
table(pred.LDAmodel4.initial_test4, initial_test4$Happy)
table(pred.LDAmodel5.initial_test5, initial_test5$Happy)
table(pred.LDAmodel6.initial_test6, initial_test6$Happy)
table(pred.LDAmodel7.initial_test7, initial_test7$Happy)

(54 + 70 + 49 + 93 + 10 + 71 + 78) / 
  nrow(initial_test)
# 43.32% on the testing set
