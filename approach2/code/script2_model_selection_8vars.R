#### further select and examine important variables: logistic regression 
summary(logModel)



#### further select and examine important variables: regression tree
prp(CARTmodel)



#### further select and examine important variables: random forest
# most selected variables ordered
RFmodel.varsUsed <- varUsed(RFmodel, count=TRUE)
RFmodel.varsUsed.sorted <- sort(RFmodel.varsUsed, decreasing = FALSE, index.return = TRUE)
dotchart(RFmodel.varsUsed.sorted$x, 
         names(RFmodel$forest$xlevels[RFmodel.varsUsed.sorted$ix]),
         cex = 0.4)  # cex controls the size of label texts

# variables that decreased "impurities" (of outcome) the most ordered
varImpPlot(RFmodel, n.var = 30, 
           main = 'Importance of Variables',
           cex = 0.5)  # cex controls the size of label texts



#### save formula with 8 leading independent variables (from RF)
formula.8vs <- as.formula('Happy ~ EducationLevel + Income + Q118237 + Party + HouseholdStatus + Q101162 + Q107869 + Q119334')



#### rebuild logistic with 8 most important variables picked from RF

# on the testing set
logModel8vs <- glm(formula.8vs, data = initial_train, family = binomial)
predProb.logModel8vs.initial_test <- predict(logModel8vs, newdata = initial_test, type = 'response')
pred.logModel8vs.initial_test <- as.integer(predProb.logModel8vs.initial_test > 0.5)
table(pred.logModel8vs.initial_test, initial_test$Happy)
(309 + 613) / (309 + 168 + 296 + 613)  # 66.52%

prediction.logModel8vs.initial_test <- prediction(predProb.logModel8vs.initial_test, initial_test$Happy)
performance.logModel8vs.initial_test <- performance(prediction.logModel8vs.initial_test, 'tpr', 'fpr')
plot(performance.logModel8vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.logModel8vs.initial_test, 'auc')
AUC.logModel8vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.logModel8vs.initial_test  # 70.94%



#### rebuild regression tree model with 8 most important variables picked from RF
set.seed(123)
trainControl <- trainControl('cv', 10)
tuneGrid <- expand.grid(.cp = (1:100 * 0.01))
train(formula.8vs, data = initial_train, method = 'rpart', 
      trControl = trainControl, tuneGrid = tuneGrid)

CARTmodel8vs <- rpart(formula.8vs, data = initial_train, cp = 0.01, method = 'class')
prp(CARTmodel8vs)

# on the testing dataset
predProb.CARTmodel8vs.initial_test <- predict(CARTmodel8vs, newdata = initial_test)[ , 2]
pred.CARTmodel8vs.initial_test <- as.integer(predProb.CARTmodel8vs.initial_test > 0.5)
table(pred.CARTmodel8vs.initial_test, initial_test$Happy)
(222 + 680) / (222 + 101 + 383 + 680)  # 65.08%

prediction.CARTmodel8vs.initial_test <- prediction(predProb.CARTmodel8vs.initial_test, initial_test$Happy)
performance.CARTmodel8vs.initial_test <- performance(prediction.CARTmodel8vs.initial_test, 'tpr', 'fpr')
plot(performance.CARTmodel8vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.CARTmodel8vs.initial_test, 'auc')
AUC.CARTmodel8vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.CARTmodel8vs.initial_test  # 64.59%



#### rebuild random forest model with 8 most important variables picked from RF
set.seed(123)
RFmodel8vs <- randomForest(formula.8vs, data = initial_train, ntrees = 200)

# on the testing dataset
predProb.RFmodel8vs.initial_test <- predict(RFmodel8vs, newdata = initial_test, type = 'prob')[ , 2]
pred.RFmodel8vs.initial_test <- as.integer(predProb.RFmodel8vs.initial_test > 0.5)
table(pred.RFmodel8vs.initial_test, initial_test$Happy)
(318 + 595) / (318 + 186 + 287 + 595)  # 65.87%

prediction.RFmodel8vs.initial_test <- prediction(predProb.RFmodel8vs.initial_test, initial_test$Happy)
performance.RFmodel8vs.initial_test <- performance(prediction.RFmodel8vs.initial_test, 'tpr', 'fpr')
plot(performance.RFmodel8vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.RFmodel8vs.initial_test, 'auc')
AUC.RFmodel8vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.RFmodel8vs.initial_test  # 69.03%



#### rebuild LDA model with 8 most important variables picked from RF
LDAmodel8vs <- lda(formula.8vs, data = initial_train)
plot(LDAmodel8vs)

# on the testing set
predProb.LDAmodel8vs.initial_test <- predict(LDAmodel8vs, newdata = initial_test)$posterior[ , 2]
pred.LDAmodel8vs.initial_test <- as.integer(predProb.LDAmodel8vs.initial_test > 0.5)
table(pred.LDAmodel8vs.initial_test, initial_test$Happy)
(310 + 614) / (310 + 167 + 295 + 614)  # 66.67%

prediction.LDAmodel8vs.initial_test <- prediction(predProb.LDAmodel8vs.initial_test, initial_test$Happy)
performance.LDAmodel8vs.initial_test <- performance(prediction.LDAmodel8vs.initial_test, 'tpr', 'fpr')
plot(performance.LDAmodel8vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.LDAmodel8vs.initial_test, 'auc')
AUC.LDAmodel8vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.LDAmodel8vs.initial_test  # 70.94%



#### averaging prediction probabilities: logistic regression + RF + LDA

# on the testing dataset
predProb.averaged8vs.initial_test <- 
  (predProb.logModel8vs.initial_test + 
     predProb.RFmodel8vs.initial_test + 
     predProb.LDAmodel8vs.initial_test) / 3
pred.averaged8vs.initial_test <- as.integer(predProb.averaged8vs.initial_test > 0.5)
table(pred.averaged8vs.initial_test, initial_test$Happy)
(319 + 607) / (319 + 174 + 286 + 607)  # 66.81%

prediction.averaged8vs.initial_test <- prediction(predProb.averaged8vs.initial_test, initial_test$Happy)
performance.averaged8vs.initial_test <- performance(prediction.averaged8vs.initial_test, 'tpr', 'fpr')
plot(performance.averaged8vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.averaged8vs.initial_test, 'auc')
AUC.averaged8vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.averaged8vs.initial_test  # 70.88%



#### selecting a more frequent outcome from three models
head(pred.logModel8vs.initial_test, 10)
head(pred.RFmodel8vs.initial_test, 10)
head(pred.LDAmodel8vs.initial_test, 10)

predProb.freq8vs.initial_test <- (pred.logModel8vs.initial_test + 
                                 pred.RFmodel8vs.initial_test + 
                                 pred.LDAmodel8vs.initial_test) / 3
pred.freq8vs.initial_test <- round(predProb.freq8vs.initial_test, 0)
table(pred.freq8vs.initial_test, initial_test$Happy)
(310 + 613) / (310 + 168 + 295 + 613)  # 66.59%

prediction.freq8vs.initial_test <- prediction(predProb.freq8vs.initial_test, initial_test$Happy)
performance.freq8vs.initial_test <- performance(prediction.freq8vs.initial_test, 'tpr', 'fpr')
plot(performance.freq8vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.freq8vs.initial_test, 'auc')
AUC.freq8vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.freq8vs.initial_test  # 66.44% (pseudo-AUC value)
