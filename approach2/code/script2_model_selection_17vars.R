#### selecting important variables from RF

# variables that decreased "impurities" (of outcome) the most ordered
varImpPlot(RFmodel, n.var = 25, 
           main = 'Importance of Variables',
           cex = 0.7)  # cex controls the size of label texts
dev.copy(png, '../figures/varImpPlot_25vars.png')
dev.off()


#### save formula with 8 leading independent variables (from RF)
formula.17vs <- as.formula('Happy ~ EducationLevel + Income + Q118237 + Party + 
                           HouseholdStatus + Q101162 + Q107869 + Q119334 + 
                           Q124742 + Q108855 + Q102289 + Q121011 + Q98869 + 
                           Q123621 + Q120014 + Q102906 + Q106997')



#### rebuild logistic with 17 most important variables picked from RF

# on the testing set
logModel17vs <- glm(formula.17vs, data = initial_train, family = binomial)
predProb.logModel17vs.initial_test <- predict(logModel17vs, newdata = initial_test, type = 'response')
pred.logModel17vs.initial_test <- as.integer(predProb.logModel17vs.initial_test > 0.5)
table(pred.logModel17vs.initial_test, initial_test$Happy)
(337 + 602) / (337 + 179 + 268 + 602)  # 67.75%

prediction.logModel17vs.initial_test <- prediction(predProb.logModel17vs.initial_test, initial_test$Happy)
performance.logModel17vs.initial_test <- performance(prediction.logModel17vs.initial_test, 'tpr', 'fpr')
plot(performance.logModel17vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.logModel17vs.initial_test, 'auc')
AUC.logModel17vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.logModel17vs.initial_test  # 73.01%



#### rebuild regression tree model with 17 most important variables picked from RF
set.seed(123)
trainControl <- trainControl('cv', 10)
tuneGrid <- expand.grid(.cp = (1:50 * 0.01))
train(formula.17vs, data = initial_train, method = 'rpart', 
      trControl = trainControl, tuneGrid = tuneGrid)

CARTmodel17vs <- rpart(formula.17vs, data = initial_train, cp = 0.01, method = 'class')
prp(CARTmodel17vs)

# on the testing dataset
predProb.CARTmodel17vs.initial_test <- predict(CARTmodel17vs, newdata = initial_test)[ , 2]
pred.CARTmodel17vs.initial_test <- as.integer(predProb.CARTmodel17vs.initial_test > 0.5)
table(pred.CARTmodel17vs.initial_test, initial_test$Happy)
(267 + 630) / (267 + 151 + 338 + 630)  # 64.72%

prediction.CARTmodel17vs.initial_test <- prediction(predProb.CARTmodel17vs.initial_test, initial_test$Happy)
performance.CARTmodel17vs.initial_test <- performance(prediction.CARTmodel17vs.initial_test, 'tpr', 'fpr')
plot(performance.CARTmodel17vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.CARTmodel17vs.initial_test, 'auc')
AUC.CARTmodel17vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.CARTmodel17vs.initial_test  # 64.41%



#### rebuild random forest model with 17 most important variables picked from RF
set.seed(123)
RFmodel17vs <- randomForest(formula.17vs, data = initial_train, ntrees = 200)

# on the testing dataset
predProb.RFmodel17vs.initial_test <- predict(RFmodel17vs, newdata = initial_test, type = 'prob')[ , 2]
pred.RFmodel17vs.initial_test <- as.integer(predProb.RFmodel17vs.initial_test > 0.5)
table(pred.RFmodel17vs.initial_test, initial_test$Happy)
(330 + 596) / (330 + 185 + 275 + 596)  # 66.81%

prediction.RFmodel17vs.initial_test <- prediction(predProb.RFmodel17vs.initial_test, initial_test$Happy)
performance.RFmodel17vs.initial_test <- performance(prediction.RFmodel17vs.initial_test, 'tpr', 'fpr')
plot(performance.RFmodel17vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.RFmodel17vs.initial_test, 'auc')
AUC.RFmodel17vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.RFmodel17vs.initial_test  # 70.31%



#### rebuild LDA model with 17 most important variables picked from RF
LDAmodel17vs <- lda(formula.17vs, data = initial_train)
plot(LDAmodel17vs)

# on the testing set
predProb.LDAmodel17vs.initial_test <- predict(LDAmodel17vs, newdata = initial_test)$posterior[ , 2]
pred.LDAmodel17vs.initial_test <- as.integer(predProb.LDAmodel17vs.initial_test > 0.5)
table(pred.LDAmodel17vs.initial_test, initial_test$Happy)
(336 + 610) / (336 + 171 + 269 + 610)  # 68.25%

prediction.LDAmodel17vs.initial_test <- prediction(predProb.LDAmodel17vs.initial_test, initial_test$Happy)
performance.LDAmodel17vs.initial_test <- performance(prediction.LDAmodel17vs.initial_test, 'tpr', 'fpr')
plot(performance.LDAmodel17vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.LDAmodel17vs.initial_test, 'auc')
AUC.LDAmodel17vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.LDAmodel17vs.initial_test  # 72.98%



#### averaging prediction probabilities: logistic regression + RF + LDA

# on the testing dataset
predProb.averaged17vs.initial_test <- 
  (predProb.logModel17vs.initial_test + 
     predProb.RFmodel17vs.initial_test + 
     predProb.LDAmodel17vs.initial_test) / 3
pred.averaged17vs.initial_test <- as.integer(predProb.averaged17vs.initial_test > 0.5)
table(pred.averaged17vs.initial_test, initial_test$Happy)
(337 + 601) / (337 + 180 + 268 + 601)  # 67.68%

prediction.averaged17vs.initial_test <- prediction(predProb.averaged17vs.initial_test, initial_test$Happy)
performance.averaged17vs.initial_test <- performance(prediction.averaged17vs.initial_test, 'tpr', 'fpr')
plot(performance.averaged17vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.averaged17vs.initial_test, 'auc')
AUC.averaged17vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.averaged17vs.initial_test  # 72.76%



#### selecting a more frequent outcome from three models
head(pred.logModel17vs.initial_test, 10)
head(pred.RFmodel17vs.initial_test, 10)
head(pred.LDAmodel17vs.initial_test, 10)

predProb.freq17vs.initial_test <- (pred.logModel.initial_test + 
                                     pred.RFmodel.initial_test + 
                                     pred.LDAmodel.initial_test) / 3
pred.freq17vs.initial_test <- round(predProb.freq17vs.initial_test, 0)
table(pred.freq17vs.initial_test, initial_test$Happy)
(355 + 594) / (355 + 187 + 250 + 594)  # 68.47%

prediction.freq17vs.initial_test <- prediction(predProb.freq17vs.initial_test, initial_test$Happy)
performance.freq17vs.initial_test <- performance(prediction.freq17vs.initial_test, 'tpr', 'fpr')
plot(performance.freq17vs.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.freq17vs.initial_test, 'auc')
AUC.freq17vs.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.freq17vs.initial_test  # 69.04% (pseudo-AUC value)
