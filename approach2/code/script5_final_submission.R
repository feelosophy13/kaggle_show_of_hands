#### setting up datasets for final model building
final_train <- initial_train_test

head(final_train)
head(final_test)



#### find significant variables by conduct ANOVA for each categorical variable
sigVarsT.final_train <- findSigVariables(final_train, dep.var = 'Happy', threshold = 0.05)
head(sigVarsT.final_train)
nrow(sigVarsT.final_train)

sigVars <- sigVarsT.final_train$sig.colname
sigVars  

formulaStr <- createFormulaStr(sigVars)
formula <- as.formula(formulaStr)
formula



#### logistic regression model
logModel <- glm(formula, data = final_train, family = binomial)
predProb.logModel.final_test <- predict(logModel, newdata = final_test, type = 'response')



#### regression tree model
set.seed(123)
trainControl <- trainControl('cv', 10)
tuneGrid <- expand.grid(.cp = (1:100 * 0.01))
train(formula, data = final_train, method = 'rpart', 
      trControl = trainControl, tuneGrid = tuneGrid)

CARTmodel <- rpart(formula, data = final_train, cp = 0.01, method = 'class')
predProb.CARTmodel.final_test <- predict(CARTmodel, newdata = final_test)[ , 2]



#### random forest model
final_train$Happy <- factor(final_train$Happy)

set.seed(123)
RFmodel <- randomForest(formula, data = final_train, ntrees = 200)
predProb.RFmodel.final_test <- predict(RFmodel, newdata = final_test, type = 'prob')[ , 2]



#### linear discriminant analysis model
LDAmodel <- lda(formula, data = final_train)
predProb.LDAmodel.final_test <- predict(LDAmodel, newdata = final_test)$posterior[ , 2]



#### GLM + RF + LDA prediction probabilities averaged
predProb.averaged.final_test <- 
  (predProb.logModel.final_test +
     predProb.RFmodel.final_test + 
     predProb.LDAmodel.final_test) / 3
head(predProb.averaged.final_test)



#### create a submission file
userID.final_test <- final_test$UserID

submission <- data.frame(userID = userID.final_test, 
                         Probability1 = predProb.averaged.final_test)
head(submission)
dim(submission) 
dim(final_test)



#### export/save the submission file
?write.csv
write.csv(submission, 
          file = 'submission_file/submission.csv',
          row.names = FALSE)