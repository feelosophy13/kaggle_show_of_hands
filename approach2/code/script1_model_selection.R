#### split initial_train_test dataset into training and testing datasets
install.packages('caTools')
library(caTools)

set.seed(123)
split <- sample.split(initial_train_test$Happy, SplitRatio = 0.7)
initial_train <- initial_train_test[split==TRUE, ]
initial_test <- initial_train_test[split==FALSE, ]



#### create a function that finds important variables for the dependent variable
findSigVariables <- function(df, dep.var, threshold) {
  colnames <- names(df)  # save all column names
  sig.colnames <- c()
  p.values <- c()
  for (colname in colnames) {
    if (tolower(colname) != tolower(dep.var)) {  
      aov.formula <- as.formula(paste('Happy~', colname))
      aov <- aov(aov.formula, data = df)  # perform ANOVA
      p.value <- summary(aov)[[1]]$'Pr(>F)'[1]  # extract p-value from ANOVA
      if (p.value <= threshold) {
        print(paste('p-value (', colname, '): ', p.value, sep = ''))
        sig.colnames <- append(sig.colnames, colname)
        p.values <- append(p.values, p.value)
      }
    }
  }
  sigVariables <- as.data.frame(cbind(sig.colname = sig.colnames, p.value = p.values), 
                                stringsAsFactors = FALSE)  # create a return data frame
  sigVariables$p.value <- as.numeric(as.character(sigVariables$p.value))  # convert p.value column to numeric
  sigVariables <- sigVariables[order(sigVariables$p.value), ]  # order df
  return(sigVariables)
}



#### save significant variables and save on tabular charts
sigVarsT.initial_train <- findSigVariables(initial_train,
                                           dep.var = 'Happy',
                                           threshold = 0.05)
nrow(sigVarsT.initial_train)   # 62 "significant" variables in the initial_train dataset

sigVars <- sigVarsT.initial_train$sig.colname
sigVars 
# obvious UserID is NOT one of the significant variables (because it's a unique ID for each observation)

sigVars <- sigVars[-60]  # removing UserID


#### define a formula string function
createFormulaStr <- function(sigVariables) {
  formula <- 'Happy~'
  for (var.name in sigVariables) {
    formula <- paste(formula, var.name, '+', sep='')
  }
  formula <- substr(formula, 1, nchar(formula) - 1)
}

formulaStr <- createFormulaStr(sigVars)
formula <- as.formula(formulaStr)
formula



#### logistic regression model
logModel <- glm(formula, data = initial_train, family = binomial)
summary(logModel)

# on the testing dataset
predProb.logModel.initial_test <- predict(logModel, newdata = initial_test, type = 'response')
pred.logModel.initial_test <- as.integer(predProb.logModel.initial_test > 0.5)
table(pred.logModel.initial_test, initial_test$Happy)
(358 + 590) / (358 + 191 + 247 + 590)  # 68.4%

library(ROCR)
prediction.logModel.initial_test <- prediction(predProb.logModel.initial_test, initial_test$Happy)
performance.logModel.initial_test <- performance(prediction.logModel.initial_test, 'tpr', 'fpr')
plot(performance.logModel.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.logModel.initial_test, 'auc')
AUC.logModel.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.logModel.initial_test  # 72.85%



#### CART model with 10-fold cross validation
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

set.seed(123)
trainControl <- trainControl('cv', 10)
tuneGrid <- expand.grid(.cp = (1:100 * 0.01))
train(formula, data = initial_train, method = 'rpart', 
      trControl = trainControl, tuneGrid = tuneGrid)

CARTmodel <- rpart(formula, data = initial_train, cp = 0.01, method = 'class')
prp(CARTmodel)

# on the testing dataset
predProb.CARTmodel.initial_test <- predict(CARTmodel, newdata = initial_test)[ , 2]
pred.CARTmodel.initial_test <- as.integer(predProb.CARTmodel.initial_test > 0.5)
table(pred.CARTmodel.initial_test, initial_test$Happy)
(267 + 630) / (267 + 151 + 338 + 630)  # 64.72%

prediction.CARTmodel.initial_test <- prediction(predProb.CARTmodel.initial_test, initial_test$Happy)
performance.CARTmodel.initial_test <- performance(prediction.CARTmodel.initial_test, 'tpr', 'fpr')
plot(performance.CARTmodel.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.CARTmodel.initial_test, 'auc')
AUC.CARTmodel.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.CARTmodel.initial_test  # 64.41%



#### Random Forest method
library(randomForest)

initial_train$Happy <- factor(initial_train$Happy)
initial_test$Happy <- factor(initial_test$Happy)

set.seed(123)
RFmodel <- randomForest(formula, data = initial_train, ntrees = 200)

# on the testing dataset
predProb.RFmodel.initial_test <- predict(RFmodel, newdata = initial_test, type = 'prob')[ , 2]
pred.RFmodel.initial_test <- as.integer(predProb.RFmodel.initial_test > 0.5)
table(pred.RFmodel.initial_test, initial_test$Happy)
(326 + 618) / (326 + 163 + 279 + 618)  # 68.11%

prediction.RFmodel.initial_test <- prediction(predProb.RFmodel.initial_test, initial_test$Happy)
performance.RFmodel.initial_test <- performance(prediction.RFmodel.initial_test, 'tpr', 'fpr')
plot(performance.RFmodel.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.RFmodel.initial_test, 'auc')
AUC.RFmodel.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.RFmodel.initial_test  # 73.68%



#### LDA method
library(MASS)

LDAmodel <- lda(formula, data = initial_train)
plot(LDAmodel)

# on the testing set
predProb.LDAmodel.initial_test <- predict(LDAmodel, newdata = initial_test)$posterior[ , 2]
pred.LDAmodel.initial_test <- as.integer(predProb.LDAmodel.initial_test > 0.5)
table(pred.LDAmodel.initial_test, initial_test$Happy)
(349 + 595) / (349 + 186 + 256 + 595)  # 68.12%

prediction.LDAmodel.initial_test <- prediction(predProb.LDAmodel.initial_test, initial_test$Happy)
performance.LDAmodel.initial_test <- performance(prediction.LDAmodel.initial_test, 'tpr', 'fpr')
plot(performance.LDAmodel.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.LDAmodel.initial_test, 'auc')
AUC.LDAmodel.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.LDAmodel.initial_test  # 72.78%



#### averaging prediction probabilities: logistic regression + RF + LDA

# on the testing dataset
predProb.averaged.initial_test <- 
  (predProb.logModel.initial_test + 
     predProb.RFmodel.initial_test + 
     predProb.LDAmodel.initial_test) / 3
pred.averaged.initial_test <- as.integer(predProb.averaged.initial_test > 0.5)
table(pred.averaged.initial_test, initial_test$Happy)
(355 + 604) / (355 + 177 + 250 + 604)  # 69.19%

prediction.averaged.initial_test <- prediction(predProb.averaged.initial_test, initial_test$Happy)
performance.averaged.initial_test <- performance(prediction.averaged.initial_test, 'tpr', 'fpr')
plot(performance.averaged.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.averaged.initial_test, 'auc')
AUC.averaged.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.averaged.initial_test  # 73.45%



#### selecting a more frequent outcome from three models

head(pred.logModel.initial_test, 10)
head(pred.RFmodel.initial_test, 10)
head(pred.LDAmodel.initial_test, 10)

predProb.freq.initial_test <- (pred.logModel.initial_test + 
                                 pred.RFmodel.initial_test + 
                                 pred.LDAmodel.initial_test) / 3
pred.freq.initial_test <- round(predProb.freq.initial_test, 0)
table(pred.freq.initial_test, initial_test$Happy)
(355 + 594) / (355 + 187 + 250 + 594)  # 68.47%

prediction.freq.initial_test <- prediction(predProb.freq.initial_test, initial_test$Happy)
performance.freq.initial_test <- performance(prediction.freq.initial_test, 'tpr', 'fpr')
plot(performance.freq.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.freq.initial_test, 'auc')
AUC.freq.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.freq.initial_test  # 69.04% (pseudo-AUC value)
