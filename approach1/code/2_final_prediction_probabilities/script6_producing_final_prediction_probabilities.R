#### setting up working directory
getwd()
setwd('C:/Users/Desk 1/Desktop/the_analytics_edge/kaggle_show_of_hands')
#setwd('/Users/hawooksong/Desktop/kaggle_show_of_hands')
dir()



#### 
final_train.data.file <- 'data/imputed/train_test_imputed.csv'
final_test.data.file <- 'data/imputed/final_test_imputed.csv'

missing.types <- c("NA", "")

final_train.column.types <- c('integer',   # X (unnecessary byproduct)
                              'integer',   # Happy
                              'integer',   # YOB
                              'factor',    # Gender
                              'factor',    # Income
                              'factor',    # HouseholdStatus
                              'factor',    # EducationLevel
                              'factor',    # Party 
                              rep('factor', 101),    # questions
                              'integer',   # votes
                              'integer')   # Age
final_test.column.types <- c('integer',   # X (unnecessary byproduct)
                             'integer',   # UserID
                             'integer',   # YOB
                             'factor',    # Gender
                             'factor',    # Income
                             'factor',    # HouseholdStatus
                             'factor',    # EducationLevel
                             'factor',    # Party 
                             'integer',   # Happy
                             rep('factor', 101),    # questions
                             'integer',   # votes
                             'integer')   # Age



#### loading imputed data
final_train <- read.csv(final_train.data.file,
                        na.strings=missing.types)
final_test <- read.csv(final_test.data.file,
                       na.strings=missing.types)



#### remove first column (unnecessary)
final_train$X <- NULL
final_test$X <- NULL



#### view training data
head(final_train)



#### convert Happy column to a factor (since we will use random forest method)
final_train$Happy <- factor(final_train$Happy)



#### build the model
library(randomForest)
RFmodel <- randomForest(Happy ~ . - YOB, data = final_train, ntrees = 200)



#### predictions and their probabilities on the final_train
predPerc.RFmodel.final_train <- predict(RFmodel, type='prob')[ , 2]
head(predPerc.RFmodel.final_train)

pred.RFmodel.final_train <- as.integer(predPerc.RFmodel.final_train > 0.5)
head(pred.RFmodel.final_train)

table(pred.RFmodel.final_train, final_train$Happy)
(860 + 1781) / (860 + 432 + 854 + 1781)  # 67.25%


#### predictions and their probabilities on the final_test for submission
predPerc.RFmodel.final_test <- predict(RFmodel, newdata = final_test, type='prob')[ , 2]
head(predPerc.RFmodel.final_test)

submit <- data.frame(UserID = final_test$UserID, 
                     Probability1 = predPerc.RFmodel.final_test)
head(submit)




#### attacheding prediction probabilities for those with invalid age entries
probs.invalid_age <- data.frame(UserID = subset(final_test.original, Age < 13 | Age > 100)$UserID,
                     Probability1 = 0.25)
head(probs.invalid_age)



#### joining two data frames for final submission
submit <- rbind(submit, probs.invalid_age)



#### exploring the final data frame
dir()
?write.csv
write.csv(submit, 
          file = 'submission/submission_output.csv',
          row.names = FALSE)

nrow(submit)
length(predPerc.RFmodel.final_test)
nrow(final_test)
nrow(final_test.original)