#### averaging models from no clustering
## averaging prediction probabilities to make predictions

## prediction percentages on initial_testing sets averaged across GLM, LDA, RF, and CART
predPerc.averaged.initial_test <- (predPerc.logModel.initial_test + predPerc.LDAmodel.initial_test + 
                                     predPerc.RFmodel.initial_test + predPerc.CARTmodel.initial_test) / 4

## predictions from averaged probabilities on initial_test sets
pred.averaged.initial_test <- as.integer(predPerc.averaged.initial_test > 0.5)


## accuracy on the initial_testing set
table(pred.averaged.initial_test, initial_test$Happy)
(221 + 441) / (221 + 112 + 207 + 441)
# 67.48% accuracy on the initial_testing set

## AUC on the initial_testing set
prediction.averaged.initial_test <- prediction(predPerc.averaged.initial_test, initial_test$Happy)
performance.averaged.initial_test <- performance(prediction.averaged.initial_test, 'tpr', 'fpr')
plot(performance.averaged.initial_test, colorize = TRUE)

AUC.tmp <- performance(prediction.averaged.initial_test, 'auc')
AUC.averaged.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.averaged.initial_test  # 72.24%

## selecing a more common prediction outcome to make predictions

## first 10 predictions on initial_test
head(pred.logModel.initial_test, 10)
head(pred.LDAmodel.initial_test, 10)
head(pred.RFmodel.initial_test, 10)
head(pred.CARTmodel.initial_test, 10)

## initial_testing set accuracies
# logModel: 66.46% 
# RFmodel: 67.58% (use outcome from RF in case there is a 50-50 vote)
# CARTmodel: 64.83%
# LDAmodel: 66.87%

## on the initial_testing set
predPerc.freq.initial_test <- (pred.logModel.initial_test + pred.LDAmodel.initial_test + 
                         pred.RFmodel.initial_test + pred.CARTmodel.initial_test) / 4
head(predPerc.freq.initial_test, 10)

RFvalues <- pred.RFmodel.initial_test[predPerc.freq.initial_test==0.5]
pred.freq.initial_test <- replace(predPerc.freq.initial_test, predPerc.freq.initial_test==0.5, RFvalues)
pred.freq.initial_test <- round(pred.freq.initial_test, 0)
head(pred.freq.initial_test, 10)

table(pred.freq.initial_test, initial_test$Happy)
(221 + 447) / (221 + 106 + 207 + 447)
# 68.09% accuracy on the initial_test data set

prediction.freq.initial_test <- prediction(predPerc.freq.initial_test, initial_test$Happy)
performance.freq.initial_test <- performance(prediction.freq.initial_test, 'tpr', 'fpr')
plot(performance.freq.initial_test, colorize=TRUE)

AUC.tmp <- performance(prediction.freq.initial_test, 'auc')
AUC.freq.initial_test <- as.numeric(AUC.tmp@y.values)
AUC.freq.initial_test  # 68.87%