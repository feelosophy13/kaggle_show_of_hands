#### converting 'Happy' column to an integer variable 
class(initial_train$Happy)
head(initial_train$Happy)
initial_train$Happy <- as.integer(as.character(initial_train$Happy))

class(initial_test$Happy)
head(initial_test$Happy)
initial_test$Happy <- as.integer(as.character(initial_test$Happy))



#### dummy coding variables (in preparation for clustering)
initial_train_test.dc <- initial_train_test
summary(initial_train_test.dc)  # binary answers to all questions
head(initial_train_test.dc)

# dummy coding all question and Gender columns (that have binary values)
for (i in c(3, 8:(ncol(initial_train_test)-2))) {  
  initial_train_test.dc[[i]] <- as.integer(initial_train_test.dc[[i]]) - 1
}

# renaming the Gender column
colnames(initial_train_test.dc)[3] <- 'Male'

# review initial_train_test.dc
head(initial_train_test)
head(initial_train_test.dc)
names(initial_train_test.dc)
dim(initial_train_test.dc)

# dummy coding: Income
dcIncome.df <- as.data.frame(model.matrix(~initial_train_test.dc$Income))[-1]
colnames(dcIncome.df) <- c('inc.25001_50000', 'inc.50000_74999',
                           'inc.75000_100000', 'inc.100001_150000',
                           'inc.over.150000')
head(dcIncome.df)
initial_train_test.dc <- cbind(initial_train_test.dc, dcIncome.df)

# dummy coding: HouseholdStatus
dcHouseholdStatus.df <- as.data.frame(model.matrix(~initial_train_test.dc$HouseholdStatus))[-1]
colnames(dcHouseholdStatus.df) <- c('Single.w.kids', 'DomParts.wo.kids', 
                                    'DomParts.w.kids', 'Married.wo.kids', 
                                    'Married.w.kids')
head(dcHouseholdStatus.df)
initial_train_test.dc <- cbind(initial_train_test.dc, dcHouseholdStatus.df)

# dummy coding: EducationLevel
dcEducation.df <- as.data.frame(model.matrix(~initial_train_test.dc$EducationLevel))[-1]
colnames(dcEducation.df) <- c('edu.HS_diploma', 'edu.associate', 
                              'edu.current_undergrad', 'edu.bachelor',
                              'edu.master', 'edu.doctoral')
head(dcEducation.df)
initial_train_test.dc <- cbind(initial_train_test.dc, dcEducation.df)

# dummy coding: Party
dcParty.df <- as.data.frame(model.matrix(~initial_train_test.dc$Party))[-1]
colnames(dcParty.df) <- c('party.independent', 'party.libertarian',
                          'party.other', 'party.republican')
head(dcParty.df)
initial_train_test.dc <- cbind(initial_train_test.dc, dcParty.df)

# removing redundant variables that have been dummy-coded
initial_train_test.dc$Income <- NULL
initial_train_test.dc$HouseholdStatus <- NULL
initial_train_test.dc$EducationLevel <- NULL
initial_train_test.dc$Party <- NULL

# review data
head(initial_train_test.dc)



#### split data to initial_train.dc and initial_test.dc

# splitting
library(caTools)
set.seed(123)
split <- sample.split(initial_train_test.dc$Happy, SplitRatio=0.75)
initial_train.dc <- initial_train_test.dc[split==TRUE, ]
initial_test.dc <- initial_train_test.dc[split==FALSE, ]

# ensuring that 'train' dataset is equivalent to 'train.dc' dataset
nrow(initial_train.dc) == nrow(initial_train)
initial_train.dc[1:5, 1:2] == initial_train[1:5, 1:2]

# ensuring that 'test' dataset is equivalent to 'test.dc' dataset
nrow(initial_test.dc) == nrow(initial_test)
initial_test.dc[1:5, 1:2] == initial_test[1:5, 1:2]



#### normalize data

# separatiung dependant and independent variables 
# (only independent variables must be normalized and joined back to the dependent variables)
initial_train.dc.dep <- subset(initial_train.dc, select = Happy)
initial_train.dc.indep <- subset(initial_train.dc, select = - Happy)

initial_test.dc.dep <- subset(initial_test.dc, select = Happy)
initial_test.dc.indep <- subset(initial_test.dc, select = - Happy)

# normalization
library(caret)
preproc <- preProcess(initial_train.dc.indep)
initial_train.dc.indep.norm <- predict(preproc, initial_train.dc.indep)
initial_test.dc.indep.norm <- predict(preproc, initial_test.dc.indep)




#### merging data frames
initial_train.dc.norm <- cbind(initial_train.dc.dep, initial_train.dc.indep.norm)
initial_test.dc.norm <- cbind(initial_test.dc.dep, initial_test.dc.indep.norm)




#### hierarchical clustering
# number of distance values to be stored
n <- nrow(initial_train.dc.indep.norm)
n * (n - 1) / 2

# create hierarchical clustering and dendrogram
d.initial_train.dc.indep.norm <- dist(initial_train.dc.indep.norm, method='euclidean')
hclust.initial_train <- hclust(d.initial_train.dc.indep.norm, method='ward')
plot(hclust.initial_train)  # two cluster groups seem ideal
rect.hclust(hclust.initial_train, k=2)
dev.copy(png, '../figures/hClust.png')
dev.off()


hclust.2Groups.initial_train <- cutree(hclust.initial_train, k = 2)
head(hclust.2Groups.initial_train)
table(hclust.2Groups.initial_train)
tapply(initial_train$Happy, hclust.2Groups.initial_train, mean)

# average happiness in initial_train
mean(initial_train$Happy)



#### hierarchical clustering: 2-groups 
library(flexclust)

hclust.2Groups.kcca <- as.kcca(hclust.initial_train, initial_train.dc.indep.norm, k = 2)
hclust.2Groups.initial_train.2 <- predict(hclust.2Groups.kcca)
hclust.2Groups.initial_test <- predict(hclust.2Groups.kcca, newdata = initial_test.dc.indep.norm)

table(hclust.2Groups.initial_train, hclust.2Groups.initial_train.2)
125 + 171
(125 + 171) / sum(table(hclust.2Groups.initial_train, hclust.2Groups.initial_train.2))  
# 10.04% discrepancy between cluster results obtained by cutree() and predict(as.kcca()) on the initial_training dataset

# Will resort to k-means clustering method