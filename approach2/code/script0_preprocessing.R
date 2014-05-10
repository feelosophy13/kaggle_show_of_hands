#### setting up working directory
rm(list=ls())
getwd()
setwd('/Users/hawooksong/Desktop/kaggle_show_of_hands/approach2')
dir()



#### loading data
# credit to: https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
initial_train_test.data.file <- "data/initial_train_test.csv"
final_test.data.file <- "data/final_test.csv"
missing.types <- c("NA", "")
initial_train_test.column.types <- c('integer',   # UserId
                                     'integer',   # YOB (year of birth)
                                     'factor',    # Gender
                                     'factor',    # Income
                                     'factor',    # HouseholdStatus
                                     'factor',    # EducationLevel
                                     'factor',    # Party
                                     'integer',   # Happy
                                     rep('factor', 101), # Questions (w/ binary answers)
                                     'integer')   # votes
final_test.column.types <- initial_train_test.column.types[-8] 

?read.csv
initial_train_test <- read.csv(initial_train_test.data.file,
                               na.strings = missing.types,
                               colClasses = initial_train_test.column.types)
final_test <- read.csv(final_test.data.file,
                       na.strings = missing.types,
                       colClasses = final_test.column.types)



#### brief examination of data
dim(initial_train_test)
dim(final_test)

head(initial_train_test)
str(initial_train_test)



#### missing values visualization
# install.packages('VIM')
# library(VIM)
# aggr(train)

# credit to: https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
install.packages('Amelia')
library(Amelia)
missmap(initial_train_test, 
        main="Show of Hands Initial Training/Testing Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

# percentage of missing values
table(is.na(subset(initial_train_test, select = c(- Happy, - UserID, - votes))))
139414 / (354819 + 139414)  # 28.2% missing data



#### create Age column
initial_train_test$Age <- 2013 - initial_train_test$YOB  # this data is from 2013
final_test$Age <- 2013 - final_test$YOB  # this data is from 2013



#### convert unrealistic YOB and Age values into NA 
range(initial_train_test$Age, na.rm = TRUE)
range(final_test$Age, na.rm = TRUE)

library(ggplot2)
ggplot(initial_train_test, aes(x = Age)) + 
  geom_histogram(binwidth = 1)
ggplot(final_test, aes(x = Age)) + 
  geom_histogram(binwidth = 1)

# for initial_train_test set
head(sort(initial_train_test$Age), 20)
tail(sort(initial_train_test$Age), 20)
initial_train_test$Age[initial_train_test$Age > 100 | initial_train_test$Age < 10] <- NA
initial_train_test$YOB[initial_train_test$Age > 100 | initial_train_test$Age < 10] <- NA

# for final_test set
head(sort(final_test$Age), 20)
tail(sort(final_test$Age), 20)
final_test$Age[final_test$Age > 100 | final_test$Age < 10] <- NA
final_test$YOB[final_test$Age > 100 | final_test$Age < 10] <- NA



#### create AgeGroup column
quantile(initial_train_test$Age, na.rm = TRUE)
quantile(final_test$Age, na.rm = TRUE)

initial_train_test$AgeGroup <- cut(initial_train_test$Age,
                                   breaks=c(12, 21, 31, 44, 82),
                                   right=F,
                                   dig.lab=2)
final_test$AgeGroup <- cut(final_test$Age,
                           breaks=c(12, 21, 31, 44, 82),
                           right=F,
                           dig.lab=2)



#### remove YOB and Age columns
initial_train_test <- subset(initial_train_test, select = c(- YOB, - Age))
final_test <- subset(final_test, select = c(- YOB, - Age))



#### replacing missing values with 'Skipped'
# the code below does NOT work because 'Skipped' has not yet been introduced 
# as a possible factor value in the factor columns
initial_train_test[is.na(initial_train_test)] <- 'Skipped'  
final_test[is.na(final_test)] <- 'Skipped'

# the code below works
for (i in names(initial_train_test)) {
  levels(initial_train_test[ , i]) <- c(levels(initial_train_test[ , i]), 'Skipped')
}
initial_train_test[is.na(initial_train_test)] <- 'Skipped'  

for (i in names(final_test)) {
  levels(final_test[ , i]) <- c(levels(final_test[ , i]), 'Skipped')
}
final_test[is.na(final_test)] <- 'Skipped'


#### view filled-in data
head(initial_train_test)
head(final_test)
