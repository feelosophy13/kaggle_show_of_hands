#### setting up working directory
rm(list=ls())
getwd()
# setwd('C:/Users/Desk 1/Desktop/kaggle_show_of_hands/approach1')
setwd('/Users/hawooksong/Desktop/kaggle_show_of_hands/approach1')
dir()



#### 
initial_train_test.data.file <- 'data/raw/initial_train_test.csv'
final_test.data.file <- 'data/raw/final_test.csv'

missing.types <- c("NA", "")

initial_train_test.column.types <- c('integer',   # UserID
                                     'integer',   # YOB
                                     'factor',    # Gender
                                     'factor',    # Income
                                     'factor',    # HouseholdStatus
                                     'factor',    # EducationLevel
                                     'factor',    # Party 
                                     'integer',   # Happy
                                     rep('factor', 101),    # questions
                                     'integer')       # votes
final_test.column.types <- initial_train_test.column.types[-8]



#### loading data
?read.csv
initial_train_test <- read.csv(initial_train_test.data.file, 
                                        colClasses=initial_train_test.column.types,
                                        na.strings=missing.types)
final_test <- read.csv(final_test.data.file,
                       colClasses=final_test.column.types,
                       na.strings=missing.types)



#### brief examination of data
dim(initial_train_test); dim(final_test)
head(initial_train_test)
str(initial_train_test)
summary(initial_train_test)



#### missing values visualization
# install.packages('VIM')
# library(VIM)
# aggr(initial_train_test)

# credit to: https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
# install.packages('Amelia')
library(Amelia)
missmap(initial_train_test, 
        main="Show of Hands Initial Training/Testing Data \n Missing Data Map", 
        col=c("yellow", "black"), legend=FALSE)
dev.copy(png, '../figures/missingDataMap.png')
dev.off()



#### order factor levels in the training/testing dataset
initial_train_test$Income <- factor(initial_train_test$Income, 
                                             levels = c('under $25,000', 
                                                        '$25,001 - $50,000', 
                                                        '$50,000 - $74,999',
                                                        '$75,000 - $100,000',
                                                        '$100,001 - $150,000', 
                                                        'over $150,000'))
initial_train_test$HouseholdStatus <- factor(initial_train_test$HouseholdStatus,
                                                      levels = c('Single (no kids)',
                                                                 'Single (w/kids)',
                                                                 'Domestic Partners (no kids)',
                                                                 'Domestic Partners (w/kids)',
                                                                 'Married (no kids)', 
                                                                 'Married (w/kids)'))
initial_train_test$EducationLevel <- factor(initial_train_test$EducationLevel,
                                                     levels = c('Current K-12', 
                                                                'High School Diploma',
                                                                'Associate\'s Degree',
                                                                'Current Undergraduate',
                                                                'Bachelor\'s Degree',
                                                                'Master\'s Degree',
                                                                'Doctoral Degree'))



#### including "Age" column in the training/testing and final datasets
initial_train_test$Age <- 2014 - initial_train_test$YOB
final_test$Age <- 2014 - final_test$YOB

class(initial_train_test$Age)
class(final_test$Age)



#### removing UserID column in the initial_train_test dataset only 
#### (not in the final_test dataset because we need it for submission)
initial_train_test$UserID <- NULL



#### initial data summary visualization
library(ggplot2)
ggplot(initial_train_test) + 
  geom_bar(aes(x=factor(Happy)))
ggplot(initial_train_test) + 
  geom_histogram(aes(x=YOB), binwidth=1)
ggplot(initial_train_test) + 
  geom_histogram(aes(x=Age), binwidth=1)
ggplot(initial_train_test) +
  geom_bar(aes(x=Gender))
ggplot(initial_train_test) + 
  geom_bar(aes(x=Income)) + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(initial_train_test) + 
  geom_bar(aes(x=HouseholdStatus)) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(initial_train_test) + 
  geom_bar(aes(x=EducationLevel)) + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(initial_train_test) + 
  geom_bar(aes(x=Party)) + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(initial_train_test) + 
  geom_histogram(aes(x=votes),
                 binwidth=1)


#### data summary visualization by happiness
ggplot(initial_train_test) + 
  geom_histogram(aes(x=YOB, fill=factor(Happy)), 
                 position='fill',
                 binwidth=1)
ggplot(initial_train_test) + 
  geom_histogram(aes(x=Age, fill=factor(Happy)),
                 position='fill',
                 binwidth=1)
ggplot(initial_train_test) +
  geom_bar(aes(x=Gender, fill=factor(Happy)),
           position='fill')
ggplot(initial_train_test) + 
  geom_bar(aes(x=Income, fill=factor(Happy)),
           position='fill') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(initial_train_test) + 
  geom_bar(aes(x=HouseholdStatus, fill=factor(Happy)),
           position='fill') +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(initial_train_test) + 
  geom_bar(aes(x=EducationLevel, fill=factor(Happy)),
           position='fill') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggplot(initial_train_test) + 
  geom_bar(aes(x=Party, fill=factor(Happy)),
           position='fill') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))



#### closer examination of invalid entries
head(sort(initial_train_test$Age), 40)
tail(sort(initial_train_test$Age), 40)
ggplot(initial_train_test) + 
  geom_histogram(aes(x=Age), binwidth=1)

head(sort(final_test$Age), 40)
tail(sort(final_test$Age), 40)
ggplot(final_test) + 
  geom_histogram(aes(x=Age), binwidth=1)

initial_train_test.invalid <- subset(initial_train_test, Age < 13 | Age > 100)
final_test.invalid <- subset(final_test, Age < 13 | Age > 100)


initial_train_test.invalid$Happy  
# for all invalid entries, predict that a person is not happy with 75% probability



#### removing observations with unlikely YOB/Age in the initial_train_test and
#### final_test datasets

initial_train_test <- subset(initial_train_test, Age > 13 & Age < 100)
final_test <- subset(final_test, Age > 13 & Age < 100)



#### imputing data (patching missing data)
# install.packages('mice')
library(mice)

# for the initial_train_test dataset
set.seed(123)
initial_train_test.dep <- subset(initial_train_test, select=Happy)
initial_train_test.indep <- subset(initial_train_test, select=-Happy)
initial_train_test.indep.imputed <- complete(mice(initial_train_test.indep))

initial_train_test.imputed <- cbind(initial_train_test.dep, 
                                    initial_train_test.indep.imputed)
summary(initial.train_test.imputed)
head(initial_train_test.imputed)

# for the final_test dataset
set.seed(123)
final_test.imputed <- complete(mice(final_test))

summary(final_test.imputed)
head(final_test.imputed)



#### saving the imputed train_test dataset
write.csv(initial_train_test.imputed, 
          file='data/imputed/initial_train_test_imputed.csv',
          row.names = FALSE)
write.csv(final_test.imputed, 
          file='data/imputed/final_test_imputed.csv',
          row.names = FALSE)