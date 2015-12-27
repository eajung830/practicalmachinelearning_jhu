## course_project.r
## course project code for Practical Machine Learning (JHU), 12/6-1/3/2015 session, 
## Submitted by Eric Jung, eajung@gmail.com
## Date: 12/22/2015


# Project objective: The goal of this predict the "quality of activity" outcome from the data obtained from the
# "Human Activity Recognition" data set. Based on the quality of the person's movement doing specific weight lifting
# exercise , as indicated by various measurements of the user's specific movement (velocity, angle, etc.), the
# sample is given an outcome that indicates how well the movement was performed. The data set given includes
# a training and testing data set, and the goal of the project is to use the various measurements as features
# to determine a strong prediction algorithm for the testing data set. The prediciton output is then compared 
# to the actual output included in the testing data set.

# Code description
#   1. Data removal, including removing predictors with dominant NA/blank and near-zero-variance
#   2. Random Forest model fit 
#   3. Testing predicted outcome vs. actual outcome on the data set

# libraries
library(caret)
library(rpart)
library(randomForest)
source("./pml_write_files.R")

# read in training data set
training <- read.csv("./pml-training.csv")
testing <- read.csv("./pml-testing.csv")

# check for two types of predictors in training set for removal
#   1.  majority of values are missing (90%)
#   2.  any values are missing (either blank, div/0, or NA)

removelist = c(1,2,3,4,5) # list of column numbers to remove due to dominant NA/blank + some extras
incompletelist = numeric() # list of remaining columns with missing values for Imputation if necessary
for(i in 1 : dim(training)[2]){
  countNA = sum(is.na(training[,i]))
  countBlank = sum(na.omit(training[,i]) == "")
  countDiv0 = sum(na.omit(training[,i]) == "#DIV/0")
  if((countNA + countBlank + countDiv0)/dim(training)[1] > .9){
    removelist = c(removelist, i)
  }
  else if((countNA + countBlank + countDiv0) > 0){
    incompletelist = c(incompletelist,i)    
  }
}

nsvlist <-  nearZeroVar(training)
removelist <- unique(c(removelist,nsvlist))
                     
## remove data from remove list
training_postprocess = training[,-removelist]
testing_postprocess = testing[,-removelist]

## modelfit and prediction
modFit1 <- randomForest(classe ~ ., training_postprocess, ntree=100, norm.votes=FALSE, replace = FALSE)
modFit <- randomForest(classe ~ ., training_postprocess, ntree=100, norm.votes=FALSE)
pred_training <- predict(modFit, training_postprocess)
pred_testing <- predict(modFit, testing_postprocess)

confusionMatrix(pred_training,training$classe)

# pml_write_files(pred_testing)
