# Unit 4 - CART With Supreme Court

setwd("~/Desktop/R_EDX/UNIT4/Files_Scripts")

#Load it baby
stevens = read.csv("stevens.csv")
str(stevens)

# Split the dataset
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse,SplitRatio = 0.7)
train = subset(stevens,spl == TRUE)
test = subset(stevens,spl == FALSE)

# Building the CART Model
# Install some packages
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# Create Model using RPART function
# 1st Arg - method = "class". This tells rpart to build a classification tree, 
# instead of a regression tree.
# 2nd Arg = minbuck = 25 - This limits the tree so that it doesn't overfit to our training set.
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class", minbucket = 25)
# PLOT THE TREE
prp(StevensTree)
# The first split of our tree is whether or not
# the lower court decision is liberal.
# If it is, then we move to the left in the tree.
# And we check the respondent.
# If the respondent is a criminal defendant, injured person,
# politician, state, or the United States,
# we predict 0, or affirm.


# Making predictions for the test set
# Need to give type = "class" if we want the majority class predictions.
# This is like using a threshold of 0.5.  Can leave this arg out.
PredictCART = predict(StevensTree,newdata=test, type="class")
# Build confusion model to see accuracy
table(test$Reverse,PredictCART)
# Compute the accuracy
(41+71)/(41+36+22+71) #0.6588235
# Baseline prediction
table(test$Reverse) #Always predicts most commone outcome (REVERSE)
93/(93+77) # 0.5470588 accuracy

# Evaluate the model
# Generate an ROC Curve
library(ROCR)
# Generate our predictions again, without type = "class" arg
PredictROC = predict(StevensTree,newdata=test)
PredictROC # If you just look at output, you get two numbers for each obs. 
# Probablitiy of Outcome 0 and probability of Outcome 1.
# More concretely, each test set observation
# is classified into a subset, or bucket, of our CART tree.
# These numbers give the percentage of training
# set data in that subset with outcome 0
# and the percentage of data in the training set
# in that subset with outcome 1.
# We'll use the second column as our probabilities
# to generate an ROC curve.

# Prediction (ROC)
# 1st Arg - second column of PredictROC, 
# 2nd Arg - True Outcome Values
pred = prediction(PredictROC[,2], test$Reverse)
# Performance
# 1st Arg - Outcome of the prediction function
# 2nd & 3rd Args - true positive rate and false positive rate.
perf = performance(pred,"tpr","fpr")
plot(perf)
# What is the test set AUC for this model.
as.numeric(performance(pred, "auc")@y.values)

# NEW Model - with minbucket as 5
StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class", minbucket = 5)
# PLOT THE TREE
prp(StevensTree2)

# NEW Model - with minbucket as 100
StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class", minbucket = 100)
# PLOT THE TREE
prp(StevensTree3)


# RANDOM FORESTS - builds a large number of CART Trees
# Less interporable than CART.
# To make a prediction for a new observation, each tree
# in the forest votes on the outcome
# and we pick the outcome that receives
# the majority of the votes.
#
# Random forests only allows each tree to split on a random subset
# of the available independent variables,
# and each tree is built from what we
# call a bagged or bootstrapped sample of the data.
# This just means that the data used
# as the training data for each tree
# is selected randomly with replacement (pick regardless of whether 
# or not it's been selected already).
#
# So since each tree sees a different set of variables
# and a different set of data, we get
# what's called a forest of many different trees.

# Random Forest Variables
# nodesize - min num of obs in a subset (smaller nodesize, leads to bigger trees, might take longer)
# ntree - number of trees (couple hundred is plenty) - larger it is, longer it takes
# Not as sensitive to parameter values as CART is. 

# Create Random Forest
install.packages("randomForest")
library(randomForest)
# Question - no method arg for random forest.  
# In CART, we added the argument method="class",
# so that it was clear that we're doing a classification problem.
# As mentioned earlier, trees can also be used for regression problems, which
# you'll see in the recitation. The randomForest function does not have a method argument.
# So when we want to do a classification problem, we need to make sure outcome is a factor.
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
# Two additional arguments
# 1st Arg - nodesize (also known as minbucket for CART)
# 2nd Arg - ntree (number of trees to build)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
# Model ready for predictions
PredictForest = predict(StevensForest, newdata=test)
# Confusion Matrix - give true outcome and our predictions
table(test$Reverse,PredictForest)
# Accuracy
(40+74)/(40+37+19+74) # 0.6705882
# So the accuracy of our Random Forest model is about 67%.
# Recall that our logistic regression
# model had an accuracy of 66.5% and our CART model
# had an accuracy of 65.9%.
# So our random forest model improved our accuracy
# a little bit over CART.

# Quick Question
# Set seed to 100
set.seed(100)
StevensForest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
# Model ready for predictions
PredictForest2 = predict(StevensForest2, newdata=test)
# Confusion Matrix - give true outcome and our predictions
table(test$Reverse,PredictForest2)
# Accuracy
(43+74)/(40+37+19+74) # 0.6882353


# Set seed to 200
set.seed(200)
StevensForest3 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
# Model ready for predictions
PredictForest3 = predict(StevensForest3, newdata=test)
# Confusion Matrix - give true outcome and our predictions
table(test$Reverse,PredictForest3)
# Accuracy
(44+76)/(40+37+19+74) # 0.7058824
# As we see here, the random component of the random forest method can change the accuracy. 
# The accuracy for a more stable dataset will not change very much, but a noisy 
# dataset can be significantly affected by the random samples.

# Parameter Selection
# In CART, the value of minbucket can affect the model's out-of-sample accuracy.
# if minbucket is too small, over-fitting might occur.
# But if minbucket is too large, the model might be too simple.
# How to set this parameter?????  
# We could select the value that gives the best testing set accuracy, but this isn't right.
# The idea of the testing set is to measure model performance on data the model has never seen before.
# By picking the value of minbucket to get the best test set performance, the testing 
# set was implicitly used to generate the model. 
# Instead, we'll use a method called K-fold Cross Validation,
# which is one way to properly select the parameter value.
# This method works by going through the following steps.
# 1- Split the training set into k equally sized subsets, or folds.
# 2- Select k - 1, or four folds, to estimate the model,
# and compute predictions on the remaining one fold, which is often referred to as the validation set.
# We build a model and make predictions for each possible parameter value we're considering.
# Then we repeat this for each of the other folds, or pieces of our training set.
# So we would build a model using folds 1, 2, 3,
# and 5 to make predictions on fold 4,
# and then we would build a model using folds 1, 2, 4,
# and 5 to make predictions on fold 3, etc.
# So ultimately, cross validation builds
# many models, one for each fold and possible parameter value.
#
# Then, for each candidate parameter value, and for each fold, 
# we can compute the accuracy of the model.
# We then average the accuracy over the k folds to determine the 
# final parameter value that we want to use.

# If the parameter value is too small,
# then the accuracy is lower, because the model is probably over-fit to the training set.
# But if the parameter value is too large,
# then the accuracy is also lower, because the model is too simple.

# So far, we've used the parameter minbucket to limit our tree in R. 
# When we use cross validation in R,
# we'll use a parameter called cp instead.
# This is the Complexity Parameter.
# It's like Adjusted R-squared for linear regression,
# and AIC for logistic regression, in that it measures
# the trade-off between model complexity and accuracy
# on the training set.
# A smaller cp value leads to a bigger tree,
# so a smaller cp value might over-fit the model
# to the training set.
# But a cp value that's too large might
# build a model that's too simple.

# Cross Validation (CP value determination)
# Install package caret
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)

# Define Cross validation experiment
# Define number of folds
numFolds = trainControl(method="cv", number = 10) #10 folds
# Pick possible values for CV Value
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01)) #defines CP parems to test .01 to .5 in increments of .01
# Perform Cross Validation
# add the arguments: 
# method = "rpart", since we want to cross validate a CART model,
# trControl = numFolds, the output of our trainControl function, 
# tuneGrid = cpGrid, the output of the expand.grid function.
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train,method = "rpart",trControl = numFolds,tuneGrid=cpGrid)  #dep var with ind vars
# The first column gives the cp parameter that was tested,
# and the second column gives the cross validation accuracy for that cp value.
# The accuracy starts lower, and then increases,
# and then will start decreasing again, as we saw in the slides.
# At the bottom of the output, it says, "Accuracy was used to select the optimal 
# model using the largest value. The final value used for the model was cp = 0.18."
# This is the cp value we want to use in our CART model.

# Creat new CART with CP instead of minbucket value
# rpart function, like we did earlier, to predict Reverse using all of our independent variables:
# Method = class since we are building classification tree
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train,method="class",cp=.18)
PredictCV = predict(StevensTreeCV, newdata=test, type="class")
# Confusion Matrix
table(test$Reverse,PredictCV)
# Accuracy
(59+64)/(59+18+29+64) #0.7235294 - Accuracy of this model.  Vs. previous CART was .659
# Cross validation helps us make sure we're
# selecting a good parameter value,
# and often this will significantly
# increase the accuracy.
# If we had already happened to select a good parameter value,
# then the accuracy might not of increased that much.
# But by using cross validation, we
# can be sure that we're selecting a smart parameter value.

# Quick Question
# Plot the tree 
prp(StevensTreeCV) # Only one split
# The tree with the best accuracy only has one split! When we were picking 
# different minbucket parameters before, it seemed like this tree was probably 
# not doing a good job of fitting the data. However, this tree with one split 
# gives us the best out-of-sample accuracy. This reminds us that sometimes the 
# simplest models are the best!

