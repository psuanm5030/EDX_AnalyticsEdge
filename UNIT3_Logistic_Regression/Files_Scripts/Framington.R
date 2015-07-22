# Framington Heart Study
# The first step is to identify risk factors, or the independent variables, 
# that we will use in our model. Then, using data, we'll create a logistic 
# regression model to predict heart disease. Using more data, we'll validate 
# our model to make sure it performs well out of sample and on different 
# populations than the training set population.  Lastly, we'll discuss how 
# medical interventions can be defined using the model.

setwd("/users/miller/desktop/r_edx/UNIT3/Files_Scripts")

# predict the 10 year risk of CHD
# Randomly split our patients
framingham = read.csv("framingham.csv")
str(framingham)
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
# When you have more data like we do here,
# you can afford to put less data in the training set
# and more in the testing set.
# This will increase our confidence
# in the ability of the model to extend to new data
# since we have a larger test set, and still
# give us enough data in the training set
# to create our model.
# use logistic regression to predict whether or not a patient experienced CHD
# within 10 years of the first examination.

# Model using training
# We'll use a nice little trick here where we predict our dependent variable
# using all of the other variables in the data set as independent variables.
# Be careful USING THE PERIOD . with data sets that have identifying variables like a patient ID 
# or name since you wouldn't want to use these as independent variables.
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)
# All of the significant variables have positive coefficients,
# meaning that higher values in these variables contribute to a higher probability
# of 10-year coronary heart disease.

# Use model to make predictions on Test data
predictTest = predict(framinghamLog, type = "response", newdata = test)
# T Value of .5
table(test$TenYearCHD, predictTest > 0.5)
# With a threshold of 0.5, we predict an outcome of 1, the true column, very rarely.
# This means that our model rarely predicts a 10-year CHD risk above 50%.
# Whats the accuracy of the model??
# Sum of cases we got right (1069 TN and 11 TP) / total number of obs in Data
(1069+11)/(1069+6+187+11)
# Accuracy of model - 0.8483896
# Need to compare to baseline - total number of True Negative
(1069+6)/(1069+6+187+11)
# Our model barely beats the baseline in terms of accuracy
# Do we still have a valuable model by varying the threshold

# we'll evaluate the predictive power of the model on the test set.
#Compute the out-of-sample AUC
library(ROCR)
ROCRpred = prediction(predictTest,test$TenYearCHD)
as.numeric(performance(ROCRpred,"auc")@y.values)
# AUC Value on test set - 0.7421095
# So we have an AUC of about 74% on our test set,
# which means that the model can differentiate between low risk
# patients and high risk patients pretty well.










