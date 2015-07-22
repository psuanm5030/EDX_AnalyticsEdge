# Assignment 2 - Test Scores
setwd("/users/miller/desktop/r_edx/UNIT2/Files_Scripts")

# Read in
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
# Each row in the datasets pisa2009train.csv and pisa2009test.csv represents one student taking the exam.

# average reading test score of males
tapply(pisaTrain$readingScore,pisaTrain$male,mean) # 483.5325 male / 512.9406 female

#Missing Data
summary(pisaTrain)
#Removing missing data
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# UNORDERED FACTORS IN REGRESSION MODELS
summary(pisaTrain$raceeth)
# R selects the first level alphabetically ("American Indian/Alaska Native") 
# as the reference level of our factor instead of the most 
# common level ("White"). Set the reference level of the factor
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

#Building a Model
# predict readingScore using all the other variables in the data frame.
lmScore = lm(readingScore ~ . , data = pisaTrain) # see the shorthand -> "~."
summary(lmScore)

# Calculate the RSME
SSE = sum(lmScore$residuals^2)
SSE
# root mean squared error - is going to be the square root of the 
# sum of squared errors divided by n, which is the number of rows 
# in our test data set.
RMSE = sqrt(SSE / nrow(pisaTrain))
RMSE #73.36555
# OR - SIMPLER WAY TO DO RMSE: 
sqrt(mean(lmScore$residuals^2))

# Student A (grade 11) vs. Student B (grade 9)
# The coefficient 29.54 on grade is the difference in reading score between 
# two students who are identical other than having a difference in grade of 1. 
# Because A and B have a difference in grade of 2, the model predicts that 
# student A has a reading score that is 2*29.54 larger.
# Diff = 2 (difference in grades) *29.54 (the coefficent on grade)

# What is the meaning of the coefficient associated with variable raceethAsian?
# Predicted difference in the reading score between an Asian student and a white student 
# who is otherwise identical

# Predicting based upon model and newdata
predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)

# Calculate the out-of-sample R-squared
SSE = sum((predTest - pisaTest$readingScore)^2)
SSE # 5762082
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST
R2 = 1 - (SSE / SST)
R2 #0.2614944
# root mean squared error - is going to be the square root of the 
# sum of squared errors divided by n, which is the number of rows 
# in our test data set.
RMSE = sqrt(SSE / nrow(pisaTest))
RMSE #76.29079 

#BASELINE PREDICTION AND TEST-SET SSE
baseline = mean(pisaTrain$readingScore) #517.9629
SST = sum((baseline - pisaTest$readingScore)^2)
SST #7802354
R2 = 1 - (SSE / SST)
R2 #0.2614944







