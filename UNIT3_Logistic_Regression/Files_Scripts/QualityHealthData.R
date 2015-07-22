# Lecture 3 - Quality Video and data

setwd("/users/miller/desktop/r_edx/UNIT3/Files_Scripts")

quality = read.csv("quality.csv")
str(quality)

#Take a look at the quality of care (1 = poor care)
table(quality$PoorCare)

# In a classification problem, a standard baseline method
# is to just predict the most frequent outcome
# for all observations.
# If we did this, we would take 98 / 131 = ~75% - this is the baseline model
# we will try to beat this with the logistic regression

# Split the dataset into training and test set
# Add new package
install.packages("caTools")
# Load the package into our current R session
library(caTools)
#IN THE FUTURE, DONT NEED TO INSTALL, BUT JUST LOAD.

# Randomly split data into training and testing data set
set.seed(88)
# Split the data.  2nd argument tells how much to go to training set
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
# This function makes sure that in our training and testing set, they are 
# comprised of observations that have 75% good care (what we saw originally 
# using the table function above). Ensures test is representative
split
# Viewing this data, we see that True shoudl go in TRAIN and FALSE should 
# go in TEST
qualityTrain = subset(quality,split == TRUE)
qualityTest = subset(quality,split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

# Build the logistic regression model
# "generalized linear model" function
# Family argument - This tells the glm function to build a logistic regression
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)
# Focus on coefficients (betas) for the log reg model
# both coefficients (estimates) are positive - 
# higher values are indicatvie of poor care
# * means significant in the model
# AIC value - measures quality of model (accounts for number of vars used)
# IMPORTANT: Unfortunately, it can only be compared
# between models on the same data set.  BUT provides means for model selection.
# The preferred model is the one with the MINIMUM AIC.

# Predictions on Training set
# Type argument tells the function to give probabilities.
predictTrain = predict(QualityLog, type="response")
summary(predictTrain) # numbers between 0 and 1 - bc we have probabilities
# Let's see if we're predicting higher probabilities
# for the actual poor care cases as we expect.
# This will compute the average prediction
# for each of the true outcomes.
tapply(predictTrain, qualityTrain$PoorCare,mean)
# We see that we are predicting a higher prob for the actual poor care cases.

# IF YOU WANTED TO INSTEAD SPLIT A DATA FRAME DATA, WHERE THE DEPENDENT 
# VARIABLE IS CONTINUOUS - YOU CAN USE SAMPLE() FUNCTION:
# Here is how to select 70% of observations for the training set 
# (called "train") and 30% of observations for the testing 
# set (called "test"):
  
spl = sample(1:nrow(data), size=0.7 * nrow(data))
train = data[spl,]
test = data[-spl,]

# NEW MODEL - QUICK QUESTION
QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog2)
# Does the StartedOnCombination (binary) predict Poor Care or Good Care??
# The coefficient value is positive, meaning that positive values of the variable 
# make the outcome of 1 more likely. This corresponds to Poor Care.


# Outcome of a logistic regression model is a probability.
# We can convert the probabilities to predictions
# using what's called a threshold value, t.
# Threshold is often selected based on which errors are "better".
# Two types of error - we predict 1 poor care, but actual outcome is 0 good care.
# and vice versa.
# If there's no preference between the errors,
# the right threshold to select is t = 0.5,
# since it just predicts the most likely outcome.
# Selecting a T value - confusion matrix (classification matrix).
# Two outcome measures: sensitivity or specificity
# Sensitivity is equal to the true positives
# divided by the true positives plus the false negatives,
# and measures the percentage of actual poor care cases
# that we classify correctly.
# This is often called the true positive rate.
# Specificity is equal to the true negatives
# divided by the true negatives plus the false positives,
# and measures the percentage of actual good care cases
# that we classify correctly.
# This is often called the true negative rate.
# A model with a higher threshold will have a lower sensitivity
# and a higher specificity.
# A model with a lower threshold will have a higher sensitivity
# and a lower specificity.


#Let's compute some CONFUSION MATRICES using diff T values
# T = 0.5
# 1st Arg is rows (should be true outcome)
# 2nd Arg is columns with T value
# This will return TRUE if our prediction is greater than 0.5,
# which means we want to predict poor care,
# and it will return FALSE if our prediction is less than 0.5,
# which means we want to predict good care.
table(qualityTrain$PoorCare, predictTrain>0.5)
# So you can see here that for 70 cases, we predict good care
# and they actually received good care, and for 10 cases,
# we predict poor care, and they actually received poor care.
# We make four mistakes where we say poor care
# and it's actually good care, and we make 15 mistakes where
# we say good care, but it's actually poor care.

# Compute the sensitivity - true positive rate
# The sensitivity here would be 10, our true positives,
# divided by 25 the total number of positive cases.
10/25
# Compute the specificity - true negative rate
# Our specificity here would be 70, the true negative cases,
# divided by 74, the total number of negative cases.
70/74

table(qualityTrain$PoorCare, predictTrain>0.7)
# sensitivity
8/25
# specificity
73/74
# So, by increasing the T value, our sensitivity went down and spec. went up.

table(qualityTrain$PoorCare, predictTrain>0.2)
# sensitivity
16/25 # .64
# specificity
54/74 # .73
# So with the lower threshold, our sensitivity went up,
# and our specificity went down.

# Deciding the T value - ROC Curve - Receiver Operatior Characteristic Curve
# The ROC curve captures all thresholds simultaneously.
# The higher the threshold, or closer to (0, 0),
# the higher the specificity and the lower the sensitivity.
# The lower the threshold, or closer to (1,1),
# the higher the sensitivity and lower the specificity.
# WHAT TO CHOOSE??
# If you're more concerned with having a high specificity
# or low false positive rate, pick the threshold
# that maximizes the true positive rate
# while keeping the false positive rate really low.
# OR
# On the other hand, if you're more concerned
# with having a high sensitivity or high true positive rate,
# pick a threshold that minimizes the false positive rate
# but has a very high true positive rate.

# GENERATE ROC CURVE
install.packages("ROCR")
library(ROCR)
# We will use these predictions to create our ROC curve.
# First, we'll call the prediction function of ROCR.
# 1st Arg - the predictions we made
# 2nd Arg - True outcomes of our data points - qualityTrain$PoorCare
ROCRpred = prediction(predictTrain,qualityTrain$PoorCare)
# Use the performance function. This defines what we'd like to plot
# on the x and y-axes of our ROC curve.
ROCRperf = performance(ROCRpred,'tpr','fpr')
plot(ROCRperf)
# AMAZING!
plot(ROCRperf,colorize = TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# Multicollinearity & Significance
# Multicollinearity occurs when the various independent
# variables are correlated, and this
# might confuse the coefficients-- the betas-- in the model.
# Want to look at Area Under the ROC Curve (AUC)
# So the area under the curve gives an absolute measure
# of quality, and it's less affected by various benchmarks.
# An AUC of 1 is perfect (no False Positive Rate)
# Confusion Matrix
# Overall Accuracy = (TN+TP)/N
# Overall Error Rt = (FP+FN)/N 
# Sensitivity = TP/(TP+FN) # Whenever we predict poor quality and it indeed is poor quality
# Specificity = TN/(TN+FP) # Whenever we predict good quality and it indeed is good quality 
# False Neg Error Rt. = FN/(TP+FN)
# False Pos Error Rt. = FP/(TN+FP)

# Quick Question
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
# You can compute the test set AUC by running the following two commands in R:
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc #0.7994792
# The AUC of a model has the following nice interpretation: given a random 
# patient from the dataset who actually received poor care, and a random patient 
# from the dataset who actually received good care, the AUC is the perecentage 
# of time that our model will classify which is which correctly.










