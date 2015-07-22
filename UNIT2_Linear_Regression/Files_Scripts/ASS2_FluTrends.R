# Assignment 2 - Flu Epidemics via Search Engine Query Data

# Read in
FluTrain = read.csv("FluTrain.csv")
FluTest = read.csv("FluTest.csv")
summary(FluTrain)
summary(FluTest)
#Finding the week with the highest columnar values
FluTrain[with(FluTrain,order(ILI)),] #This orders the DF by ILI column
FluTrain[with(FluTrain,order(Queries)),] #This orders the DF by Queries column

#Plot a histogram
hist(FluTrain$ILI)
#Most of the ILI values are small, with a relatively small number of much 
#larger values (in statistics, this sort of data is called "skew right").

#Dealing with skewed values (use log)
# When handling a skewed dependent variable, it is often useful to predict 
# the logarithm of the dependent variable instead of the dependent variable 
# itself -- this prevents the small number of unusually large or small 
# observations from having an undue influence on the sum of squared errors 
# of predictive models.
plot(FluTrain$Queries,log(FluTrain$ILI))
# SUGGESTS: There is a positive, linear relationship between log(ILI) and Queries.
# This means that linear regression could be a good model choice.

#Build the Model
#log(ILI) = intercept + coefficient x Queries, where the coefficient is positive 
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(model) #R2 = 0.709

#R-Squared
# It provides a measure of how well observed outcomes are replicated by 
# the model, as the proportion of total variation of outcomes explained 
# by the model.
Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation #0.8420333
Correlation^2 #= 0.7090201 (the R2 from our model)
# RELATIONSHIP: R-squared = Correlation^2

# Performance on Test Data
# IMPORTANT: USE exp() becasue the dependent variable in our model is log(ILI),
# which essentially reverses the log() function (I think thats what it does).
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
#Estimate for the percentage of ILI-related physician visits for the 
#week of March 11, 2012?
which(FluTest$Week == "2012-03-11 - 2012-03-17")
Estimated_ILI = PredTest1[11] #2.187378 - predicted / estimated value
# What is the relative error between the estimate (our Prediction) and
# the observed value for the week of March 11 2012.
# Relative error calculated as: (Observed ILI - Estimated ILI)/Observed ILI
Observed_ILI = FluTest$ILI[11] 
Observed_ILI #2.293422 - observed value for March 11
rel_error = (Observed_ILI - Estimated_ILI) / Observed_ILI
rel_error #0.04623827

#What is the Root Mean Square Error (RMSE) between our estimates and the 
#actual observations for the percentage of ILI-related physician visits, 
#on the test set?
# Calculate the RSME
SSE = sum((PredTest1-FluTest$ILI)^2)
SSE
# root mean squared error - is going to be the square root of the 
# sum of squared errors divided by n, which is the number of rows 
# in our test data set.
RMSE = sqrt(SSE / nrow(FluTest))
RMSE #0.7490645

# TRAINING A TIME SERIES MODEL
# Often, statistical models can be improved by predicting the current value 
# of the dependent variable using the value of the dependent variable from 
# earlier weeks.
# Because the ILI variable is reported with a 1- or 2-week lag, a decision 
# maker cannot rely on the previous week's ILI value to predict the current 
# week's value. Instead, the decision maker will only have data available 
# from 2 or more weeks ago. We will build a variable called ILILag2 that 
# contains the ILI value from 2 weeks before the current observation.

# We will use the "zoo" package, which provides a number of 
# helpful methods for time series models. 
install.packages("zoo")
library(zoo)
# Create the ILILag2 variable in the training set
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE) 
# -2 means to return 2 observations before the current one
# na.pad=TRUE means to add missing values for the first two weeks of our 
# dataset, where we can't compute the data from 2 weeks earlier.
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)

# Use the plot() function to plot the log of ILILag2 against the log of ILI.
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

#Build a new model - 
#on the FluTrain dataset to predict the log of the ILI variable using the 
# Queries variable as well as the log of the ILILag2 variable. 
FluTrend2 = lm(log(ILI)~Queries + log(ILILag2),data = FluTrain)
summary(FluTrend2) #0.9063
# Moving from FluTrend1 to FluTrend2, in-sample R^2 improved from 
# 0.709 to 0.9063, and the new variable is highly significant. 
# As a result, there is no sign of overfitting, and FluTrend2 is 
# superior to FluTrend1 on the training set.

# Create the ILILag2 variable in the TEST set
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE) 
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

# Filling the missing values in TEST data (because those two observations
# are in the TRAIN data - since this is time series data).  We need to add.
rows = nrow(FluTrain)
rows

FluTrain$ILI[rows-1]
FluTest$ILILag2[1] = FluTrain$ILI[rows-1]
FluTest$ILILag2[1]

FluTrain$ILI[rows]
FluTest$ILILag2[2] = FluTrain$ILI[rows]
FluTest$ILILag2[2]

# EVALUATING THE TIME SERIES MODEL IN THE TEST SET
# Obtain test set predictions of the ILI variable from the FluTrend2 model
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
# What is the test-set RMSE of the FluTrend2 model?
SSE = sum((PredTest2-FluTest$ILI)^2)
SSE
RMSE = sqrt(SSE / nrow(FluTest))
RMSE #0.2942029
# Lower RSME is better
# Used measure of the differences between values (sample and population 
# values) predicted by a model or an estimator and the values actually 
# observed. Basically, the RMSD represents the sample standard deviation 
# of the differences between predicted values and observed values. These 
# individual differences are called residuals when the calculations are 
# performed over the data sample that was used for estimation, and are 
# called prediction errors when computed out-of-sample.
