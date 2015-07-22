# Assignment 2 - Climate Change

# Read in
clim = read.csv("climate_change.csv")

# Split the data into training set and testing set
train = subset(clim, Year < 2007)
test = subset(clim, Year > 2006) # This is the data we give to lm() function
# Testing data set used to test our predictive ability

# Build a linear regression model to predict:
# Dep var: Temp
# Ind Var: MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols
model_clim1 = lm(Temp ~ MEI + CO2+ CH4+ N2O+ CFC.11+ CFC.12 + TSI + Aerosols,data = train)
summary(model_clim) # R2 = 0.7509
# What values in the model are significant (p value < 0.05)
# While N2O and CFC.11 are negative regression coefficients, meaning they indicate
# that increasing concentrations of these is associated with lower temps.
# This is WRONG, reflecting that they are correlated with other vars in data.

# CORRELATIONS - all at once
cor(train)

# New Model - removing some highly correlated vars
model_clim2 = lm(Temp ~ MEI + TSI + Aerosols + N2O,data = train)
summary(model_clim) # R2 = 0.7261
# We have observed that, for this problem, when we remove many variables 
# the sign of N2O flips. The model has not lost a lot of explanatory 
# power (the model R2 is 0.7261 compared to 0.7509 previously) despite 
# removing many variables.

# Automatically building the model - using STEP
# Akaike information criterion (AIC) - it can be informally thought of as the 
# quality of the model with a penalty for the number of variables in the model.
model_clim3 = step(model_clim1)
summary(model_clim3) #0.7508 - new model produced has better R2
# the step function does not address the collinearity of the variables, 
# except that adding highly correlated variables will not improve the R2 
# significantly. The consequence of this is that the step function will 
# not necessarily produce a very interpretable model - just a model that 
# has balanced quality and simplicity for a particular weighting of quality 
# and simplicity (AIC).


# Calculate temperature predictions for the testing data set, 
# using the predict function.
predictTest = predict(model_clim3, newdata = test)
summary(predictTest)
#shows you predictions based upon the model for each observation / data point in the new df
# This is a measurement of how well the model predicts on test data.
# For our model, we had R2 of 0.7508 "In-Sample R-Squared" - how well the 
# model fits the training data.
# Need to calculate how well the model fits the test data, so 
# Calculate the out-of-sample R-squared
SSE = sum((predictTest - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - (SSE / SST)
R2 #0.6286051



