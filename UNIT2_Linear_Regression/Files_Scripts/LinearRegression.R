# Wine in Lecture
setwd("/users/miller/desktop/r_edx/UNIT2/Files_Scripts")
wine = read.csv("wine.csv")
str(wine)
summary(wine)

#Create a one-variable linear regression equation using AGST to predict price
# Function lm - means linear model
model1 = lm(Price ~ AGST,data = wine)
summary(model1)
# The result from the summary shows
# - Residuals of error terms
# - Description of the coefficients of the model
# - The first row corresponds to the intercept term,
# - and the second row corresponds to our independent variable, AGST.
# - The Estimate column gives estimates of the beta values for our model.

#Comput the sum of squared errors (SSE) for our model
model1$residuals
SSE = sum(model1$residuals^2) #5.734875

# Adding another variable to our regression model, HarvestRain
model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
# Now have a third row in Coefficients
# Looking at the R-squared and adjusted R2 shows this new model is better with the additl variable

SSE = sum(model2$residuals^2)
SSE #2.970373 - which much better tahn the SSE for model1

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
# R2 is better
SSE = sum(model3$residuals^2)
SSE #1.732113 - better than model2

# Create a linear regression model to predict Price 
# using HarvestRain and WinterRain as independent variables. 
modelQ = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(modelQ)

# NOTES
# Estimate = gives the coefficients for the intercept 
# and for each of the independent variables in our model.
# The remaining columns help us to determine if a variable should be 
# included in the model, or if its coefficient is significantly different 
# from 0. A coefficient of 0 means that the value of the 
# independent variable does not change our prediction 
# for the dependent variable. If a coefficient is not 
# significantly different from 0, then we should probably 
# remove the variable from our model since it's not helping to 
# predict the dependent variable.
#
# Standard Error = gives a measure of how much the coefficient is 
# likely to vary from the estimate value.
#
# T Value = the estimate divided by the standard error.
# Will be negative if the estimate is negative and positive if the est is positive.
# IMPORTANT: The larger the absolute value of the t value, the more likely
# the coefficient is to be significant. So we want independent variables 
# with a large absolute value in this column.
#
# Pr(>ltl) = gives a measure of how plausible it is that the coefficient is 
# actually 0, given the data we used to build the model.  
# The smaller the probability number (ie the less plausible it is), 
# the less likely it is that our coefficent estimate is actually 0.
# WE WANT INDEPENDENT VARIABLES WITH SMALL VALUES HERE.
#
# Star Coding Scheme
# *** = Three stars is the highest level of significance
# and corresponds to a probability value less than 0.001,
# or the smallest possible probabilities.
# ** = Two stars is also very significant and corresponds
# to a probability between 0.001 and 0.01.
# Period (.) = A period, or dot, means that the coefficient is almost
# significant and corresponds to a probability between 0.05
# and 0.10.
# NOTHING - MEANS VARIABLE IS NOT SIGNIFICANT

# BOTH Age and FrancePopulation
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4) #R2 is .8286 - increased by remvoing FrancePopulation
# Some insights
# - Looking at the stars, we see that Age is NOW significant (b4 it wasnt)
# This is due to multicollinearity.  Age and FrancePopulation are highly
# correlated.

#NOTES
# CORRELATION
# +1 A correlation of +1 means a perfect positive linear relationship
# 0 means that there is no linear relationship between the two variables.
# -1 A correlation of +1 means a perfect negative linear relationship

# Compute correlation
cor(wine$WinterRain, wine$Price) #0.137
cor(wine$Age,wine$FrancePop) #-0.9944851 - highly correlated
cor(wine) #compute all correlations

#NOTES - Multicollinearity
# multicollinearity
# refers to the situation when two independent variables are
# highly correlated.
# A high correlation between an independent variable
# and the dependent variable is a good thing
# since we're trying to predict the dependent variable using
# the independent variables.
# Now due to the possibility of multicollinearity,
# you always want to remove the insignificant variables
# one at a time.

# Predict price using certain ind variables
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model5) #R2 = .7537 vs the model that incl. Age (.83)
# So if we had removed Age and FrancePopulation
# at the same time, we would have missed a significant variable,
# and the R-squared of our final model would have been lower.

# We should stick with MODEL 4 - which includes Age and doesnt present
# a multicollinearity issue (due to the high correlation with FrancePop).

# Correlation between HarvestRain and WinterRain
cor(wine$HarvestRain, wine$WinterRain)



# Using Test Data
wineTest = read.csv('wine_test.csv')
str(winetest)
predictTest = predict(model4, newdata = wineTest)
predictTest #shows you predictions based upon the model for each observation / data point in the new df
# Compute the R2 for the test data
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
SSE
SST
R2 = 1 - SSE/SST
R2 #out-of-sample R-squared = .7944
# However, our test set is really small
#NOTES: 
# The model R-squared will always increase or stay the same
# as we add more independent variables.
# However, this is not true for the test set.
# When selecting a model, we want one with a good model R-squared
# but also with a good test set R-squared.
# Test R2 can be negative (model worse on the test data).









