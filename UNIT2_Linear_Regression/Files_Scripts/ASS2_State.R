## Assignment 2 - State Data

# Read in
state = read.csv("statedata.csv")
# Convert to a data frame
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
summary(state)

# Plot the state locations
plot(statedata$x, statedata$y)

# highest average high school graduation rate of all the states in the region:
tapply(statedata$HS.Grad,statedata$state.region,mean)

# Boxplot of the murder rate by region
boxplot(statedata$Murder ~ statedata$state.region)
# Figure out the outlier in Northeast
outlier = subset(statedata, state.region == "Northeast" & Murder > 10)

# Model: LifeExpectancy
lexp = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(lexp)
# Income coefficent is -0.0000218, meaning for a one unit increase in 
# income, predicted life expectancy decrease by that amount.

# Plot it
plot(statedata$Income, statedata$Life.Exp)
# This does NOT correlate to our model!!!  WHY??
# Although income is an insignificant variable in the model, this does not 
# mean that there is no association between income and life expectancy. 
# However, in the presence of all of the other variables, income does not 
# add statistically significant explanatory power to the model. This means 
# that multicollinearity is probably the issue.

# Refining the model
# Removing Variables
# Remember to use the significance of the coefficients to decide which 
# variables to remove (remove the one with the largest "p-value" first, or the 
# one with the "t value" closest to zero), and to remove them one at a time 
# (this is called "backwards variable selection"). This is important due to 
# multicollinearity issues - removing one insignificant variable may make 
# another previously insignificant variable become significant.
summary(lexp) #0.7362 / 0.6922
lexp2 = lm(Life.Exp ~ Population + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(lexp2) #0.7361 / 0.6993
lexp3 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost + Area, data = statedata)
summary(lexp3) #0.736 / 0.706
lexp4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(lexp4) #0.736 / 0.7126 (BEST Adjusted R2)
# It is always possible for the regression model to make a coefficient zero, 
# which would be the same as removing the variable from the model. The fact 
# that the coefficient is not zero in the intial model means it must be helping 
# the R-squared value, even if it is only a very small improvement.

#PREDICTING LIFE EXPECTANCY
# Look at the vector of predictions - since we are just looking at them, you 
# dont need to pass a newdata argument to predict function.
sort(predict(lexp4)) # Which has the lowest predicted life expectancy
which.min(statedata$Life.Exp) #Which has the lowest life expectancy - actual
statedata$state.name[40] #Find the state name by row from previous command

# Highest life exp
sort(predict(lexp4)) # Which has the lowest predicted life expectancy
which.max(statedata$Life.Exp) #Which has the lowest life expectancy - actual
statedata$state.name[11] #Find the state name by row from previous command

# Residuals
sort(abs(lexp4$residuals))
#OR
which.min(abs(lexp4$residuals))
#OR
sort(abs(statedata$Life.Exp - predict(lexp4)))
# MAX
which.max(abs(lexp4$residuals))










