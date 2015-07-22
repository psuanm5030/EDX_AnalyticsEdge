# Unit 2: Recitation - Linear Regression (Moneyball in the NBA)

NBA = read.csv("NBA_Train.csv")
str(NBA) #835 obs. of  20 variables

# How many games needed to win to make playoffs
table(NBA$W, NBA$Playoffs)

# Predict wins by looking at PS vs PA (points allowed)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS

# Make Scatter plot to see if it looks like there's a linear relationship
# between the number of wins that a team wins and the point difference.
plot(NBA$PTSdiff, NBA$W)
WinsReg = lm(W~PTSdiff, data = NBA)
summary(WinsReg) #R2 .9423 - verifies the strong linear relationship.
# W = 41+0.0326*PTSdiff  --- if we want >= 42
# Then --> PTSdiff >= (42-41) / 0.0326 = 30.67 
# NEED TO SCORE at least 31 more points than we allow in order to win at least 42 games.

# Build an equation to predict points scored
# using some common basketball statistics.
# dep var = PTS
# indep var = some of the statistics in our data set

# Build the Regression
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg) # 0.8992 - shows strong relationship
# Some are significatn, some are NOT (DRB, TOV, BLK)
# Comput Residuals
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE #28394314 - QUITE ALOT - Not very calculable - should use root mean sq. error
RMSE = sqrt(SSE/nrow(NBA))
RMSE # 184.4049 - on average, we make an error of 184.4 pts (not alot considering the average points in a season)
mean(NBA$PTS) #8370.24 - SO, being off 184, for the average being 8300, not bad
# Refining the Model - check to find which variables can be removed
summary(PointsReg)
# Remove Turnovers first, bc the p value is the highest (we want smaller) - meaning
# Turnovers is the least statistically sig var in the model.
# New Model
PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2) # 0.8991 VS 0.8992 (1st model) - SHOWS WE ARE JUSTIFIED TO REMOVE!!

# New Model - #2
PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2) # 0.8991 VS 0.8992 (1st model) - SHOWS WE ARE JUSTIFIED TO REMOVE!!
# The next one, based on p-value, that we would want to remove
# is defensive rebounds.
# New Model - #3
PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsReg3) # 0.8991 VS 0.8991 (2ND MODEL) VS 0.8992 (1st model) - SHOWS WE ARE JUSTIFIED TO REMOVE!!
# SAME R2 - we are justified in our removal
# New Model - #4
PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4) # 0.8991 vs 0.8991 (3RD MODEL) VS 0.8991 (2ND MODEL) VS 0.8992 (1st model) - SHOWS WE ARE JUSTIFIED TO REMOVE!!
# SAME R2 - we are justified in our removal


# Look at SSE and RMSE - to make sure we didnt inflate by removing
# Compared to oriignal model: SSE 28,394,314 & RMSE 184.4049
SSE4 = sum(PointsReg4$residuals^2)
RMSE4 = sqrt(SSE4/nrow(NBA))
SSE4 # 28,421,465 -- difficult to interpret
RMSE4 # 184.493 -- easier to understand
# SO, the new SSE and RMSE isnt different, MEANING we narrowed down on a much better model.

# Making predictions for 2012-2013
NBA_test = read.csv("NBA_test.csv")
PointsPredictions = predict(PointsReg4, newdata = NBA_test)
# How good is it?? Do the out of sample R2
# This is a measurement of how well the model predicts on test data.
# For our model, we had R2 of .8991 "In-Sample R-Squared" - how well the 
# model fits the training data.
# Need to calculate how well the model fits the training data, so 
# Calculate the out-of-sample R-squared
SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - (SSE / SST)
R2 #0.8127142
# root mean squared error - is going to be the square root of the 
# sum of squared errors divided by n, which is the number of rows 
# in our test data set.
RMSE = sqrt(SSE / nrow(NBA_test))
RMSE #196.3723 --- little higher than before - making an average error of 196 pts


