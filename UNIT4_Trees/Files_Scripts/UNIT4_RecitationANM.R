# Unit 4 - Recitation - Trees

setwd("~/Desktop/R_EDX/UNIT4/Files_Scripts")

# In the lecture, we mostly discuss classification trees with the output
# as a factor or a category. Trees can also be used for regression tasks.
# The output at each leaf of a tree is no longer a category, but a number.
# Just like classification trees, regression trees can capture
# nonlinearities that linear regression can't.
# So what does that mean? Well, with classification trees we report the average outcome
# at each leaf of our tree.

# CLASSIFICATION TREES we report the average outcome at each leaf of our tree.
# REGRESSION TREES we have continuous variables, so we simply report the average
# of the values at that leaf. 

#Load it baby
boston = read.csv("boston.csv")
str(boston)
# Each obs corresponds to a census tract.

plot(boston$LON,boston$LAT)
# Show all the places that lie along the river in a different color
# if you already have a PLOT, then you can do this as follows: 
# pch=19 is a solid version of the dots already on the plot
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue",pch=19)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red",pch=19)

# Distribution of the air quality variable - NOW
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],col="green",pch=19)
# Makes sense - because this is the dense area of boston

# Look at prices - need a new plot
plot(boston$LON,boston$LAT)
summary(boston$MEDV)
# Census plots with above average housing prices:
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
# Not very linear.... 

plot(boston$LON,boston$MEDV)
plot(boston$LAT,boston$MEDV)
# Create a linear model
latlonlm = lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
# What does a LM think is above median?
latlonlm$fitted.values # The predictions for each tract
points(boston$LON[latlonlm$fitted.values>=21.2],boston$LAT[latlonlm$fitted.values>=21.2],col="blue",pch="$")

library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
# Want to predict what the Tree thinks is above media, just like we did with linear reg.
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>=21.2], boston$LAT[fittedvalues>=21.2],col="blue",pch="$")
# Now we see that we've done a much better job
# than linear regression was able to do.
# We've correctly left the low value area in Boston
# and below out, and we've correctly
# managed to classify some of those points
# in the bottom right and top right.
# We're still making mistakes, but we're
# able to make a nonlinear prediction
# on latitude and longitude.


# too Complicated??  
latlontree = rpart(MEDV~LAT + LON, data=boston, minbucket=50)
plot(latlontree)
text(latlontree) # add text
plot(boston$LON,boston$LAT)
# Abline can plot horizonatl or vertical lines easily
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)

# Predict house prices - Linear Regression
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV,SplitRatio=.7)
train=subset(boston,split==TRUE)
test=subset(boston,split==FALSE)
linreg= lm(MEDV ~ LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data=train)
summary(linreg)
# Calculate Sum of Squared Errors
linreg.pred = predict(linreg, newdata=test)
# Simply the sum of the predicted values less the actual values, squared
linreg.sse=sum((linreg.pred - test$MEDV)^2)
linreg.sse #3037.088 - can we beat this using regression trees???

# Predict house prices - Trees
tree= rpart(MEDV ~ LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data=train)
prp(tree) # Lat and Lon are not important; non-linear on rooms; same with polution
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred-test$MEDV)^2)
tree.sse # 4328.988
# Simply put - regression trees are not as good as linear regression

# What this says to us, given what we saw with the latitude
# and longitude, is that latitude and longitude are nowhere near
# as useful for predicting, apparently,
# as these other variables are.

# Complexity Parameter
# The intuition we gain is, having too many splits
# is bad for generalization-- that is, performance on the test
# set-- so we should penalize the complexity.
# Let us define RSS to be the residual sum of squares, also
# known as the sum of square differences.
# Our goal when building the tree is
# to minimize the RSS by making splits,
# but we want to penalize having too many splits now.

# Small numbers of cp encourage large trees,
# and large values of cp encourage small trees.

# Build Tree using Cross Validation
# CP varies between 0 and 1
library(caret)
library(e1071)
# how to do parameter tuning
tr.control = trainControl(method="cv",number=10)
# Make grid of CP values to try
cp.grid = expand.grid(.cp = (0:10)*0.001)
1*0.001
10*0.001
0:10
0:10 * 0.001 #numbers 0 through 10, scaled by 0.001, our values for CP that caret will try

# lets store the results of the CV fitting 
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
# You can see it tried 11 different values of cp.
# And it decided that cp equals 0.001 was the best because it
# had the best RMSE-- Root Mean Square Error.
# And it was 5.03 for 0.001.

# Tree
best.tree = tr$finalModel # thats the model that corresponds to 0.01
prp(best.tree)
best.tree.pred=predict(best.tree,newdata=test)
best.tree.see = sum((best.tree.pred - test$MEDV)^2)
best.tree.see #3660.149 vs tree in previous video, which was > 4000
# The linear regression SSE was more around 3,030.
# So the best tree is not as good as the linear regression model.
# But cross validation did improve performance.
# So the takeaway is, I guess, that trees
# aren't always the best method you have available to you.
# But you should always try cross validating
# them to get as much performance out of them as you can.
