# Unit 3 Recitation - Polling Data

# Read in 
polling = read.csv("PollingData.csv")
str(polling)
# only 145 obs vs expected 150
table(polling$Year)
# each row represents a state
# 2012 - missing 5 - pollsters were so sure about the 5 states that they didnt
# performa any polls in the months leading up to the 2012 election

summary(polling)
# There is some missing data for Rasmussen and SurveyUSA
# How to deal with this??
# 1. Delete those observations... Not good here - would remove half dataset
# 2. Could remove those variables - BUT we want them.
# 3. Fill missing data points with average value...  But there could be 
# other values that could help us put the right average in there.
# MULTIPLE IMPUTATION!
# Fill in the missing values based on the non-missing values for an 
# observation. So for instance, if the Rasmussen variable is reported
# and is very negative, then a missing SurveyUSA value
# would likely be filled in as a negative value as well.
# Multiple runs of multiple imputation results in different results.

# Install a new package
install.packages("mice")
library(mice)

# Multiple imputation
# Limit  DF to 4 polling variables - IMPORTANT GENERAL TOOL!
simple = polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)

# Set the random seed -for class
set.seed(144)
imputed = complete(mice(simple))
# 5 Rounds of imputation have been run and now all the variables have been
# filled in (no more NA values)
summary(imputed)
# Copy the Rasmussen and SurveyUSA variables back into our original 
# polling data frame, which has all the variables for the problem.
# And we can do that with two simple assignments.
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling) # No longer missing values.

# BUILD OUR MODEL
# split our data set (Test on 2012, train on 04 and 08)
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
# Understand the prediction of baseline (later compare to log model)
table(Train$Republican)
# 47 - democrate one, 53 - Republican will win
# Therefore, our simple baseline model will predict the more common outcome - 
# that a republican will win, and will have an accuracy of 53%.
# Obviously this is weak.  Always predicts republican.
# Need smarter baseline - take a poll and make prediction based on poll.
# SMART BASELINE - using the SIGN function
# passed positive, returns 1, passed negative, returns -1, passed 0 returns 0
sign(20) #Signifies smart basedline predicts Republican
sign(-20) # Signifies smart basedline predicts democrat wins
sign(0)
# Compute the prediction.
# take a look at the breakdown of that using the table function applied
# to the sign of the training set's Rasmussen variable.
table(sign(Train$Rasmussen))
# So what we really want to do is to see the breakdown of how
# the smart baseline model does, compared to the actual result
# Compare training set outcome versus the smart baseline (using sign)
table(Train$Republican, sign(Train$Rasmussen))
# RESULTS 
# We have 42 observations where the Rasmussen smart baseline
# predicted the Democrat would win, and the Democrat actually did win.
# There were 52 observations where the smart baseline predicted
# the Republican would win, and the Republican actually did win.
# Again, there were those three inconclusive observations.
# 3 mistakes (predicted the repbulican but democrat one)
# Now we know that this is a much better baseline model to later compare against
# our log model results.

# Build the Model
# What about multicollinearity???
# Well, we know that all the variables are measuring the same thing (how 
# strong a republican is fairing in a state).
# Normally we would run a correlation function - but it wont work:
cor(Train) # "X must be numeric"
str(Train) # once you look, you see that states is included.
cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])
# So let's first consider the case where
# we want to build a logistic regression model with just one
# variable.
# So in this case, it stands to reason
# that the variable we'd want to add
# would be the one that is most highly
# correlated with the outcome, Republican.
# So if we read the bottom row, which
# is the correlation of each variable to Republican,
# we see that PropR is probably the best candidate
# to include in our single-variable model,
# because it's so highly correlated,
# meaning it's going to do a good job of predicting
# the Republican status.

# Build model
mod1 = glm(Republican ~ PropR, data = Train, family = binomial)
summary(mod1) #AIC - 19.772

# Let's see how it does in terms of actually predicting the Repub outcome on 
# the training set.
# we want to compute the predictions, the predicted probabilities that the 
# Republican is going to win on the training set.
# CREATE A VECTOR
pred1 = predict(mod1, type = "response") # NO Newdata - just preds on training
table(Train$Republican, pred1>=0.5) # 4 mistakes - same as smart baseline

# Can we improve model by adding variable??
# Look for lower correlation of variables with each other, but correlated with
# dependent variable

# Build model2
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = binomial)
summary(mod2) #AIC - 18.439 vs 19.772 for mod1
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2>=0.5) # 3 mistakes - better than smart baseline
# Does model make snese?
summary(mod2)

# Evaluating the models on Test set
# Look at smart baseline
# Compute the outcome
table(Test$Republican,sign(Test$Rasmussen))
# REsults: 18 and 21 times where they were correct, 2 - inconclusive, 4 mistakes
# Compare log model against

# Obtain final test set predictions
TestPrediction = predict(mod2, newdata = Test, type="response")
table(Test$Republican,TestPrediction>=0.5) #50% prob of Repub winning
# In all but 1, we were correct.... can try changing T value (compute ROIC curve)
# But doesnt really matter with this data

# Lets take a look at the one state where we made our MISTAKE!
subset(Test, TestPrediction >=0.5 & Republican == 0) # Finding the mistake
# We see why we made the mistake.
# The Rasmussen poll gave the Republican a two percentage point lead, 
# SurveyUSA called a tie,
# DiffCount said there were six more polls that predicted Republican than Democrat,
# and two thirds of the polls predicted the Republican was going to win.
# But actually in this case, the Republican didn't win.
# Barack Obama won the state of Florida
# in 2012 over Mitt Romney.







