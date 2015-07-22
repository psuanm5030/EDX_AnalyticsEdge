# Unit 4 - Assignment1 - Gerber

setwd("~/Desktop/R_EDX/UNIT4/Files_Scripts")

#Load it baby
gerb = read.csv("gerber.csv")
str(gerb)

#% of people voting
table(gerb$voting)
108696/ (235388+ 108696) #0.3158996 voted
#What treatment group had the largest percentace voting
summary(gerb)
civic = subset(gerb,civicduty==TRUE)
haw = subset(gerb,hawthorne==TRUE)
self1 = subset(gerb,self==TRUE)
neb = subset(gerb,neighbors==TRUE)
summary(civic) #0.3145
summary(haw) #0.3224
summary(self1) #0.3452
summary(neb) #0.3779
# Or you can get these answers this way
tapply(gerb$voting, gerb$civicduty, mean)
tapply(gerb$voting, gerb$hawthorne, mean)
tapply(gerb$voting, gerb$self, mean)
tapply(gerb$voting, gerb$neighbors, mean)

# Build a logistic Regression model
logreg = glm(voting~civicduty+ hawthorne+ self+ neighbors, data = gerb, family="binomial")
summary(logreg)
# Using treshold of .3, what is the accuracy of the logreg model
logreg.pred = predict(logreg, type="response")
#confusion matrix:
table(gerb$voting,logreg.pred>0.3)
# We can compute the accuracy of the sum of the true positives and true negatives, 
# divided by the sum of all numbers in the table:
(134513+51966)/sum(134513,100875, 56730,51966) # accuracy 0.5419578

#confusion matrix with t-value 0.5:
table(gerb$voting,logreg.pred>0.5)
#We can compute the accuracy of the sum of the true positives and true negatives, 
#divided by the sum of all numbers in the table:
235388/(235388+108696) # accuracy 0.6841004

# Baseline (the people who did not vote): 
table(gerb$voting)
235388/(235388+108696) # accuracy 0.6841004
# Figure out the AUC (Area Under Curve)
library(ROCR)
ROCRpred = prediction(logreg.pred,gerb$voting)
as.numeric(performance(ROCRpred,"auc")@y.values)
# AUC Value on test set - 0.5308461


# create a regression tree -to explore the fraction of people who vote, or the probability of voting.
# dont use method = "class"
# If we used method=‘class’, CART would only split if one of the groups had 
# a probability of voting above 50% and the other had a probability of voting 
# less than 50% (since the predicted outcomes would be different). However, 
# with regression trees, CART will split even if both groups have probability 
# less than 50%.
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerb)
prp(CARTmodel)
# There are no splits in the tree, because none of the variables make a big enough effect to be split on.

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerb, cp=0.0)
prp(CARTmodel2)
# Determine which fraction of "Civic Duty" people voted: 
# You can find this answer by reading the tree - the people in the civic duty group correspond to 
# the bottom right split, which has value 0.31 in the leaf.
CARTmodel2


CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerb, cp=0.0)
prp(CARTmodel3)

CARTmodel_ct = rpart(voting ~ control, data=gerb, cp=0.0)
CARTmodel_ct_sex = rpart(voting ~ control + sex, data=gerb, cp=0.0)

# what is the absolute value of the difference in the predicted probability of voting between being 
# in the control group versus being in a different group? 
prp(CARTmodel_ct, digits = 6)
.34-.296638 #0.043362

# who is affected more by NOT being in the control group 
# (being in any of the four treatment groups):
prp(CARTmodel_ct_sex, digits = 6)
CARTmodel_ct_sex
wmn = abs(0.2904558-0.3341757)
men = abs(0.3027947-0.3458183)
wmn
men
# The first split says that if control = 1, go left. Then, if sex = 1 (female)
# predict 0.290456, and if sex = 0 (male) predict 0.302795. On the other side of 
# the tree, where control = 0, if sex = 1 (female) predict 0.334176, and if sex = 0 
# (male) predict 0.345818. So for women, not being in the control group increases 
# the fraction voting by 0.04372. For men, not being in the control group increases
# the fraction voting by 0.04302. So men and women are affected about the same.

# create a logistic model using "sex" and "control". Interpret the coefficient for "sex"
LogModelSex = glm(voting~sex+control, data=gerb, family="binomial")
summary(LogModelSex)

# If you look at the summary of the model, you can see that the coefficient 
# for the "sex" variable is -0.055791. This means that women are less likely
# to vote, since women have a larger value in the sex variable, and a 
# negative coefficient means that larger values are predictive of 0.

# Create the following dataframe (this contains all of the possible values of 
# sex and control), and evaluate your logistic regression using the predict 
# function (where "LogModelSex" is the name of your logistic regression model
# that uses both control and sex):
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
# ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) )
predict(LogModelSex, newdata=Possibilities, type="response")
# What is the absolute difference between the tree and the logistic 
# regression for the (Woman, Control) case?
abs(0.2908065-0.290456) #0.0003505
# The CART tree predicts 0.290456 for the (Woman, Control) case, and 
# the logistic regression model predicts 0.2908065. So the absolute difference, 
# to five decimal places, is 0.00035.

# add a new term to our logistic regression now, that is the combination 
# of the "sex" and "control" variables - so if this new variable is 1, 
# that means the person is a woman AND in the control group.
LogModel2 = glm(voting ~ sex+ control + sex:control, data=gerb, family="binomial")
summary(LogModel2)
# This coefficient is negative, so that means that a value of 1 in this variable 
# decreases the chance of voting. This variable will have variable 1 if the 
# person is a woman and in the control group.

#Calculate the average
predict(LogModel2, newdata=Possibilities, type="response")
# Now what is the difference between the logistic regression model and 
# the CART model for the (Woman, Control) case?
abs(0.2904558 - 0.290456)
# The logistic regression model now predicts 0.2904558 for the (Woman, Control) case, 
# so there is now a very small difference (practically zero) between CART and 
# logistic regression.






