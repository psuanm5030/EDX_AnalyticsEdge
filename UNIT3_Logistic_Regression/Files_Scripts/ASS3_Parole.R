# Assignment 3 - Parole Records

setwd("~/Desktop/R_EDX/UNIT3/Files_Scripts")

#Load it baby
parole = read.csv("parole.csv")
str(parole)
summary(parole)
# Violated their parole
table(parole$violator)

# Unordered Factors - Convert to Factors
# Crime and State are the unordered factors with at least 3 levels
# as.factor() function - convert these variables to factors.
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# Split into Training and Test sets
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Building the log regression model
mod1 = glm(violator ~ ., data = train, family="binomial")
summary(mod1)

# Figure out the Odds based on info: 
# parolee who is male, of white race, aged 50 years at prison release, 
# from the state of Maryland, served 3 months, had a maximum 
# sentence of 12 months, did not commit multiple offenses, and committed a larceny.
# Dont know what do do about the state
val = -4.2411574+(1*0.3869904)+(1*0.8867192)+(50*-0.0001756)+(3*-0.1238867)+(12*0.0802954)+(1*0.6837143)
val
exp(val) # Odds - 0.1825687
prob = 1/(1+(exp(-val)))
prob # Probability - 0.1543832
# From the logistic regression equation, we have log(odds) = -4.2411574 + 
# 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 
#   0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 
#   0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 
# - 0.2781054*crime3 - 0.0117627*crime4. This parolee has male=1, race=1, 
# age=50, state2=0, state3=0, state4=0, time.served=3, max.sentence=12, 
# multiple.offenses=0, crime2=1, crime3=0, crime4=0. We conclude that 
# log(odds) = -1.700629.
# 
# Therefore, the odds ratio is exp(-1.700629) = 0.183, and the predicted 
# probability of violation is 1/(1+exp(1.700629)) = 0.154.


# Prediction
pred1 = predict(mod1, newdata = test, type="response")
# what is the maximum predicted probability of a violation?
summary(pred1) # 0.907300 - WOW

# Threshold and sensitivity / specificity
table(test$violator, pred1 >= 0.5)
# OR use this one - turns True / False to 0 or 1
table(test$violator, as.numeric(pred1 >= 0.5))
nrow(test) # 202 rows
# Sensitivity - TP rate
12/23 #.5217
# Specificity - TN rate
167/179 #.9329
# Accuracy 
179/202 # .8861

# What is the accuracy of a simple model that predicts 
# that every parolee is a non-violator?
table(test$violator)
179/202 # 0.886


# Evaluating the model
# The model at cutoff 0.5 has 12 false positives and 11 false negatives, 
# while the baseline model has 0 false positives and 23 false negatives. 
# Because a parole board is likely to assign more cost to a false negative, 
# the model at cutoff 0.5 is likely of value to the board.
# 
# From the previous question, the parole board would likely benefit from 
# decreasing the logistic regression cutoffs, which decreases the false negative 
# rate while increasing the false positive rate.


# What is the AUC Value for the model
library(ROCR)
ROCRpred = prediction(pred1,test$violator)
as.numeric(performance(ROCRpred,"auc")@y.values)
# AUC Value on test set - 0.8945834
# The AUC deals with differentiating between a randomly selected positive and 
# negative example. It is independent of the regression cutoff selected.


