# Unit 4 - D2Hawkeye

setwd("~/Desktop/R_EDX/UNIT4/Files_Scripts")

#Load it baby
Claims = read.csv("ClaimsData.csv")
str(Claims)

# Get a percentage of patients in each cost bucket.
table(Claims$bucket2009)/nrow(Claims)

# Split data
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl ==TRUE)
ClaimsTest = subset(Claims, spl ==FALSE)

#Average Age of patients
mean(ClaimsTrain$age) #72.63773

#Proportion of ppl in Training set had at least one diagnosis code for diabetes
table(ClaimsTrain$diabetes)/nrow(ClaimsTrain) # 0.3808983
summary(ClaimsTrain)

# Baseline
# Would predict that the cost bucket for a patient in 2009 will be 
# the same as it was in 2008.
# Classification Matrix - to compute the accuracy
# for the baseline method on the test set.
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
# Accuracy = sum of diagonal (classified correctly)
(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest) # 0.6838135
# What is the Accuracy of the most common bucket, bucket 1?
(110138)/nrow(ClaimsTest) # 0.6011834
# baseline = 0.6838135
# Whats the penalty error??? 

# MATRIX
# Actual (Left) Predicted (Top)
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix

# compute the penalty error of the baseline method,
# we can multiply our classification matrix
# by the penalty matrix.

as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))* PenaltyMatrix
# Takes each number
# in the classification matrix and multiplies it
# by the corresponding number in the penalty matrix.

# Compute penalty error - sum observations and divide by # of obs
sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))* PenaltyMatrix)/nrow(ClaimsTest)
# Penalty Error for baseline method: 0.7386055


# What is the Accuracy of the most common bucket, bucket 1?
table(ClaimsTest$bucket2009)
122978/ sum(table(ClaimsTest$bucket2009)) # 0.67127
# What is the Penalty Error
(0*122978 + 2*34840 + 4*16390 + 6*7937 + 8*1057)/nrow(ClaimsTest) #1.044301
# Penalty Error for baseline method: 

# Create CART 
# goal will be to create a CART model that has an accuracy higher than 68%
# and a penalty error lower than 0.74.

library(rpart)
library(rpart.plot)
# Note that even though we have a multi-classification problem, we are building this
# in the same way as a binary classification problem.
# The CP was selected through cross-validation on the training set (not perf. here as it would take a significant amt of time)
ClaimsTree = rpart(bucket2009 ~ age + arthritis+cancer+copd+depression+diabetes+heart.failure+ihd+kidney+osteoporosis+stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class",cp=0.00005)
prp(ClaimsTree)
# HUGE tree here
# This makes sense for a few reasons.
# 1- the large number of observations in our training set.
# 2- its a five-class classification problem, so the classification is
# more complex than a binary classification case.

# MAKE Preds on Test
PredictTest = predict(ClaimsTree, newdata=ClaimsTest, type="class")
# Classification Matrix
table(ClaimsTest$bucket2009, PredictTest)
# Compute the accuracy (numbers on diagonal / total num of obs)
(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest) # Accur. 0.7126669

#Penalty Error
# This will take each entry in classifcaiton matrix and multiplies it by the corresponding
#  number in the penalty matrix.
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
# Penalty Error - 0.7606904 vs. baseline of .74 (and accur of .68), SO
# while we increased the accuracy by 3%, the penalty error went up. 
# WHY??
# By default, rpart will try to maximize the overall accuracy,
# and every type of error is seen as having a penalty of one.
# Our CART model predicts 3, 4, and 5 so rarely because there are very few observations in these classes.
# So we don't really expect this model to do better on the penalty error than the baseline method.
# So how can we fix this? 
# The rpart function allows us to specify a parameter called loss.
# ADD this Parameter: parms = list(loss=PenaltyMatrix)
ClaimsTree = rpart(bucket2009 ~ age + arthritis+cancer+copd+depression+diabetes+heart.failure+ihd+kidney+osteoporosis+stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class",cp=0.00005,parms = list(loss=PenaltyMatrix))
# If the rpart function knows that we'll be giving a higher penalty to some types of errors
# over others, it might choose different splits when building the model to minimize
# the worst types of errors.  Will probably bring down accuracy, but should bring 
# down the penalty error too.
# Regenerate our Test Set Predictions
PredictTest = predict(ClaimsTree, newdata=ClaimsTest, type="class")
# Accuracy 
table(ClaimsTest$bucket2009,PredictTest)
(94593+18853+4692+636+2)/nrow(ClaimsTest) # 0.6483335 - lower accuracy (vs. 0.712), BUT whats
# the penalty error?
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
# Penalty errror 0.6419635 Vs 0.7606904
# If you look at the classification matrix for the second CART model, 
# we predicted bucket 1 less frequently. This is because, according to the 
# penalty matrix, some of the worst types of errors are to predict bucket 1 
# when the actual cost bucket is higher.


# Insights
# 1 - Substantial improvement over the baseline
# 2 - Doubled Accuracy over the baseline in some cases
# 3 - Smaller Accuracy improvement on bucket 5, but much lower penalty.











