# Unit 4 - Assignment2 - Letter Recognition

setwd("~/Desktop/R_EDX/UNIT4/Files_Scripts")

#Load it baby
letters = read.csv("letters_ABPR.csv")
str(letters)
letters$isB = as.factor(letters$letter == "B")

# split the data
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = .50)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

# What is the baseline?  always predicts the most frequent outcome, which is NOT B
table(train$isB)
# To compute the accuracy of the baseline method on the test set, 
# we first need to see which outcome value is more frequent in the 
# training set, by using the table function. The output of table(train$isB) 
# tells us that "not B" is more common. So our baseline method is to predict 
# "not B" for everything. How well would this do on the test set? We need 
# to run the table command again, this time on the test set:
table(test$isB)
1175/(1175+383) # Accur. 0.754172

# build a classification tree to predict whether a letter is a B or not,
# We are just using the default parameters in our CART model, so we don't 
# need to add the minbucket or cp arguments at all. 
CARTb = rpart(isB ~ . - letter, data=train, method="class")
CARTb.pred = predict(CARTb,newdata=test,type="class")
# Classification Matrix
table(test$isB, CARTb.pred)
# Compute the accuracy (numbers on diagonal / total num of obs)
(1118+340)/(1118+340+57+43) #0.9358151

# Create Random Forest
library(randomForest)
set.seed(1000)
# default nodesize and ntree
LetterForest = randomForest(isB ~ . - letter, data = train)
# Model ready for predictions
PredictForestLetter = predict(LetterForest, newdata=test)
# Confusion Matrix - give true outcome and our predictions
table(test$isB,PredictForestLetter)
# Accuracy
(1165+374)/(1165+374+9+10) # 0.9878049

# Because this is a multiclass classification problem
letters$letter = as.factor( letters$letter )
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = .50)
# Why split? sample.split balances the outcome variable in the training and testing sets. 
# With a new outcome variable, we want to re-generate our split.)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

# What is baseline accruacy?
table(test$letter) #P is the most common
401/nrow(test) #0.2573813

# build a classification tree to predict "letter"
CARTletter= rpart(letter ~ . - isB, data=train, method="class")
prp(CARTletter)
# MAKE Preds on Test
letter.pred = predict(CARTletter, newdata=test, type="class")
# Classification Matrix
table(test$letter, letter.pred)
# Compute the accuracy (numbers on diagonal / total num of obs)
sum(348,318,363,340)/nrow(test) # Accur. 0.8786906

# Build a random forest 
set.seed(1000)
# default nodesize and ntree
RFl = randomForest(letter ~ . - isB, data = train)
# Model ready for predictions
RF.pred = predict(RFl, newdata=test)
# Confusion Matrix - give true outcome and our predictions
table(test$letter,RF.pred)
# Accuracy
sum(390,380,393,364)/nrow(test) # 0.9801027
# You should find this value rather striking, for several reasons. 
# The first is that it is significantly higher than the value for CART, 
# highlighting the gain in accuracy that is possible from using random 
# forest models. The second is that while the accuracy of CART decreased 
# significantly as we transitioned from the problem of predicting B/not B 
# (a relatively simple problem) to the problem of predicting the four letters 
# (certainly a harder problem), the accuracy of the random forest model 
# decreased by a tiny amount.






