# Unit 4 - Assignment3 - Census

setwd("~/Desktop/R_EDX/UNIT4/Files_Scripts")

#Load it baby
census = read.csv("census.csv")
str(census)

# split the data
library(caTools)
set.seed(2000)
spl = sample.split(ensus$over50k,SplitRatio = .60)
train = subset(census,spl == TRUE)
test = subset(census,spl == FALSE)

# LOG REG MODEL
LogReg = glm(over50k~., data = train, family="binomial")
summary(LogReg)

# Accuracy on log
LogReg.pred = predict(LogReg, newdata = test, type="response")
#confusion matrix:
table(test$over50k,LogReg.pred>0.5)
sum(10421,2123)/nrow(test) #accur:  0.8498645

# Baseline Accuracy
table(test$over50k) # So <=50K is the most common
11198/nrow(test) #0.7586721

# What is the Area Under Curve (AUC)
library(ROCR)
ROCRpred = prediction(LogReg.pred,test$over50k)
as.numeric(performance(ROCRpred,"auc")@y.values)
# AUC Value on test set - 0.9049434

# build a classification tree to predict "over50k"
CART50k= rpart(over50k ~ ., data=train, method="class")
prp(CART50k)
# MAKE Preds on Test
CART50k.pred = predict(CART50k, newdata=test, type="class")
# Classification Matrix
table(test$over50k, CART50k.pred)
# Compute the accuracy (numbers on diagonal / total num of obs)
sum(10531,1890)/nrow(test) # Accur. 0.8415312

# What is the Area Under Curve (AUC) - DID NOT GET!!!!
library(ROCR)
ROCRpred = prediction(CART50k.pred,test$over50k)
as.numeric(performance(ROCRpred,"auc")@y.values)
# AUC Value on test set - 0.9049434

# DOWN-SAMPLE  prior to building a random forest
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
# default nodesize and ntree
set.seed(1)
RF50K = randomForest(over50k ~ ., data = trainSmall)
# Model ready for predictions
RF50K.pred = predict(RF50K, newdata=test)
# Confusion Matrix - give true outcome and our predictions
table(test$over50k,RF50K.pred)
# Accuracy
sum(11144,1006)/nrow(test) #0.8231707

# One metric that we can look at is the number of times, aggregated over 
# all of the trees in the random forest model, that a certain variable is 
# selected for a split. 
vu = varUsed(RF50K, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
# This code produces a chart that for each variable measures the 
# number of times that variable was selected for splitting (the value on the x-axis).
dotchart(vusorted$x, names(RF50K$forest$xlevels[vusorted$ix]))


# A different metric we can look at is related to "impurity", which measures how 
# homogenous each bucket or leaf of the tree is. In each tree in the forest, 
# whenever we select a variable and perform a split, the impurity is decreased. 
# Therefore, one way to measure the importance of a variable is to average the 
# reduction in impurity, taken over all the times that variable is selected for 
# splitting in all of the trees in the forest.
varImpPlot(RF50K)




# Define Cross validation experiment
# Define number of folds
set.seed(2)
numFolds = trainControl(method="cv", number = 10) #10 folds
# Pick possible values for CV Value
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 
# Perform Cross Validation
# add the arguments: 
# method = "rpart", since we want to cross validate a CART model,
# trControl = numFolds, the output of our trainControl function, 
# tuneGrid = cpGrid, the output of the expand.grid function.
train(over50k ~ ., data = train,method = "rpart",trControl = numFolds,tuneGrid=cartGrid)  #dep var with ind vars
# The first column gives the cp parameter that was tested,
# and the second column gives the cross validation accuracy for that cp value.
# The accuracy starts lower, and then increases,
# and then will start decreasing again, as we saw in the slides.
# At the bottom of the output, it says, "Accuracy was used to select the optimal 
# model using the largest value. The final value used for the model was cp = 0.18."
# This is the cp value we want to use in our CART model.

# Fit a cart model using this cp = 0.002
CART50k2= rpart(over50k ~ ., data=train, method="class",cp=.002)
prp(CART50k2)
CART50k2
# MAKE Preds on Test
CART50k2.pred = predict(CART50k2, newdata=test, type="class")
# Classification Matrix
table(test$over50k, CART50k2.pred)
# Compute the accuracy (numbers on diagonal / total num of obs)
sum(10515,2116)/sum(10515,2116,1446,683) # Accur. 0.8557588 vs 0.8415312 (prevs. cart mod)





