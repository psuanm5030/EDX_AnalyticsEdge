# Unit 6 - Assignment - Clustering - Predicting Stock Returns with Cluster-then-Predict

# cluster-then-predict, a methodology in which you first cluster observations and then build cluster-specific prediction models. In this problem, we will first use clustering to identify clusters of stocks that have similar returns over time. Then, we'll use logistic regression to predict whether or not the stocks will have positive future returns.

setwd("~/Desktop/R_EDX/UNIT6/Files_Scripts")

#Load it baby
stocks = read.csv("StocksCluster.csv")
str(stocks)
summary(stocks)
# You can compute the proportion of observations with positive returns by using the table function:
table(stocks$PositiveDec) # or
mean(stocks$PositiveDec)
# What is the maximum correlation between any two return variables in the dataset?
cor(stocks)
sort(cor(stocks))
# Which month (from January through November) has the largest mean return across all observations in the dataset?
summary(stocks)


# Split the data into training and testing
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
# train a LogRegModel to predict PositiveDec
StocksModel = glm(PositiveDec ~ .,data = stocksTrain, family="binomial")
summary(StocksModel)
# Using treshold of .5, what is the accuracy of the logreg model
logreg.pred = predict(StocksModel, type="response")
#confusion matrix:
table(stocksTrain$PositiveDec,logreg.pred>0.5)
sum(990,3640)/nrow(stocksTrain) #accur:  0.5711818

# Obtain the test set preds
logreg.predTEST = predict(StocksModel, newdata=stocksTest, type="response")
# what is the overall accuracy using a threshold of 0.5
table(stocksTest$PositiveDec,logreg.predTEST >0.5)
sum(417,1553)/nrow(stocksTest) #accur:  0.5670697

# What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
table(stocksTest$PositiveDec)
1897 / sum(1577,1897) # accur: 0.5460564

# Clustering the Stocks 
# Remove the dep vars
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# In cluster-then-predict, our final goal is to predict the dependent variable, which is unknown to us at the time of prediction. Therefore, if we need to know the outcome value to perform the clustering, the methodology is no longer useful for prediction of an unknown outcome value.

# This is an important point that is sometimes mistakenly overlooked. If you use the outcome value to cluster, you might conclude your method strongly outperforms a non-clustering alternative. However, this is because it is using the outcome to determine the clusters, which is not valid.

# NORMALIZE THE INFORMATION
# Using CARET package - PREPROCESS function normalizes variables by subtracting by the mean and dividing by the standard deviation
# In cases where we have a training and testing set, we'll want to normalize by the mean and standard deviation of the variables in the training set. We can do this by passing just the training set to the preProcess function:
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
# Mean of ReturnJan in normTrain
mean(normTrain$ReturnJan) # 2.100586e-17
mean(normTest$ReturnJan) # 0.0004185886
# From mean(stocksTrain$ReturnJan) and mean(stocksTest$ReturnJan), we see that the average return in January is slightly higher in the training set than in the testing set. Since normTest was constructed by subtracting by the mean ReturnJan value from the training set, this explains why the mean value of ReturnJan is slightly negative in normTest.

k = 3
set.seed(144)
km = kmeans(normTrain, centers=k)
summary(km)
table(km$cluster) # or you can do: 
km$size

#use the flexclust package to obtain training set and testing set cluster assignments for our observations
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
# How many test-set observations were assigned to Cluster 2?
table(clusterTest)

# Using the subset function, build data frames stocksTrain1, stocksTrain2, and stocksTrain3, containing the elements in the stocksTrain data frame assigned to clusters 1, 2, and 3, respectively 
stocksTrain1 = subset(stocksTrain,clusterTrain == 1)
stocksTrain2 = subset(stocksTrain,clusterTrain == 2)
stocksTrain3 = subset(stocksTrain,clusterTrain == 3)
stocksTest1 = subset(stocksTest,clusterTest == 1)
stocksTest2 = subset(stocksTest,clusterTest == 2)
stocksTest3 = subset(stocksTest,clusterTest == 3)

# Which training set data frame has the highest average value of the dependent variable?
mean(stocksTrain1$PositiveDec) # 0.6025
mean(stocksTrain2$PositiveDec) # 0.5141
mean(stocksTrain3$PositiveDec) # 0.4387

# Build Log Models on each
StocksModel1 = glm(PositiveDec~., data=stocksTrain1,family="binomial")
StocksModel2 = glm(PositiveDec~., data=stocksTrain2,family="binomial")
StocksModel3 = glm(PositiveDec~., data=stocksTrain3,family="binomial")

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

# Using StocksModel1, make test-set predictions called PredictTest1 on the data frame stocksTest1. 
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")
# What is the overall accuracy of StocksModel1 on the test set stocksTest1, using a threshold of 0.5?
table(stocksTest1$PositiveDec,PredictTest1>0.5)
sum(30,774)/nrow(stocksTest1) # 0.6194145

# Using StocksModel2, make test-set predictions called PredictTest2 on the data frame stocksTest2. 
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")
# What is the overall accuracy of StocksModel2 on the test set stocksTest2, using a threshold of 0.5?
table(stocksTest2$PositiveDec,PredictTest2>0.5)
sum(388,757)/nrow(stocksTest2) # 0.5504808

# Using StocksModel3, make test-set predictions called PredictTest3 on the data frame stocksTest3.
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")
# What is the overall accuracy of StocksModel3 on the test set stocksTest3, using a threshold of 0.5?
table(stocksTest3$PositiveDec,PredictTest3>0.5)
sum(49,13)/nrow(stocksTest3) # 0.6458333

# To compute the overall test-set accuracy of the cluster-then-predict approach, we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

# What is the overall test-set accuracy of the cluster-then-predict approach, again using a threshold of 0.5?
table(AllOutcomes,AllPredictions>0.5)
(467+1544)/(467+1544+353+1110) # Accuracy: 0.5788716 (vs. Log Reg - 0.5711818)


