# FINAL - Ebay
- Log Regression / AUC and Accuracy / Cross-Validation
- Text Analytics

setwd("/users/miller/desktop/r_edx/FINAL")
eBay = read.csv("ebay.csv",stringsAsFactors=FALSE)
table(eBay$sold)
799/nrow(eBay)
summary(eBay)
table(eBay$size)


# Which of the following methods requires the dependent variable be stored as a factor variable when training a model for classification? RANDOM FOREST (randomForest)
eBay$sold = as.factor(eBay$sold)
eBay$condition = as.factor(eBay$condition)
eBay$heel = as.factor(eBay$heel)
eBay$style = as.factor(eBay$style)
eBay$color = as.factor(eBay$color)
eBay$material = as.factor(eBay$material)

# Split
library(caTools)
set.seed(144)
spl = sample.split(eBay$sold, 0.7)
training = subset(eBay,spl == TRUE)
testing = subset(eBay,spl == FALSE)

# Train the model - Logistic Regression
model2 = glm(sold~biddable + startprice + condition+heel+style+color+ material, data = training,family="binomial")
summary(model2)
# The observation has biddable=0, startprice=100, condition="Pre-owned", heel="High", style="Open Toe", color="Black", and material="Satin". Therefore, the prediction has logistic function value 0.5990788 + 100*-0.0044423 - 0.4952981 + 0.1224260 + 0.2226547 - 1.1078098 = -1.103178. Then you need to plug this into the logistic response function to get the predicted probability.

# Logistic Response Fucntion - to get predicted probabliliy. 1 / 1+e^(-x)
1 / (1+exp(1.103178))


# What is the meaning of the coefficient labeled "styleStiletto" in the logistic regression summary output?
exp(0.8325406) - 1 # Stilettos are predicted to have 129.9% higher odds of being sold than an otherwise identical open-toed shoe.
# -1 for the removal of the baseline.
# The coefficients of the model are the log odds associated with that variable; so we see that the odds of being sold are exp(0.8325406)=2.299153 those of an otherwise identical shoe in the baseline category for the style variable (which is "Open Toe"). This means the stiletto is predicted to have 129.9% higher odds of being sold.

# Obtain test-set predictions for your logistic regression model. Using a probability threshold of 0.5, on how many observations does the logistic regression model make a different prediction than the naive baseline model? Remember that the naive baseline model always predicts the most frequent outcome in the training set.
# Baseline
table(testing$sold)
899/nrow(testing) # 0.7892888
# Prediction
model2.pred = predict(model2, newdata = testing,type="response")
table(testing$sold,model2.pred >0.5)
(877 + 58) / nrow(testing)
# Using a probability threshold of 0.5, on how many observations does the logistic regression model make a different prediction than the naive baseline model?
table(model2.pred >0.5)

# What is the AUC Value for the model
library(ROCR)
ROCRpred = prediction(model2.pred,testing$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # 0.7444244
# Meaning of AUC: The proportion of the time the model can differentiate between a randomly selected shoe that was sold and a randomly selected shoe that was not sold
ROCRperf = performance(ROCRpred,'tpr','fpr')
plot(ROCRperf)
plot(ROCRperf,colorize = TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))


# Cross-Validation

library(caret)
library(e1071)
set.seed(144)
# Define number of folds
numFolds = trainControl(method="cv", number = 10) #10 folds
# Pick possible values for CV Value
cpGrid = expand.grid(.cp=seq(0.001,0.05,0.001)) #defines CP parems to test .01 to .5 in increments of .01
# Perform Cross Validation
# add the arguments: 
# method = "rpart", since we want to cross validate a CART model,
# trControl = numFolds, the output of our trainControl function, 
# tuneGrid = cpGrid, the output of the expand.grid function.
train(sold~biddable + startprice + condition+heel+style+color+ material, data = training, method = "rpart", trControl = numFolds, tuneGrid=cpGrid)
# CP that maximizes the CV Accuracy - 0.005

model3.cart = rpart(sold ~ biddable + startprice + condition+heel+style+color+ material, data = training, method="class",cp=0.005)
prp(model3.cart)

# Text Analytics
library(tm)
corpus = Corpus(VectorSource(eBay$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords, stopwords("english"))
corpus = tm_map(corpus,stemDocument)
dtm = DocumentTermMatrix(corpus)
summary(dtm)
head(dtm$Terms)
ncol(dtm) #10530 terms
spdtm = removeSparseTerms(dtm, 0.90) # 145 Unique Terms in spdtm
spdtm
descriptionText = as.data.frame(as.matrix(spdtm))
# puts x in front of numbers
colnames(descriptionText) = make.names(colnames(descriptionText))
summary(descriptionText)

names(descriptionText) = paste0("D", names(descriptionText))

descriptionText$sold = eBay$sold
descriptionText$biddable = eBay$biddable
descriptionText$startprice = eBay$startprice
descriptionText$condition = eBay$condition
descriptionText$heel = eBay$heel
descriptionText$style = eBay$style
descriptionText$color = eBay$color
descriptionText$material = eBay$material

trainText = subset(descriptionText, spl == TRUE)
testText =  subset(descriptionText, spl == FALSE)

glmText = glm(sold ~ ., data = trainText,family="binomial")
summary(glmText)

Train.pred = predict(glmText, type="response")
Test.pred = predict(glmText, newdata=testText, type="response")

# Training Set ACCURACY
table(trainText$sold, Train.pred >0.5)
(2038+209)/nrow(trainText) # Accuracy 0.8456906
# Training set AUC
predictionTrainLog = prediction(Train.pred, trainText$sold)
as.numeric(performance(predictionTrainLog, "auc")@y.values) # 0.8191927
# TESTING Set ACCURACY
table(testText$sold, Test.pred >0.5)
(840+86)/nrow(testText) # Accuracy 0.8129939
# TESTING set AUC
predictionTestLog = prediction(Test.pred, testText$sold)
as.numeric(performance(predictionTestLog, "auc")@y.values) #0.7337922
#glmText has more variables than the base logistic regression model, but it exhibits worse test-set performance (AUC of 0.734 vs. 0.744). Therefore, it is overfitted and removing variables would improve the test-set performance.








