# Unit 5 - Text - Assignment 1 - Medical Literature

setwd("~/Desktop/R_EDX/UNIT5/Files_Scripts")

#Load it baby
# IMPORTANT: When working on text analytics - you need to add an extra arg to make
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(trials)
str(trials)
# How many chars in the longest abstract?
summary(nchar(trials$abstract))
max(nchar(trials$abstract))
# How many provide no abstract?
table(nchar(trials$abstract) == 0)
# Min num of characters in the title
which.min(nchar(trials$title))
trials$title[1258]

# Prepare the Corpera
library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle,removePunctuation)
corpusTitle = tm_map(corpusTitle,removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle,stemDocument)
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract,removePunctuation)
corpusAbstract = tm_map(corpusAbstract,removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract,stemDocument)

#Build a Document Term Matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
sparseTitle = removeSparseTerms(dtmTitle, 0.95)
dfTitle = as.data.frame(as.matrix(sparseTitle))

dtmAbstract = DocumentTermMatrix(corpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)
dfAbstract = as.data.frame(as.matrix(sparseAbstract))

# What is the most frequent word stem across all the abstracts???
findFreqTerms(sparseAbstract,4000) # find terms with 4000 hits
sort(colSums(dfAbstract)) # Sort all the terms

# We want to combine dtmTitle and dtmAbstract into a single data frame
# to make predictions. However, some of the variables in these data frames 
# have the same names. 
colnames(dfTitle) = paste0("T", colnames(dfTitle)) # pastes a T at the beginning of each column name for dtmTitle, which are the variable names. 
colnames(dfAbstract) = paste0("A", colnames(dfAbstract))
dtm = cbind(dfTitle, dfAbstract) # Combine into single DF
dtm$trial = trials$trial
ncol(dtm) #367

# Split the data and Build a model
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
# Accuracy of the baseline on the training set
table(train$trial)
730/nrow(train) # Accur. 0.5606759

library(rpart)
library(rpart.plot)
trialCART = rpart(trial~.,data=train,method="class")
prp(trialCART)
#Obtain the training set predictions for the model -
# means - do not do "predict" but look at the 2nd column of the predict output
predTrain = predict(trialCART)[,2] # MEANS KEEP ONLY SECOND COLUMN
summary(predTrain) # MAX 0.87190 predicted prob for any result
# What is the training set accuracy of the CART model?
table(train$trial, predTrain >= 0.5)
(631+441)/nrow(train) # accuracy 0.8233487
# What is the training set sensitivity of the CART model?
441/(441+131) # TP / TP+FN = 0.770979
# What is the training set specificity of the CART model?
631/(631+99) # TN / TN+FP = 0.8643836
# Evaluate CART on test set
predTest = predict(trialCART, newdata = test, type="class")
table(test$trial,predTest)
(261+162)/nrow(test) # Accuracy 0.7580645

# AUC 
library(ROCR)
predROCR = prediction(predTest, test$trial)
# perfROCR = performance(predROCR, "tpr", "fpr")
# plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values




