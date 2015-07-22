# Unit 5 - Text - Assignment 3 - SPAM 

setwd("~/Desktop/R_EDX/UNIT5/Files_Scripts")

#Load it baby
# IMPORTANT: When working on text analytics - you need to add an extra arg to make
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
summary(emails)
str(emails)
table(emails$spam)
which.max(nchar(emails$text))
nchar(emails$text[2651])
# Row with least chars
which.min(nchar(emails$text))
# or 
min(nchar(emails$text)) # chars = 13
which(nchar(emails$text) == 13) # Row 1992

# Prepare the Corpera
library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords, stopwords("english"))
corpus = tm_map(corpus,stemDocument)
dtm = DocumentTermMatrix(corpus)
ncol(dtm)
dtm
spdtm = removeSparseTerms(dtm, 0.95)
spdtm
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))
emailsSparse$spam = emails$spam
sort(colSums(subset(emailsSparse, spam == 0))) # GOOD ONE
sort(colSums(subset(emailsSparse, spam == 1)))


# Building Machine Learning Models
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

# Logistic Regression Model
spamLog = glm(spam~., data = train,family="binomial")
# predicted spam probabilities
predictTestLog = predict(spamLog, newdata=test, type="response")
predTrainLog = predict(spamLog, type="response")
# How many of the train probs from spamLog are less than 0.00001
table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)
# OR
length(subset(predTrainLog,predTrainLog<0.00001)) # 3056
length(subset(predTrainLog,predTrainLog>.99999)) # 954
# Training Set Accuracy
table(train$spam, predTrainLog >0.5)
(3052+954)/nrow(train) # Accuracy 
# Training set AUC of spamLog
predictionTrainLog = prediction(predTrainLog, train$spam)
as.numeric(performance(predictionTrainLog, "auc")@y.values)
# TESTING Set Accuracy
table(test$spam, predictTestLog >0.5)
(1257+376)/nrow(test) # Accuracy 0.9505239
# Training set AUC of spamLog
predictionTestLog = prediction(predictTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values) #0.9627517



# CART Model
spamCART = rpart(spam~., data = train,method="class")
prp(spamCART)
# predicted spam probabilities
predTestCART = predict(spamCART, newdata=test)[,2]
predTrainCART = predict(spamCART)[,2]

# Training set accuracy
table(train$spam, predTrainCART>0.5)
(2885+894)/nrow(train) # Accuracy 0.942394
# Training set AUC of spamLog
predictionTrainCART = prediction(predTrainCART, train$spam)
as.numeric(performance(predictionTrainCART, "auc")@y.values)
# TESTING Set Accuracy
table(test$spam, predTestCART)
(1228+386)/nrow(test) # Accuracy 0.9394645
# Testing set AUC of spamLog
predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values) #0.9627517





# Random Forest
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~., data=train) 
# predicted spam probabilities
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]
predTrainRF = predict(spamRF, type="prob")[,2]

# Training set accuracy
table(train$spam, predTrainRF>0.5)
(3013+914)/nrow(train) # Accuracy 0.9793017
# Training set AUC of spamLog
predictionTrainRF = prediction(predTrainRF, train$spam)
as.numeric(performance(predictionTrainRF, "auc")@y.values) #0.9979116

# TESTING Set Accuracy
table(test$spam, predTestRF)
(1290+385)/nrow(test) # Accuracy 0.9749709
# Testing set AUC of spamLog
predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values) #0.9975656




