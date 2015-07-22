# Kaggle Submission Try #1

#******************* Setup ***********************

setwd("~/Desktop/R_EDX/KAGGLE")

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields

ntrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
ntest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
# Deal with the factor problem
temp = c(ntrain$SectionName, ntest$SectionName)
temp = as.factor(temp)
ntrain$SectionName = temp[1:nrow(ntrain)]
ntest$SectionName = temp[(nrow(ntrain)+1):length(temp)]

temp = c(ntrain$NewsDesk, ntest$NewsDesk)
temp = as.factor(temp)
ntrain$NewsDesk = temp[1:nrow(ntrain)]
ntest$NewsDesk = temp[(nrow(ntrain)+1):length(temp)]

# Turn chars into factors where approp.
# ntrain$NewsDesk = as.factor(ntrain$NewsDesk)
# ntest$NewsDesk = as.factor(ntest$NewsDesk)
# ntrain$SectionName = as.factor(ntrain$SectionName)
# ntest$SectionName = as.factor(ntest$SectionName)
# ntest$NewsDesk = factor(ntest$NewsDesk,levels=levels(ntrain$NewsDesk))

# The train / test sets dont have same levels for the factor variable
# levels(ntest$SectionName) <- levels(ntrain$SectionName)
# levels(ntest$NewsDesk) <- levels(ntrain$NewsDesk)

#******************* Dates / Times ***********************
# Convert the date/time:
ntrain$PubDate = strptime(ntrain$PubDate, "%Y-%m-%d %H:%M:%S")
ntest$PubDate = strptime(ntest$PubDate, "%Y-%m-%d %H:%M:%S")
# Setup new Variable - Weekday
ntrain$Weekday = ntrain$PubDate$wday
ntrain$WKday <- ntrain$Weekday == 0 | ntrain$Weekday == 6

ntest$Weekday = ntest$PubDate$wday
ntest$WKday <- ntest$Weekday == 0 | ntest$Weekday == 6
str(ntrain)
# Setup New Variable - Is there a Question Mark??
ntrain$Question <- grepl("\\?",ntrain$Headline)
ntest$Question <- grepl("\\?",ntest$Headline)

# Setup New Variable - Hour
ntrain$Hour = ntrain$PubDate$hour
ntest$Hour = ntest$PubDate$hour

# New Variable for Prime Reading Hours

# Three levels: Midnight to 7am; 7am to 6pm, 6pm to midnight
ntrain$HourCat = cut(ntrain$Hour, c(-1,7,12,18,25)) 
table(ntrain$HourCat)
ntest$HourCat = cut(ntest$Hour, c(-1,7,12,18,25)) 
table(ntest$HourCat)

# Five levels
# ntrain$HourCat = cut(ntrain$Hour, c(0,6,11,15,18,22,24)) 
# table(ntrain$HourCat)
# ntest$HourCat = cut(ntest$Hour, c(0,6,11,15,18,22,24)) 
# table(ntest$HourCat)

#******************* Text - Headline ***********************
library(tm)

# HEADLINE
# Note that we are creating a corpus out of the training and testing data.
CorpusHeadline = Corpus(VectorSource(c(ntrain$Headline, ntest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
# Remember this extra line is needed after running the tolower step:
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
# Convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
dtm = DocumentTermMatrix(CorpusHeadline)
dtm
# MODIFYIABLE - SPARSE TERMS THRESHOLD:
sparse = removeSparseTerms(dtm, 0.99) # Want ~ 200 words
sparse
inspect(sparse[1:1,]) # Gives you all the columns
HeadlineWords = as.data.frame(as.matrix(sparse))
# Ensure variable names are okay for R:
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

# Split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
HeadlineWordsTrain = head(HeadlineWords, nrow(ntrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(ntest))

# Remove words that dont make sense?

#******************* Text - Abstract ***********************
# ABSTRACT
# Note that we are creating a corpus out of the training and testing data.
CorpusAbstract = Corpus(VectorSource(c(ntrain$Abstract, ntrain$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
# Remember this extra line is needed after running the tolower step:
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
# Convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
dtmAb = DocumentTermMatrix(CorpusAbstract)
inspect(dtmAb[1:5,])
# MODIFYIABLE - SPARSE TERMS THRESHOLD:
sparseAb = removeSparseTerms(dtmAb, 0.989) # Want ~ 200 words
sparseAb
inspect(sparseAb[1:1,]) # Gives you all the columns
AbstractWords = as.data.frame(as.matrix(sparseAb))
# Ensure variable names are okay for R:
colnames(AbstractWords) = make.names(colnames(AbstractWords))

# Split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
AbstractWordsTrain = head(AbstractWords, nrow(ntrain))
AbstractWordsTest = tail(AbstractWords, nrow(ntest))


#*******************Creating final file ***********************
# Add back the original variables from our dataset: 

ftrain = HeadlineWordsTrain
ftest = HeadlineWordsTest
# TRAIN
ftrain$Popular = ntrain$Popular # DEP. VAR
ftrain$WordCount = log(ntrain$WordCount + 50) #TAKING LOG OF VALUE
ftrain$Weekday = ntrain$Weekday
ftrain$WKday = ntrain$WKday
ftrain$Question = ntrain$Question
ftrain$HourCat = ntrain$HourCat
ftrain$NewsDesk = ntrain$NewsDesk
ftrain$SectionName = ntrain$SectionName
ftrain$UniqueID = ntrain$UniqueID

# TEST
# No dep var
ftest$WordCount = log(ntest$WordCount + 50)
ftest$Weekday = ntest$Weekday
ftest$WKday = ntest$WKday
ftest$Question = ntest$Question
ftest$HourCat = ntest$HourCat
ftest$NewsDesk = ntest$NewsDesk
ftest$SectionName = ntest$SectionName
ftest$UniqueID = ntest$UniqueID

#*******************Creating a new sample set***********************
# Making an additional Training / Test Set from the Training data
library(caTools)
set.seed(144)
split = sample.split(ftrain$Popular, SplitRatio = 0.75)
anmtrain = subset(ftrain, split == TRUE)
anmtest = subset(ftrain, split == FALSE)

#******************* GLM / LOGISTIC REGRESSION MODELS ***********************
library(ROCR)
# Evaluate BASELINE (always predicts the most popular outcome - which is that it is NOT popular)
table(ftrain$Popular)
5439/(5439+1093) #Baseline Accuracy is 0.8326699

# LOGISTIC REGRESSION - Build 1
anmLog.model = glm(Popular ~ ., data = ftrain,family="binomial")
anmLog.pred = predict(anmLog.model, type = "response")
summary(anmLog.model)
# anmLog.pred = predict(anmLog.model, newdata = anmtest, type="response")

# Evaluate anmLog.model
table(anmLog.pred>0.5)
(4171)/sum(4171+633) # accuracy 0.8682348 - better than the baseline, but not that good.

# LOGISTIC REGRESSION - Build 2 (Remove Var: SectionName)
anmLog.model = glm(Popular ~ . - SectionName, data = anmtrain,family="binomial")
anmLog.pred = predict(anmLog.model, type = "response")
summary(anmLog.model)
# anmLog.pred = predict(anmLog.model, newdata = anmtest, type="response")

# Evaluate anmLog.model
table(anmLog.pred>0.5)
(4231)/sum(4231+573) # accuracy 0.8807244 - better than the baseline, by about .05

# LOGISTIC REGRESSION - Build 3 (Remove Var: SectionName and Various words)
anmLog.model = glm(Popular ~ . - SectionName - Weekday - X2014 - X2015 - can - make - week - will - pari - daili - springsumm - small , data = ftrain,family="binomial")
anmLog.pred = predict(anmLog.model, newdata= ftest, type = "response")
# anmLog.pred = predict(anmLog.model, newdata = anmtest, type="response")

# Evaluate anmLog.model
table(anmLog.pred>0.5)
(4229)/sum(4229+575) # accuracy 0.8803081 - WORSE THAN PREVIOUS MODEL 2

# LOGISTIC REGRESSION - Build 4 (BUILD 2 & Remove Var: Weekday)
anmLog.model = glm(Popular ~ . - SectionName - Weekday, data = ftrain,family="binomial")
anmLog.pred = predict(anmLog.model, type = "response")
anmLog.pred = predict(anmLog.model, newdata = ftest, type ="response")
summary(anmLog.model)
# anmLog.pred = predict(anmLog.model, newdata = anmtest, type="response")

# Evaluate anmLog.model
table(anmLog.pred>0.5)
(4231)/sum(4231+573) # accuracy 0.8807244 - SAME AS MODEL 2

# LOGISTIC REGRESSION - Build 5 (BUILD 4 & Remove Var: NewsDesk)
anmLog.model = glm(Popular ~ . - SectionName - Weekday - NewsDesk, data = ftrain,family="binomial")
anmLog.pred = predict(anmLog.model,newdata=ftest, type = "response")
summary(anmLog.model)
# anmLog.pred = predict(anmLog.model, newdata = anmtest, type="response")

# Evaluate anmLog.model
table(anmLog.pred>0.5)
(4519)/sum(4519+285) # accuracy 0.9406744 - Much Improved?  Like, really good

#******************* CART MODELS ***********************

library(rpart)
library(rpart.plot)
# Build model - CART
anmCART.model = rpart(Popular ~., data=ftrain, method="class") # Classification model - no min bucket or CP (using defaults)
anmCART.pred = predict(anmCART.model, newdata=ftest, type="class")
# EVALUATE
prp(anmCART.model)
table(anmCART.pred)
(1652)/nrow(ftest) # Accruacy 0.8834225


#******************* RANDOM FOREST MODELS ***********************
library(randomForest)
set.seed(123)
anmRF.model = randomForest(Popular ~., data=ftrain,na.action = na.omit)
anmRF.pred = predict(anmRF.model, newdata=ftest)
table(anmRF.pred)
(293+21)/nrow(testSparse) # Accuracy  













# Now we can prepare our submission file for Kaggle:
# Logistic Regression - 0.90552
anmLog.model = glm(Popular ~ . - SectionName - Weekday - X2014 - X2015 - can - make - week - will - pari - daili - springsumm - small , data = ftrain,family="binomial")
summary(anmLog.model)
anmLog.pred = predict(anmLog.model, newdata= ftest, type = "response")
MySubmission = data.frame(UniqueID = ntest$UniqueID, Probability1 = anmLog.pred)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

# CART - 0.75347
anmCART.model = rpart(Popular ~., data=ftrain, method="class") # Classification model - no min bucket or CP (using defaults)
anmCART.pred = predict(anmCART.model, newdata=ftest, type="class")
rm(MySubmission)
MySubmission = data.frame(UniqueID = ntest$UniqueID, Probability1 = anmCART.pred)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

# Random Forest - 0.89919
anmRF.model = randomForest(Popular ~ . - SectionName - Weekday - X2014 - X2015 - can - make - week - will - pari - daili - springsumm - small , data=ftrain)
anmRF.pred = predict(anmRF.model, newdata=ftest)
rm(MySubmission)
MySubmission = data.frame(UniqueID = ntest$UniqueID, Probability1 = anmRF.pred)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)
