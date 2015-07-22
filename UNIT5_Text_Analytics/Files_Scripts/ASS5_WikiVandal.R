# Unit 5 - Text - Assignment 1 - Wikipedia Vandalism

setwd("~/Desktop/R_EDX/UNIT5/Files_Scripts")

#Load it baby
# IMPORTANT: When working on text analytics - you need to add an extra arg to make
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)
str(wiki)

# Corpus time (Bag of Words) - WORDS ADDED 
# Text is already lowercase and stripped of punctuation
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
# Remove stopwords
corpusAdded = tm_map(corpusAdded,removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded,stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
# Remove the sparse terms
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded
# Convert to DF
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded)) # Prepend all words with the letter A

# Corpus time (Bag of Words) - WORDS REMOVED 
# Text is already lowercase and stripped of punctuation
corpusRemoved = Corpus(VectorSource(wiki$Removed))
# Remove stopwords
corpusRemoved = tm_map(corpusRemoved,removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved,stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
# Remove the sparse terms
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
# Convert to DF
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved)) # Prepend all words with the letter A
str(wordsRemoved)

# COMBINE THE DATA FRAMES!
#The cbind function combines two sets of variables for the same observations 
# into one data frame.
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

# Split the data and Build a model
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)
# Accruacy on the test set of a BASELINE method that always predicts "not vandalism"
table(test$Vandal)
618 / nrow(test) # Accruacy 0.5313844

# Build a CART Model
library(rpart)
library(rpart.plot)
vandalCART = rpart(Vandal ~., data=train,method = "class")
prp(vandalCART)
# Evaluation
predCART = predict(vandalCART, newdata=test, type = "class") # automatically uses .5 threshold
table(test$Vandal,predCART)
(618+12)/nrow(test) # Accuracy 0.5417025

# Copy the DF
wikiWords2 = wikiWords
# Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)
# New test and Training
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
#New CART Model
newCART = rpart(Vandal~.,data=wikiTrain2,method="class")
pred.newCART = predict(newCART,newdata=wikiTest2,type="class")
table(wikiTest2$Vandal,pred.newCART)
(609+57)/nrow(wikiTest2) # Accuracy 0.5726569

#Sum the rows of dtmAdded and dtmRemoved and add them as new variables 
# in your data frame wikiWords2 
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2)
# What is the average number of words added??
mean(wikiWords2$NumWordsAdded)
#New CART Model
# New test and Training
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
new2CART = rpart(Vandal~.,data=wikiTrain3,method="class")
pred.new2CART = predict(new2CART,newdata=wikiTest3,type="class")
table(wikiTest3$Vandal,pred.new2CART)
(514+248)/nrow(wikiTest3) # Accuracy 0.6552021


wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
new3CART = rpart(Vandal~.,data=wikiTrain4,method="class")
pred.new3CART = predict(new3CART,newdata=wikiTest4,type="class")
table(wikiTest4$Vandal,pred.new3CART)
(595+241)/nrow(wikiTest4) # Accuracy 0.7188306
prp(new3CART)
# You can plot the tree with prp(wikiCART4). The first split is on the 
# variable "Loggedin", the second split is on the number of words added, 
# and the third split is on the number of words removed.
# 
# By adding new independent variables, we were able to significantly 
# improve our accuracy without making the model more complicated!