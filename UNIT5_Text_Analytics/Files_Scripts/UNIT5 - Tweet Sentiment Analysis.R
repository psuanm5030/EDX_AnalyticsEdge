# Unit 5 - Text - Tweets (Sentiment Analysis)

setwd("~/Desktop/R_EDX/UNIT5/Files_Scripts")

#Load it baby
# IMPORTANT: When working on text analytics - you need to add an extra arg to make
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

# Text Mining Packages
install.packages("tm")
install.packages("SnowballC") # Helps us use the TM package
library(tm)
library(SnowballC)

#One of the concepts introduced by the tm package is that of a corpus.
# A corpus is a collection of documents.
# We'll need to convert our tweets to a CORPUS for pre-processing.

# tm can create a CORPUS in many different ways, but we'll create it from the 
# tweet column of our data frame using two functions, Corpus and VectorSource.
# We'll call our corpus "corpus" and then use the Corpus and the VectorSource functions
# called on our tweets variable of our tweets data set.
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]] # Shows first item

# Pre-processing - tmMap function
# Lowercase
corpus = tm_map(corpus, tolower) #standard function in R - like when we use mean in tapply
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]
# Remove punctuation
corpus = tm_map(corpus, removePunctuation) 
corpus[[1]]
# Remove stopwords
stopwords("english")[1:100]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english"))) 
corpus[[1]]
# Stem the items
corpus = tm_map(corpus, stemDocument) 
corpus[[1]]

# Generate a matrix of word frequencies
frequencies = DocumentTermMatrix(corpus)
frequencies
# Looking at the "Sparse" data
inspect(frequencies[1000:1005,505:515]) # look at docs 1000-1005 and words 505-515
# Most popular terms
findFreqTerms(frequencies, lowfreq=20) # Min number of times a term must appear
# SO, 56 different words.  Out of the 3,289 words in our matrix, only 56 appear at least 
# 20 times in our tweets.  That means that we have a lot of terms that will be 
# useless for our prediction model.
# This is an issue: 
# 1) Computations - More terms means more independent variables,
# which usually means it takes longer to build our models.
# 2) Building models - the ratio of ind vars to observations will affect how good the 
# model will generalize.  
# Therefore - lets remove some terms.
sparse = removeSparseTerms(frequencies, 0.995) # Sparcity threshold
# means that we only keep terms taht appear in 2% or more of the tweets.  
# Here we only kept terms appearing in .5%
sparse # now only 309 terms (vs 3,289 terms in previous count)

# Convert sparse matrix into data frame
tweetsSparse = as.data.frame(as.matrix(sparse))
# Since R struggles with variable names that start with a number, run the
# make.names function to make sure all words are appropriate variable names:
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
# Add dep var to dataset
tweetsSparse$Negative = tweets$Negative
# Split the data
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse = subset(tweetsSparse,split==TRUE)
testSparse = subset(tweetsSparse,split==FALSE)

# Words appearing at least a 100 times
findFreqTerms(frequencies, lowfreq=100)

# USING CART AND LOGISTIC REGRESSION TO PREDICT NEGATIVE SENTIMENT
library(rpart)
library(rpart.plot)
# Build model - CART
tweetCART = rpart(Negative ~., data=trainSparse, method="class") # Classification model - no min bucket or CP (using defaults)
prp(tweetCART)
# Our tree says that if the word "freak" is in the tweet,
# then predict TRUE, or negative sentiment.
# If the word "freak" is not in the tweet,
# but the word "hate" is, again predict TRUE.
# If neither of these two words are in the tweet,
# but the word "wtf" is, also predict TRUE, or negative
# sentiment.

# Evaluate the numerical performance of our model - predic on test
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)
(294+18)/nrow(testSparse) # Accruacy 0.8788732
# BAseline - always predicts non-negative
table(testSparse$Negative)
300/(300+55) # Accuracy 0.8450704
# Accuracy of a baseline model that always predicts non-negative


# Build model - RANDOM FOREST
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~., data=trainSparse) # default settings
# Takes longer - so many independent vars. (300 different words)
# Predictions
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
(293+21)/nrow(testSparse) # Accuracy 0.884507 
# better accuracy than CART, but due to the interpretability of our CART model,
# the CART model is preferred.

# If you were to use cross-validation to pick the cp
# parameter for the CART model, the accuracy
# would increase to about the same as the random forest model.

# So by using a bag-of-words approach and these models,
# we can reasonably predict sentiment even
# with a relatively small data set of tweets.

# Quick Question - build logistic regression model
tweetLOG = glm(Negative~., data = trainSparse,family="binomial")
predictLOG = predict(tweetLOG, newdata=testSparse, type="response")
table(testSparse$Negative, predictLOG >0.5)
(253+32)/nrow(testSparse) # Accuracy 0.8028169
# The accuracy .80281 is worse than the baseline. If you were to compute the 
# accuracy on the training set instead, you would see that the model does really 
# well on the training set - this is an example of over-fitting. The model fits 
# the training set really well, but does not perform well on the test set. 
# A logistic regression model with a large number of variables is particularly 
# at risk for overfitting.




