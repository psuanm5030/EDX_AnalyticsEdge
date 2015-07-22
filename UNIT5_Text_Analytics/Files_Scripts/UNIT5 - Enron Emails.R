# Unit 5 - Text - Enron Emails

setwd("~/Desktop/R_EDX/UNIT5/Files_Scripts")

#Load it baby
# IMPORTANT: When working on text analytics - you need to add an extra arg to make
emails = read.csv("energy_bids.csv", stringsAsFactors=FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1]) # break down an email to read easier
emails$responsive[1] # shows us that the first email is NOT responsive
strwrap(emails$email[2])
emails$responsive[2]

# Breakdown
table(emails$responsive)

# Construct Corpus
library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
# Processing
# Lowercase / Punctuation / Stopwords
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords, stopwords("english"))
corpus = tm_map(corpus,stemDocument)

strwrap(corpus[[1]])

# Now emails are ready for the Machine Learning Algorithms.
# Build document term matrix
dtm = DocumentTermMatrix(corpus)
dtm #(855 emails, 22K terms that showed up at least once - too many)
dtm = removeSparseTerms(dtm, 0.97) # remove any term that doesnt appear in at least 3% of emails
dtm # Decreased terms to 788 - more resonable
labeledTerms = as.data.frame(as.matrix(dtm)) # ONLY includes the frequencies of the words
labeledTerms$responsive = emails$responsive
str(labeledTerms)

# Split the data and Build a model
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

# CART MODEL (train the model)
library(rpart)
library(rpart.plot)
emailCART = rpart(responsive ~., data=train,method = "class")
prp(emailCART)
# Evaluation
predCART = predict(emailCART, newdata=test)
predCART[1:10,] # all columns, only first 10 rows
# So the left column here is the predicted probability
# of the document being non-responsive.
# And the right column is the predicted probability
# of the document being responsive.
pred.prob = predCART[,2] # contains our test set predicted probabilities
table(test$responsive, pred.prob >= 0.5)
(195+25)/nrow(test) # accuracy --> 0.8560311
# Baseline model (predicts non-responsive)
table(test$responsive)
215/(215+42) # Accuracy 0.8365759 (CART is only moderately better)

# However, as in most document retrieval applications,
# there are uneven costs for different types of errors here.
# Typically, a human will still have to manually review
# all of the predicted responsive documents
# to make sure they are actually responsive.
# Therefore, if we have a false positive,
# in which a non-responsive document is labeled
# as responsive, the mistake translates
# to a bit of additional work in the manual review
# process but no further harm, since the manual review process
# will remove this erroneous result.
# But on the other hand, if we have a false negative,
# in which a responsive document is labeled as non-responsive
# by our model, we will miss the document entirely
# in our predictive coding process.
# IMPORTANT: Therefore, we're going to assign a higher cost to false negatives
# than to false positives, which makes this a good time to look
# at other cut-offs on our ROC curve.

# ROC
library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
# The best cutoff to select is entirely dependent on the costs assigned by the decision
# maker to false positives and true positives.
# However, again, we do favor cutoffs that give us a high sensitivity.
# We want to identify a large number of the responsive documents.
# Maybe a TPR of 70% (20% are FP) - threshold around .15 
# (signficantly lower than 50% since we favor false positives to false negatives)
performance(predROCR, "auc")@y.values # 0.7936323 - 
# Means that our model can differentiate between a randomly selected 
# responsive and non-responsive document about 80% of the time.
