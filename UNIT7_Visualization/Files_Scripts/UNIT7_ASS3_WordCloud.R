# Unit 7 - Assignment 3 - Visualizing Text Data Using Word Clouds

setwd("~/Desktop/R_EDX/UNIT7/Files_Scripts")

#Load it up
tweets = read.csv("tweets.csv",stringsAsFactors = FALSE)


# Create a corpus
library(tm)
corpust = Corpus(VectorSource(tweets))
corpust = tm_map(corpust, tolower)
corpust = tm_map(corpust, PlainTextDocument)
corpust = tm_map(corpust, removePunctuation)
corpust = tm_map(corpust, removeWords, c("apple",stopwords("english")))
frequencies = DocumentTermMatrix(corpust)
allTweets = as.data.frame(as.matrix(frequencies))
allTweets

# Wordclous
install.packages("wordcloud")
library(wordcloud)
words = colnames(allTweets) # Returns a vector of all words
words
freqs = colSums(allTweets) # Returns a vector of all counts
freqs
# Each tweet represents a row in allTweets, and each word represents a column. Therefore, we need to access the sums of each column in allTweets, which is returned by colSums(allTweets).

# Building word Cloud
wordcloud(words,freqs,scale=c(2,0.25), min.freq = 5)
# Now going back to corpus to remove the word "Apple"

# Negative tweets
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))


brewer.pal()
display.brewer.all()


brewer.pal(9, "Blues")[c(-1, -2, -3, -4)] 
brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)] 
# The fourth option limits to elements 5-9, which removes the first four. The second option uses negative indexes, which means remove elements 1-4. The first and third options actually keep colors 1-4, discarding the rest.
# 
# A shorthand for this indexing is:
#   
#   brewer.pal(9, "Blues")[-1:-4]
# 
# brewer.pal(9, "Blues")[5:9]
# 
# 
# 
# 
# 


