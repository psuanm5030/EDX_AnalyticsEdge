# Assignment 3 - Music Records

#Load it baby
songs = read.csv("songs.csv")
str(songs)
# songs in 2010
table(songs$year)
# songs for Michael Jackson
df = which(songs$artistname == "Michael Jackson")
length(df) 
# OR: 
MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson) # or nrow(MichaelJackson)

# Which song made it to the top ten
MichaelJacksonTOP = subset(MichaelJackson, Top10 == TRUE)
length(MichaelJacksonTOP$songtitle)
MichaelJacksonTOP$songtitle
#--- BEST WAY ---
MichaelJackson[c('songtitle', 'Top10')]

# What is the range of timesignature variable
summary(songs$timesignature)
sort(songs$timesignature)
# What is the most frequent variable?
table(songs$timesignature) #4 is most freq

# What is the song with the highest tempo?
which.max(songs$tempo) #gives you the row
songs$songtitle[6206] # Find the songtitle for the row


# Predictions and models
#Split the data
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
str(SongsTrain)
str(SongsTest)


# Model 1
# Variables we dont want in our model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
# Remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)
# What is the correlation between loudness and energy??
cor(SongsTrain$loudness, SongsTrain$energy) #0.7399067

#Model 2 (notice how i removed loudness)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#Model 3 (notice how i removed loudness)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Validating our Model
pred3 = predict(SongsLog3, newdata = SongsTest, type="response")
table(SongsTest$Top10, pred3>0.45)
# Whats the Accuracy??
(309+19)/(314+59) #0.8793566

#Calulating Baseline
table(SongsTest$Top10)
314/(314+59) #0.8418231 - accuracy of baseline - not a top10 hit

# How many songs does Model 3 correctly predict as Top 10 hits in 2010 
# (remember that all songs in 2010 went into our test set), using a 
# threshold of 0.45?
table(SongsTest$Top10, pred3>0.45)
(309+19) #328










