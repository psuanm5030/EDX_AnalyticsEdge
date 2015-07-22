# INTERNET PRIVACY POLL

poll = read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)
nrow(poll) # tells you the number of rows in the df

#LOADING AND SUMMARIZING DATA
#Number of ppl with smartphones
table(poll$Smartphone)

# Using TABLE with two values
# The possible values of the first variable listing on LEFT
# second value on TOP
table(poll$Sex, poll$Region)
table(poll$State,poll$Region)
#Or we could have done subset
MidwestInterviewees = subset(poll, Region=="Midwest")
sort(table(MidwestInterviewees$State))
SouthInterviewees = subset(poll, Region=="South")
table(SouthInterviewees$State)

#INTERNET AND SMARTPHONE USERS  
#Users that didnt use internet and dont have smartphone
table(poll$Internet.Use,poll$Smartphone)
#Missing value for their internet use
summary(poll$Smartphone)
#Create a subset
limited = subset(poll, Internet.Use == TRUE | Smartphone == TRUE)
nrow(limited)
summary(limited)
mean(limited$Info.On.Internet)
#How many reported a specific value for a variable?
table(limited$Info.On.Internet)
#Proportion of interviewees who answered worry.about.info question worry 
# about how much info is available about them on the internet
table(limited$Worry.About.Info)
# OR Simply look at the mean value from the summary table
summary(limited)

#RELATING DEMOGRAPHICS TO POLLING RESULTS
#Build a historgram
hist(limited$Age)
#Plot
plot(limited$Age, limited$Info.On.Internet)
# What is the largest num of overlapping points in the plot - 
# i.e., exactly the same value in their age variable AND the same value
# in their info.on.internet variable
table(limited$Age,limited$Info.On.Internet)
#More efficiently??
max(table(limited$Age, limited$Info.On.Internet))
#AVOID POINTS COVERING EACH OTHER UP
#jitter adds or subtracts a small amount of random noise to the values 
#passed to it, and two runs will yield different results
jitter(c(1, 2, 3, 3))
# Now, using jitter with plot
plot(limited$Age,limited$Info.On.Internet) # WITHOUT JITTER
plot(jitter(limited$Age), jitter(limited$Info.On.Internet)) # WITH JITTER
#Shows us that: Older age seems moderately associated with a smaller value for Info.On.Internet 

# Using tapply() - VERY USEFUL
tapply(limited$Info.On.Internet,limited$Smartphone,summary)
tapply(limited$Tried.Masking.Identity,limited$Smartphone,summary)
