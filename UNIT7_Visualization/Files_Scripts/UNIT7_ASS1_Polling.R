# Unit 7 - Assignment 1 - ELECTION FORECASTING REVISITED 

setwd("~/Desktop/R_EDX/UNIT7/Files_Scripts")

#Load it baby
library(ggplot2)
library(maps)
library(ggmap)
statesMap = map_data("state")
# FYI The maps package contains other built-in maps, including a US county map, a world map, and maps for France and Italy.
str(statesMap)
# How many different groups?
table(statesMap$group)
#Or 
length(table(statesMap$group)) 

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "dark blue")

# COLORING THE STATES BY PREDICTIONS
polling = read.csv("PollingImputed.csv")
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
summary(mod2)
TestPrediction = predict(mod2, newdata=Test, type="response")
# TestPrediction gives the predicted probabilities for each state, but let's also create a vector of Republican/Democrat predictions by using the following command:
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
# Now, put the predictions and state labels in a data.frame so that we can use ggplot:  
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
predictionDataFrame

# For how many states is our binary prediction 1 (for 2012), corresponding to Republican? 
predictionDataFrame # 22
#OR
table(TestPredictionBinary) # 22
# What is the average predicted probability of our model (on the Test set, for 2012)?
mean(predictionDataFrame[,1]) # 0.4852626
# Or 
mean(TestPrediction) # 0.4852626

# Need to merge the predictionDataFram with the map data (statesMap)
# need to conver Test.State variable to lowercase (to match the region var in statesMap)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
# Merge togetehr
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
# Need to make sure the observations are in order so that the map is drawn properly, by typing the following:
predictionMap = predictionMap[order(predictionMap$order),]

# Color the US with our predictions: 
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
# replot the map with discrete outcomes.  
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red",name = "Prediction 2012")

# What was our predicted probability for the state of Florida
predictionDataFrame

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
#The "alpha" parameter controls the transparency or darkness of the color. A smaller value of alpha will make the colors lighter.













