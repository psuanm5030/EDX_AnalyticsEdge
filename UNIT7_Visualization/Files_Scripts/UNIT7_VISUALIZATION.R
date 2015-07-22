# Unit 7 - Lecture 1 - Visualization

setwd("~/Desktop/R_EDX/UNIT7/Files_Scripts")

#Load it baby
WHO = read.csv("WHO.csv")
str(WHO)

# ORiginal Scatter Plot
plot(WHO$GNI,WHO$FertilityRate)

# Using GGPLOT2
library(ggplot2)
# Remember we need three things: 
# 1) Data, 2) An aesthetic mapping of variables in the data frame to visual output,
scatterplot = ggplot(WHO, aes(x=GNI,y=FertilityRate))
# 3) and a geometric object (this is the main difference between R plots and GGPLot)
scatterplot + geom_point() # uses points
scatterplot + geom_line() # uses lines


# Re-do the plot
scatterplot + geom_point(color="blue",size=3, shape=17) # traingles
scatterplot + geom_point(color="darkred",size=3, shape=8) # stars
scatterplot + geom_point(color="darkred",size=3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income") # stars with Title

# Save to file
fertilityGNIplot = scatterplot + geom_point(color="darkred",size=3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income") # stars with Title
pdf("MyPlot.pdf") # Create a file
print(fertilityGNIplot) # Print to the file
dev.off() # CLOSES THE FILE

colors() # SHOWS YOU ALL AVAILABLE COLORS

# COLOR THE POINTS BY REGION!!
ggplot(WHO, aes(x=GNI, y = FertilityRate, size = 5,color =Region)) + geom_point()

# Coloring by a numerical variable
ggplot(WHO, aes(x=GNI, y = FertilityRate, size = 5,color =LifeExpectancy)) + geom_point()
# This next plot shows non-log.  Shows non-linear relationship
ggplot(WHO, aes(x=FertilityRate, y = Under15, size = 5)) + geom_point()

# This next plot shows LOG.  Shows linear relationship.
ggplot(WHO, aes(x=log(FertilityRate), y = Under15, size = 5)) + geom_point()
# Now lets build a simple linear regression model.
model = lm(Under15~log(FertilityRate), data=WHO)
summary(model)
# If we instead had just used the FertilityRate,
# the R-squared would have been 0.87.
# That's a pretty significant decrease in R-squared vs. the log one at .9391.

# add regression line to our plot
ggplot(WHO, aes(x=log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm")
# By default, ggplot will draw a 95% confidence
# interval shaded around the line.

# Want a 99% confidence interval??
ggplot(WHO, aes(x=log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", level = 0.99)

# Want NO confidence interval??
ggplot(WHO, aes(x=log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se = FALSE, color ="orange")


# QUICK QUESTION
  
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer(palette="Dark2") # Gives dark pallette















