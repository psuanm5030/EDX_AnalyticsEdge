# Unit 7 - Recitation - Visualization 

setwd("~/Desktop/R_EDX/UNIT7/Files_Scripts")

#Load it baby
intl = read.csv("intl.csv") 
str(intl)

ggplot(intl,aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))
# what is this stat = "identity"?
# Well, it's pretty simple.
# Geometry bar has multiple modes of operation,
# and stat = "identity" says, use the value of the y variable
# as is, which is what we want.
# The height of the bar is the value of the y variable.
# Now, there are other modes, including
# one that counts the number of rows
# for each value of x, and plots that instead.

# Plot is not very good... Lets work on it.
# X-axis is out of order - GGPLOT defaults to alphabetic.  Need an ordered factor
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)
# Make numbers look better
intl$PercentOfIntl = intl$PercentOfIntl * 100
# Fix the text that was overlying and the x-axis being all bunhced up
ggplot(intl,aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat="identity", fill="dark blue") + 
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) + 
  ylab("Percent of International Students") + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# moves the values off of the bars
library(ggmap)
intlall = read.csv("intlall.csv", stringsAsFactors = FALSE)
head(intlall)
# There are a bunch of NAs, but we want them treated like zeros
intlall[is.na(intlall)] = 0
head(intlall)
world_map = map_data("world")
str(world_map)
# Group - group for each country
world_map = merge(world_map, intlall, by.x="region", by.y="Citizenship") # calling out the names
str(world_map)

ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") + coord_map("mercator")
# There's a few things going on here.
# So first of all, all the countries
# look like big black blobs.
# What on earth is going on, you might say.
# Well, sometimes the merge can reorder the data.
# And it turns out that what the world_map data frame really is
# is actually a list of latitude and longitude points
# that define the border of each country.
# So if we accidentally reorder the data frame
# they no longer make any sense.
# And as it goes from point to point,
# the points might be on the other side of the country
# as it defines the polygon.
# So, we have to reorder the data in the correct order.

# REORDER THE DATA
world_map = world_map[order(world_map$group,world_map$order),]# Taking all the columns
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") + coord_map("mercator")
# PROBLEM - 
# Countries missing.  Some because there is no students from those countries.
# The reason China is missing is that it
# has a different name in the MIT data frame
# than in the world_map data frame.
# So when we merged them, it was dropped
# from the data set because it didn't match up.
# So to see what it's called in the MIT data frame,
# let's just do a table.
table(intlall$Citizenship)
# China (People's Republic Of) vs. China
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"
table(intlall$Citizenship) # Now consistent
world_map = merge(map_data("world"), intlall, by.x="region", by.y="Citizenship")
world_map = world_map[order(world_map$group,world_map$order),]
ggplot(world_map,aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total),color="black") + coord_map("mercator")
# So we can see that Canada, and China, and India supply
# a large number of international students to MIT.
# But it is a little bit confusing doing it
# on a per country basis, because Europe, presumably,
# has quite a few students at MIT.
# But because Europe is made up of many small countries,
# it doesn't look very impressive.

# Using different projection
ggplot(world_map,aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total),color="black") + coord_map("ortho", orientation= c(20,30,0)) # centered above north africa
ggplot(world_map,aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total),color="black") + coord_map("ortho", orientation= c(-37,175,0))

# New Video
households = read.csv("households.csv")
str(households)
# PROBLEM!!  Structure of the data frame is not conducive for ggplot (in the aes argument)
# GGPLOT needs data in the form of YEAR, GROUP, FRACTION
# SOLUTION: MELT function from RESHAPE package
# Melt will take a 2-dimensional data frame like ours,
# and convert it into exactly the right form we need for ggplot2.
library(reshape2)
households[,1:2]
head(melt(households,id="Year")) #VERY VERY Convenient
households[,1:3]
melt(households,id="Year")[1:10,] # first 10 rows

ggplot(melt(households, id="Year"),aes(x=Year,y=value,color=variable)) +geom_line(size=2) + geom_point(size=5) + ylab("Percentage of Households")
