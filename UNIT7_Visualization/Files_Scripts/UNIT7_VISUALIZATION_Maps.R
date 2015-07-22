# Unit 7 - Lecture 2 - Visualization (MORE)

setwd("~/Desktop/R_EDX/UNIT7/Files_Scripts")

#Load it baby
mvt = read.csv("mvt.csv", stringsAsFactors=FALSE) # since we have a text field
str(mvt)

# Convert Date into proper format (its Char now)
mvt$Date = strptime(mvt$Date,format="%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

table(mvt$Weekday)
# Save as a data frame to pass to a plot later
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

# Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) # groups all data in one line
# But shit, the days are not in order
# Can make an ORDERED FACTOR variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday","Monday", "Tuesday","Wednesday", "Thursday", "Friday","Saturday"))

ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# Quick Question
# Make dashed line
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")
# Make lighter line color
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# Add the hour to the line plote, then CREATE A HEATMAP
# Create a counts table
table(mvt$Weekday, mvt$Hour)

DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)
# This is how we convert a factor variable to a numeric variable.
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, col=Var1, size=2), alpha=0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# Heatmap
# First, though, we need to fix the order of the days
# so that they'll show up in chronological order
# instead of in alphabetical order.
# Can make an ORDERED FACTOR variable
str(DayHourCounts)
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday"))
# Add Legend Title and Remove / Blank out a label (Y-axis)
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())
# Change Color Scheme (common color scheme in policing)
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white",high="red") + theme(axis.title.y = element_blank())


# PLotting Crime on a Map of Chicago
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)
# Load map of chicago into R
chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)
# Plot the first 100 Motor Vehicle Thefts
ggmap(chicago) + geom_point(data=mvt[1:100,],aes(x=Longitude, y=Latitude))
# If we plotted all 190,000 motor vehicle thefts,
# we would just see a big black box,
# which wouldn't be helpful at all.
# We're more interested in whether or not
# an area has a high amount of crime,
# so let's round our latitude and longitude
# to two digits of accuracy and create a crime counts data
# frame for each area.
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
str(LatLonCounts)
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1)) #Converts a factor var
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2)) #Converts a factor var
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long,y=Lat,color = Freq, size=Freq))
# Change the color scheme
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long,y=Lat,color = Freq, size=Freq)) + scale_color_gradient(low="yellow", high="red")
# use geom_tile to make something that looks more like a traditional heat map.
# HEAT MAP!!!
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long,y=Lat,alpha=Freq), fill="red")

# Quick Question
LatLonCounts2 = subset(LatLonCounts, Freq > 0)
str(LatLonCounts2)
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long,y=Lat,alpha=Freq), fill="red")

# USA Heat Map (Murders - from FBI data)
murders = read.csv("murders.csv")
str(murders) # 50 states and Wash DC
statesMap = map_data("state") # This map is included with R
str(statesMap)
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color = "black") # Group is the variable that defines how to draw the US into groups by state.
# In the murders data frame, our state names
# are in the State variable, and they
# start with a capital letter.
# But in the statesMap data frame, our state names
# are in the region variable, and they're all lowercase.
# So let's create a new variable called region in our murders
# data frame to match the state name variable in the statesMap
# data frame.
murders$region = tolower(murders$State) # Simply converts to lowercase and adds as new var
# Join the two data frames - Region
murderMap = merge(statesMap, murders, by="region") # By is the identifier used to merge the rows
str(murderMap)
ggplot(murderMap, aes(x=long,y=lat, group=group, fill=Murders)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")
# So it looks like California and Texas
# have the largest number of murders.
# But is that just because they're the most populous states?
# Let's create a map of the population of each state
# to check.
ggplot(murderMap, aes(x=long,y=lat, group=group, fill=Population)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")
# EXACTLY THE SAME!
# Murder rate variable creation
murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000 # created a new variable that's the number of murders per 100,000 population.
# Redo the Pot 
ggplot(murderMap, aes(x=long,y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")

# Redo the Pot - No Wash DC 
ggplot(murderMap, aes(x=long,y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend", limits=c(0,10)) #Added the limits argument (excludes wash DC)

# Quick Question - Gun Ownership
ggplot(murderMap, aes(x=long,y=lat, group=group, fill=GunOwnership)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend") 






