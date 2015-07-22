# Get and set the working directory
getwd()
setwd("/users/miller/desktop/r_edx/UNIT1/Files_Scripts")
# Read in the following files:
GE = read.csv("GEStock.csv")
IBM = read.csv("IBMStock.csv")
PG = read.csv("ProcterGambleStock.csv")
K = read.csv("CocaColaStock.csv")
BA = read.csv("BoeingStock.csv")
summary(GE)
str(GE)
str(IBM)
str(PG)
str(K)
str(BA)

# Convert the format of the date information - overwriting the original date
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
K$Date = as.Date(K$Date, "%m/%d/%y")
PG$Date = as.Date(PG$Date, "%m/%d/%y")
BA$Date = as.Date(BA$Date, "%m/%d/%y")

# Find the earliest year in the dataset
summary(GE$Date)
summary(IBM$Date)
summary(K$Date)
summary(PG$Date)
summary(BA$Date)

# Find the mean stock price
summary(GE$StockPrice)
summary(IBM$StockPrice)
summary(K$StockPrice)
summary(PG$StockPrice)
summary(BA$StockPrice)

# Find the SD of PG over the time period
sd(PG$StockPrice)

# Visualizing stock prices - Using PLOT
# since we have continuous time period, we want lines --> type='l'
plot(K$Date,K$StockPrice, type='l',col='red') #added the color argument

# Using the LINES Function instead of plot - ADDS TO THE PLOT
lines(PG$Date, PG$StockPrice, col='blue', lty=2) #changed to a dashed line
# Draw a vertical line at a particular date
abline(v=as.Date(c("1983-03-01")), lwd=2) # lwd = 2 (makes the line THICKER)

# Now we want all FIVE companies on the same plot
# First command adds K from 1995-2005
plot(K$Date[301:432], K$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
# Add the rest of the companies (to see all colors - type colors() into console)
lines(PG$Date, PG$StockPrice, col = 'blue')
lines(IBM$Date, IBM$StockPrice, col = 'green')
lines(BA$Date, BA$StockPrice, col = 'orange')
lines(GE$Date, GE$StockPrice, col = 'black')
abline(v=as.Date(c("2004-01-01")), lwd=2) # lwd = 2 (makes the line THICKER)
abline(v=as.Date(c("2005-12-31")), lwd=2) # lwd = 2 (makes the line THICKER)

# Monthly Trends - Using tapply
# Calculate the mean stock price, sorted by months
tapply(IBM$StockPrice,months(IBM$Date),mean) # 2nd Arg. is to sort by months
mean(IBM$StockPrice) # Average stock price is $144.38
tapply(GE$StockPrice,months(GE$Date),mean) # 2nd Arg. is to sort by months
mean(GE$StockPrice) # Average stock price is $59.3035
tapply(K$StockPrice,months(K$Date),mean) # 2nd Arg. is to sort by months
mean(K$StockPrice) # Average stock price is $60.02973
tapply(BA$StockPrice,months(BA$Date),mean) # 2nd Arg. is to sort by months
mean(BA$StockPrice) # Average stock price is $46.59293
tapply(PG$StockPrice,months(PG$Date),mean) # 2nd Arg. is to sort by months
mean(PG$StockPrice) # Average stock price is $77.70452