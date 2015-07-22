#Moneyball
#95 wins to make it to the playoffs.
#To win 95, a team needed 135 more runs than they allowed during the regular season.

# Loading data
baseball = read.csv('baseball.csv')
str(baseball)

# Subset Data
moneyball = subset(baseball, Year < 2002)
str(moneyball)

#Linear Regression to predict wins using the diff between Runs scored vs. allowed
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
#check to see if there's a linear relationship between RD and Wins.
plot(moneyball$RD, moneyball$W)

# Create Model
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)
# RD is very significant with three stars,
# and the R-squared of our model is 0.88.
# So we have a strong model to predict wins
# using the difference between runs scored and runs allowed.

# We need to confirm the calim that a team needs to score at least 135 more
# runs than they allow to win 95 games.
# Intercept Term (Wins) 80.8814 plus the coefficient for RD, 0.1508.
# W = 80.8814 + 0.1058(RD)
# W >= 95
# We want Wins to be greater than or equal to 95
# This will be true if and only if our regression equation
# is greater than or equal to 95.
# 80.8814 + 0.1058(RD) >= 95
# RD >= (95 - 80.8814) / 0.1058 
# = 133.4

# Use linear regression in R, to verify which baseball statistics are
# important for predicting runs scored.
str(moneyball)
# Want to see if we can use linear regression to predict
# runs scored, RS, using these three hitting statistics--
# on-base percentage (OBP), slugging percentage (SLG) and batting average (BA).
RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg) #.9302
# We can see that the coefficient for batting average
# is negative, which implies that, all else being equal,
# a team with a lower batting average
# will score more runs, which is a little counterintuitive.
# This is due to multicollinearity (since these are all highly correlated)
RunsReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg) # R2 = .9296
# The R2 is similar, but uses only two variables, overall a better model.
# OBP has a larger coefficient than SLG, meaning OBP is worth more.
# Now we can confirm the claims.

# Predicting Runs Allowed
runsAllow = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(runsAllow)
# So runs allowed is equal to: 
# Runs Allowed = -837.38 + 2913.60(OOBP) + 1514.29(OSLG) 
# R2 = .91  --- strong model with both variables are significant
# Key insight - strong models can be made with very few independent variables.


# Rank
teamRank = c(1,2,3,3,4,4,4,4,5,5) # Rank Vector
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012) # 0.3477129
cor(teamRank,wins2013) # -0.6556945












