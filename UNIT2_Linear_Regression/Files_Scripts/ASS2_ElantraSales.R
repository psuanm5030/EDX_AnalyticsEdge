# Assignment 2 - Elantra Sales

# Read in
eAll = read.csv("elantra.csv")
eTrain = subset(eAll,Year<=2012)
eTest = subset(eAll,Year>2012)
summary(eTrain)
summary(eTest)
str(eTrain)
str(eTest)

# Linear Regression Model
eModel = lm(ElantraSales~Unemployment + CPI_all + CPI_energy + Queries, data = eTrain)
summary(eModel) #R2 - 0.4282
# IMPORTANT: the coefficient is defined as the change in the prediction 
# of the dependent variable (ElantraSales) per unit change in the independent 
# variable in question (Unemployment).
# The statistical significance (PR(>|t|)) indicates how likely it is that, by chance, 
# the true coefficient is not different from zero.

# MODELING SEASONALITY
#Since our data includes the month of the year in which the units were sold, 
#it is feasible for us to incorporate monthly seasonality.
eModel2 = lm(ElantraSales~Month + Unemployment + CPI_all + CPI_energy + Queries, data = eTrain)
summary(eModel2) #R2 - 0.4344 BUT ADJUSTED R2 went DOWN - 0.3402 (from 0.3544)
# INSIGHTS: The model is not better because the adjusted R-squared has gone down 
# and none of the variables (including the new one) are very significant. 
# - (ordinary) R-Squared always increases (or at least stays the same) 
#   when you add new variables.
# - the adjusted R-Squared is the R-Squared but adjusted to take into 
#   account the number of variables.

#A NEW MODEL
# Turn Month into a factor variable
eTrain$MonthF = as.factor(eTrain$Month)
eTest$MonthF = as.factor(eTest$Month)
eModel2 = lm(ElantraSales~ MonthF + Unemployment + CPI_all + CPI_energy + Queries, data = eTrain)
summary(eModel2) #R2 - 0.8193
# PROBLEM- SOM OF OUR COEFFICIENTS CHANGED (CPI_energy is now positive - this 
# doesnt make sense.  Neither does Queries.  This is due to multicollinearity).
cor(eAll)
# Correlations of all the relevant variables!!
cor(eTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# Refine the Model - Remove Queries
eModel3 = lm(ElantraSales~ MonthF + Unemployment + CPI_all + CPI_energy, data = eTrain)
summary(eModel3) #R2 - 0.818

# Test Set Predictions
predTest1 = predict(eModel3, newdata = eTest)
SSE = sum((predTest1 - eTest$ElantraSales)^2)
SSE
# Comparing to Baseline
mean(eTrain$ElantraSales)
# The baseline method that is used in the R-Squared calculation 
# (to compute SST, the total sum of squares) simply predicts the mean of 
# ElantraSales in the training set for every observation 
# (i.e., without regard to any of the independent variables).


# What is the TEST R2
SST = sum((mean(eTrain$ElantraSales) - eTest$ElantraSales)^2)
SST
R2 = 1 - (SSE / SST)
R2 #0.7280232

# WHAT IS THE LARGEST ABS ERROR THAT WE MAKE IN OUR TEST SET PREDICTIONS
max(abs(predTest1 - eTest$ElantraSales)) #7491.488

# What period do we make the largest error
predTest1 - eTest$ElantraSales
which.max(abs(predTest1 - eTest$ElantraSales)) #Returns the ROW Number
eTest[5]
eTest
