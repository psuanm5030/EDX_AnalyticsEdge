# FINAL - Airport Delays
# - Linear Regression
# - Correlation 
# - SSE / SST / R2
# - CART / Confusion Matrix

setwd("/users/miller/desktop/r_edx/FINAL")
Airlines = read.csv("AirlineDelay.csv")

# SPLIT - Randomly
set.seed(15071)
spl = sample(nrow(Airlines), 0.7*nrow(Airlines))
# Normally use sample.split function, however The sample.split function is typically used to split data with a categorical dependent variable, and we have a continuous dependent variable.
AirlinesTrain = Airlines[spl,]
AirlinesTest = Airlines[-spl,]


# Linear Regresssion Model
model1 = lm(TotalDelay ~., data = AirlinesTrain)
summary(model1) # R2 - 0.09475

# Correlation 
cor(AirlinesTrain$NumPrevFlights, AirlinesTrain$PrevFlightGap) # -0.6520532
cor(AirlinesTrain$OriginAvgWind, AirlinesTrain$OriginWindGust) # 0.5099535

#the coefficient is defined as the change in the prediction of the dependent variable per unit change in the independent variable in question. 

# In the linear regression model, given two flights that are otherwise identical, what is the absolute difference in predicted total delay given that one flight is on Thursday and the other is on Sunday?
1.571501 + (5.418356) # 6.989857
# In the linear regression model, given two flights that are otherwise identical, what is the absolute difference in predicted total delay given that one flight is on Saturday and the other is on Sunday?
-4.506943 - -5.418356 # 0.911413

# SSE Sum of Squared Errors
model1.pred = predict(model1,newdata = AirlinesTest)
SSE = sum((model1.pred - AirlinesTest$TotalDelay)^2)
SSE # 4744764

# What is the TEST R2
SST = sum((mean(AirlinesTrain$TotalDelay) - AirlinesTest$TotalDelay)^2)
SST # 5234023
R2 = 1 - (SSE / SST)
R2 # 0.09347674
# Predictions on the test set can be computed with the predict function. Then the SSE can be computed by taking the sum of the squared differences between the predictions and the actual values, the SST can be computed by taking the sum of the squared differences between the mean on the training set and the actual values, and the R-squared can be computed as 1 - SSE/SST.

Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))
table(Airlines$DelayClass)
Airlines$TotalDelay = NULL

# Split the data - Sample.Split
library(caTools)
set.seed(15071)
spl = sample.split(Airlines$DelayClass,SplitRatio = 0.7)
Atrain = subset(Airlines,spl == TRUE)
Atest = subset(Airlines,spl == FALSE)

# CART Model
library(rpart)
library(rpart.plot)

# Create Model using RPART function
model.cart = rpart(DelayClass ~ ., data=Atrain, method="class")
# PLOT THE TREE
prp(model.cart)
# Predictions
model.cart.pred = predict(model.cart, newdata = Atest, type="class")
# Confusion Matrix
table(Atest$DelayClass,model.cart.pred)
(153+1301)/(nrow(Atest)) # Accuracy - 0.5167022

# What is the accuracy on the training set of a baseline model that predicts the most frequent outcome (No Delay) for all observations?
table(Atrain$DelayClass)
3282/nrow(Atrain) # Baseline Accur - 0.4997716

# According to the CART model, the best predictor of future delays is historical delays, which helps improve our predictive ability. Turning this problem into a classification problem did not really improve our ability to predict delays, and the CART model didn't really give us any insights that we didn't see in the linear regression model. However, the CART model is very simple, and could be easier to interpret.










