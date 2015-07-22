# Assignment 3 - Lending Club

setwd("~/Desktop/R_EDX/UNIT3/Files_Scripts")

#Load it baby
loans = read.csv("loans.csv")
str(loans)
summary(loans)

#proportion of the loans in the dataset were not paid in full? 
table(loans$not.fully.paid)
1533/(1533+8045) # 0.1600543 not fully paid

# Missing values
miss = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
miss
nrow(miss)
table(miss$not.fully.paid)
12/62

# Multiple Imputation to fill in the missing values
# Imputation predicts missing variable values for a given observation using the 
# variable values that are reported. We called the imputation on a data frame with 
# the dependent variable not.fully.paid removed, so we predicted the missing values 
# using only other independent variables.

library(mice)
set.seed(144)
# Set vars.for.imputation to all variables in the data frame 
# except for not.fully.paid, to impute the values using all of the other 
# independent variables.
vars.for.imputation = setdiff(names(loans), "not.fully.paid") # leaves out not.fully.paid
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# Usings loans
loans = read.csv("loans_imputed.csv")
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
mod1 = glm(not.fully.paid ~.,data=train, family="binomial")
summary(mod1)

# Problem 2.2 - What is the value of Logit(A) - Logit(B)?
logitA = 9.187 + (700*-0.009317) # Loan A
logitB = 9.187 + (710*-0.009317) # Loan B 
ans = a-b
ans # Value of 0.09317
# Because Application A is identical to Application B other than having a FICO 
# score 10 lower, its predicted log odds differ by -0.009317 * -10 = 0.09317 
# from the predicted log odds of Application B.

# O(A)/O(B)?  The predicted odds of loan A not being paid back in full:
exp(logitA)/exp(logitB) # 1.097648
# Intuitively, it makes sense that loan A should have higher odds of non-payment than loan B, 
# since the borrower has a worse credit score.

# Prediction Models
predicted.risk = predict(mod1, newdata=test, type="response")
test$predicted.risk = predicted.risk
# Confusion Matrix 
table(test$not.fully.paid, predicted.risk >= 0.5)
# What is the accruacy of log reg model? 
(2400+3) / (2400+13+457+3) # 0.8364079
# What is the accuracy of the baseline?
table(test$not.fully.paid)
2413/(2413+460) #0.8398886
# 2413 predictions would be correct in the baseline model of guessing every 
# loan would be paid back in full (accuracy 2413/2873=0.8399).

# What is the AUC Value for the model
library(ROCR)
ROCRpred = prediction(test$predicted.risk,test$not.fully.paid)
as.numeric(performance(ROCRpred,"auc")@y.values)
# AUC Value on test set - 0.6720995
# The AUC deals with differentiating between a randomly selected positive and 
# negative example. It is independent of the regression cutoff selected.

# Bivariate logistic regression model 
# (aka a logistic regression model with a single independent variable) 
# that predicts the dependent variable not.fully.paid using only the 
# variable int.rate.
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
# Decreased significance between a bivariate and multivariate model is typically 
# due to correlation. From cor(train$int.rate, train$fico), we can see that the 
# interest rate is moderately well correlated with a borrower's credit score.
pred2 = predict(bivariate, newdata = test, type="response")
summary(pred2) #0.42660 - Highest predicted probablity of loan not be paid in full on test.
# With a logistic regression cutoff of 0.5, how many loans would be predicted as not 
# being paid in full on the testing set?
table(test$not.fully.paid,pred2 >= 0.5) # 0 are predicted.  
# According to the summary function, the maximum predicted probability of 
# the loan not being paid back is 0.4266, which means no loans would be 
# flagged at a logistic regression cutoff of 0.5.

#What is the AUC
ROCRpred = prediction(pred2,test$not.fully.paid)
as.numeric(performance(ROCRpred,"auc")@y.values) #0.6239081

# How much does a $10 investment with an annual interest rate of 6% pay 
# back after 3 years, using continuous compounding of interest?
c_inv = 10
rt = .06
years = 3
int_revenue = c_inv * exp(rt * years)
int_revenue

# Adding the profit to each loan
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)
which.max(test$profit)
test$profit[1780]

# An investment strategy
highInterest = subset(test, int.rate >= .15)
summary(highInterest)
#We read that the mean profit is $0.2251.
#110 of the 437 loans were not paid back in full, for a proportion of 0.2517.



# Determine the 100th smallest predicted probability of not paying in 
# full by sorting the predicted risks in increasing order and selecting 
# the 100th element of this sorted list. 
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest,highInterest$predicted.risk<=cutoff)
summary(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)






