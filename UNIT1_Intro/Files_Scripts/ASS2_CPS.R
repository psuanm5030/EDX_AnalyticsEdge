# Get and set the working directory
getwd()
setwd("/users/miller/desktop/r_edx/UNIT1/Files_Scripts")
# Read in the following files:
CPS = read.csv("CPSData.csv")

# Summarizing the dataset
summary(CPS) #Rich data
str(CPS) # Summary of the structure
sort(table(CPS$State))

# What proportion of interviewees are US Citizens
sort(table(CPS$Citizenship))
sum = 7073 + 7590 + 116639
result = (7073 + 116639)/sum
result

# Looking at RACE and ETHNICITY
# For which races are there at least 250 interviewees in the CPS dataset 
# of Hispanic ethnicity? 
table(CPS$Race,CPS$Hispanic)

# Which variables have "na" in the set??
summary(CPS)

# Evaluating Missing Values
is.na(CPS$Married) #returns a vector of TRUE/FALSE values for whether the Married variable is missing.
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

# Evaluating Missing Values
table(CPS$State,is.na(CPS$MetroAreaCode))

# Evaluating Missing Values - now with mean
# Shows porportion of interviewees living in a non-metro area
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))
# IMPORTANT FOR TAPPLY: Takes arg1, groups by arg2, applies arg3

# DICTIONARIES
MetroAreaMap = read.csv('MetroAreaCodes.csv')
CountryMap = read.csv('CountryCodes.csv')
str(MetroAreaMap)
str(CountryMap)

# MERGE - connecting the field MetroAreaCode from CPS with the field code in MetroAreaMap
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
# arg1 and arg2 determine the DFs to be merged (x and y), then the matching the x and y variables from the diff DFs
# all.x=TRUE means we want to keep all ros from the x df, even if some of the rows doesnt match
# Results in a left outer join (instead of an inner join)

# How many have a missing value for the new metro area variable - MetroArea
sort(summary(CPS$MetroArea))

# Integrating Metro Area Data
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

# Returning a TRUE / FALSE vector
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

# Sorted proportion of interviewees from metro areas who have not received a HS Diploma
vec1 = sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))
vec1[1:10] #shows first 10

#INTEGRATING COUNTRY OF BIRTH DATA
CPS = merge(CPS,CountryMap,by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
# Interviewees with a missing value for new country of birth variable
summary(CPS$Country)
# Among those outside North America, which country was most common place of birth
sort(table(CPS$Country))
# What proportion of interviewees from XX have a country of birth that is not USA? (remove NA)
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
# What metro has the largest sum of interviewees w/ a country of birth in india
sort(tapply(CPS$Country == 'India',CPS$MetroArea,sum,na.rm=TRUE))
sort(tapply(CPS$Country == 'Brazil',CPS$MetroArea,sum,na.rm=TRUE))
sort(tapply(CPS$Country == 'Somalia',CPS$MetroArea,sum,na.rm=TRUE))
