#Clearing Workspace
rm(list = ls())

#Setting Directory
setwd("C:/Users/atul/Desktop/Data Science/Project-1")

#Importing required Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
lapply(x, require, character.only = TRUE) #Checking if all the packages are available. If for any package the output is "FALSE", 
#we will install that respective package using 
#install.packages("package_name)
rm(x) #Removing the object x
#Importing the data skipping the blank lines if any
data = read.csv("train_cab.csv", blank.lines.skip = TRUE)

#Setting the dependent variable as the last column
data = data[, c(2,3,4,5,6,7,1)]

#Checking the summary of the whole data set
summary(data)

#Checking class of each column
class(data$pickup_datetime)             #factor
class(data$pickup_longitude)            #numeric
class(data$pickup_latitude)             #numeric
class(data$dropoff_longitude)           #numeric
class(data$dropoff_latitude)            #numeric
class(data$passenger_count)             #numeric
class(data$fare_amount)                 #factor

#***********************************DATA PREPROCESSING***********************************

#Data type conversion
#converting"fare_amount" as numeric
data$fare_amount = as.numeric(as.character(data$fare_amount))

#Converting "pickup_datetime" as date-time format
data$pickup_datetime = as.POSIXct(data$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

#***********************************MISSING VALUE ANALYSIS***********************************

#Checking null values
missing_value = data.frame(apply(data, 2, function(x){sum(is.na(x))}))

#Calculating Percentage of missing values
missing_value$percentage = (missing_value[, 1]/nrow(data))*100

#As the amount of missing value is too less,i.e.-less than 1% so deleting the rows won't result in any information loss
#Deleting the missing rows
data = na.omit(data)

#***********************************OUTLIER ANALYSIS***********************************

#Selecting numerical variables
numeric_index = sapply(data, is.numeric)
numeric_data = data[, numeric_index]
numerical_cnames = colnames(numeric_data)

#Imputing null values in place of outliers
for (a in numerical_cnames[-c(5,6)])                    #"passenger_count" and "fare_amount" is removed and dealt separately
{
  val = data[,a][data[,a]%in%boxplot.stats(data[,a])$out]
  data[,a][data[,a]%in%val] = NA
}

#Checking number of outliers(null values)
outliers = data.frame(apply(data, 2, function(y){sum(is.na(y))}))

#Calculating Percentage of outliers(null values)
outliers$percentage = (outliers[,1]/nrow(data))*100

#As we can see that the number of outliers(NAs) is huge so we have to go for imputation

#***********************************ONLY FOR FINDING THE BEST METHOD***********************************
#Selecting variables containing NAs
NA_index = sapply(data, anyNA)
NA_data = data[, NA_index]
NA_cnames = colnames(NA_data)

#Choosing the best method for missing value imputation

#Making a sample to check which method works best
#Choosing a sample and saving its value
sample_NA = data[50, -c(1,7)]

#Putting values of sample equal to NA for required columns
data[50,c(NA_cnames)] = NA

# duplicating data
data_duplicate = data

#MEAN Method
for(b in NA_cnames)
  data[, b][is.na(data[, b])] = mean(data[, b], na.rm = TRUE)

#rounding off passenger count as it can not be fractional
data[, 6] = round(data[,6])

sample_NA_mean = data[50, -c(1,7)]

#MEDIAN Method
data = data_duplicate
for(c in NA_cnames)
  data[, c][is.na(data[, c])] = median(data[, c], na.rm = TRUE)

sample_NA_median = data[50, -c(1,7)]

#Comparing different imputing methods
sample = rbind(sample_NA, sample_NA_mean, sample_NA_median)

#Inserting a new blank row in "sample"
sample[nrow(sample)+1, ]=NA

#Changing row names
row.names(sample) = c("sample_NA","sample_NA_mean","sample_NA_median","Best Method")

#Finding the best method of imputation for each column
for (d in (1:ncol(sample)))
{
  if(abs(as.numeric(sample[1,d])-as.numeric(sample[2,d]))<abs(as.numeric(sample[1,d])-as.numeric(sample[3,d])))
  {
    sample[4,d] = "MEAN"
  } else {
    sample[4,d] = "MEDIAN"
  }
}

#From "sample" dataframe we can find the best method for each column

#**************************************************************************************

#Imputing the best fit method for each column
#Re-Run the data till-"ONLY FOR FINDING THE BEST METHOD"
data$pickup_longitude[is.na(data$pickup_longitude)] = median(data$pickup_longitude, na.rm = TRUE)
data$pickup_latitude[is.na(data$pickup_latitude)] = mean(data$pickup_latitude, na.rm = TRUE)
data$dropoff_longitude[is.na(data$dropoff_longitude)] = mean(data$dropoff_longitude, na.rm = TRUE)
data$dropoff_latitude[is.na(data$dropoff_latitude)] = median(data$dropoff_latitude, na.rm = TRUE)

#"passenger_count" column is treated differently as on applying these method many values(4,5,6) are getting deleted which are practically possible

#***********************************DATA MANIPULATION***********************************

#Latitude range(+90 to -90)
summary(data$pickup_latitude)
summary(data$dropoff_latitude)

#Longitude range(+180 to -180)
summary(data$pickup_longitude)
summary(data$dropoff_longitude)

#Pickup and dropoff location can't be same,so counting such cases
num = 0
for (e in (1:nrow(data)))
{
  if((data[e,2] == data[e,4])&&(data[e,3] == data[e,5]))
  {
    num = num + 1
  }
}

#num = 118, so as these rows are irrelevant and count is low so best way to deal with them is deleting them
data_copy = data.frame()
for (f in (1:nrow(data)))
{
  if((data[f,2] != data[f,4])&&(data[f,3] != data[f,5]))
  {
    data_copy = rbind(data_copy, data[f, ])
  }
}

data = data_copy

#Dealing with "passenger_count" variable
#Checking unique values in "passenger_count"
table(data$passenger_count)

#In a cab upto passenger_count=6 is feasible, so checking number of instances greater than that
pc = 0
for (g in (1:nrow(data)))
{
  if(data[g,6] > 6)
  {
    pc =pc + 1
  }
}

#we got pc = 19, as this is too small so we can delete such instances
data = subset(data, data$passenger_count<=6)

#"passenger_count" can never be 0
summary(data$passenger_count)

#Putting minimum "passenger_count" equal to 1
for (h in (1:nrow(data)))
{
  if(data[h,6] < 1)
  {
    data[h,6] = 1
  }
}

#rounding off "passenger_count" as it can not be fractional
data[, 6] = round(data[,6])

#Converting "passenger_count" to factor
data$passenger_count = as.factor(data$passenger_count)

#Checking "fare_amount" to analise 
summary(data$fare_amount)

#From the frequency plot of factor "fare_amount" in Tableau we can see that number of instances of values greater than 58 is too less

#Checking the number of instances where the maximum value is greater than 58
fare_greater = 0
for (i in (1:nrow(data)))
{
  if(data[i,7] > 58)
  {
    fare_greater = fare_greater + 1
  }
}

#we got only 38 such cases
#Deleting those 38 cases
data = subset(data, data$fare_amount <= 58)

#fare can not be less than 1
#Moreover from the frequency plot we have seen that there are very less number of instances of fare less than 2.5
#So counting such instances
fare_lower = 0
for (j in (1:nrow(data)))
{
  if(data[j,7] < 2.5)
  {
    fare_lower = fare_lower + 1
  }
}
#So these 6 cases might be due to wrong input of data
#Deleting those cases
data = subset(data, data$fare_amount >= 2.5)

#***********************************FEATURE CREATION***********************************

#Creating a new column calculating actual distance travelled
#The function:distVincentyEllipsoid() used here calculates the distance between two points on the earth's surface very accurately
library(geosphere)
for (k in (1:nrow(data)))
{
  data[k,8] = distVincentyEllipsoid(c(data[k,2], data[k,3]), c(data[k,4], data[k,5]))
}

#renaming the column
names(data)[8] = "distance"

#creating new columns separating pickup_datetime
data$year = as.factor(format(as.POSIXct(data$pickup_datetime, format="%Y-%m-%d", tz = "UTC"), "%Y"))
data$month = as.factor(format(as.POSIXct(data$pickup_datetime, format="%Y-%m-%d", tz = "UTC"), "%m"))
data$weekday = as.factor(weekdays(as.Date(data$pickup_datetime)))
data$time = format(as.POSIXct(data$pickup_datetime, format="%H:%M:%S", tz = "UTC"), "%H:%M:%S")

#Self made function to change time to hourly format
To_hour = function(time){
  hour = as.numeric(format(as.POSIXct(time, format="%H:%M:%S", tz = "UTC"), "%H"))
  minute = as.numeric(format(as.POSIXct(time, format="%H:%M:%S", tz = "UTC"), "%M"))
  second = as.numeric(format(as.POSIXct(time, format="%H:%M:%S", tz = "UTC"), "%S"))
  actual_hour = hour + (minute/60) + (second/3600)
  return(actual_hour)
}

#converting the time to hourly format
data$hour = sapply(data$time, To_hour)

#Creating bins for "hour" column every hours
data$hour_bin = cut(data$hour, breaks = c(-1,1:24), by = 1, labels = (1:24))

#***********************************REMOVING UNNECESSARY COLUMNS***********************************

data = data[, -c(1,2,3,4,5,12,13)]

#Rearranging the column to put dependent variable at the end
data = data[, c(1,3,4,5,6,7,2)]

#Saving the data for Tableau Visualisations
#write.csv(data, "CAB.csv", row.names = FALSE)

#***********************************EXPOLATORY DATA ANALYSIS***********************************

library(dplyr)
library(ggplot2)

mean_fare_by_year <- data %>% group_by(year) %>% summarise(mean = mean(fare_amount))

#Bar Plot of Fare(mean) and Year
ggplot(mean_fare_by_year, aes(x = year, y = mean, fill = year)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Year") +
  ylab("Mean of Fare") +
  ggtitle("Bar Plot of Year v/s Fare(mean)") +
  theme(text = element_text(size = 15))

#So we can see that the mean fare has increased from 2012 onwards and a slight decrease in 2015

#Frequency Plot of "year"
ggplot(data, aes(data$year, fill = year)) +
  geom_bar() +
  theme_bw() +
  xlab("Year") +
  ylab("Count of users") +
  ggtitle("Frequency Plot:Year") +
  theme(text = element_text(size = 15))

#Number of users also has a harsh decrease in 2015(or there is less number of observations)
#So we can say that actual fare has some how increased from 2012 onwards as number of users was almost constant for those years
#But due to the increase in fare the number of users is decreasing from 2014 onwards and it has a harsh decrease in 2015

mean_fare_by_month <- data %>% group_by(month) %>% summarise(mean = mean(fare_amount))

#Bar Plot of Fare(mean) and Month
ggplot(mean_fare_by_month, aes(x = month, y = mean, fill = month)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Month") +
  ylab("Mean of Fare") +
  ggtitle("Bar Plot of Month v/s Fare(mean)") +
  theme(text = element_text(size = 15))

#So we can see that mean fare is almost same for all months ranging between 10 - 12.5
#The fare is just slightly high from 7th month and highest at 10th

#Frequency Plot of "Month"
ggplot(data, aes(data$month, fill = month)) +
  geom_bar() +
  theme_bw() +
  xlab("Month") +
  ylab("Count of users") +
  ggtitle("Frequency Plot:Month") +
  theme(text = element_text(size = 15))

#So here from the distribution we can see that the no. of users is high for first 6 months and low for next 6 months
#Thus, the actual fare is more in the last 6 months compared to the first 6 months
mean_fare_by_passengers <- data %>% group_by(passenger_count) %>% summarise(mean = mean(fare_amount))

#Bar Plot of Fare(mean) and Passenger_count
ggplot(mean_fare_by_passengers, aes(x = passenger_count, y = mean, fill = passenger_count)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Passenger_count") +
  ylab("Mean of Fare") +
  ggtitle("Bar Plot of Passenger_count v/s Fare(mean)") +
  theme(text = element_text(size = 15))

#So mean of fare is almost similar for any passenger group ranging from 11-12 approx.

#Frequency Plot of "Passenger_count"
ggplot(data, aes(data$passenger_count, fill = passenger_count)) +
  geom_bar() +
  theme_bw() +
  xlab("Passenger_count") +
  ylab("Count of users") +
  ggtitle("Frequency Plot:Passenger_count") +
  theme(text = element_text(size = 15))

#So number of users travelling single is very much higher compared to others

mean_fare_by_weekday <- data %>% group_by(weekday) %>% summarise(mean = mean(fare_amount))

#Bar Plot of Fare(mean) and Weekday
ggplot(mean_fare_by_weekday, aes(x = weekday, y = mean, fill = weekday)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Weekday") +
  ylab("Mean of Fare") +
  ggtitle("Bar Plot of Weekday v/s Fare(mean)") +
  theme(text = element_text(size = 15))

#So mean of fare is almost equal irrespective of weekday

#Frequency Plot of "Weekday"
ggplot(data, aes(data$weekday, fill = weekday)) +
  geom_bar() +
  theme_bw() +
  xlab("Weekday") +
  ylab("Count of users") +
  ggtitle("Frequency Plot:Weekday") +
  theme(text = element_text(size = 15))

#So number of users is almost similar for everyday and a bit high on friday and saturday and lower on sunday and monday
#So this variable shouldn't have much effect on fare amount

mean_fare_by_hours <- data %>% group_by(hour_bin) %>% summarise(mean = mean(fare_amount))

#Bar Plot of Fare(mean) and Hours
ggplot(mean_fare_by_hours, aes(x = hour_bin, y = mean, fill = hour_bin)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Hours") +
  ylab("Mean of Fare") +
  ggtitle("Bar Plot of Hours v/s Fare(mean)") +
  theme(text = element_text(size = 15))

#Mean fare is highest at 6am and other notable higher fares are at 5am, 3pm and 5pm 

#Frequency Plot of "Hours"
ggplot(data, aes(data$hour_bin, fill = hour_bin)) +
  geom_bar() +
  theme_bw() +
  xlab("Hours") +
  ylab("Count of users") +
  ggtitle("Frequency Plot:Hours") +
  theme(text = element_text(size = 15))

#The number of users is increasing from 6pm and highest during 8pm and then has a decreasing slope having lowest value during 6am
#This increase in number of users after 6pm might be due office hours
#So we can say that the actual fare remains high after midnight till 8am and becomes very high at 6am

#Scatter plot of Distance and fare amount
ggplot(data, aes_string(x = data[, 2], y = data[, 7])) +
  geom_point(aes_string(colour = data[, 1])) +
  theme_bw() +
  xlab("Distance Travelled") +
  ylab("Fare") +
  scale_colour_discrete(name = "Passenger Count") +
  ggtitle("Scatterplot of Distance and Fare")

#From the scatter plot we can as that on increasing distance the fare increases so they have a linear relationship

#Histogram plot of fare amount
ggplot(data, aes(x = data$fare_amount)) +
  geom_histogram(fill = "blue", colour = "yellow") + geom_density() +
  theme_bw() + xlab("Fare_amount") + ylab("Frequency") +
  ggtitle("Histogram plot of Fare_amount") + theme(text = element_text(size = 15))

#The distribution is slightly right skewed

#Histogram plot of logarithm of fare amount
ggplot(data, aes(x = log(data$fare_amount))) +
  geom_histogram(fill = "yellow", colour = "blue") + geom_density() +
  theme_bw() + xlab("Logarithm of Fare_amount") + ylab("Frequency") +
  ggtitle("Histogram plot of Log of Fare_amount") + theme(text = element_text(size = 15))

#Taking the log value gives us a better distribution very close to bell shape

#So taking the log of fare amount for further calculations
for (l in (1:nrow(data)))
{
  data[l,7] = log(data[l,7])
}

#***********************************FEATURE SELECTION***********************************

#Correlation Analysis
cor_index = sapply(data, is.numeric)

library(corrgram)
corrgram(data[, cor_index], order = FALSE, upper.panel = panel.pie, text.panel = panel.txt, main = "Correlation Plot")

#Calculating correlation value
cor.test(data[,2], data[,7])

#So distance(m) have a moderate positive correlation(0.56) with fare_amount
#But as 'p-value' is less than 0.05 so we will not delete this variable from our analysis

#ANOVA(Analysis of Variance) Test

#ANOVA for passenger_count
plot(fare_amount ~ passenger_count, data = data)
summary(aov(fare_amount ~ passenger_count, data = data))

#ANOVA for year
plot(fare_amount ~ year, data = data)
summary(aov(fare_amount ~ year, data = data))

#ANOVA for month
plot(fare_amount ~ month, data = data)
summary(aov(fare_amount ~ month, data = data))

#ANOVA for weekday
plot(fare_amount ~ weekday, data = data)
summary(aov(fare_amount ~ weekday, data = data))

#ANOVA for hour_bin
plot(fare_amount ~ hour_bin, data = data)
summary(aov(fare_amount ~ hour_bin, data = data))

#From the ANOVA test we will delete the variables having 'p-value' greater than 0.05
#So "weekday" needs to be deleted
data = data[, -5]

#***********************************FEATURE SCALING***********************************

#As there is only one numerical variable so feature scaling is irrelevant

#***********************************TRAIN-TEST SPLIT***********************************
#Checking the number of values in each factor variable
table(data$passenger_count)                #highly baised
table(data$year)                           #almost equally distributed
table(data$month)                          #almost equally distributed
table(data$hour_bin)                       #slightly biased

#As passenger count is highly biased so if we apply random sampling in this case
#Then there might be a chance that no observations or very few number of rows of passenger count is included
#So we need to apply stratified spliting in this case taking passenger count as reference variable

library(caret)
train.index = createDataPartition(data$passenger_count, p = 0.8, list = FALSE)
training_set = data[train.index,]
test_set = data[-train.index,]


# library(caTools)
# set.seed(12345)
# split = sample.split(data$fare_amount, SplitRatio = 0.8)
# 
# training_set = subset(data, split == TRUE)
# test_set = subset(data, split == FALSE)

#***********************************MODEL BUILDING***********************************

#Linear Regression
LR_model = lm(fare_amount ~ ., data = training_set)
summary(LR_model)
predictions_LR = predict(LR_model, test_set[, -6])

#VIF(Variance Inflation factor) Test
library(car)
#install.packages("car")
vif(LR_model)

#So we can see that VIF value of all the variable is low and very close to ideal
#Thus we can conclude that there is no multicollinearity in the model

#Calculating different Error Metrics
library(DMwR)
LR = regr.eval(exp(test_set[, 6]), exp(predictions_LR) , stats = c("mape", "rmse"))

#Decision Tree Regressor
library(rpart)
DT_model = rpart(fare_amount ~ ., data = training_set, method = "anova")
predictions_DT = predict(DT_model , test_set[, -6])

#Calculating different Error Metrics
DT = regr.eval(exp(test_set[, 6]), exp(predictions_DT) , stats = c("mape", "rmse"))

#Visualising the decision tree
library(rpart.plot)
#install.packages("rpart.plot")
rpart.plot(DT_model, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

#Random Forest Regression
library(randomForest)
RF_model = randomForest(x = training_set[1:5], y = training_set$fare_amount)
predictions_RF = predict(RF_model, test_set[, -6])

#Calculating different Error Metrics
RF = regr.eval(exp(test_set[, 6]), exp(predictions_RF) , stats = c("mape", "rmse"))

#Support Vector Regression(SVR)
library(e1071)
SVR_model = svm(formula = fare_amount ~ ., data = training_set, type = "eps-regression")
predictions_SVR = predict(SVR_model, test_set[, -6])

#Calculating different Error Metrics
SVR = regr.eval(exp(test_set[, 6]), exp(predictions_SVR) , stats = c("mape", "rmse"))

#To compare all models
Error_metrics = as.data.frame(cbind(LR,DT,RF,SVR))
#Changing row names
row.names(Error_metrics) = c("MAPE","RMSE") 

#The SVR(Support Vector Regression) has the lowest value of error
#So SVR is choosen as the BEst Model

#*************************************************************************

#TEST DATA
#Importing the data skipping the black lines if any
test = read.csv("test.csv")

#Converting "pickup_datetime" as date-time format
test$pickup_datetime = as.POSIXct(test$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz = "UTC")

#Distance Calculation
for (n in (1:nrow(test)))
{
  test[n,7] = distVincentyEllipsoid(c(test[n,2], test[n,3]), c(test[n,4], test[n,5]))
}

#renaming the column
names(test)[7] = "distance"

#creating new columns separating pickup_datetime
test$year = as.factor(format(as.POSIXct(test$pickup_datetime, format="%Y-%m-%d", tz = "UTC"), "%Y"))
test$month = as.factor(format(as.POSIXct(test$pickup_datetime, format="%Y-%m-%d", tz = "UTC"), "%m"))
test$weekday = as.factor(weekdays(as.Date(test$pickup_datetime)))
test$time = format(as.POSIXct(test$pickup_datetime, format="%H:%M:%S", tz = "UTC"), "%H:%M:%S")

#converting the time to hourly format
test$hour = sapply(test$time, To_hour)

#Creating bins for "hour" column every hours
test$hour_bin = cut(test$hour, breaks = c(-1,1:24), by = 1, labels = (1:24))

#Converting "passeneger_count" to factor
test$passenger_count = as.factor(test$passenger_count)

#Selecting Required Columns
test = test[, c(6,7,8,9,13)]

#***********************************PREDICTING RESULTS***********************************

#Applying the best model(SVR) to whole data set
Best_Model = svm(formula = fare_amount ~ ., data = data, type = "eps-regression")

#Predicitng the Results
Test_Predictions = predict(Best_Model, test)

#Actual predictions taking antilog
Actual_Predictions = exp(Test_Predictions)