#CAB FARE PREDICTION

#Setting the working directory
import os
os.chdir("C:/Users/atul/Desktop/Data Science/Project-1")

#Importing the data skipping the blank lines if any
import pandas as pd
data = pd.read_csv("train_cab.csv", skip_blank_lines = True)

#Setting the dependent variable as the last column
data = data.drop(columns = ['fare_amount']).assign(fare_amount = data['fare_amount'])

#Checking the summary of the whole data set
data.describe(include = "all")

#Checking class of each column
data.dtypes
#***********************************DATA PREPROCESSING***********************************

#Data type conversion
#converting"fare_amount" as numeric
data["fare_amount"] = pd.to_numeric(data["fare_amount"],errors = "coerce")                 #"coerce" will convert all non-numeric value

#Somehow the "pickup_datetime" column is containing a value '43'
#So removing the value is necessary
data = data[data.pickup_datetime != '43']

#Converting "pickup_datetime" as date-time format
data['pickup_datetime'] =  pd.to_datetime(data['pickup_datetime'], format='%Y-%m-%d %H:%M:%S UTC')

#***********************************MISSING VALUE ANALYSIS***********************************

#Checking null values
missing_value = pd.DataFrame(data.isnull().sum())

#Resetting Index
missing_value = missing_value.reset_index()

#Renaming Variable
missing_value = missing_value.rename(columns = {'index':'Variable Name', 0 : 'Missing-Percentage'})

#Calculating Missing Value Percentage
missing_value['Missing-Percentage'] = (missing_value['Missing-Percentage']/len(data))*100

#As the amount of missing value is too less,i.e.-less than 1% so deleting the rows won't result in any information loss
#Deleting the missing rows
data = data.dropna()

#***********************************OUTLIER ANALYSIS***********************************

#Selecting numerical variables
numerical_cnames = ['pickup_longitude', 'pickup_latitude', 'dropoff_longitude', 'dropoff_latitude']
#passenger_count and fare_amount is dealt separately

import numpy as np

#Detecting Outliers and replacing them with NA's
for a in numerical_cnames:
    q75, q25 = np.percentile(data.loc[:,a], [75, 25])
    iqr = q75 - q25
    min = q25 - (iqr*1.5)
    max = q75 + (iqr*1.5)
    data.loc[data[a]<min, a] = np.nan
    data.loc[data[a]>max, a] = np.nan


#Checking null values
outliers = pd.DataFrame(data.isnull().sum())

#Resetting Index
outliers = outliers.reset_index()

#Renaming Variables
outliers = outliers.rename(columns = {'index':'Variable Name', 0 : 'Missing-Percentage'})

#Calculating Missing Value Percentage
outliers['Missing-Percentage'] = (outliers['Missing-Percentage']/len(data))*100

#As we can see that the number of outliers(NAs) is huge so we have to go for imputation

#***********************************ONLY FOR FINDING THE BEST METHOD***********************************

#Making a sample to check which method works best
#Choosing a sample and saving its value
sample_NA = data.iloc[149, 1:5]

#Putting values of sample equal to NA for required columns
data.iloc[149, 1:5] = np.nan
#MEAN Method
for b in numerical_cnames :
    data[b] = data[b].fillna(data[b].mean())
    
sample_NA_mean = data.iloc[149, 1:5]
#Re_Run the above part of code without the MEAN Method

#MEDIAN Method
for c in numerical_cnames :
    data[c] = data[c].fillna(data[c].median())

sample_NA_median = data.iloc[149, 1:5]   

#Comparing different imputing methods
sample = pd.concat([sample_NA, sample_NA_mean, sample_NA_median], axis = 1)

sample.columns = ['sample_NA', 'sample_NA_mean', 'sample_NA_median']
 
#Inserting a new blank row in "sample"
sample['Best Method'] = np.nan

#Finding the best method of imputation for each column
for d in range(sample.shape[0]):
    if  (abs(sample.iloc[d, 0]-sample.iloc[d, 1]) < abs(sample.iloc[d, 0]-sample.iloc[d, 2])):
        sample.iloc[d, 3] = "MEAN"
    else:
        sample.iloc[d, 3] = "MEDIAN"


#From "sample" dataframe we can find the best method for each column
       
#**************************************************************************************

#Imputing the best fit method for each column
#Re-Run the data till-"ONLY FOR FINDING THE BEST METHOD"
data['pickup_longitude'] = data['pickup_longitude'].fillna(data['pickup_longitude'].mean())   
data['pickup_latitude'] = data['pickup_latitude'].fillna(data['pickup_latitude'].median())
data['dropoff_longitude'] = data['dropoff_longitude'].fillna(data['dropoff_longitude'].mean())          
data['dropoff_latitude'] = data['dropoff_latitude'].fillna(data['dropoff_latitude'].median()) 

#***********************************DATA MANIPULATION***********************************
#Latitude range(+90 to -90)
data["pickup_latitude"].describe()
data["dropoff_latitude"].describe()

#Longitude range(+180 to -180)
data["pickup_longitude"].describe()
data["dropoff_longitude"].describe()

#Pickup and dropoff location can't be same,so counting such cases
num = 0
for e in range(len(data)):
  if (data.iloc[e, 1] == data.iloc[e, 3])&(data.iloc[e, 2] == data.iloc[e, 4]):
    num = num + 1

#n = 118, so as these rows are irrelevant and count is low so best way to deal with them is deleting them
data = data.drop(data[(data["pickup_latitude"] == data["dropoff_latitude"])&(data["pickup_longitude"] == data["dropoff_longitude"])].index, axis=0)

#Dealing with "passenger_count" variable
#Checking unique values in "passenger_count"
count = data['passenger_count'].unique()

#"passenger_count" column is treated differently as on applying these method many values(4,5,6) are getting deleted which are practically possible

#Dealing with "passenger_count" variable

#In a cab upto passenger_count=6 is feasible, so checking number of instances greater than that
pc = 0
for f in range(len(data)):
    if data.iloc[f, 5] > 6:
        pc = pc + 1

#we got pc = 19, as this is too small so we can delete such instances    
data = data.drop(data[data["passenger_count"]> 6 ].index, axis=0)

#"passenger_count" can never be 0
data['passenger_count'].describe()

#Putting minimum "passenger_count" equal to 1
for g in range(len(data)):
    if data.iloc[g, 5] < 1 :
        data.iloc[g, 5] = 1

#rounding off "passenger_count" as it can not be fractional
data['passenger_count'] = data['passenger_count'].round()

#Converting "passenger_count" to categorical
data['passenger_count'].astype('category')

#Checking "fare_amount" to analise 
data['fare_amount'].describe()

#From the frequency plot of factor "fare_amount" in Tableau we can see that number of instances of values greater than 58 is too less

#Checking the number of instances where the maximum value is greater than 58
fare_greater = 0
for h in range(len(data)):
    if data.iloc[h, 6] > 58:
        fare_greater = fare_greater + 1

#we got only 40 such cases
#Deleting those 40 cases
data = data.drop(data[data["fare_amount"] > 58 ].index, axis=0)        

#fare can not be less than 1
#Moreover from the frequency plot we have seen that there are very less number of instances of fare less than 2.5
#So counting such instances
fare_lesser = 0
for i in range(len(data)):
    if data.iloc[i, 6] < 2.5 :
        fare_lesser = fare_lesser + 1

#So these 6 cases might be due to wrong input of data
#Deleting those cases
data = data.drop(data[data["fare_amount"] < 2.5 ].index, axis=0)

#***********************************FEATURE CREATION***********************************
#Creating a new column blank column named "distance(m)"
data["distance(km)"] = np.nan

#calculating actual distance travelled using vincenty formula
from vincenty import vincenty
for j in range(len(data)):
    data.iloc[j, 7] = vincenty((data.iloc[j, 2], data.iloc[j, 1]), (data.iloc[j, 4], data.iloc[j, 3]))

#creating new columns separating pickup_datetime
data['year'] = data['pickup_datetime'].dt.year
data['month'] = data['pickup_datetime'].dt.month
data['weekday'] = data['pickup_datetime'].dt.dayofweek
data['hour'] = data['pickup_datetime'].dt.hour
data['minute'] = data['pickup_datetime'].dt.minute   

#Creating a function to change hour and minute to hourly format
def To_hour(time):
    h = time[0]
    t = time[1]
    hour = h + t/60
    return hour

#converting the time to hourly format
data['Hour'] = data[['hour', 'minute']].apply(To_hour, axis = 1)

#Creating bins for "hour" column every hours
data['hour_bin'] = pd.cut(x = data['hour'], bins = range(-1,24), labels = range(24))

#***********************************REMOVING UNNECESSARY COLUMNS***********************************

#Saving the column names to be dropped in an object
data = data.drop(["pickup_datetime", "pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude", "hour", "minute", "Hour"], axis = 1)

#Rearranging the column to put dependent variable at the end
data = data.drop(columns = ['fare_amount']).assign(fare_amount = data['fare_amount'])

#Converting data types
data['passenger_count'] = data['passenger_count'].astype('category')
data['year'] = data['year'].astype('category')
data['month'] = data['month'].astype('category')
data['weekday'] = data['weekday'].astype('category')

#***************************************VISUALIZATIONS******************************************

import seaborn as sns

#Bar Plot of Fare(mean) and Year
sns.barplot(x = 'year', y = 'fare_amount', data = data)
#So we can see that the mean fare has increased from 2012 onwards and a slight decrease in 2015

#Frequency Plot of "Year"
sns.countplot(x = 'year', data = data)
#Number of users also has a harsh decrease in 2015(or there is less number of observations)
#So we can say that actual fare has some how increased from 2012 onwards as number of users was almost constant for those years
#But due to the increse in fare the number of users is decreasing from 2014 onwards and it has a harsh decrese in 2015

#Bar Plot of Fare(mean) and Month
sns.barplot(x = 'month', y = 'fare_amount', data = data)
#So we can see that mean fare is almost same for all months ranging between 10 - 12.5
#The fare is just slightly high from 7th month and highest at 10th

#Frequency Plot of "Month"
sns.countplot(x = 'month', data = data)
#So here from the distribution we can see that the no. of users is high for first 6 months and low for next 6 months
#Thus, the actual fare is more in the last 6 months compared to the first 6 months

#Bar Plot of Fare(mean) and Passenger_count
sns.barplot(x = 'passenger_count', y = 'fare_amount', data = data)
#So mean of fare is almost similar for any passenger group ranging from 11-12 approx.

#Frequency Plot of "Passenger_count"
sns.countplot(x = 'passenger_count', data = data)
#So number of users travelling single is very much higher compared to others

#Bar Plot of Fare(mean) and Weekdays
sns.barplot(x = 'weekday', y = 'fare_amount', data = data)
#So mean of fare is almost equal irrespective of weekdays

#Frequency Plot of "Weekday"
sns.countplot(x = 'weekday', data = data)
#So number of users is almost similar for everyday and a bit high on friday and saturday and lower on sunday and monday
#So this variable shouldn't have much on fare amount

#Bar Plot of Fare(mean) and Hours
sns.barplot(x = 'hour_bin', y = 'fare_amount', data = data)
#Mean fare is highest at 6am and other notable higher fares are at 5am, 3pm and 5pm 

#Frequency Plot of "Hours"
sns.countplot(x = 'hour_bin', data = data)
#The number of users is increasing from 6pm and highest during 8pm and then has a decresing slope having lowest value during 6am
#This increase in number of users after 6pm might be due office hours
#So we can say that the actual fare remains high after midnight till 8am and becomes very high at 6am

#Scatter plot of Distance and fare amount
sns.scatterplot(x = 'distance(km)', y = 'fare_amount', data = data)
#From the scatter plot we can as that on increasing distance the fare increases

#Histogram plot of fare amount
sns.distplot(data['fare_amount'])
#The data is slightly right skewed

#Histogram plot of logarithm of fare amount
sns.distplot(np.log(data['fare_amount']))
#Taking the log value gives us a better distribution very close to bell shape

#So taking the log of fare amount for further calculations
data.iloc[:,6] = data.iloc[:,6].apply(np.log)

#***********************************FEATURE SELECTION***********************************

#Correlation analysis
import matplotlib.pyplot as plt
num_cnames = ['distance(km)', 'fare_amount']
data_corr = data.loc[:, num_cnames]

#Set the height and width of the plot
f,ax = plt.subplots(figsize = (7,5))

#Generate correlation matrix
corr = data_corr.corr()

#plot using seaborn
import seaborn as sns
sns.heatmap(corr, mask = np.zeros_like(corr, dtype = np.bool),
            cmap = sns.diverging_palette(220, 10, as_cmap = True),
            square = True, ax = ax)

#ANOVA(Analysis of Variance) Test
import statsmodels.api as sm
from statsmodels.formula.api import ols

#ANOVA for passenger_count
data.boxplot('fare_amount', by = 'passenger_count')
passenger_count_ols = ols('fare_amount ~ passenger_count', data = data).fit()
sm.stats.anova_lm(passenger_count_ols, type = 1)

#ANOVA for year
data.boxplot('fare_amount', by = 'year')
year_ols = ols('fare_amount ~ year', data = data).fit()
sm.stats.anova_lm(year_ols, type = 1)

#ANOVA for month
data.boxplot('fare_amount', by = 'month')
month_ols = ols('fare_amount ~ month', data = data).fit()
sm.stats.anova_lm(month_ols, type = 1)

#ANOVA for weekday
data.boxplot('fare_amount', by = 'weekday')
weekday_ols = ols('fare_amount ~ weekday', data = data).fit()
sm.stats.anova_lm(weekday_ols, type = 1)

#ANOVA for hour_bin
data.boxplot('fare_amount', by = 'hour_bin')
hour_bin_ols = ols('fare_amount ~ hour_bin', data = data).fit()
sm.stats.anova_lm(hour_bin_ols, type = 1)

#From the ANOVA test we will delete the variables having 'p-value' greater than 0.05
#So "weekday" needs to be deleted

data = data.drop(["weekday"], axis = 1)

#***********************************FEATURE SCALING***********************************

#As there is only one numerical variable so feature scaling is irrelevant

#***********************************TRAIN-TEST SPLIT***********************************
#Setting the dependent variable as the last column
data = data.drop(columns = ['fare_amount']).assign(fare_amount = data['fare_amount'])

#Checking the number of values in each factor variable
data['passenger_count'].value_counts()                               #Highly biased
data['year'].value_counts()                                          #almost equally distributed
data['month'].value_counts()                                         #almost equally distributed
data['hour_bin'].value_counts()                                      #Slightly biased

#As passenger count is highly biased so if we apply random sampling in this case
#Then there might be a chance that no observations or very few number of rows of passenger count is included
#So we need to apply stratified spliting in this case taking passenger count as reference variable

from sklearn.model_selection import train_test_split
#Categorical variable to be set as an array
y = np.array(data['passenger_count'])
training_set,test_set = train_test_split(data, test_size = 0.2, stratify = y) 

#***********************************MODEL BUILDING***********************************

#Creating Error metrics calculation function:
def MAPE_RMSE(y_true, y_pred):
    mape = np.mean(np.abs((y_true - y_pred) / y_true))*100
    from sklearn.metrics import mean_squared_error
    from math import sqrt
    rmse = sqrt(mean_squared_error(y_true, y_pred))
    return [mape, rmse]

#VIF(Variance Inflation factor) Test
vif = pd.DataFrame()

from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.tools.tools import add_constant

X = add_constant(training_set.iloc[:, 0:5])
vif["Variables"] = X.columns
vif["VIF_value"] = [variance_inflation_factor(np.array(X.values,dtype = float), k) for k in range(0,6)]

#So we can see that VIF value of all the variable is low and very close to ideal
#Thus we can conclude that there is no multicollinearity in the model

#Linear Regression
import statsmodels.api as sm
LR_model = sm.OLS(training_set.iloc[:, 5], training_set.iloc[:, 0:5].astype(float)).fit()
predictions_LR = LR_model.predict(test_set.iloc[:, 0:5])

#Calculating different Error Metrics
LR = pd.DataFrame(MAPE_RMSE(np.exp(test_set.iloc[:, 5]), np.exp((predictions_LR).astype(float))))

#Decision Tree Regressor
from sklearn.tree import DecisionTreeRegressor
DT_model = DecisionTreeRegressor(random_state = 0)
fit_DT = DT_model.fit(X = training_set.iloc[:, 0:5], y = training_set.iloc[:, 5])
predictions_DT = fit_DT.predict(test_set.iloc[:, 0:5])

#Calculating different Error Metrics
DT = pd.DataFrame(MAPE_RMSE(np.exp(test_set.iloc[:, 5]), np.exp((predictions_DT).astype(float))))

#Random Forest Regression
from sklearn.ensemble import RandomForestRegressor
RF_model = RandomForestRegressor()
fit_RF = RF_model.fit(training_set.iloc[:, 0:5], training_set.iloc[:, 5])
predictions_RF = fit_RF.predict(test_set.iloc[:, 0:5])

#Calculating different Error Metrics
RF = pd.DataFrame(MAPE_RMSE(np.exp(test_set.iloc[:, 5]), np.exp((predictions_RF).astype(float))))

#Support Vector Regression(SVR)
from sklearn.svm import SVR
SVR_model = SVR(kernel = 'rbf').fit(training_set.iloc[:, 0:5], training_set.iloc[:, 5])
predictions_SVR = SVR_model.predict(test_set.iloc[:, 0:5])

#Calculating different Error Metrics
SVR = pd.DataFrame(MAPE_RMSE(np.exp(test_set.iloc[:, 5]), np.exp((predictions_SVR).astype(float))))

#To compare all models
Error_Metrics = pd.concat([LR, DT, RF, SVR], axis = 1)
Error_Metrics.columns = [ 'LR', 'DT', 'RF', 'SVR',]
Error_Metrics.rename(index = {0:'mape',1:'rmse'}, inplace = True)

#So, among the models the SVR is the best model compairing RMSE and MAPE

#*************************************************************************
#TEST DATA

#Importing the data skipping the black lines if any
test = pd.read_csv("test.csv", skip_blank_lines = True)

#converting to date-time format
test['pickup_datetime'] =  pd.to_datetime(test['pickup_datetime'], format='%Y-%m-%d %H:%M:%S UTC')

#converting 'passenger_count' to categorical
test['passenger_count'].astype('category')

#Creating new variable to calculate distance
test["distance(km)"] = np.nan

for m in range(len(test)):
    test.iloc[m, 6] = vincenty((test.iloc[m, 2], test.iloc[m, 1]), (test.iloc[m, 4], test.iloc[m, 3]))

#creating new columns separating pickup_datetime
test['year'] = test['pickup_datetime'].dt.year
test['month'] = test['pickup_datetime'].dt.month
test['weekday'] = test['pickup_datetime'].dt.dayofweek
test['hour'] = test['pickup_datetime'].dt.hour
test['minute'] = test['pickup_datetime'].dt.minute   

#converting the time to hourly format
test['Hour'] = test[['hour', 'minute']].apply(To_hour, axis = 1)

#Creating bins for "hour" column every hours
test['hour_bin'] = pd.cut(x = test['hour'], bins = range(-1,24), labels = range(24))

#Saving the column names to be dropped in an object
test = test.drop(["weekday", "pickup_datetime", "pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude", "hour", "minute", "Hour"], axis = 1)

#Converting data types
test['year'] = test['year'].astype('category')
test['month'] = test['month'].astype('category')
test['passenger_count'] = test['passenger_count'].astype('category')

#***********************************PREDICTING RESULTS***********************************

#Applying the best model(SVR) to whole data set
from sklearn.svm import SVR
Best_Model = SVR(kernel = 'rbf').fit(data.iloc[:, 0:5], data.iloc[:, 5])
Test_Predictions  = Best_Model.predict(test.iloc[:, 0:5])

#Actual predictions taking antilog
Actual_Predictions = np.exp(Test_Predictions)
