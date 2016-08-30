# Predicting-Health-Expenditure
Predicting-Health-Expenditure

This project involves collection of 15 individual datasets from data.worldbank.org.
The data is then filtered and reshaped for certain number of years and NAs are replaced with the mean.
Some of the countries have all the fields missing for all the years, we used kNN technique to fill the missing values,
Outliers are also handled using kNN imputation
EDA is performed and correlations are identified.
Classification of the health expenditure is done on the training set
RandomForest model is trained on the training set and then tested on the test set.
Our model predicted the health expenditure of the country as low or medium or high or very high based on various variables.
The accuracy is around 90%
