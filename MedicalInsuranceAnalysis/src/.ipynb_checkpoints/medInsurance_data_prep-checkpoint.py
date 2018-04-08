# -*- coding: utf-8 -*-
"""
Created on Mon Apr  2 13:30:11 2018

@author: Grejell
"""

import sys
sys.modules[__name__].__dict__.clear()

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
from sklearn import linear_model
from sklearn.model_selection import GridSearchCV
from sklearn import metrics
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
%matplotlib inline

os.chdir("C:\\Users\Grejell\AnacondaProjects\MedicalInsuranceAnalysis")

data = pd.read_csv(".\dta\insurance.csv")


## Part 1 -- Exploring the Data ##
## check the data
data.head
data.describe()

data.groupby(by = 'sex').size()
data.groupby(by = 'smoker').size()
data.groupby(by = 'region').size()

## check the distribution of charges
distPlot = sns.distplot(data['charges'])

## check charges vs features
boxPlot1 = sns.boxplot(x = "sex", y = "charges", data = data)
boxPlot2 = sns.boxplot(x = "smoker", y = "charges", data = data)
boxPlot3 = sns.boxplot(x = "sex", y = "charges", hue = "smoker", data = data)

boxPlot4 = sns.boxplot(x = "region", y = "charges", data = data)

pairPlot = sns.pairplot(data)


## Part 2 -- Pre-processing the data ##
## Dummify sex, smoker and region
scaleMinMax = MinMaxScaler()
data[["age", "bmi", "children"]] = scaleMinMax.fit_transform(data[["age", "bmi", "children"]])
data = pd.get_dummies(data, prefix = ["sex", "smoker", "region"])
## retain sex = male, smoker = yes, and remove 1 region = northeast to avoid dummytrap
data = data.drop(data.columns[[4,6,11]], axis = 1)

## creat train and test data ##
dataX = data.drop(data.columns[[3]], axis = 1)
dataY = data.iloc[:, 3]
X_train, x_test, Y_train, y_test = train_test_split(dataX, dataY, random_state = 0)

## fit model ##


model = linear_model.ElasticNet()

parametersGrid = {"max_iter": [10, 20, 50, 80],
                  "alpha": [0.001, 0.01, 0.02, 0.5, 0.08, 1],
                  "l1_ratio": np.arange(0.0, 1.0, 0.1)}
grid = GridSearchCV(model, parametersGrid, cv = 5)
grid.fit(X_train, Y_train)

print (grid.best_params_)
print(grid.best_score_)

## fit the final model ##
ElasticNetModel = linear_model.ElasticNet(alpha = 0.01, l1_ratio = 0.9, max_iter = 20, normalize = False)
ElasticNetModel = model2.fit(X_train, Y_train)

## predict using test data ##
elasticPred = ElasticNetModel.predict(x_test)
mseElastic = metrics.mean_squared_error(y_test, elasticPred)
rmseElastic = mseElastic**(1/2)
print(mseElastic)
print(rmseElastic)

## check the R-Squared
r2_Elastic = r2_score(y_test, elasticPred)
print(r2_Elastic)



## try Linear Regression ##
linearModel = linear_model.LinearRegression()
linear = linearModel.fit(X_train, Y_train)
linearPred = linear.predict(x_test)
mseLinear = metrics.mean_squared_error(y_test, linearPred)
rmseLinear = mseLinear**(1/2)
print(mseLinear)
print(rmseLinear)

r2_linear = r2_score(y_test, linearPred)
print(r2_linear)


lassoModel = linear_model.Lasso(alpha = 0.1)
lasso = lassoModel.fit(X_train, Y_train)
lassoPred = lasso.predict(x_test)
mseLasso = metrics.mean_squared_error(y_test, linearPred)














## faceted plot f or residuals ##
residualPlot = sns.residplot(elasticPred, y_test, lowess = True, color="g")
