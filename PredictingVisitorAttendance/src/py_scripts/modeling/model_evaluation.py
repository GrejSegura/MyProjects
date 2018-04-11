# -*- coding: utf-8 -*-

import random
import numpy as np
import pandas as pd
import os
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.ensemble import GradientBoostingClassifier
import pickle
random.seed(23)

## set the directory
os.chdir(r'C:\Users\Grejell\AnacondaProjects\GulfoodDataMining')

## load data
# mainData = pd.read_pickle(r'.\data\processed\cleanData.pkl') --------> use this to use the 2017data only as training set
mainData = pd.read_pickle(r'.\data\processed\cleanData.pkl') # --------> use this to use 2017data AND 2018data as training set

## split data into train and test
dataX = mainData.drop(['att'], axis = 1)
dataY = mainData['att']
X_train, x_test, Y_train, y_test = train_test_split(dataX, dataY, random_state = 0)

## try fitting a gradient boosting model
model = GradientBoostingClassifier(n_estimators = 600, learning_rate = 0.1, max_depth = 4, subsample = 1)
model = model.fit(X_train, Y_train)

#print accuracy_score on training data
score = model.score(X_train, Y_train)
print(score)

## also try predicting the test data
predicted = model.predict(x_test)

#print accuracy_score(y_test, predicted) for test data
accuracy = metrics.accuracy_score(predicted, y_test)
print(accuracy)

## check precision - recall for test data
preds = pd.DataFrame({'true': y_test, 'predicted': predicted})
confusion = pd.crosstab(preds['predicted'], preds['true'])
print(confusion)
precision = metrics.precision_score(y_test, predicted)
print(precision)
recall = metrics.recall_score(y_test, predicted)
print(recall)

## save the model - this will be used in the deployment part to generate new predictions
pickle.dump(model, open(r'.\pkl\GradientBoostingModel.sav', 'wb'))