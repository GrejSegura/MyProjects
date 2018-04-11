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

## load data for prediction
forPredictionData = pd.read_pickle(r'.\data\processed\forPredictionData.pkl')
id = forPredictionData.index
attendanceData = pd.read_pickle(r".\data\processed\attendanceData.pkl")
attendanceData = attendanceData.fillna(0)

## load model
model = pickle.load(open(r'.\pkl\GradientBoostingModel.sav', 'rb'))

predicted = model.predict(forPredictionData)
predicted = pd.DataFrame({"predicted": predicted}, index = id)
predicted = pd.concat([predicted, attendanceData], axis = 1)

confusion = pd.crosstab(predicted['predicted'], predicted['att'])
print(confusion)

precision = metrics.precision_score(predicted['att'], predicted['predicted'])
print(precision)
recall = metrics.recall_score(predicted['att'], predicted['predicted'])
print(recall)
accuracy = metrics.accuracy_score(predicted['predicted'], predicted['att'])
print(accuracy)