# -*- coding: utf-8 -*-

import random
import numpy as np
import pandas as pd
import os
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import GradientBoostingClassifier
random.seed(23)

## set the directory
os.chdir(r'C:\Users\Grejell\AnacondaProjects\GulfoodDataMining')

## load data -- this is the processed data output from data_prep
mainData = pd.read_pickle(r'.\data\processed\cleanData.pkl')

dataX = mainData.drop(['att'], axis = 1)
dataY = mainData['att']
X_train, x_test, Y_train, y_test = train_test_split(dataX, dataY, random_state = 0)

model = GradientBoostingClassifier(n_estimators = 800, max_depth = )
    #'''n_estimators = 600, learning_rate = 0.1, max_depth = 4, subsample = 1'''

parametersGrid = {
                  
                  #'n_estimators' : [400, 500, 600, 700, 800],
                  'max_depth':[4,5,6,7,8],
                  #'learning_rate': [0.01,0.1,0.5,1.0],
                  #'subsample':[1.0, .8, .7, .6],
                  #'min_samples_split':[0.5,1],
                  #'min_samples_leaf':[1.0,2.0],
                  #'min_weight_fraction_leaf':[0.0,0.3]
                  
                 }
grid = GridSearchCV(model, parametersGrid, cv = 5) ## 5-fold
grid.fit(X_train, Y_train)

print (grid.best_params_)
print(grid.best_score_)