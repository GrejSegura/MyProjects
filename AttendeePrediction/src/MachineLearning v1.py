
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
from sklearn import svm
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.neural_network import MLPClassifier
from sklearn.neighbors import KNeighborsClassifier

dataTrain = pd.read_csv("C:/Users/Grejell/Documents/Data Mining/BIG 5/BIG5_Data_Mining_2016/dta/train.csv")
dataTest = pd.read_csv("C:/Users/Grejell/Documents/Data Mining/BIG 5/BIG5_Data_Mining_2016/dta/test.csv")
cols = dataTrain.columns
dataX = dataTrain.iloc[:, 1:]
dataY = dataTrain.iloc[:, 0]

## logistic Regression
model1 = LogisticRegression()
model1 = model.fit(dataX, dataY)
model1.score(dataX, dataY)
predicted1 = model.predict(dataTest.iloc[:, 1:])
print metrics.accuracy_score(dataTest.iloc[:, 0], predicted1)

## SVM
model2 = svm.SVC(gamma = 0.01, C = 100)
model2 = model2.fit(dataX, dataY)
model2.score(dataX, dataY)
predicted2 = model2.predict(dataTest.iloc[:, 1:])
print metrics.accuracy_score(dataTest.iloc[:, 0], predicted2)

## Naive Bayes
model3 = GaussianNB()
model3 = model3.fit(dataX, dataY)
model3.score(dataX, dataY)

predicted3 = model3.predict(dataTest.iloc[:, 1:])
print metrics.accuracy_score(dataTest.iloc[:, 0], predicted3)


## Random Forest
model4 = RandomForestClassifier()
model4 = model4.fit(dataX, dataY)
model4.score(dataX, dataY)

predicted4 = model4.predict(dataTest.iloc[:, 1:])
print metrics.accuracy_score(dataTest.iloc[:, 0], predicted4)


## AdaBoost
model5 = AdaBoostClassifier()
model5 = model5.fit(dataX, dataY)
model5.score(dataX, dataY)

predicted5 = model5.predict(dataTest.iloc[:, 1:])
print metrics.accuracy_score(dataTest.iloc[:, 0], predicted5)


## Gradient Boosting
model6 = GradientBoostingClassifier()
model6 = model6.fit(dataX, dataY)
model6.score(dataX, dataY)

predicted6 = model6.predict(dataTest.iloc[:, 1:])
print metrics.accuracy_score(dataTest.iloc[:, 0], predicted6)


## MultiLayer Perceptron
model7 = MLPClassifier()
model7 = model7.fit(dataX, dataY)
model7.score(dataX, dataY)

predicted7 = model7.predict(dataTest.iloc[:, 1:])
print metrics.accuracy_score(dataTest.iloc[:, 0], predicted7)


## K nearest neighbors
model8 = KNeighborsClassifier()
model8 = model8.fit(dataX, dataY)
model8.score(dataX, dataY)

predicted8 = model8.predict(dataTest.iloc[:, 1:])
print metrics.accuracy_score(dataTest.iloc[:, 0], predicted8)
