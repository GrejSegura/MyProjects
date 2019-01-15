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
os.chdir(r'C:\Users\Grejell\AnacondaProjects\DataMining')

## load data
newData = pd.read_csv(r".\data\for_prediction\resp2018.csv", engine = "python")

## load supporting data - codes to convert responses to text
code = pd.read_excel(r".\data\raw\codes.xlsx")

newData = newData[['10 Digit Card Number', 'region', 'My Job Function',
                   'Job Role', 'Gender', 'My Companys Main Activity',
                   'My Main Objective to Visiting the Show:',
                   'I am interested in the following food/beverage products:',
                   'I am interested in the following food/beverage products: DRY GOODS',
                   'I am interested in the following food/beverage products: BEVERAGES – SOFT DRINKS',
                   'I am interested in the following food/beverage products: BEVERAGES  BEVERAGES – HOT',
                   'I am interested in the following food/beverage products: DAIRY',
                   'I am interested in the following food/beverage products: MEAT & POULTRY',
                   'I am interested in the following food/beverage products: CHILLED & FRESH FOOD',
                   'I am interested in the following food/beverage products: FROZEN FOOD',
                   'I am interested in the following food/beverage products: SPECIALITY',
                   'I am interested in the following food/beverage products: MISCELLANEOUS',
                   'NATIONALITY - DWTC',
                   'PAYMENT STATUS', 'attendance']]

newData = newData.rename(columns = {"10 Digit Card Number":"id"})

newData = pd.melt(newData, id_vars=['id', 'attendance'],
                  value_vars = ['region', 'My Job Function',
                   'Job Role', 'Gender', 'My Companys Main Activity',
                   'My Main Objective to Visiting the Show:',
                   'I am interested in the following food/beverage products:',
                   'I am interested in the following food/beverage products: DRY GOODS',
                   'I am interested in the following food/beverage products: BEVERAGES – SOFT DRINKS',
                   'I am interested in the following food/beverage products: BEVERAGES  BEVERAGES – HOT',
                   'I am interested in the following food/beverage products: DAIRY',
                   'I am interested in the following food/beverage products: MEAT & POULTRY',
                   'I am interested in the following food/beverage products: CHILLED & FRESH FOOD',
                   'I am interested in the following food/beverage products: FROZEN FOOD',
                   'I am interested in the following food/beverage products: SPECIALITY',
                   'I am interested in the following food/beverage products: MISCELLANEOUS',
                   'NATIONALITY - DWTC','PAYMENT STATUS'],
                    value_name = 'value')

## replace strings "]" and split  -- id and attendance will be lost in the process
newData = newData.replace({']':'-18]'}, regex = True)
value = newData['value'].str.split(']', expand = True)

newData = pd.concat([newData[['id']], newData[['attendance']], value], axis = 1)

## melt the responses again after splitting
newData = pd.melt(newData, id_vars = ['id', 'attendance'], value_name = 'Code')
newData = newData.drop(['variable'], axis = 1)

## merge the data to codes to convert the "codes" into text
newData = pd.merge(newData, code, how = 'left', on = 'Code')

## retain only id, attendance and Decode
newData = newData[['id', 'attendance', 'Decode']].dropna()
newData['value'] = 1

## create id and attendance dataframe to preserve id and attendance
getIDAttendance = newData[['id','attendance', 'value']]
getIDAttendance = getIDAttendance.drop_duplicates()

## get id
id = getIDAttendance[['id']]

## get attendance
attendance = getIDAttendance.pivot(index = 'id', columns = 'attendance', values = 'value')
attendance = attendance[['att']]

## pivot the decode column -- will automatically dummify the columns but with NaN as 0
newData = newData.pivot_table(index = 'id', columns = 'Decode', values = 'value', aggfunc = 'max')

## load the variable names - the ones used in building the model
variableNames = pd.read_pickle(r'.\data\processed\variableNames.pkl')
names1 = variableNames.set_index('names').T.columns

existsName = newData.columns.intersection(names1)
notExistName = names1.difference(newData.columns)

## use only the variables that are existent in the model
newData = newData[existsName]


## add the variables that are non-existent in the new data
notExistData = pd.DataFrame(columns = notExistName)
newData = pd.concat([newData, notExistData, attendance], axis = 1)

## convert NaN to 0
newData = newData.fillna(0)
newData = newData.drop(['att'], axis = 1)

newData.to_pickle(r".\data\processed\forPredictionData.pkl")
attendance.to_pickle(r".\data\processed\attendanceData.pkl")
