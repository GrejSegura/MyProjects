# -*- coding: utf-8 -*-

import random
import numpy as np
import pandas as pd
import os
random.seed(23)

## set the directory
os.chdir(r'C:\Users\Grejell\AnacondaProjects\GulfoodDataMining')

## load data
r2016data = pd.read_csv(r".\data\raw\resp2016.csv", engine = "python")
r2017data = pd.read_csv(r".\data\raw\resp2017.csv", engine = "python")

## load supporting data - codes to convert responses to text
code = pd.read_excel(r".\data\raw\codes.xlsx")

## retain features that are of use
r2017data = r2017data[['10 Digit Card Number',
       'region', 'Nationality', 'My Companys Main Activity',
       'My Companys Size', 'My companys annual procurement budget',
       'My Job Function',
       'I am interested in the following foodbeverage products',
       'DRY GOODS',
       'BEVERAGESSOFT DRINKS', 'BEVERAGESBEVERAGESHOT', 'DAIRY',
       'MEAT  POULTRY', 'CHILLED  FRESH FOOD', 'FROZEN FOOD', 'SPECIALITY',
       'My Main Objective to Visiting the Show',
       'Payment type','attendance']]
r2017data = r2017data.rename(columns = {"10 Digit Card Number":"id"})

## melt responses into one column
r2017data = pd.melt(r2017data, id_vars=['id', 'attendance'], value_vars = [
       'region','Nationality', 'My Companys Main Activity',
       'My Job Function',
       'I am interested in the following foodbeverage products', 
       'DRY GOODS',
       'BEVERAGESSOFT DRINKS', 'BEVERAGESBEVERAGESHOT', 'DAIRY',
       'MEAT  POULTRY', 'CHILLED  FRESH FOOD', 'FROZEN FOOD', 'SPECIALITY',
       'My Main Objective to Visiting the Show', 'Payment type'], value_name = 'value')

## replace strings "]" and split  -- id and attendance will be lost in the process
r2017data = r2017data.replace({']':'-17]'}, regex = True)
value = r2017data['value'].str.split(']', expand = True)

## re-join id and attendanc
r2017data = pd.concat([r2017data[['id']], r2017data[['attendance']], value], axis = 1)

## melt the responses again after splitting
r2017data = pd.melt(r2017data, id_vars=['id', 'attendance'], value_name = 'Code')
r2017data = r2017data.drop(['variable'], axis = 1)

## merge the data to codes to convert the "codes" into text
r2017data = pd.merge(r2017data, code, how = 'left', on = 'Code')

## retain only id, attendance and Decode
r2017data = r2017data[['id', 'attendance', 'Decode']].dropna()
r2017data['value'] = 1

## create attendance dataframe to preserve id and attendance
attendance = r2017data[['id','attendance', 'value']]
attendance = attendance.drop_duplicates()
attendance = attendance.pivot(index = 'id', columns = 'attendance', values = 'value')

## pivot the decode column -- will automatically dummify the columns but with NaN as 0
r2017data = r2017data.pivot_table(index = 'id', columns = 'Decode', values = 'value', aggfunc = 'max')

r2017data = pd.concat([r2017data, attendance], axis = 1)

## convert NaN to 0
r2017data = r2017data.fillna(0)

## "ns" column dropped and retain "att" only as label for attendance
r2017data = r2017data.drop(['ns'], axis = 1)

## save data as csv -- will be used in the next phase -- Modeling
r2017data.to_pickle(r".\data\processed\resp2017CleanData.pkl")