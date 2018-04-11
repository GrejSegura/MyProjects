# -*- coding: utf-8 -*-

import random
import numpy as np
import pandas as pd
import os
random.seed(23)

## set the directory
os.chdir(r'C:\Users\Grejell\AnacondaProjects\GulfoodDataMining')

# part 1
# ---------------------------------------------------------------------------#
## load data
r2016data = pd.read_csv(r".\data\raw\resp2016.csv", engine = "python")
r2017Data = pd.read_csv(r".\data\raw\resp2017.csv", engine = "python")

## load supporting data - codes to convert responses to text
code = pd.read_excel(r".\data\raw\codes.xlsx")

## retain features that are of use
r2017Data = r2017Data[['10 Digit Card Number',
       'region', 'Nationality', 'My Companys Main Activity',
       'My Companys Size', 'My companys annual procurement budget',
       'My Job Function',
       'I am interested in the following foodbeverage products',
       'DRY GOODS',
       'BEVERAGESSOFT DRINKS', 'BEVERAGESBEVERAGESHOT', 'DAIRY',
       'MEAT  POULTRY', 'CHILLED  FRESH FOOD', 'FROZEN FOOD', 'SPECIALITY',
       'My Main Objective to Visiting the Show',
       'Payment type','attendance']]
r2017Data = r2017Data.rename(columns = {"10 Digit Card Number":"id"})

## melt responses into one column
r2017Data = pd.melt(r2017Data, id_vars=['id', 'attendance'], value_vars = [
       'region','Nationality', 'My Companys Main Activity',
       'My Job Function',
       'I am interested in the following foodbeverage products', 
       'DRY GOODS',
       'BEVERAGESSOFT DRINKS', 'BEVERAGESBEVERAGESHOT', 'DAIRY',
       'MEAT  POULTRY', 'CHILLED  FRESH FOOD', 'FROZEN FOOD', 'SPECIALITY',
       'My Main Objective to Visiting the Show', 'Payment type'], value_name = 'value')

## replace strings "]" and split  -- id and attendance will be lost in the process
r2017Data = r2017Data.replace({']':'-17]'}, regex = True)
value = r2017Data['value'].str.split(']', expand = True)

## re-join id and attendanc
r2017Data = pd.concat([r2017Data[['id']], r2017Data[['attendance']], value], axis = 1)

## melt the responses again after splitting
r2017Data = pd.melt(r2017Data, id_vars=['id', 'attendance'], value_name = 'Code')
r2017Data = r2017Data.drop(['variable'], axis = 1)

## merge the data to codes to convert the "codes" into text
r2017Data = pd.merge(r2017Data, code, how = 'left', on = 'Code')

## retain only id, attendance and Decode
r2017Data = r2017Data[['id', 'attendance', 'Decode']].dropna()
r2017Data['value'] = 1

## create attendance dataframe to preserve id and attendance
attendance = r2017Data[['id','attendance', 'value']]
attendance = attendance.drop_duplicates()
attendance = attendance.pivot(index = 'id', columns = 'attendance', values = 'value')

## pivot the decode column -- will automatically dummify the columns but with NaN as 0
r2017Data = r2017Data.pivot_table(index = 'id', columns = 'Decode', values = 'value', aggfunc = 'max')

r2017Data = pd.concat([r2017Data, attendance], axis = 1)

## convert NaN to 0
r2017Data = r2017Data.fillna(0)

## "ns" column dropped and retain "att" only as label for attendance
r2017Data = r2017Data.drop(['ns'], axis = 1)


# part 2
# ---------------------------------------------------------------------------------------------------#
####################################### 2018 2018 2018 2018 2018 2018 ################################

## load data
r2018Data = pd.read_csv(r".\data\for_prediction\resp2018.csv", engine = "python")

## load supporting data - codes to convert responses to text
code = pd.read_excel(r".\data\raw\codes.xlsx")

r2018Data = r2018Data[['10 Digit Card Number', 'region', 'My Job Function',
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

r2018Data = r2018Data.rename(columns = {"10 Digit Card Number":"id"})

r2018Data = pd.melt(r2018Data, id_vars=['id', 'attendance'],
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
r2018Data = r2018Data.replace({']':'-18]'}, regex = True)
value = r2018Data['value'].str.split(']', expand = True)

r2018Data = pd.concat([r2018Data[['id']], r2018Data[['attendance']], value], axis = 1)

## melt the responses again after splitting
r2018Data = pd.melt(r2018Data, id_vars = ['id', 'attendance'], value_name = 'Code')
r2018Data = r2018Data.drop(['variable'], axis = 1)

## merge the data to codes to convert the "codes" into text
r2018Data = pd.merge(r2018Data, code, how = 'left', on = 'Code')

## retain only id, attendance and Decode
r2018Data = r2018Data[['id', 'attendance', 'Decode']].dropna()
r2018Data['value'] = 1

## create id and attendance dataframe to preserve id and attendance
getIDAttendance = r2018Data[['id','attendance', 'value']]
getIDAttendance = getIDAttendance.drop_duplicates()

## get id
id = getIDAttendance[['id']]

## get attendance
attendance = getIDAttendance.pivot(index = 'id', columns = 'attendance', values = 'value')
attendance = attendance[['att']]

## pivot the decode column -- will automatically dummify the columns but with NaN as 0
r2018Data = r2018Data.pivot_table(index = 'id', columns = 'Decode', values = 'value', aggfunc = 'max')

## load the variable names - the ones used in building the model
variableNames = pd.read_pickle(r'.\data\processed\variableNames.pkl')
names1 = variableNames.set_index('names').T.columns

existsName = r2018Data.columns.intersection(names1)
notExistName = names1.difference(r2018Data.columns)

## use only the variables that are existent in the model
#r2018Data = r2018Data[existsName]


## add the variables that are non-existent in the new data
#notExistData = pd.DataFrame(columns = notExistName)
#r2018Data = pd.concat([r2018Data, notExistData, attendance], axis = 1)
r2018Data = pd.concat([r2018Data, attendance], axis = 1)

## convert NaN to 0
r2018Data = r2018Data.fillna(0)
r2018Data = r2018Data.drop(['att'], axis = 1)

newData = pd.concat([r2017Data, r2018Data])
newData = newData.fillna(0)

newData.to_pickle(r".\data\processed\cleanData.pkl")