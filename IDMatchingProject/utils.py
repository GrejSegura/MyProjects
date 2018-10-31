# -*- coding: utf-8 -*-
"""
Created on Mon Sep 30, 2018

This is my python version of the guest record matching project.

The application will match the guests record in different years and show versions.

@author: Grejell
"""
import numpy as np
import pandas as pd
import os

os.chdir('C:/Users/Grejell/AnacondaProjects/IDMatchingProject/')

yearOneData = pd.read_csv("./dta/yearOne.csv", engine='python')
yearTwoData = pd.read_csv("./dta/yearTwo.csv", engine='python')
yearOneData = yearOneData[["10DigitCardNumber", "FirstName", "Surname", "Position", "Email", "CompanyName"]]
yearTwoData = yearTwoData[["10DigitCardNumber", "FirstName", "Surname", "Position", "Email", "CompanyName"]]

def function_1(df):
    columns = ["FirstName", "Surname", "Position", "Email", "CompanyName"]
    for column in columns:
        df[column] = df[column].str.replace(r'[^A-z0-9]', '').str.lower()
    return df

def function_2(df):
    df.replace(r'^\s*$', np.nan, regex=True, inplace = True)
    df['countNan'] = df.loc[:,'FirstName':'CompanyName'].count(axis=1)
    return df

def function_3(df):
    newCols = ['combo1', 'combo2', 'combo3', 'combo4', 'combo5', 'combo6', 'combo7', 'combo8', 'combo9', 'combo10', 'combo11']
    for cols in newCols:
        df.cols = pd.Series()
    df['Email'] = df['Email'].str.replace(r'_', '')
    df.loc[df.countNan > 2,'combo1' ] = df.FirstName+df.Surname+df.Position+df.Email+df.CompanyName
    df.loc[pd.notnull(df.FirstName) & pd.notnull(df.Surname) & pd.notnull(df.Position) & pd.notnull(df.CompanyName), 'combo2' ] = df.FirstName+df.Surname+df.Position+df.CompanyName
    df.loc[pd.notnull(df.FirstName) & pd.notnull(df.Surname) & pd.notnull(df.Email) & pd.notnull(df.CompanyName),'combo3' ] = df.FirstName+df.Surname+df.Email+df.CompanyName
    df.loc[pd.notnull(df.FirstName) & pd.notnull(df.Surname) & pd.notnull(df.CompanyName), 'combo4' ] = df.FirstName+df.Surname+df.CompanyName
    df.loc[pd.notnull(df.FirstName) & pd.notnull(df.Surname) & pd.notnull(df.Position), 'combo5' ] = df.FirstName+df.Surname+df.Position
    df.loc[pd.notnull(df.Position) & pd.notnull(df.Email) & pd.notnull(df.CompanyName), 'combo6' ] = df.Position+df.Email+df.CompanyName
    df.loc[pd.notnull(df.FirstName) & pd.notnull(df.Surname) & pd.notnull(df.Email), 'combo7' ] = df.FirstName+df.Surname+df.Email
    df['combo8'] = df.Email
    df.loc[pd.notnull(df.Position) & pd.notnull(df.Email), 'combo9' ] = df.Position+df.Email
    df.loc[pd.notnull(df.FirstName) & pd.notnull(df.Surname), 'combo10'] = df.FirstName+df.Surname
    df.loc[pd.notnull(df.Position) & pd.notnull(df.CompanyName), 'combo11'] = df.Position+df.CompanyName
    return df

def function_4(df1, df2):
    data = df2[['10DigitCardNumber', 'FirstName', 'Surname', 'Position', 'Email', 'CompanyName']]    
    combos = ['combo1', 'combo2', 'combo3', 'combo4', 'combo5', 'combo6', 'combo7', 'combo8', 'combo9', 'combo10', 'combo11']
    for combo in combos:
        mergeData = pd.merge(df2[['10DigitCardNumber', combo]], df1.loc[pd.notnull(df1[combo]),['10DigitCardNumber', combo]], how = 'left', on = combo)
        mergeData = mergeData[mergeData['10DigitCardNumber_y'].notnull()]
        #mergeData = mergeData.drop([combo], axis = 1)
        mergeData = mergeData.rename(columns = {'10DigitCardNumber_x': '10DigitCardNumber', '10DigitCardNumber_y': combo+'10DigitCardNumber'})
        data = pd.merge(data, mergeData, how = 'left', on = '10DigitCardNumber').drop_duplicates()
    return data

def function_5(data):
    data['match'] = pd.Series()
    data.loc[pd.notnull(data.combo1), 'match'] = data['combo110DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.notnull(data.combo2), 'match'] = data['combo210DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.notnull(data.combo3), 'match'] = data['combo310DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.isnull(data.combo3) & pd.notnull(data.combo4), 'match'] = data['combo410DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.isnull(data.combo3) & pd.isnull(data.combo4) & pd.notnull(data.combo5), 'match'] = data['combo510DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.isnull(data.combo3) & pd.isnull(data.combo4) & pd.isnull(data.combo5) & pd.notnull(data.combo6), 'match'] = data['combo610DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.isnull(data.combo3) & pd.isnull(data.combo4) & pd.isnull(data.combo5) & pd.isnull(data.combo6) & pd.notnull(data.combo7), 'match'] = data['combo710DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.isnull(data.combo3) & pd.isnull(data.combo4) & pd.isnull(data.combo5) & pd.isnull(data.combo6) & pd.isnull(data.combo7) & pd.notnull(data.combo8), 'match'] = data['combo810DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.isnull(data.combo3) & pd.isnull(data.combo4) & pd.isnull(data.combo5) & pd.isnull(data.combo6) & pd.isnull(data.combo7) & pd.isnull(data.combo8) & pd.notnull(data.combo9), 'match'] = data['combo910DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.isnull(data.combo3) & pd.isnull(data.combo4) & pd.isnull(data.combo5) & pd.isnull(data.combo6) & pd.isnull(data.combo7) & pd.isnull(data.combo8) & pd.isnull(data.combo9) & pd.notnull(data.combo10), 'match'] = data['combo1010DigitCardNumber']
    data.loc[pd.isnull(data.combo1) & pd.isnull(data.combo2) & pd.isnull(data.combo3) & pd.isnull(data.combo4) & pd.isnull(data.combo5) & pd.isnull(data.combo6) & pd.isnull(data.combo7) & pd.isnull(data.combo8) & pd.isnull(data.combo9) & pd.isnull(data.combo10) & pd.notnull(data.combo11), 'match'] = data['combo1110DigitCardNumber']
    data = data[['10DigitCardNumber', "FirstName", "Surname", "Position", "Email", "CompanyName", 'match']]
    data.to_csv(r'./dta/matchedFile.csv', index = False)
    return data