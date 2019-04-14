import pandas as pd
import numpy as np
import os
import matplotlib as plt

def load_drop_merge(data_path, dropped_columns=[], merge_columns=[]):
    data = pd.read_pickle(data_path)
    data = data.drop(dropped_columns, axis=1)
    data['merged_responses'] = data[merge_columns].apply(lambda x: ','.join(x.astype(str)), axis=1)
    return data
    
def with_website(data):
    if 'Website' in data.columns.values:
        data.loc[pd.isnull(data['Website']), 'with_website'] = 0
        data.loc[pd.notnull(data['Website']), 'with_website'] = 1
        return data
    else:
        print('There is no website column')

def days_to_go_reg(data, show_date):
    data['show_date'] = pd.to_datetime(show_date)
    if 'Date_Created' in data.columns.values:
        data['days_to_go_reg'] = pd.to_numeric(data['show_date'] - data['Date_Created'])
        return data
    else:
        print('There is no Date_Created column')
        
def days_to_go_reminded(data):
    if 'RemindedOn' in data.columns.values:
        data['days_to_go_reminded'] = pd.to_numeric(data['show_date'] - data['RemindedOn'])
        return data
    else:
        print('There is no RemindedOn column')

def dummify_responses(data, codes):
    columns_to_check = {'merged', 'key_id', 'Decode'}
    for cols in columns_to_check:
        if cols not in data.columns.values:
            print('There is no ',cols,' column')
            break
    else:
        responses = dara['merged'].str.split(',', expand=True)
        responses['key_id'] = data['key_id']
        responses = responses[['key_id', 'Decode']].dropna()
        responses['value'] = 1
        responses = responses.pivot_table(index = 'key_id', columns = 'Decode', values = 'value', aggfunc = 'max')
        return responses

def dummify_columns(data, columns):
    columns_to_check = {'key_id'}
    for cols in columns_to_check:
        if cols not in data.columns.values:
            print('There is no ',cols,' column')
            break
    data = data[['key_id', columns]]
    data_1 = pd.get_dummies(data[[columns]])
    data_1['key_id'] = data['key_id']
    data = data.drop([columns,'merged'], axis=1)
    data = data.merge(data_1, on='key_id', how = 'left')
    return data

def merge_all_data(data, responses, attendance):
    data = data.merge(responses, on='key_id', how = 'left')
    data = data.merge(attendance, on='key_id', how = 'left')
    return data