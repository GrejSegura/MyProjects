
import numpy as np
import pandas as pd
import sklearn
import os

dir = os.getcwd()
os.chdir('C:/Users/Grejell/Documents/Data Analysis/GITEX 2017/Recommender/GITEX_recommender/dta')

data = pd.read_csv('gate_scans.csv', sep = ",")

data = data.rename(columns = {'Card Number' : 'Card.Number'})

length = np.vectorize(len) ## count the number of character in a string

data['Len'] = length(data['Card.Number']) ## making a new column
data['Len'].describe()

data = data.sort_values(by = ['Card.Number', 'Date', 'Time'])
data = data[data['Len'] == 10]

data['sequence'] = data.groupby(['Card.Number', 'Show']).cumcount()

data = data[data['sequence'] == 0]

data = data.drop(['Len', 'sequence'], axis = 1)

data['Date'] = pd.to_datetime(data.Date)

data['day'] = data['Date'].dt.weekday_name

data['sequence'] = data.groupby(['Card.Number']).cumcount()

varnames = pd.Series(range(data['sequence'].max()))
varnames = 'v' + varnames.astype(str)

j = range(data['sequence'].max())

for i in j:
        data[varnames[i]] = np.where(data['sequence'] <= i, data['Show'].shift(data.groupby(['Card.Number']).max() - i), "")
        data[varnames[i]] = [data['Show'].shift(data.groupby(['Card.Number']).max() - i) if x <= i else "" for x in data['sequence']]
        
        
